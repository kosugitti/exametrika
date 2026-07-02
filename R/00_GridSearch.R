#' @title Stop with a diagnostic message if every grid cell failed
#' @description Shared by GridSearch()'s Biclustering and LCA/LRA branches.
#' @param ret Matrix (Biclustering) or vector (LCA/LRA) of fit indices, with
#' `NA` marking cells that failed to converge or errored.
#' @noRd
stop_if_all_grid_failed <- function(ret) {
  if (all(is.na(ret))) {
    message("Error: All parameter combinations failed to converge.")
    message("Grid search cannot find optimal parameters.")
    message("Consider:")
    message("  - Increasing maxiter parameter")
    message("  - Using different parameter ranges")
    message("  - Checking data quality")
    stop("Grid search terminated due to convergence failure in all combinations.")
  }
}

#' @title Report grid cells that failed to converge
#' @description Shared by GridSearch()'s Biclustering and LCA/LRA branches.
#' @param failed_settings List of named-list parameter settings (e.g.
#' `list(ncls=, nfld=)` or `list(ncls=)`) that failed to converge.
#' @noRd
report_failed_grid_settings <- function(failed_settings) {
  if (length(failed_settings) == 0) {
    return(invisible(NULL))
  }
  message("\nWarning: The following settings may have failed to converge:")
  for (setting in failed_settings) {
    parts <- vapply(
      names(setting),
      function(nm) sprintf("%s=%d", nm, setting[[nm]]),
      character(1)
    )
    message("  ", paste(parts, collapse = ", "))
  }
}

#' @title Select the first tied optimum in a grid-search fit-index array
#' @description
#' Shared by GridSearch()'s Biclustering (2-D, `ret` is a matrix) and LCA/LRA
#' (1-D, `ret` is a vector) branches. `which(..., arr.ind = TRUE)` returns
#' one match per tie; this deterministically takes the first one instead of
#' letting ties silently combine mismatched positions.
#' @param ret Matrix or vector of fit indices (may contain `NA`)
#' @param minimize `TRUE` to select the minimum (e.g. BIC), `FALSE` for the
#' maximum (e.g. log-likelihood)
#' @return For a vector `ret`, a single integer index. For a matrix `ret`, a
#' named integer vector with `row`/`col` entries.
#' @noRd
select_optimal_grid_index <- function(ret, minimize) {
  target <- if (minimize) min(ret, na.rm = TRUE) else max(ret, na.rm = TRUE)
  idx <- which(ret == target, arr.ind = TRUE)
  if (is.matrix(idx)) idx[1, , drop = TRUE] else idx[1]
}

#' Grid Search for Optimal Parameters
#'
#' Performs a grid search to find optimal parameters for different
#' analysis methods.
#' Supports Biclustering, LCA (Latent Class Analysis),
#' and LRA (Latent Rank Analysis).
#'
#' @param obj Input data matrix or object to be analyzed
#' @param max_ncls Maximum number of classes/clusters to test (default: 10)
#' @param max_nfld Maximum number of fields to test for
#' Biclustering (default: 10)
#' @param fun Function name to use for analysis.
#' Options: "Biclustering", "LCA", "LRA" (default: "Biclustering")
#' @param index Fit index to optimize from TestFitIndices returned by
#'  each function. Valid options: "BIC" (default), "AIC", "CAIC",
#'  "model_log_like", "model_Chi_sq", "RMSEA", "NFI", "RFI", "IFI",
#'  "TLI", "CFI". Aliases are also accepted: "loglik", "log_lik",
#'  "LogLik", "LL" (all map to "model_log_like"), "Chi_sq", "chi_sq"
#'  (map to "model_Chi_sq").
#' @param verbose Logical; if TRUE, displays detailed progress messages during grid search. Default is FALSE.
#' @param ... Additional arguments passed to the analysis function
#'
#' @return A list containing:
#'   For Biclustering:
#'   \item{index_matrix}{Matrix of fit indices for each
#'    ncls/nfld combination}
#'   \item{optimal_ncls}{Optimal number of classes/clusters}
#'   \item{optimal_nfld}{Optimal number of fields}
#'   \item{optimal_result}{Analysis result using optimal parameters}
#'   \item{failed_settings}{List of parameter combinations that failed to converge}
#'
#'   For LCA/LRA:
#'   \item{index_vec}{Vector of fit indices for each ncls}
#'   \item{optimal_ncls}{Optimal number of classes/clusters}
#'   \item{optimal_result}{Analysis result using optimal parameters}
#'   \item{failed_settings}{List of parameter combinations that failed to converge}
#'
#' @examples
#' \dontrun{
#' # Grid search for Biclustering
#' result <- GridSearch(J35S515, max_ncls = 5, max_nfld = 5)
#'
#' # Grid search for LCA
#' result <- GridSearch(J35S515, max_ncls = 8, fun = "LCA")
#' }
#'
#' @export
#'
GridSearch <- function(
  obj,
  max_ncls = 10,
  max_nfld = 10,
  fun = "Biclustering",
  index = "BIC",
  verbose = FALSE,
  ...
) {
  obj <- dataFormat(obj)
  # Handle different data types for testlength calculation
  if (!is.null(obj$U)) {
    testlength <- NCOL(obj$U) # binary data
    nobs <- NROW(obj$U)
  } else {
    testlength <- NCOL(obj$Q) # ordinal data
    nobs <- NROW(obj$Q)
  }

  # Index alias mapping (normalize user-friendly names to internal field names)
  index_aliases <- c(
    "loglik" = "model_log_like",
    "log_lik" = "model_log_like",
    "LogLik" = "model_log_like",
    "LL" = "model_log_like",
    "Chi_sq" = "model_Chi_sq",
    "chi_sq" = "model_Chi_sq"
  )
  if (index %in% names(index_aliases)) {
    original_index <- index
    index <- index_aliases[[index]]
    message(sprintf("Note: index '%s' mapped to '%s'", original_index, index))
  }

  # Valid indices and their optimization direction
  minimize_indices <- c("model_Chi_sq", "RMSEA", "AIC", "CAIC", "BIC")
  maximize_indices <- c("model_log_like", "NFI", "RFI", "IFI", "TLI", "CFI")
  valid_indices <- c(minimize_indices, maximize_indices)

  if (!(index %in% valid_indices)) {
    stop(
      "Unknown index: ", index, ". Please specify one of: ",
      paste(valid_indices, collapse = ", "),
      "\nAliases also accepted: ",
      paste(names(index_aliases), collapse = ", ")
    )
  }

  # Guard against max_ncls/max_nfld < 2: without this, `2:max_ncls`
  # below would silently evaluate as a descending sequence (e.g. `2:1`)
  # and test values the caller never asked for.
  if (max_ncls < 2) {
    stop("max_ncls must be at least 2.")
  }
  if (fun == "Biclustering" && max_nfld < 2) {
    stop("max_nfld must be at least 2.")
  }

  # ------------------------------------------ Biclustering
  if (fun == "Biclustering") {
    if (max_ncls >= nobs) {
      max_ncls <- nobs - 1
    }
    if (max_nfld >= testlength) {
      max_nfld <- testlength - 1
    }
    ret <- matrix(NA, nrow = length(2:max_ncls), ncol = length(2:max_nfld))
    colnames(ret) <- paste("nfld", 2:max_nfld)
    rownames(ret) <- paste("ncls", 2:max_ncls)
    args_list <- list()
    extra_args <- list(...)
    failed_settings <- list()

    for (ncls in 2:max_ncls) {
      if (verbose) {
        message(sprintf("ncls = %d: ", ncls), appendLF = FALSE)
      }
      for (nfld in 2:max_nfld) {
        # Display progress
        if (verbose) {
          message(sprintf("nfld=%d ", nfld), appendLF = FALSE)
        }

        args_list <- c(
          list(
            U = obj,
            ncls = ncls,
            nfld = nfld,
            verbose = FALSE
          ),
          extra_args
        )
        # Tolerate per-cell errors: a single Biclustering failure (e.g.
        # empty-cluster edge cases at grid corners) should not abort the
        # entire grid search. Treat errors the same as non-convergence.
        result <- tryCatch(do.call(fun, args_list),
          error = function(e) NULL
        )
        if (is.null(result) ||
          (!is.null(result$converge) && isFALSE(result$converge))) {
          ret[ncls - 1, nfld - 1] <- NA
          failed_settings <- append(
            failed_settings,
            list(list(ncls = ncls, nfld = nfld))
          )
        } else {
          ret[ncls - 1, nfld - 1] <- result$TestFitIndices[[index]]
        }
      }
      if (verbose) {
        message("") # New line after each ncls
      }
    }

    # Check if all parameters failed to converge
    stop_if_all_grid_failed(ret)

    optimal_idx <- select_optimal_grid_index(ret, minimize = index %in% minimize_indices)
    optimal_ncls <- optimal_idx["row"] + 1
    optimal_nfld <- optimal_idx["col"] + 1

    # Display warning for failed convergence
    report_failed_grid_settings(failed_settings)

    message(paste(
      "\nOptimal ncls/nrank is ",
      optimal_ncls, "and",
      "Optimal nfld is",
      optimal_nfld
    ))

    # Run analysis with optimal parameters
    message("Running analysis with optimal parameters...")
    optimal_args <- c(
      list(
        U = obj,
        ncls = optimal_ncls,
        nfld = optimal_nfld
      ),
      extra_args
    )
    optimal_result <- do.call(fun, optimal_args)

    ret_list <- structure(list(
      index_matrix = ret,
      optimal_ncls = optimal_ncls,
      optimal_nfld = optimal_nfld,
      optimal_result = optimal_result,
      failed_settings = failed_settings
    ), class = c("exametrika", "GridSearch"))
    # ------------------------------------------ LCA / LRA
  } else if (fun == "LCA" || fun == "LRA") {
    if (max_ncls >= nobs) {
      max_ncls <- nobs - 1
    }
    ret <- vector(length = length(2:max_ncls))
    names(ret) <- paste0("ncls", 2:max_ncls)
    extra_args <- list(...)
    failed_settings <- list()

    for (i in seq_along(2:max_ncls)) {
      ncls <- (2:max_ncls)[i]
      # Display progress
      if (verbose) {
        message(sprintf("Executing ncls = %d...", ncls))
      }

      args_list <- c(list(
        U = obj,
        ncls = ncls,
        verbose = FALSE
      ), extra_args)
      # Tolerate per-cell errors (see Biclustering branch above for rationale).
      result <- tryCatch(do.call(fun, args_list),
        error = function(e) NULL
      )
      if (is.null(result) ||
        (!is.null(result$converge) && isFALSE(result$converge))) {
        ret[i] <- NA
        failed_settings <- append(
          failed_settings,
          list(list(ncls = ncls))
        )
      } else {
        ret[i] <- result$TestFitIndices[[index]]
      }
    }

    # Check if all parameters failed to converge
    stop_if_all_grid_failed(ret)

    optimal_ncls <- select_optimal_grid_index(ret, minimize = index %in% minimize_indices) + 1

    # Display warning for failed convergence
    report_failed_grid_settings(failed_settings)

    message("\nOptimal ncls/nrank is ", optimal_ncls)

    # Run analysis with optimal parameters
    message("Running analysis with optimal parameters...")
    optimal_args <- c(
      list(
        U = obj,
        ncls = optimal_ncls
      ),
      extra_args
    )
    optimal_result <- do.call(fun, optimal_args)

    ret_list <- structure(list(
      index_vec = ret,
      optimal_ncls = optimal_ncls,
      optimal_result = optimal_result,
      failed_settings = failed_settings
    ), class = c("exametrika", "GridSearch"))
  }
  return(ret_list)
}
