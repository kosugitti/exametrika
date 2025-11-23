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
#'  each function. Options: "AIC", "BIC", etc. (default: "BIC")
#' @param verbose Logical; if TRUE, displays detailed progress messages during grid search. Default is TRUE.
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
#' result <- grid_serch(data_matrix, max_ncls = 5, max_nfld = 5)
#'
#' # Grid search for LCA
#' result <- grid_serch(data_matrix, max_ncls = 8, fun = "LCA")
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
  verbose = TRUE,
  ...
) {
  obj <- dataFormat(obj)
  testlength <- NCOL(obj$U)
  nobs <- NROW(obj$U)
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
        result <- do.call(fun, args_list)

        # Check convergence
        if (!is.null(result$converge) && result$converge == FALSE) {
          # Mark as failed and set to NA
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
    if (all(is.na(ret))) {
      message("Error: All parameter combinations failed to converge.")
      message("Grid search cannot find optimal parameters.")
      message("Consider:")
      message("  - Increasing maxiter parameter")
      message("  - Using different parameter ranges")
      message("  - Checking data quality")
      stop("Grid search terminated due to convergence failure in all combinations.")
    }

    # Indices where smaller is better
    minimize_indices <- c("model_log_like", "model_Chi_sq", "RMSEA", "AIC", "CAIC", "BIC")
    # Indices where larger is better
    maximize_indices <- c("NFI", "RFI", "IFI", "TLI", "CFI")

    if (index %in% minimize_indices) {
      optimal_idx <- which(ret == min(ret, na.rm = TRUE), arr.ind = TRUE)
    } else if (index %in% maximize_indices) {
      optimal_idx <- which(ret == max(ret, na.rm = TRUE), arr.ind = TRUE)
    } else {
      stop(
        "Unknown index: ", index, ". Please specify one of: ",
        paste(c(minimize_indices, maximize_indices), collapse = ", ")
      )
    }
    optimal_ncls <- optimal_idx[1] + 1
    optimal_nfld <- optimal_idx[2] + 1

    # Display warning for failed convergence
    if (length(failed_settings) > 0) {
      message("\nWarning: The following settings may have failed to converge:")
      for (i in 1:length(failed_settings)) {
        setting <- failed_settings[[i]]
        message(sprintf("  ncls=%d, nfld=%d", setting$ncls, setting$nfld))
      }
    }

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

    ret_list <- list(
      index_matrix = ret,
      optimal_ncls = optimal_ncls,
      optimal_nfld = optimal_nfld,
      optimal_result = optimal_result,
      failed_settings = failed_settings
    )
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
      result <- do.call(fun, args_list)

      # Check convergence
      if (!is.null(result$converge) && result$converge == FALSE) {
        # Mark as failed and set to NA
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
    if (all(is.na(ret))) {
      message("Error: All parameter combinations failed to converge.")
      message("Grid search cannot find optimal parameters.")
      message("Consider:")
      message("  - Increasing maxiter parameter")
      message("  - Using different parameter ranges")
      message("  - Checking data quality")
      stop("Grid search terminated due to convergence failure in all combinations.")
    }

    # Indices where smaller is better
    minimize_indices <- c("model_log_like", "model_Chi_sq", "RMSEA", "AIC", "CAIC", "BIC")
    # Indices where larger is better
    maximize_indices <- c("NFI", "RFI", "IFI", "TLI", "CFI")

    if (index %in% minimize_indices) {
      optimal_ncls <- which.min(ret) + 1
    } else if (index %in% maximize_indices) {
      optimal_ncls <- which.max(ret) + 1
    } else {
      stop(
        "Unknown index: ", index, ". Please specify one of: ",
        paste(c(minimize_indices, maximize_indices), collapse = ", ")
      )
    }

    # Display warning for failed convergence
    if (length(failed_settings) > 0) {
      message("\nWarning: The following settings may have failed to converge:")
      for (i in 1:length(failed_settings)) {
        setting <- failed_settings[[i]]
        message(sprintf("  ncls=%d", setting$ncls))
      }
    }

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

    ret_list <- list(
      index_vec = ret,
      optimal_ncls = optimal_ncls,
      optimal_result = optimal_result,
      failed_settings = failed_settings
    )
  }
  return(ret_list)
}
