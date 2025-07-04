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
#' @param ... Additional arguments passed to the analysis function
#'
#' @return A list containing:
#'   For Biclustering:
#'   \item{index_matrix}{Matrix of fit indices for each
#'    ncls/nfld combination}
#'   \item{optimal_ncls}{Optimal number of classes/clusters}
#'   \item{optimal_nfld}{Optimal number of fields}
#'   \item{optimal_result}{Analysis result using optimal parameters}
#'
#'   For LCA/LRA:
#'   \item{index_vec}{Vector of fit indices for each ncls}
#'   \item{optimal_ncls}{Optimal number of classes/clusters}
#'   \item{optimal_result}{Analysis result using optimal parameters}
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
    ...) {
  if (fun == "Biclustering") {
    ret <- matrix(NA, nrow = length(2:max_ncls), ncol = length(2:max_nfld))
    colnames(ret) <- paste("nfld", 2:max_nfld)
    rownames(ret) <- paste("ncls", 2:max_ncls)
    args_list <- list()
    extra_args <- list(...)
    for (ncls in 2:max_ncls) {
      for (nfld in 2:max_nfld) {
        args_list <- c(
          list(
            U = obj,
            ncls = ncls,
            nfld = nfld
          ),
          extra_args
        )
        result <- do.call(fun, args_list)
        ret[ncls - 1, nfld - 1] <- result$TestFitIndices[[index]]
      }
    }
    if (index %in% c("AIC", "BIC")) {
      optimal_idx <- which(ret == min(ret, na.rm = TRUE), arr.ind = TRUE)
    } else {
      optimal_idx <- which(ret == max(ret, na.rm = TRUE), arr.ind = TRUE)
    }
    optimal_ncls <- optimal_idx[1] + 1
    optimal_nfld <- optimal_idx[2] + 1
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
      optimal_result = optimal_result
    )
  } else if (fun == "LCA" || fun == "LRA") {
    ret <- vector(length = length(2:max_ncls))
    names(ret) <- paste0("ncls", 2:max_ncls)
    extra_args <- list(...)
    for (i in seq_along(2:max_ncls)) {
      ncls <- (2:max_ncls)[i]
      args_list <- c(list(
        U = obj,
        ncls = ncls
      ), extra_args)
      result <- do.call(fun, args_list)
      ret[i] <- result$TestFitIndices[[index]]
    }
    if (index %in% c("AIC", "BIC")) {
      optimal_ncls <- which.min(ret) + 1
    } else {
      optimal_ncls <- which.max(ret) + 1
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
      optimal_result = optimal_result
    )
  }
  return(ret_list)
}
