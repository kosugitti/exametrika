#' @title Latent Rank Analysis
#' @description
#' A general function for estimating Latent Rank Analysis across different response types.
#' This function automatically dispatches to the appropriate method based on the response type:
#'
#' \itemize{
#'   \item For binary data (\code{LRA.binary}): Analysis using either SOM or GTM method
#'   \item For ordinal data (\code{LRA.ordinal}): Analysis using the GTM method with category thresholds
#'   \item For rated data (\code{LRA.rated}): Analysis using the GTM method with rating categories
#' }
#'
#' Latent Rank Analysis identifies underlying rank structures in test data and assigns
#' examinees to these ranks based on their response patterns.
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#' it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return
#' A list of class "exametrika" and the specific subclass (e.g., "LRA", "LRAordinal", "LRArated")
#' containing the following common elements:
#' \describe{
#'  \item{msg}{A character string indicating the model type. }
#'  \item{testlength}{Length of the test (number of items).}
#'  \item{nobs}{Sample size (number of rows in the dataset).}
#'  \item{Nrank}{Number of latent ranks specified.}
#'  \item{N_Cycle}{Number of EM algorithm iterations performed.}
#'  \item{converge}{Logical value indicating whether the algorithm converged within maxiter iterations}
#'  \item{TRP}{Test Reference Profile vector showing expected scores at each rank.}
#'  \item{LRD}{Latent Rank Distribution vector showing the number of examinees at each rank.}
#'  \item{RMD}{Rank Membership Distribution vector showing the sum of probabilities for each rank.}
#'  \item{Students}{Rank Membership Profile matrix showing the posterior probabilities of
#'       examinees belonging to each rank, along with their estimated ranks and odds ratios.}
#'  \item{ItemFitIndices}{Fit indices for each item. See also \code{\link{ItemFit}}.}
#'  \item{TestFitIndices}{Overall fit indices for the test. See also \code{\link{TestFit}}.}
#' }
#'
#' Each subclass returns additional specific elements, detailed in their respective documentation.
#'
#' @seealso \code{\link{plot.exametrika}} for visualizing LRA results.
#' @export
#'
LRA <- function(U, ...) {
  UseMethod("LRA")
}

#' @rdname LRA
#' @param na Values to be treated as missing values.
#' @param Z Missing indicator matrix of type matrix or data.frame. 1 indicates observed values, 0 indicates missing values.
#' @param w Item weight vector.
#' @export
LRA.default <- function(U, na = NULL, Z = NULL, w = NULL, ...) {
  if (inherits(U, "exametrika")) {
    if (U$response.type == "binary") {
      return(LRA.binary(U, ...))
    } else if (U$response.type == "ordinal") {
      return(LRA.ordinal(U, ...))
    } else if (U$response.type == "rated") {
      return(LRA.rated(U, ...))
    } else {
      response_type_error(U$response.type, "LRA")
    }
  }

  U <- dataFormat(U, na = na, Z = Z, w = w)
  LRA(U, ...)
}

#' @rdname LRA
#' @section Binary Data Method:
#' \code{LRA.binary} analyzes dichotomous (0/1) response data using either Self-Organizing Maps (SOM)
#' or Gaussian Topographic Mapping (GTM).
#'
#' @param nrank Number of latent ranks to estimate. Must be between 2 and 20.
#' @param method For binary data only. Either "SOM" (Self-Organizing Maps) or "GTM" (Gaussian Topographic Mapping). Default is "GTM".
#' @param mic Logical; if TRUE, forces Item Reference Profiles to be monotonically increasing. Default is FALSE.
#' @param maxiter Maximum number of iterations for estimation. Default is 100.
#' @param BIC.check For binary data with SOM method only. If TRUE, convergence is checked using BIC values. Default is FALSE.
#' @param seed For binary data with SOM method only. Random seed for reproducibility.
#' @param verbose Logical; if TRUE, displays detailed progress during estimation. Default is TRUE.
#' @param beta1 Beta distribution parameter 1 for prior density of rank reference matrix (GTM method only). Default is 1.
#' @param beta2 Beta distribution parameter 2 for prior density of rank reference matrix (GTM method only). Default is 1.
#' @param conf Confirmatory IRP matrix (items x nrank) for test equating.
#'   Same format as the IRP output. Non-NA values are fixed throughout estimation,
#'   NA values are freely estimated. Fixed values must be in the open interval (0, 1).
#'   When row names are present, items are matched by label; otherwise by position.
#'   Default is NULL (fully exploratory).
#'
#' @return
#' For binary data (\code{LRA.binary}), the returned list additionally includes:
#' \describe{
#'  \item{IRP}{Item Reference Profile matrix showing the probability of correct response for each item across different ranks.}
#'  \item{IRPIndex}{Item Response Profile indices including the location parameters B and Beta,
#'    slope parameters A and Alpha, and monotonicity indices C and Gamma.}
#' }
#'
#' @examples
#' \donttest{
#' # Binary data example
#' # Fit a Latent Rank Analysis model with 6 ranks to binary data
#' result.LRA <- LRA(J15S500, nrank = 6)
#'
#' # Display the first few rows of student rank membership profiles
#' head(result.LRA$Students)
#'
#' # Plot Item Reference Profiles (IRP) for the first 6 items
#' plot(result.LRA, type = "IRP", items = 1:6, nc = 2, nr = 3)
#'
#' # Plot Test Reference Profile (TRP) showing expected scores at each rank
#' plot(result.LRA, type = "TRP")
#' }
#'
#' @importFrom stats runif
#' @export
LRA.binary <- function(U,
                       nrank = 2,
                       method = "GTM",
                       mic = FALSE,
                       maxiter = 100,
                       BIC.check = FALSE,
                       seed = NULL,
                       verbose = FALSE,
                       beta1 = 1,
                       beta2 = 1,
                       conf = NULL, ...) {
  tmp <- U
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  samplesize <- NROW(tmp$U)
  const <- exp(-testlength)
  ncls <- nrank

  if (method != "SOM" & method != "GTM") {
    stop("The method must be selected as either SOM or GTM.")
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  # Validate and align confirmatory IRP matrix
  if (!is.null(conf)) {
    conf <- validate_conf(conf, ncls, tmp$ItemLabel)
  }

  if (method == "SOM") {
    fit <- somclus(tmp$U, tmp$Z, ncls,
      mic = mic, maxiter = maxiter,
      BIC.check = BIC.check, seed = seed, verbose = verbose,
      conf = conf
    )
  } else {
    # GTM.
    Filter <- create_filter_matrix(ncls)

    fit <- emclus(tmp$U, tmp$Z,
      ncls = ncls,
      Fil = Filter, beta1, beta2, mic = mic,
      conf = conf
    )
  }

  ## Returns
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)
  #### Class Information
  TRP <- fit$classRefMat %*% tmp$w
  bMax <- matrix(rep(apply(fit$postDist, 1, max), ncls), ncol = ncls)
  clsNum <- apply(fit$postDist, 1, which.max)
  cls01 <- sign(fit$postDist - bMax) + 1
  LRD <- colSums(cls01)
  RMD <- colSums(fit$postDist)
  StudentClass <- fit$postDist
  RU <- ifelse(clsNum + 1 > ncls, NA, clsNum + 1)
  RD <- ifelse(clsNum - 1 < 1, NA, clsNum - 1)
  RUO <- StudentClass[cbind(1:nobs, RU)] / StudentClass[cbind(1:nobs, clsNum)]
  RDO <- StudentClass[cbind(1:nobs, RD)] / StudentClass[cbind(1:nobs, clsNum)]
  StudentClass <- cbind(StudentClass, clsNum, RUO, RDO)
  colnames(StudentClass) <- c(
    paste("Membership", 1:ncls), "Estimate",
    "Rank-Up Odds", "Rank-Down Odds"
  )
  rownames(StudentClass) <- tmp$ID
  ### Item Information
  IRP <- t(fit$classRefMat)
  colnames(IRP) <- paste0("IRP", 1:ncls)
  rownames(IRP) <- tmp$ItemLabel

  IRPIndex <- IRPindex(IRP)

  if (sum(IRPIndex$C) == 0) {
    if (verbose) {
      message("Strongly ordinal alignment condition was satisfied.")
    }
  }

  ### Model Fit
  # each Items
  ell_A <- item_log_lik(tmp$U, tmp$Z, fit$postDist, fit$classRefMat)
  if (method == "GTM") {
    nparam <- sum(diag(Filter))
  } else {
    nparam <- ncls
  }
  FitIndices <- ItemFit(tmp$U, tmp$Z, ell_A, nparam)

  ret <- structure(list(
    method = method,
    mic = mic,
    msg = "Rank",
    converge = fit$converge,
    testlength = testlength,
    nobs = nobs,
    n_rank = ncls, # New naming convention
    n_cycle = fit$iter, # New naming convention
    TRP = as.vector(TRP),
    LRD = as.vector(LRD),
    RMD = as.vector(RMD),
    Students = StudentClass,
    IRP = IRP,
    IRPIndex = IRPIndex,
    ItemFitIndices = FitIndices$item,
    TestFitIndices = FitIndices$test,
    log_lik = FitIndices$test$model_log_like,
    # Deprecated fields (for backward compatibility)
    Nrank = ncls,
    N_Cycle = fit$iter
  ), class = c("exametrika", "LRA"))
  return(ret)
}
