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
#'  \item{testlength}{Length of the test (number of items).}
#'  \item{nobs}{Sample size (number of rows in the dataset).}
#'  \item{Nrank}{Number of latent ranks specified.}
#'  \item{N_Cycle}{Number of EM algorithm iterations performed.}
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
LRA.default <- function(U, na = NULL, Z = Z, w = w, ...) {
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
  LRA(U)
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
                       verbose = TRUE, ...) {
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


  if (method == "SOM") {
    somt <- 0
    alpha1 <- 1
    alphaT <- 0.01
    sigma1 <- 1
    sigmaT <- 0.12

    alpha_list <- ((maxiter - 1:maxiter) * alpha1 + (1:maxiter - 1) * alphaT) / (maxiter - 1)
    sigma_list <- ((maxiter - 1:maxiter) * sigma1 + (1:maxiter - 1) * sigmaT) / (maxiter - 1)

    kappa1 <- 0.01
    kappaT <- 0.0001

    kappa_list <- ((maxiter - 1:maxiter) * kappa1 + (1:maxiter - 1) * kappaT) / (maxiter - 1)

    prior_list <- rep(1 / ncls, ncls)

    r_list <- seq(-ncls + 1, ncls - 1)
    hhhmat <- array(NA, c(maxiter, length(r_list)))
    for (t in 1:maxiter) {
      hhhmat[t, ] <- alpha_list[t] * ncls / samplesize * exp(-(r_list)^2 / (2 * ncls^2 * sigma_list[t]^2))
    }

    clsRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)
    RefMat <- t(clsRefMat)
    oldBIC <- 1e5
    ### SOM iteration
    FLG <- TRUE
    while (FLG) {
      somt <- somt + 1

      if (somt <= maxiter) {
        h_count <- somt
      } else {
        h_cout <- maxiter
      }

      loglike <- 0

      if (is.null(seed)) {
        set.seed(sum(tmp$U) + somt)
      } else {
        set.seed(seed)
      }

      is <- order(runif(samplesize, 1, 100))

      for (s in 1:samplesize) {
        ss <- is[s]
        mlrank <- tmp$U[ss, ] %*% log(RefMat + const) + (1 - tmp$U[ss, ]) %*% log(1 - RefMat + const) + log(prior_list)
        winner <- which.max(mlrank)
        loglike <- loglike + mlrank[winner]
        hhh <- matrix(rep(hhhmat[h_count, (ncls + 1 - winner):(2 * ncls - winner)], testlength),
          nrow = testlength, byrow = T
        )
        RefMat <- RefMat + hhh * (tmp$U[ss, ] - RefMat)
        prior_list <- prior_list + (kappa_list[h_count] / ncls)
        prior_list[winner] <- prior_list[winner] - kappa_list[h_count]
        prior_list[prior_list > 1] <- 1
        prior_list[prior_list < const] <- const
      }
      if (mic) {
        RefMat <- t(apply(RefMat, 1, sort))
      }
      llmat <- tmp$U %*% t(log(t(RefMat) + const)) + (tmp$Z * (1 - tmp$U)) %*%
        t(log(1 - t(RefMat) + const))
      expllmat <- exp(llmat)
      postdist <- expllmat / rowSums(expllmat)
      item_ell <- itemEll(tmp$U, tmp$Z, postdist, t(RefMat))
      if (BIC.check) {
        if (somt > maxiter * 10) {
          message("\nReached ten times the maximum number of iterations.")
          FLG <- FALSE
          break
        }
        FI <- ItemFit(tmp$U, tmp$Z, item_ell, ncls)
        diff <- abs(oldBIC - FI$test$BIC)
        oldsBIC <- FI$test$BIC
        if (diff < 1e-4) {
          message("\nConverged before reaching maximum iterations.")
          FLG <- FALSE
          break
        }
      } else {
        if (somt == maxiter) {
          FLG <- FALSE
        }
      }
    }

    fit <- list(
      iter = somt,
      postDist = postdist,
      classRefMat = t(RefMat)
    )
  } else {
    # GTM.
    f0 <- ifelse(ncls < 5, 1.05 - 0.05 * ncls,
      ifelse(ncls < 10, 1.00 - 0.04 * ncls,
        0.80 - 0.02 * ncls
      )
    )
    f1 <- diag(0, ncls)
    f1[row(f1) == col(f1) - 1] <- (1 - f0) / 2
    Filter <- diag(rep(f0, ncls)) + t(f1) + f1
    Filter[, 1] <- Filter[, 1] / sum(Filter[, 1])
    Filter[, ncls] <- Filter[, ncls] / sum(Filter[, ncls])

    fit <- emclus(tmp$U, tmp$Z,
      ncls = ncls,
      Fil = Filter, 1, 1, mic = mic
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
    message("Strongly ordinal alignment condition was satisfied.")
  }

  ### Model Fit
  # each Items
  ell_A <- itemEll(tmp$U, tmp$Z, fit$postDist, fit$classRefMat)
  if (method == "GTM") {
    nparam <- sum(diag(Filter))
  } else {
    nparam <- ncls
  }
  FitIndices <- ItemFit(tmp$U, tmp$Z, ell_A, nparam)

  ret <- structure(list(
    method = method,
    mic = mic,
    testlength = testlength,
    nobs = nobs,
    Nrank = ncls,
    N_Cycle = fit$iter,
    TRP = as.vector(TRP),
    LRD = as.vector(LRD),
    RMD = as.vector(RMD),
    Students = StudentClass,
    IRP = IRP,
    IRPIndex = IRPIndex,
    ItemFitIndices = FitIndices$item,
    TestFitIndices = FitIndices$test
  ), class = c("exametrika", "LRA"))
  return(ret)
}
