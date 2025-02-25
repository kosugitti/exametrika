#' @title Latent Rank Analysis
#' @description
#' A function for estimating Latent Rank Analysis. This is a generic function that
#' dispatches to specific methods depending on the response type of the data:
#' \itemize{
#'   \item For binary data (LRA.binary): Estimation using either SOM or GTM method
#'   \item For ordinal/rated data (LRA.ordinal): Estimation using GTM method
#'   \item For nominal data (LRA.nominal): Estimation using GTM method
#' }
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param ... Additional arguments passed to methods.
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{Nrank}{number of rnaks you set}
#'  \item{N_Cycle}{Number of EM algorithm iterations}
#'  \item{TRP}{Test Reference Profile matrix. The TRP is the column sum vector of estimated class reference matrix,
#' \eqn{\hat{\Pi}_c}}
#'  \item{LRD}{Latent Rank Distribution table.see also [plot.exametrika]}
#'  \item{RMD}{Rank Membership Distribution table. see also [plot.exametrika]}
#'  \item{ItemFitIndices}{Fit index for each item.See also [ItemFit]}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#' }
#'
#' @export
LRA <- function(U, ...) {
  UseMethod("LRA")
}

#' @rdname LRA
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export
LRA.default <- function(U, na = NULL, Z = Z, w = w, ...) {
  if (inherits(U, "exametrika")) {
    if (U$response.type == "binary") {
      return(LRA.binary(U, ...))
    } else if (U$response.type == "ordinal") {
      return(LRA.ordinal(U, ...))
    } else if (U$response.type == "rated"){
      return(LRA.rated(U, ...))
    }else{
      response_type_error(U$response.type, "LRA")
    }
  }

  U <- dataFormat(U, na = na, Z = Z, w = w)
  LRA(U)
}

#' @rdname LRA
#' @param nrank number of latent rank
#' @param method Specify either "SOM" or "GTM".
#' @param mic Monotonic increasing IRP option. The default is FALSE.
#' @param maxiter Maximum number of iterations. default is 100.
#' @param BIC.check During estimation with SOM
#' @param seed random seed for SOM
#' @param verbose verbose output Flag. default is TRUE
#' @return
#' \describe{
#'  \item{Students}{Rank Membership Profile matrix.The s-th row vector of \eqn{\hat{M}_c}, \eqn{\hat{m}_c}, is the
#' rank membership profile of Student s, namely the posterior probability distribution representing the student's
#' belonging to the respective latent ranks. It also includes the rank with the maximum estimated membership probability,
#' as well as the rank-up odds and rank-down odds.}
#'  \item{IRP}{Item Reference Profile matrix.The IRP of item j is the j-th row vector in the class reference matrix,
#' \eqn{\hat{\pi}_c}}
#'  \item{IRPIndex}{The IRP information includes the item location parameters B and Beta,
#'  the slope parameters A and Alpha, and the monotonicity indices C and Gamma.}
#' }
#' @examples
#' \donttest{
#' # Fit a Latent Rank Analysis model with 6 ranks to the sample dataset
#' result.LRA <- LRA(J15S500, nrank = 6)
#'
#' # Display the first few rows of student rank membership profiles
#' # This shows posterior probabilities of students belonging to each rank
#' head(result.LRA$Students)
#'
#' # Plot Item Reference Profiles (IRP) for items 1-6 in a 2x3 grid
#' # Shows the probability of correct response for each rank
#' plot(result.LRA, type = "IRP", items = 1:6, nc = 2, nr = 3)
#'
#' # Plot Rank Membership Profiles (RMP) for students 1-9 in a 3x3 grid
#' # Shows the posterior probability distribution of rank membership for each student
#' plot(result.LRA, type = "RMP", students = 1:9, nc = 3, nr = 3)
#'
#' # Plot Test Reference Profile (TRP)
#' # Shows the column sum vector of estimated rank reference matrix
#' plot(result.LRA, type = "TRP")
#'
#' # Plot Latent Rank Distribution (LRD)
#' # Shows the distribution of students across different ranks
#' plot(result.LRA, type = "LRD")
#' }
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
  rownames(StudentClass) <- tmp$IDas.
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
