#' @title Latent Class Analysis
#' @description
#' A function for estimating LCA using the EM algorithm.
#' @param U U is either a data class of Exametrika, or raw data. When raw data is given,
#' it is converted to the Exametrika class with the [dataFormat] function.
#' @param ncls number of latent class
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param maxiter Maximum number of iterations.
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{Nclass}{number of classes you set}
#'  \item{TRP}{Test Reference Profile matrix. The TRP is the column sum vector of estimated class reference matrix,
#' \eqn{\hat{\Pi}_c}}
#'  \item{LCD}{Latent Class Distribution table.see also [plot.Exametrika]}
#'  \item{CMD}{Class Membership Distribution table. see also [plot.Exametrika]}
#'  \item{Students}{Class Membership Profile matrix.The s-th row vector of \eqn{\hat{M}_c}, \eqn{\hat{m}_c}, is the
#' class membership profile of Student s, namely the posterior probability distribution representing the student's
#' belonging to the respective latent classes. The last column indicates the latent class estimate.}
#'  \item{IRP}{Item Reference Profile matrix.The IRP of item j is the j-th row vector in the class reference matrix,
#' \eqn{\hat{\pi}_c}}
#'  \item{ItemFitIndices}{Fit index for each item.See also [ItemFit]}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#' }
#' @export
#'

LCA <- function(U, ncls = 2, na = NULL, Z = NULL, w = NULL, maxiter = 100) {
  # data format
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  fit <- emclus(tmp$U, tmp$Z, ncls,
    Fil = diag(rep(1, ncls)),
    beta1 = 1, beta2 = 1, maxiter,
    mic = FALSE
  )

  ## Returns
  #### Class Information
  TRP <- fit$classRefMat %*% tmp$w
  bMax <- matrix(rep(apply(fit$postDist, 1, max), ncls), ncol = ncls)
  clsNum <- apply(fit$postDist, 1, which.max)
  cls01 <- sign(fit$postDist - bMax) + 1
  LCD <- colSums(cls01)
  CMD <- colSums(fit$postDist)
  StudentClass <- cbind(fit$postDist, clsNum)
  colnames(StudentClass) <- c(paste("Membership", 1:ncls), "Estimate")
  ### Item Information
  IRP <- t(fit$classRefMat)
  colnames(IRP) <- paste0("IRP", 1:ncls)

  ### Model Fit
  # each Items
  ell_A <- itemEll(tmp$U, tmp$Z, fit$postDist, fit$classRefMat)
  FitIndices <- ItemFit(tmp$U, tmp$Z, ell_A, ncls)

  ret <- structure(list(
    testlength = testlength <- NCOL(tmp$U),
    nobs = NROW(tmp$U),
    Nclass = ncls,
    N_Cycle = fit$iter,
    TRP = as.vector(TRP),
    LCD = as.vector(LCD),
    CMD = as.vector(CMD),
    Students = StudentClass,
    IRP = IRP,
    ItemFitIndices = FitIndices$item,
    TestFitIndices = FitIndices$test
  ), class = c("Exametrika", "LCA"))
  return(ret)
}
