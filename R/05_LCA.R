#' @title Latent Class Analysis
#' @description
#' Performs Latent Class Analysis (LCA) on binary response data using the Expectation-Maximization (EM) algorithm.
#' LCA identifies unobserved (latent) subgroups of examinees with similar response patterns,
#' and estimates both the class characteristics and individual membership probabilities.
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#' it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param ncls Number of latent classes to identify (between 2 and 20). Default is 2.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#' observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @param maxiter Maximum number of EM algorithm iterations. Default is 100.
#' @param verbose Logical; if TRUE, displays progress during estimation. Default is TRUE.
#'
#' @return
#' An object of class "exametrika" and "LCA" containing:
#' \describe{
#'  \item{msg}{A character string indicating the model type. }
#'  \item{testlength}{Length of the test (number of items).}
#'  \item{nobs}{Sample size (number of rows in the dataset).}
#'  \item{Nclass}{Number of latent classes specified.}
#'  \item{N_Cycle}{Number of EM algorithm iterations performed.}
#'  \item{TRP}{Test Reference Profile vector showing expected scores for each latent class.
#'    Calculated as the column sum of the estimated class reference matrix.}
#'  \item{LCD}{Latent Class Distribution vector showing the number of examinees assigned to each latent class.}
#'  \item{CMD}{Class Membership Distribution vector showing the sum of membership probabilities for each latent class.}
#'  \item{Students}{Class Membership Profile matrix showing the posterior probability of each examinee
#'    belonging to each latent class. The last column ("Estimate") indicates the most likely class assignment.}
#'  \item{IRP}{Item Reference Profile matrix where each row represents an item and each column
#'    represents a latent class. Values indicate the probability of a correct response
#'    for members of that class.}
#'  \item{ItemFitIndices}{Fit indices for each item. See also \code{\link{ItemFit}}.}
#'  \item{TestFitIndices}{Overall fit indices for the test. See also \code{\link{TestFit}}.}
#' }
#'
#' @details
#' Latent Class Analysis is a statistical method for identifying unobserved subgroups within
#' a population based on observed response patterns. It assumes that examinees belong to one
#' of several distinct latent classes, and that the probability of a correct response to each
#' item depends on class membership.
#'
#' The algorithm proceeds by:
#' 1. Initializing class reference probabilities
#' 2. Computing posterior class membership probabilities for each examinee (E-step)
#' 3. Re-estimating class reference probabilities based on these memberships (M-step)
#' 4. Iterating until convergence or reaching the maximum number of iterations
#'
#' Unlike Item Response Theory (IRT), LCA treats latent variables as categorical rather than
#' continuous, identifying distinct profiles rather than positions on a continuum.
#'
#' @references
#' Goodman, L. A. (1974). Exploratory latent structure analysis using both identifiable and
#' unidentifiable models. Biometrika, 61(2), 215-231.
#'
#' Lazarsfeld, P. F., & Henry, N. W. (1968). Latent structure analysis.
#' Boston: Houghton Mifflin.
#'
#' @examples
#' \donttest{
#' # Fit a Latent Class Analysis model with 5 classes to the sample dataset
#' result.LCA <- LCA(J15S500, ncls = 5)
#'
#' # Display the first few rows of student class membership probabilities
#' head(result.LCA$Students)
#'
#' # Plot Item Response Profiles (IRP) for items 1-6 in a 2x3 grid
#' # Shows probability of correct response for each item across classes
#' plot(result.LCA, type = "IRP", items = 1:6, nc = 2, nr = 3)
#'
#' # Plot Class Membership Probabilities (CMP) for students 1-9 in a 3x3 grid
#' # Shows probability distribution of class membership for each student
#' plot(result.LCA, type = "CMP", students = 1:9, nc = 3, nr = 3)
#'
#' # Plot Test Response Profile (TRP) showing expected scores for each class
#' plot(result.LCA, type = "TRP")
#'
#' # Plot Latent Class Distribution (LCD) showing class sizes
#' plot(result.LCA, type = "LCD")
#'
#' # Compare models with different numbers of classes
#' # (In practice, you might try more class counts)
#' lca2 <- LCA(J15S500, ncls = 2)
#' lca3 <- LCA(J15S500, ncls = 3)
#' lca4 <- LCA(J15S500, ncls = 4)
#' lca5 <- LCA(J15S500, ncls = 5)
#'
#' # Compare BIC values to select optimal number of classes
#' # (Lower BIC indicates better fit)
#' data.frame(
#'   Classes = 2:5,
#'   BIC = c(
#'     lca2$TestFitIndices$BIC,
#'     lca3$TestFitIndices$BIC,
#'     lca4$TestFitIndices$BIC,
#'     lca5$TestFitIndices$BIC
#'   )
#' )
#' }
#'
#' @export
LCA <- function(U, ncls = 2, na = NULL, Z = NULL, w = NULL, maxiter = 100, verbose = TRUE) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (U$response.type != "binary") {
    response_type_error(U$response.type, "LCA")
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  fit <- emclus(tmp$U, tmp$Z, ncls,
    Fil = diag(rep(1, ncls)),
    beta1 = 1, beta2 = 1, maxiter,
    mic = FALSE,
    verbose = verbose
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
  rownames(StudentClass) <- tmp$ID
  ### Item Information
  IRP <- t(fit$classRefMat)
  colnames(IRP) <- paste0("IRP", 1:ncls)

  ### Model Fit
  # each Items
  ell_A <- itemEll(tmp$U, tmp$Z, fit$postDist, fit$classRefMat)
  FitIndices <- ItemFit(tmp$U, tmp$Z, ell_A, ncls)

  ret <- structure(list(
    msg <- "Class",
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
  ), class = c("exametrika", "LCA"))
  return(ret)
}
