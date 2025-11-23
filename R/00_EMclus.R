#' @title emclus funciton
#' @description
#' This function is used for both LCA (Latent Class Analysis) and
#' LRA (Latent Rank Analysis). LCA is considered a special case of LRA,
#' in which the filtering matrix is reduced to the identity matrix.
#' This function takes a dataset, the number of classes, and a filtering
#' matrix as input and returns the latent rank.
#' @param U response matrix U of the examData class.
#' @param Z missing indicator matrix Z of the examData class.
#' @param Fil Filter matrix
#' @param ncls number of latent class
#' @param beta1 beta distribution parameter1 as prior density of rank reference matrix
#' @param beta2 beta distribution parameter2 as prior density of rank reference matrix
#' @param maxiter Maximum number of iterations.
#' @param mic Monotonic increasing IRP option
#' @param verbose verbose output Flag. default is FALSE
#' @noRd

emclus <- function(U, Z, ncls, Fil, beta1, beta2, maxiter = 100, mic = FALSE, verbose = FALSE) {
  # Initialize
  testlength <- NCOL(U)
  const <- exp(-testlength)
  test_log_lik <- -1 / const
  old_test_log_lik <- -2 / const
  item_log_lik <- rep(test_log_lik / testlength, testlength)
  classRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)

  ## EM algorithm
  emt <- 0
  converge <- TRUE
  FLG <- TRUE

  while (FLG) {
    emt <- emt + 1
    old_test_log_lik <- test_log_lik

    llmat <- U %*% t(log(classRefMat + const)) + (Z * (1 - U)) %*% t(log(1 - classRefMat + const))
    exp_llmat <- exp(llmat)
    postDist <- exp_llmat / rowSums(exp_llmat)

    smoothPost <- postDist %*% Fil
    correct_cls <- t(smoothPost) %*% U
    incorrect_cls <- t(smoothPost) %*% (Z * (1 - U))

    old_classRefMat <- classRefMat
    classRefMat <- (correct_cls + beta1 - 1) / (correct_cls + incorrect_cls + beta1 + beta2 - 2)
    classRefMat <- pmax(pmin(classRefMat, 1 - const), const)
    if (mic) {
      classRefMat <- apply(classRefMat, 2, sort)
    }

    item_log_lik <- colSums(correct_cls * log(classRefMat + const) + incorrect_cls * log(1 - classRefMat + const))
    test_log_lik <- sum(item_log_lik)
    if (verbose) {
      message(
        sprintf(
          "\r%-80s",
          paste0(
            "iter ", emt, " log_lik ", format(test_log_lik, digits = 6)
          )
        ),
        appendLF = FALSE
      )
    }
    if (test_log_lik - old_test_log_lik <= 0) {
      classRefMat <- old_classRefMat
      FLG <- FALSE
    }
    if ((test_log_lik - old_test_log_lik) <= 0.0001 * abs(old_test_log_lik)) {
      FLG <- FALSE
    }
    if (emt == maxiter) {
      message("\nReached the maximum number of iterations.")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
      converge <- FALSE
      FLG <- FALSE
    }
  }

  ret <- list(
    iter = emt,
    converge = converge,
    postDist = postDist,
    classRefMat = classRefMat
  )
  return(ret)
}


#' @title calc final item log-likelihood
#' @description
#' Using the original data, class membership matrix, and class reference matrix,
#'  the log-likelihood for each item is calculated.
#' @param U response matrix U of the examData class.
#' @param Z missing indicator matrix Z of the examData class.
#' @param postDist class membership matrix
#' @param classRefMat class reference matrix
#' @noRd

item_log_lik <- function(U, Z, postDist, classRefMat) {
  const <- exp(-NCOL(U))
  correct_cls <- t(postDist) %*% U
  incorrect_cls <- t(postDist) %*% (Z * (1 - U))
  item_ll <- colSums(correct_cls * log(classRefMat + const) + incorrect_cls * log(1 - classRefMat + const))
  return(item_ll)
}
