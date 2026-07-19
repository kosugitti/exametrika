#' @title Weighted pool-adjacent-violators algorithm (PAVA)
#' @description
#' Weighted isotonic (monotone non-decreasing) regression by the
#' pool-adjacent-violators algorithm. Pools adjacent blocks whose fitted
#' values violate the ordering, using the block-mean weighted by \code{w},
#' and backtracks after each merge. Returns the fitted (monotone) vector and
#' the number of resulting blocks, the latter serving as the shape-restricted
#' degrees of freedom (Meyer & Woodroofe 2000).
#' @param y numeric vector to be monotonized (non-decreasing).
#' @param w non-negative weight vector, same length as \code{y}.
#' @return A list with \code{fitted} (monotone vector, same length as \code{y})
#'   and \code{nblock} (number of distinct level blocks).
#' @noRd
pava_up <- function(y, w = rep(1, length(y))) {
  val <- y
  wt <- w
  len <- rep(1, length(y))
  i <- 1
  while (i < length(val)) {
    if (val[i] > val[i + 1]) {
      val[i] <- (val[i] * wt[i] + val[i + 1] * wt[i + 1]) / (wt[i] + wt[i + 1])
      wt[i] <- wt[i] + wt[i + 1]
      len[i] <- len[i] + len[i + 1]
      val <- val[-(i + 1)]
      wt <- wt[-(i + 1)]
      len <- len[-(i + 1)]
      if (i > 1) {
        i <- i - 1
      }
    } else {
      i <- i + 1
    }
  }
  list(fitted = rep(val, len), nblock = length(val))
}


#' @title Isotonic (order-restricted) EM for binary Latent Rank Analysis
#' @description
#' EM estimation for binary LRA under the order restriction that each item's
#' correct-response probability is monotonically non-decreasing across ranks.
#' Unlike the GTM core (\code{emclus}), no filter matrix is applied in the
#' E-step; the rank ordering is instead imposed in the M-step by a weighted
#' PAVA down each item column (weights are the per-rank expected counts). For
#' the default flat prior (\code{beta1 = beta2 = 1}) this weighted PAVA is the
#' exact order-restricted MLE (Ayer et al. 1955); with an informative prior it
#' applies the same weighted pooling to the MAP proportions.
#' @param U response matrix U of the examData class.
#' @param Z missing indicator matrix Z of the examData class.
#' @param ncls number of latent ranks.
#' @param beta1 beta distribution parameter1 (prior successes) for the rank reference matrix.
#' @param beta2 beta distribution parameter2 (prior failures) for the rank reference matrix.
#' @param maxiter Maximum number of iterations.
#' @param mic retained for interface compatibility; the isotonic core is always
#'   monotone, so this argument has no effect.
#' @param verbose verbose output flag. default is FALSE.
#' @param conf Confirmatory IRP matrix (ncls x testlength). Non-NA values are
#'   fixed (applied after the PAVA step), NA values are freely estimated.
#'   NULL means fully exploratory.
#' @return A list with \code{iter}, \code{converge}, \code{postDist},
#'   \code{classRefMat}, and \code{item_nparam} (per-item block count = the
#'   shape-restricted degrees of freedom), matching the \code{emclus} structure
#'   plus \code{item_nparam}.
#' @noRd
emclus_isotonic <- function(U, Z, ncls, beta1, beta2, maxiter = 100, mic = FALSE,
                            verbose = FALSE, conf = NULL) {
  # Initialize
  testlength <- NCOL(U)
  const <- exp(-testlength)
  test_log_lik <- -1 / const
  old_test_log_lik <- -2 / const
  classRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)

  # Prepare confirmatory constraints
  if (!is.null(conf)) {
    fixed <- !is.na(conf)
    classRefMat[fixed] <- conf[fixed]
  }

  ## EM algorithm
  emt <- 0
  converge <- TRUE
  FLG <- TRUE

  while (FLG) {
    emt <- emt + 1
    old_test_log_lik <- test_log_lik

    # E-step (no filter smoothing)
    llmat <- U %*% t(log(classRefMat + const)) + (Z * (1 - U)) %*% t(log(1 - classRefMat + const))
    exp_llmat <- exp(llmat - apply(llmat, 1, max))
    postDist <- exp_llmat / rowSums(exp_llmat)

    # M-step (posterior used directly; no filter)
    correct_cls <- t(postDist) %*% U
    incorrect_cls <- t(postDist) %*% (Z * (1 - U))
    old_classRefMat <- classRefMat
    classRefMat <- (correct_cls + beta1 - 1) / (correct_cls + incorrect_cls + beta1 + beta2 - 2)
    classRefMat <- pmax(pmin(classRefMat, 1 - const), const)

    # Order restriction: weighted PAVA down each item column across ranks
    nmat <- correct_cls + incorrect_cls
    for (j in 1:testlength) {
      classRefMat[, j] <- pava_up(classRefMat[, j], nmat[, j])$fitted
    }

    # Apply confirmatory constraints after PAVA (fixed cells win)
    if (!is.null(conf)) {
      classRefMat[fixed] <- conf[fixed]
    }

    item_log_lik <- colSums(correct_cls * log(classRefMat + const) + incorrect_cls * log(1 - classRefMat + const))
    test_log_lik <- sum(item_log_lik)
    if (verbose) {
      message(
        sprintf(
          "\n%-80s",
          paste0("iter ", emt, " log_lik ", format(test_log_lik, digits = 6))
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

  # Shape-restricted df per item = number of PAVA blocks in the final matrix
  item_nparam <- apply(classRefMat, 2, function(col) length(unique(round(col, 10))))

  ret <- list(
    iter = emt,
    converge = converge,
    postDist = postDist,
    classRefMat = classRefMat,
    item_nparam = item_nparam
  )
  return(ret)
}
