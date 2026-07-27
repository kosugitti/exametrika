#' @title EM engine for nominal (unordered polytomous) latent class analysis
#' @description
#' Shared estimation core for \code{LCA.nominal}. Fits a finite mixture of
#' product-multinomial distributions: every latent class carries an independent
#' category distribution for each item. No ordering is imposed, either on the
#' classes or on the response categories, which is what distinguishes this
#' engine from the ordinal/rank machinery in \code{\link{emclus}} (whose
#' \code{Fil} filter smooths neighbouring ranks) and from the order-restricted
#' estimator in \code{R/00_isotonic_CORE.R}.
#'
#' Ragged category counts are supported: \code{ncat} may differ across items,
#' and slots beyond an item's own category count are held at zero throughout.
#'
#' @param Q Category response matrix (respondents x items), already formatted
#'   by \code{\link{dataFormat}}. Codes are integers in \code{1:ncat[j]}.
#' @param Z Missing indicator matrix; 1 marks an observed response, 0 a missing one.
#' @param ncls Number of latent classes.
#' @param ncat Integer vector of category counts, one entry per item.
#' @param alpha Dirichlet prior parameter for the category profiles. Default 1
#'   (no smoothing; the M-step is then the plain multinomial MLE).
#' @param maxiter Maximum number of EM iterations.
#' @param verbose Logical; if TRUE, reports the log-likelihood each cycle.
#'
#' @return A list with the estimated \code{profile} (items x classes x categories),
#'   the posterior class membership matrix \code{clsmemb}, the model
#'   \code{log_lik}, the number of cycles \code{iter}, and \code{converge}.
#'
#' @noRd
emclus_nominal <- function(Q, Z, ncls, ncat, alpha = 1, maxiter = 100,
                           verbose = FALSE) {
  nobs <- NROW(Q)
  nitems <- NCOL(Q)
  maxQ <- max(ncat)
  const <- exp(-nitems)

  # One-hot encode the responses. Missing cells (Z == 0) are left all-zero and
  # are never read, because every downstream use of Uq is masked by Z.
  Uq <- array(0, dim = c(nobs, nitems, maxQ))
  valid <- as.vector(Z) == 1
  Uq[cbind(
    rep(seq_len(nobs), times = nitems)[valid],
    rep(seq_len(nitems), each = nobs)[valid],
    as.vector(Q)[valid]
  )] <- 1

  # Ragged category support: valid_cat[j, q] is TRUE only while q <= ncat[j].
  # Slots outside an item's range must never accumulate probability mass.
  valid_cat <- outer(ncat, seq_len(maxQ), FUN = ">=")

  # Initial profiles. The class effect makes the classes start out distinct,
  # otherwise the EM has no gradient to separate them.
  profile <- array(0, dim = c(nitems, ncls, maxQ))
  for (j in seq_len(nitems)) {
    for (k in seq_len(ncls)) {
      p <- init_field_membership_probs(maxQ, k / ncls, (nitems - j + 1) / nitems)
      p[!valid_cat[j, ]] <- 0
      profile[j, k, ] <- p / sum(p)
    }
  }

  # -Inf, not -1/const = -exp(J): the old sentinel sits above the real
  # log-likelihood on short tests with many respondents, which ended the
  # loop after one cycle while reporting convergence. The first pass skips
  # the comparison instead (emt == 0).
  test_log_lik <- -Inf
  old_test_log_lik <- -Inf
  emt <- 0
  converge <- TRUE
  clsmemb <- matrix(1 / ncls, nrow = nobs, ncol = ncls)
  FLG <- TRUE

  while (FLG) {
    # Convergence is judged on the change in log-likelihood, never on a
    # constraint-violation measure (see feedback_clm/isotonic convergence bug).
    if (emt > 0 && (!is.finite(test_log_lik) ||
      test_log_lik - old_test_log_lik < 1e-8 * abs(old_test_log_lik))) {
      if (!is.finite(test_log_lik)) converge <- FALSE
      FLG <- FALSE
      break
    }
    if (emt == maxiter) {
      message("\nReached the maximum number of iterations (", maxiter, ").")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
      converge <- FALSE
      FLG <- FALSE
    }

    emt <- emt + 1
    old_test_log_lik <- test_log_lik

    ## Expectation: posterior class membership for each respondent
    tmpL <- matrix(0, nrow = nobs, ncol = ncls)
    for (q in seq_len(maxQ)) {
      tmpL <- tmpL + (Z * Uq[, , q]) %*% log(profile[, , q] + const)
    }
    clsmemb <- row_softmax(tmpL)

    ## Maximization: category profiles are membership-weighted frequencies
    Ujcq <- array(0, dim = c(nitems, ncls, maxQ))
    for (q in seq_len(maxQ)) {
      Ujcq[, , q] <- t(Z * Uq[, , q]) %*% clsmemb
    }
    # ncat enters the Dirichlet denominator per item, so ragged counts stay
    # correctly normalised. With the default alpha = 1 both correction terms
    # vanish and this is the plain multinomial MLE.
    denom <- rowSums(Ujcq, dims = 2) + (ncat * alpha - ncat)
    denom <- pmax(denom, const)
    profile <- (Ujcq + alpha - 1) / array(denom, dim = dim(Ujcq))
    for (q in seq_len(maxQ)) {
      profile[!valid_cat[, q], , q] <- 0
    }

    ## Observed-data log-likelihood of the mixture,
    ##   sum_s log sum_c (1/C) prod_j rho_{j q_sj | c},
    ## which is what EM increases. (The posterior-weighted predictive quantity
    ## used before is not monotone across cycles; see emclus() and NEWS.)
    tmpL_new <- matrix(0, nrow = nobs, ncol = ncls)
    for (q in seq_len(maxQ)) {
      tmpL_new <- tmpL_new + (Z * Uq[, , q]) %*% log(profile[, , q] + const)
    }
    row_max <- apply(tmpL_new, 1, max)
    test_log_lik <- sum(row_max + log(rowSums(exp(tmpL_new - row_max)))) -
      nobs * log(ncls)

    if (verbose) {
      message(
        sprintf(
          "\n%-80s",
          paste0("iter ", emt, " log_lik ", format(test_log_lik, digits = 6))
        ),
        appendLF = FALSE
      )
    }
  }

  ret <- list(
    profile = profile,
    clsmemb = clsmemb,
    log_lik = test_log_lik,
    iter = emt,
    converge = converge
  )
  return(ret)
}
