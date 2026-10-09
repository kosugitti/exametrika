# Variational and completed-likelihood fit quantities ---------------------
#
# The latent class / latent rank / biclustering models in this package are
# fitted by (variational) EM. For model comparison four quantities are
# reported alongside the fit indices:
#
#   elbo              The evidence lower bound of the observed-data
#                     log-likelihood under the mean-field posterior the EM
#                     produced. For a one-mode model (LCA, LRA) the posterior is
#                     exact and the bound is the observed-data log-likelihood
#                     itself. For a two-mode model (Biclustering) the respondent
#                     and item posteriors are taken independent, so the value is
#                     a lower bound. This is the quantity the biclustering EM
#                     ascends, which is why it is also used to monitor it.
#   log_lik_marginal  The observed-data log-likelihood of the respondent-side
#                     mixture with the items fixed at their MAP field
#                     assignment: sum_s log sum_c (1/C) prod_j pi_{f_j c q_sj}.
#                     Exact for that conditional model; equal to elbo for a
#                     one-mode model.
#   entropy           The entropy of the posterior memberships, respondent side
#                     and (for biclustering) item side. elbo = (expected
#                     complete-data log-likelihood) + entropy, so this is how
#                     far the bound sits from the plug-in value.
#   ICL               The integrated completed likelihood criterion of
#                     Biernacki, Celeux and Govaert (2000), in the BIC-type
#                     approximation of Keribin, Brault, Celeux and Govaert
#                     (2015) for the latent block model. Reported on the same
#                     scale as AIC and BIC in this package (-2 x log scale,
#                     smaller is better):
#                       -2 [ log p(Q | c_hat, f_hat) + sum_c n_c log(n_c/n)
#                            + sum_f m_f log(m_f/J) ]
#                       + (C-1) log n + (F-1) log J + nparam log(nJ)
#                     with hard MAP assignments c_hat, f_hat. For a one-mode
#                     model the item terms are absent and the block penalty is
#                     nparam log n.
#
# Mixing proportions are fixed at 1/C (and 1/F) in the E-steps of this package,
# following the reference implementation, and elbo / log_lik_marginal use the
# same constants. ICL uses the empirical proportions of the hard assignments as
# in the cited definition.

#' Entropy of a row-stochastic membership matrix, summed over rows
#' @param M matrix whose rows are probability vectors
#' @noRd
membership_entropy <- function(M) {
  M <- as.matrix(M)
  v <- M * log(M)
  v[!is.finite(v)] <- 0 # 0 log 0, and rounding noise below zero
  return(-sum(v))
}

#' Row-wise log-sum-exp, summed over rows
#' @param L matrix of log values (rows = respondents, columns = classes)
#' @noRd
sum_row_logsumexp <- function(L) {
  L <- as.matrix(L)
  m <- apply(L, 1, max)
  return(sum(m + log(rowSums(exp(L - m)))))
}

#' Hard (0/1) assignment from a membership matrix, ties to the first column
#' @noRd
hard_membership <- function(M) {
  M <- as.matrix(M)
  H <- matrix(0, nrow = nrow(M), ncol = ncol(M))
  H[cbind(seq_len(nrow(M)), max.col(M, ties.method = "first"))] <- 1
  return(H)
}

#' Mixing-proportion term of a completed log-likelihood
#'
#' sum_k n_k log(n_k / n) over the groups of a hard assignment; empty groups
#' contribute nothing.
#' @noRd
completed_mixing_term <- function(counts) {
  n <- sum(counts)
  k <- counts[counts > 0]
  return(sum(k * log(k / n)))
}

#' Variational fit quantities for a polytomous bicluster model
#'
#' @param ZU one-hot response array (nobs x nitems x maxQ), already masked by Z
#' @param clsmemb respondent posterior (nobs x ncls)
#' @param fldmemb item posterior (nitems x nfld)
#' @param logP log category probabilities per block (nfld x ncls x maxQ),
#'   already clamped away from zero the same way the E-step clamps them
#' @param nparam number of free parameters counted for the block probabilities
#' @return list(elbo, log_lik_marginal, entropy, ICL, completed_log_lik)
#' @noRd
vb_fit_block <- function(ZU, clsmemb, fldmemb, logP, nparam) {
  nobs <- dim(ZU)[1]
  nitems <- dim(ZU)[2]
  maxQ <- dim(ZU)[3]
  ncls <- NCOL(clsmemb)
  nfld <- NCOL(fldmemb)
  fld01 <- hard_membership(fldmemb)
  L_soft <- matrix(0, nrow = nobs, ncol = ncls)
  L_hard <- matrix(0, nrow = nobs, ncol = ncls)
  for (q in seq_len(maxQ)) {
    lp <- matrix(logP[, , q], nrow = nfld, ncol = ncls)
    L_soft <- L_soft + ZU[, , q] %*% fldmemb %*% lp
    L_hard <- L_hard + ZU[, , q] %*% fld01 %*% lp
  }
  return(vb_fit_from_loglik(L_soft, L_hard, clsmemb, fldmemb, fld01, nparam, nitems))
}

#' Variational fit quantities for a binary bicluster model
#'
#' @param U,Z binary responses and missing indicator (nobs x nitems)
#' @param clsmemb,fldmemb posteriors as in vb_fit_block()
#' @param PiFR correct response probability per block (nfld x ncls)
#' @param nparam number of free parameters counted for PiFR
#' @param const clamp added inside the logarithms, as in the EM
#' @noRd
vb_fit_block_binary <- function(U, Z, clsmemb, fldmemb, PiFR, nparam, const) {
  fld01 <- hard_membership(fldmemb)
  ZU1 <- Z * U
  ZU0 <- Z * (1 - U)
  lp1 <- log(PiFR + const)
  lp0 <- log(1 - PiFR + const)
  L_soft <- ZU1 %*% fldmemb %*% lp1 + ZU0 %*% fldmemb %*% lp0
  L_hard <- ZU1 %*% fld01 %*% lp1 + ZU0 %*% fld01 %*% lp0
  return(vb_fit_from_loglik(L_soft, L_hard, clsmemb, fldmemb, fld01, nparam, NCOL(U)))
}

#' Variational fit quantities for a one-mode model (LCA, LRA)
#'
#' @param L respondent x class log-likelihood matrix log p(u_s | c), without
#'   the class prior
#' @param postDist respondent posterior (nobs x ncls)
#' @param nparam number of free parameters counted for the reference profiles
#' @param log_prior log class prior (length ncls, normalised). Default: uniform.
#' @noRd
vb_fit_single <- function(L, postDist, nparam, log_prior = NULL) {
  L <- as.matrix(L)
  nobs <- nrow(L)
  ncls <- ncol(L)
  if (is.null(log_prior)) {
    log_prior <- rep(-log(ncls), ncls)
  }
  elbo <- sum_row_logsumexp(sweep(L, 2, log_prior, "+"))
  cls01 <- hard_membership(postDist)
  completed <- sum(cls01 * L)
  icl <- -2 * (completed + completed_mixing_term(colSums(cls01))) +
    (ncls - 1) * log(nobs) + nparam * log(nobs)
  return(list(
    elbo = elbo,
    log_lik_marginal = elbo,
    entropy = c(class = membership_entropy(postDist)),
    ICL = icl,
    completed_log_lik = completed
  ))
}

#' Shared tail of the two-mode variants
#' @noRd
vb_fit_from_loglik <- function(L_soft, L_hard, clsmemb, fldmemb, fld01, nparam, nitems) {
  nobs <- nrow(L_soft)
  ncls <- ncol(L_soft)
  nfld <- ncol(fld01)
  H_class <- membership_entropy(clsmemb)
  H_field <- membership_entropy(fldmemb)
  elbo <- sum(clsmemb * L_soft) + H_class + H_field -
    nobs * log(ncls) - nitems * log(nfld)
  marginal <- sum_row_logsumexp(L_hard) - nobs * log(ncls)
  cls01 <- hard_membership(clsmemb)
  completed <- sum(cls01 * L_hard)
  icl <- -2 * (completed +
    completed_mixing_term(colSums(cls01)) +
    completed_mixing_term(colSums(fld01))) +
    (ncls - 1) * log(nobs) + (nfld - 1) * log(nitems) +
    nparam * log(nobs * nitems)
  return(list(
    elbo = elbo,
    log_lik_marginal = marginal,
    entropy = c(class = H_class, field = H_field),
    ICL = icl,
    completed_log_lik = completed
  ))
}

#' Print the variational fit block of a fitted object, if it has one
#'
#' @param x fitted object
#' @param digits rounding
#' @param suffix "" for the main layer, "_nominal" for the nominal layer of a
#'   rated model
#' @noRd
print_variational_block <- function(x, digits = 3, suffix = "") {
  g <- function(k) x[[paste0(k, suffix)]]
  if (is.null(g("elbo"))) {
    return(invisible(NULL))
  }
  cat("\nVariational Fit", if (nzchar(suffix)) " (Nominal layer)" else "", "\n", sep = "")
  H <- g("entropy")
  v <- c(ELBO = g("elbo"), log_lik_marginal = g("log_lik_marginal"))
  v <- c(v, stats::setNames(H, paste0("entropy_", names(H))), ICL = g("ICL"))
  y <- matrix(v, ncol = 1, dimnames = list(names(v), "value"))
  print(round(y, digits))
  return(invisible(NULL))
}
