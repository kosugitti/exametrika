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
#                       -2 [ log p(Q | c_hat, f_hat) + sum_c n_c log w_c
#                            - J log F ]
#                       + (nparam + n_weight) log n
#                     with hard MAP assignments c_hat, f_hat, the mixing
#                     weights w_c of the fitted model (1/C unless they were
#                     estimated, in which case n_weight = C - 1 counts them)
#                     and the field weights fixed at 1/F. For a one-mode
#                     model the field term is absent. Since 2.3.0 the sample
#                     size in the penalty is n for every parameter, as in the
#                     package's BIC; until 2.2.0 the block parameters of a
#                     biclustering were penalised with log(nJ) and the mixing
#                     terms used the empirical proportions.
#   BIC_elbo          -2 elbo + (nparam + n_weight) log n (2.3.0).
#   ICL_exact         Computed separately from the hard assignments, see
#                     R/00_ExactICL.R.
#
# Mixing proportions are fixed at 1/C (and 1/F) in the E-steps of this package,
# following the reference implementation, unless mixing = "estimate"; elbo,
# log_lik_marginal and ICL use the same weights.

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
vb_fit_block_binary <- function(U, Z, clsmemb, fldmemb, PiFR, nparam, const,
                                log_prior = NULL, n_weight_params = 0) {
  fld01 <- hard_membership(fldmemb)
  ZU1 <- Z * U
  ZU0 <- Z * (1 - U)
  lp1 <- log(PiFR + const)
  lp0 <- log(1 - PiFR + const)
  L_soft <- ZU1 %*% fldmemb %*% lp1 + ZU0 %*% fldmemb %*% lp0
  L_hard <- ZU1 %*% fld01 %*% lp1 + ZU0 %*% fld01 %*% lp0
  return(vb_fit_from_loglik(L_soft, L_hard, clsmemb, fldmemb, fld01, nparam, NCOL(U),
    log_prior = log_prior, n_weight_params = n_weight_params
  ))
}

#' Variational fit quantities for a one-mode model (LCA, LRA)
#'
#' @param L respondent x class log-likelihood matrix log p(u_s | c), without
#'   the class prior
#' @param postDist respondent posterior (nobs x ncls)
#' @param nparam number of free parameters counted for the reference profiles
#' @param log_prior log class prior (length ncls, normalised). Default: uniform.
#' @noRd
vb_fit_single <- function(L, postDist, nparam, log_prior = NULL, n_weight_params = 0) {
  L <- as.matrix(L)
  nobs <- nrow(L)
  ncls <- ncol(L)
  if (is.null(log_prior)) {
    log_prior <- rep(-log(ncls), ncls)
  }
  elbo <- sum_row_logsumexp(sweep(L, 2, log_prior, "+"))
  cls01 <- hard_membership(postDist)
  completed <- sum(cls01 * L)
  # The mixing term uses the model's weights (1/C, or the estimated ones),
  # and the weights are counted as parameters only when they were estimated.
  icl <- -2 * (completed + sum(colSums(cls01) * log_prior)) +
    (nparam + n_weight_params) * log(nobs)
  # BIC on the observed-data log-likelihood (2.3.0). Unlike the BIC in
  # TestFitIndices, which is built on the expected complete-data
  # log-likelihood against the benchmark model, this one penalises the
  # marginal likelihood directly; the class weights count only when they
  # were estimated (n_weight_params).
  bic_elbo <- -2 * elbo + (nparam + n_weight_params) * log(nobs)
  return(list(
    elbo = elbo,
    log_lik_marginal = elbo,
    entropy = c(class = membership_entropy(postDist)),
    ICL = icl,
    BIC_elbo = bic_elbo,
    completed_log_lik = completed
  ))
}

#' Shared tail of the two-mode variants
#' @noRd
vb_fit_from_loglik <- function(L_soft, L_hard, clsmemb, fldmemb, fld01, nparam, nitems,
                               log_prior = NULL, n_weight_params = 0) {
  nobs <- nrow(L_soft)
  ncls <- ncol(L_soft)
  nfld <- ncol(fld01)
  # Class prior: uniform 1/C unless the class weights were estimated
  # (Biclustering mixing = "estimate", 2.3.0). The field prior stays 1/F.
  if (is.null(log_prior)) {
    log_prior <- rep(-log(ncls), ncls)
  }
  H_class <- membership_entropy(clsmemb)
  H_field <- membership_entropy(fldmemb)
  elbo <- sum(clsmemb * L_soft) + H_class + H_field +
    sum(sweep(clsmemb, 2, log_prior, "*")) - nitems * log(nfld)
  marginal <- sum_row_logsumexp(sweep(L_hard, 2, log_prior, "+"))
  cls01 <- hard_membership(clsmemb)
  completed <- sum(cls01 * L_hard)
  # Class weights of the model (1/C or estimated), field weights fixed at
  # 1/F; every parameter is penalised with log n (2.3.0, see the header).
  icl <- -2 * (completed + sum(colSums(cls01) * log_prior) - nitems * log(nfld)) +
    (nparam + n_weight_params) * log(nobs)
  # BIC on the ELBO (2.3.0), same penalty.
  bic_elbo <- -2 * elbo + (nparam + n_weight_params) * log(nobs)
  return(list(
    elbo = elbo,
    log_lik_marginal = marginal,
    entropy = c(class = H_class, field = H_field),
    ICL = icl,
    BIC_elbo = bic_elbo,
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
  if (!is.null(g("BIC_elbo"))) {
    v <- c(v, BIC_elbo = g("BIC_elbo"))
  }
  if (!is.null(g("ICL_exact"))) {
    v <- c(v, ICL_exact = g("ICL_exact"))
    if (!is.null(g("ICL_exact_se")) && isTRUE(g("ICL_exact_se") > 0)) {
      v <- c(v, ICL_exact_se = g("ICL_exact_se"))
    }
  }
  y <- matrix(v, ncol = 1, dimnames = list(names(v), "value"))
  print(round(y, digits))
  return(invisible(NULL))
}
