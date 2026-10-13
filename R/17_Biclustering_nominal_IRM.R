#' @rdname Biclustering_IRM
#' @param alpha Dirichlet distribution concentration parameter for the prior density of
#' field reference probabilities (nominal IRM only). Must be positive. The default is 1.
#' @return
#' For nominal data, the returned list includes:
#' \describe{
#'  \item{Q}{Response matrix.}
#'  \item{Z}{Missing indicator matrix.}
#'  \item{testlength}{Number of items.}
#'  \item{nobs}{Sample size.}
#'  \item{n_class}{Optimal number of classes.}
#'  \item{n_field}{Optimal number of fields.}
#'  \item{n_cycle}{Number of EM algorithm iterations.}
#'  \item{FRP}{Field Reference Profile, a 3D array (nfld x ncls x maxQ).}
#'  \item{LFD}{Latent Field Distribution.}
#'  \item{LCD}{Latent Class Distribution.}
#'  \item{FieldMembership}{Field membership probability matrix.}
#'  \item{ClassMembership}{Class membership probability matrix.}
#'  \item{FieldEstimated}{Estimated field assignment for each item.}
#'  \item{ClassEstimated}{Estimated class assignment for each student.}
#'  \item{Students}{Rank Membership Profile matrix with estimated class.}
#'  \item{TestFitIndices}{Overall fit index for the test.}
#'  \item{log_lik}{Evidence lower bound of the model (equal to \code{elbo}).}
#'  \item{elbo}{Evidence lower bound of the observed-data log-likelihood under the
#'    posterior memberships the EM produced. Exact (equal to the observed-data
#'    log-likelihood) for a one-mode model; a lower bound for biclustering, where
#'    the respondent and item posteriors are taken independent. Since 2.2.0.}
#'  \item{log_lik_marginal}{Observed-data log-likelihood of the respondent-side
#'    mixture with the items fixed at their MAP field assignment. Equal to
#'    \code{elbo} for a one-mode model.}
#'  \item{entropy}{Posterior entropy of the memberships (named: \code{class},
#'    and \code{field} for biclustering). \code{elbo} minus this is the expected
#'    complete-data log-likelihood.}
#'  \item{ICL}{Integrated completed likelihood criterion (Biernacki, Celeux and
#'    Govaert, 2000) in its BIC-type approximation, on the same scale as AIC and
#'    BIC (-2 log scale, smaller is better).}
#' }
#' @examples
#' \donttest{
#' # Fit a nominal Biclustering IRM model
#' result <- Biclustering_IRM(J20S600, gamma_c = 1, gamma_f = 1, verbose = TRUE)
#' plot(result, type = "Array")
#' }
#' @export
Biclustering_IRM.nominal <- function(U,
                                     gamma_c = 1, gamma_f = 1, alpha = 1,
                                     maxiter = 100, stable_limit = 5,
                                     minSize = 20, EM_limit = 20,
                                     seed = 123, verbose = FALSE, ...) {
  maxiter <- resolve_deprecated_max_iter(maxiter, list(...))
  tmp <- U
  # Codes are shifted, not renumbered per item: a field's items share the
  # block probabilities, so category q must mean the same on every item.
  tmp$Q <- shift_category_codes(tmp$Q)

  nitems <- NCOL(tmp$Q)
  nobs <- NROW(tmp$Q)
  const <- exp(-nitems)
  maxQ <- max(tmp$Q[tmp$Z == 1])
  alpha_vec <- rep(alpha, maxQ)

  if (alpha <= 0) {
    stop("alpha must be positive (alpha > 0)")
  }

  # One-hot encoding of response matrix: Uq[s, j, q] = 1 if student s chose
  # category q on item j. Missing cells (tmp$Z == 0) are left at zero; a raw
  # -1 sentinel written directly as an array index would otherwise corrupt
  # the array via R's negative-index semantics.
  Uq <- array(0, dim = c(nobs, nitems, maxQ))
  valid <- as.vector(tmp$Z) == 1
  Uq[cbind(
    rep(seq_len(nobs), times = nitems)[valid],
    rep(seq_len(nitems), each = nobs)[valid],
    as.vector(tmp$Q)[valid]
  )] <- 1

  # Initialize -------------------------------------------------------------
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ## Initial Class: assign by mode category (missing cells excluded)
  Q_masked <- tmp$Q
  Q_masked[tmp$Z == 0] <- NA
  mode_cat <- apply(Q_masked, 1, function(x) {
    tab <- table(x)
    as.integer(names(which.max(tab)))
  })
  unique_vec <- unique(mode_cat)
  ncls <- length(unique_vec)
  cls01 <- matrix(0, ncol = ncls, nrow = nobs)
  for (i in 1:nobs) {
    # position among the distinct modal categories, not the raw code (which
    # need not be 1..k; 2.2.0)
    cls01[i, match(mode_cat[i], unique_vec)] <- 1
  }
  colnames(cls01) <- paste("Rank", 1:ncls)
  rownames(cls01) <- tmp$ID

  ## Initial Field: one item per field
  nfld <- nitems
  fld01 <- matrix(0, nrow = nitems, ncol = nfld)
  for (i in 1:nitems) {
    fld01[i, i] <- 1
  }

  # Phase 1: Collapsed Gibbs Sampler (shared core) --------------------------
  gibbs <- irm_gibbs_core(
    Uq = Uq, Z = tmp$Z, cls01 = cls01, fld01 = fld01,
    gamma_c = gamma_c, gamma_f = gamma_f, alpha_vec = alpha_vec,
    max_iter = maxiter, stable_limit = stable_limit, verbose = verbose
  )

  cls01 <- gibbs$cls01
  fld01 <- gibbs$fld01
  ncls <- gibbs$ncls
  nfld <- gibbs$nfld

  # Phase 2: BCRM estimation (direct normalization) -------------------------
  U_fcq <- irm_calc_Ufcq(Uq, tmp$Z, cls01, fld01, maxQ)
  BCRM <- sweep(U_fcq, 3, alpha_vec, "+")
  denom <- apply(BCRM, c(1, 2), sum)
  BCRM <- sweep(BCRM, c(1, 2), denom, "/")

  # Reorganizing small-sized classes ----------------------------------------
  bic <- irm_bic_calc(Uq, tmp$Z, fld01, cls01, BCRM, maxQ, const)
  best_BCRM <- BCRM
  best_cls01 <- cls01
  EMt <- 0

  DelRepFLG <- TRUE
  while (DelRepFLG) {
    bestfit <- 10^10
    Nc <- colSums(cls01)
    NcTable <- matrix(Nc)
    minclass <- NcTable[order(NcTable[, 1]), ]
    if (minclass[1] < minSize) {
      if (verbose) {
        message(
          sprintf(
            "Adjusting classes: BIC=%.1f ncls=%d (min size < %d)",
            bic, ncls, minSize
          )
        )
      }
    } else {
      DelRepFLG <- FALSE
      break
    }

    if (ncls <= 1) {
      warning("Every class is smaller than minSize (", minSize, ") and only one class is left; returning the current solution.")
      break
    }
    ncls <- ncls - 1
    del_class <- which.min(NcTable)
    BCRM <- BCRM[, -del_class, , drop = FALSE]

    EMrepFLG <- TRUE
    while (EMrepFLG) {
      EMt <- EMt + 1
      # E-step
      V_scq <- array(0, dim = c(nobs, ncls, maxQ))
      for (q in 1:maxQ) {
        V_scq[, , q] <- (Uq[, , q] %*% fld01) %*% log(BCRM[, , q])
      }
      log_S <- apply(V_scq, c(1, 2), sum)
      prob_S <- matrix(0, nrow = nobs, ncol = ncls)
      for (i in seq_len(nobs)) {
        prob_S[i, ] <- irm_log_to_prob(log_S[i, ])
      }
      cls <- apply(prob_S, 1, which.max)
      cls01 <- matrix(0, ncol = ncls, nrow = nobs)
      for (i in 1:nobs) {
        cls01[i, cls[i]] <- 1
      }

      # M-step
      U_fcq <- irm_calc_Ufcq(Uq, tmp$Z, cls01, fld01, maxQ)
      BCRM <- sweep(U_fcq, 3, alpha_vec, "+")
      denom <- apply(BCRM, c(1, 2), sum)
      BCRM <- sweep(BCRM, c(1, 2), denom, "/")

      # Model fit
      bic <- irm_bic_calc(Uq, tmp$Z, fld01, cls01, BCRM, maxQ, const)

      # Converge check
      if (bic < bestfit) {
        bestfit <- bic
        best_BCRM <- BCRM
        best_cls01 <- cls01
      } else {
        BCRM <- best_BCRM
        cls01 <- best_cls01
        EMrepFLG <- FALSE
      }

      if (EMt >= EM_limit) {
        EMrepFLG <- FALSE
      }
    }
  }

  # Output -----------------------------------------------------------------

  ## Class membership
  V_scq <- array(0, dim = c(nobs, ncls, maxQ))
  for (q in 1:maxQ) {
    V_scq[, , q] <- (Uq[, , q] %*% fld01) %*% log(BCRM[, , q])
  }
  log_S <- apply(V_scq, c(1, 2), sum)
  clsmemb <- matrix(0, nrow = nobs, ncol = ncls)
  for (i in seq_len(nobs)) {
    clsmemb[i, ] <- irm_log_to_prob(log_S[i, ])
  }
  cls <- apply(clsmemb, 1, which.max)
  cls01 <- matrix(0, ncol = ncls, nrow = nobs)
  for (i in 1:nobs) {
    cls01[i, cls[i]] <- 1
  }

  ## Field membership
  V_jfq <- array(0, dim = c(nitems, nfld, maxQ))
  for (q in 1:maxQ) {
    V_jfq[, , q] <- (t(Uq[, , q]) %*% cls01) %*% log(t(BCRM[, , q]))
  }
  log_J <- apply(V_jfq, c(1, 2), sum)
  fldmemb <- matrix(0, nrow = nitems, ncol = nfld)
  for (i in seq_len(nitems)) {
    fldmemb[i, ] <- irm_log_to_prob(log_J[i, ])
  }
  fld <- apply(fldmemb, 1, which.max)
  check_empty_fields(fld, nfld)
  fld01 <- matrix(0, ncol = nfld, nrow = nitems)
  for (i in 1:nitems) {
    fld01[i, fld[i]] <- 1
  }

  fldmemb01 <- sign(fldmemb - apply(fldmemb, 1, max)) + 1
  flddist <- colSums(fldmemb01)
  clsmemb01 <- sign(clsmemb - apply(clsmemb, 1, max)) + 1
  clsdist <- colSums(clsmemb01)
  StudentRank <- cbind(clsmemb, Estimate = cls)
  rownames(StudentRank) <- tmp$ID
  colnames(StudentRank) <- c(paste("Membership", 1:ncls), "Estimate")

  # Model fit ---------------------------------------------------------------
  # Evidence lower bound at the final memberships, as in Biclustering.nominal();
  # see R/00_VariationalFit.R.
  nparam <- ncls * nfld * (maxQ - 1)
  vb <- vb_fit_block(Uq * as.vector(tmp$Z), clsmemb, fldmemb, log(BCRM + const), nparam)
  testell <- vb$elbo

  # Null model
  Zrep <- replicate(maxQ, tmp$Z)
  NullFRQ <- apply(Zrep * Uq, c(2, 3), sum) / apply(tmp$Z, 2, sum)
  ell_N <- sum(apply(Zrep * Uq, c(2, 3), sum) * log(NullFRQ + const))

  # Fit indices: For nominal data, no meaningful benchmark (saturated) model exists
  # because response patterns are almost always unique with many items and categories.
  # Only information criteria (AIC, BIC, CAIC) are reported.
  AIC <- -2 * testell + 2 * nparam
  CAIC <- -2 * testell + nparam * (log(nobs) + 1)
  BIC <- -2 * testell + nparam * log(nobs)

  FitIndices <- structure(
    list(
      model_log_like = testell,
      bench_log_like = NA,
      null_log_like = ell_N,
      model_Chi_sq = NA,
      null_Chi_sq = NA,
      model_df = NA,
      null_df = NA,
      NFI = NA,
      RFI = NA,
      IFI = NA,
      TLI = NA,
      CFI = NA,
      RMSEA = NA,
      AIC = AIC,
      CAIC = CAIC,
      BIC = BIC
    ),
    class = c("exametrika", "ModelFit")
  )

  # Return ------------------------------------------------------------------
  msg <- "Class"
  # Field x class/rank x category, named as the rated fits name theirs
  dimnames(BCRM) <- list(paste0("Field", 1:nfld), paste0(msg, 1:ncls), paste0("Cat", 1:maxQ))
  ret <- structure(list(
    Q = tmp$Q,
    Z = tmp$Z,
    testlength = nitems,
    msg = msg,
    nobs = nobs,
    n_class = ncls,
    n_field = nfld,
    n_cycle = EMt,
    LFD = flddist,
    LRD = clsdist,
    LCD = clsdist,
    FRP = BCRM,
    CMD = colSums(clsmemb),
    RMD = colSums(clsmemb),
    FieldMembership = fldmemb,
    ClassMembership = clsmemb,
    FieldEstimated = fld,
    ClassEstimated = cls,
    Students = StudentRank,
    TestFitIndices = FitIndices,
    log_lik = testell, # the evidence lower bound; identical to elbo
    elbo = vb$elbo,
    log_lik_marginal = vb$log_lik_marginal,
    entropy = vb$entropy,
    ICL = vb$ICL
  ), class = c("exametrika", "nominalBiclustering"))

  ret <- label_result(ret, id = tmp$ID, item = tmp$ItemLabel, class_prefix = msg)
  return(ret)
}
