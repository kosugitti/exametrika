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
#'  \item{log_lik}{Log-likelihood of the model.}
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
                                     max_iter = 100, stable_limit = 5,
                                     minSize = 20, EM_limit = 20,
                                     seed = 123, verbose = TRUE, ...) {
  tmp <- U

  nitems <- NCOL(tmp$Q)
  nobs <- NROW(tmp$Q)
  const <- exp(-nitems)
  maxQ <- max(tmp$categories)
  alpha_vec <- rep(alpha, maxQ)

  if (alpha <= 0) {
    stop("alpha must be positive (alpha > 0)")
  }

  # One-hot encoding of response matrix: Uq[s, j, q] = 1 if student s chose category q on item j
  Uq <- array(0, dim = c(nobs, nitems, maxQ))
  for (s in 1:nobs) {
    for (j in 1:nitems) {
      Uq[s, j, tmp$Q[s, j]] <- 1
    }
  }

  # Initialize -------------------------------------------------------------
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ## Initial Class: assign by mode category
  mode_cat <- apply(tmp$Q, 1, function(x) {
    tab <- table(x)
    as.integer(names(which.max(tab)))
  })
  unique_vec <- unique(mode_cat)
  ncls <- length(unique_vec)
  cls01 <- matrix(0, ncol = ncls, nrow = nobs)
  for (i in 1:nobs) {
    cls01[i, mode_cat[i]] <- 1
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
    max_iter = max_iter, stable_limit = stable_limit, verbose = verbose
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
      prob_S <- t(apply(log_S, 1, irm_log_to_prob))
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
  clsmemb <- t(apply(log_S, 1, irm_log_to_prob))
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
  fldmemb <- t(apply(log_J, 1, irm_log_to_prob))
  fld <- apply(fldmemb, 1, which.max)
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
  testell <- 0
  for (q in 1:maxQ) {
    pred_prob <- t(fldmemb %*% BCRM[, , q] %*% t(clsmemb))
    observed_mask <- (tmp$Z * Uq[, , q]) == 1
    testell <- testell + sum(log(pmax(pred_prob[observed_mask], const)))
  }
  nparam <- ncls * nfld * (maxQ - 1)

  # Full model
  ptn <- apply(tmp$Q * tmp$Z, 1, function(x) paste(x, collapse = ""))
  benchGroup <- as.numeric(as.factor(ptn))
  fullG <- length(unique(benchGroup))
  benchmemb <- matrix(0, nrow = nobs, ncol = fullG)
  for (i in 1:nobs) {
    benchmemb[i, benchGroup[i]] <- 1
  }

  BenchFRQ <- array(NA, dim = c(nitems, fullG, maxQ))
  Bfcq <- array(0, dim = c(nitems, fullG, maxQ))
  for (q in 1:maxQ) {
    Bfcq[, , q] <- (t(tmp$Z * Uq[, , q])) %*% benchmemb
  }

  BenchFRQ <- Bfcq / array(apply(Bfcq, c(1, 2), sum), dim = dim(BenchFRQ))
  BenchFRQ[is.nan(BenchFRQ)] <- const

  ell_B <- 0
  for (q in 1:maxQ) {
    ell_B <- ell_B + sum(t(tmp$Z * Uq[, , q]) %*% benchmemb * log(BenchFRQ[, , q] + const))
  }
  bench_nparam <- nitems * fullG

  # Null model
  Zrep <- replicate(maxQ, tmp$Z)
  NullFRQ <- apply(Zrep * Uq, c(2, 3), sum) / apply(tmp$Z, 2, sum)
  ell_N <- sum(apply(Zrep * Uq, c(2, 3), sum) * log(NullFRQ + const))
  null_nparam <- nitems

  df_B <- bench_nparam - null_nparam
  chi_B <- 2 * (ell_B - ell_N)
  chi_A <- 2 * (ell_B - testell)
  df_A <- bench_nparam - nparam
  FitIndices <- structure(
    c(list(
      model_log_like = testell,
      bench_log_like = ell_B,
      null_log_like = ell_N,
      model_Chi_sq = chi_A,
      null_Chi_sq = chi_B,
      model_df = df_A,
      null_df = df_B
    ), calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)),
    class = c("exametrika", "ModelFit")
  )

  # Return ------------------------------------------------------------------
  msg <- "Rank"
  ret <- structure(list(
    Q = tmp$Q,
    Z = tmp$Z,
    testlength = nitems,
    msg = msg,
    nobs = nobs,
    n_class = ncls,
    n_field = nfld,
    n_cycle = EMt,
    Nclass = ncls,
    Nfield = nfld,
    N_Cycle = EMt,
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
    log_lik = testell,
    # Deprecated fields (for backward compatibility)
    LogLik = testell
  ), class = c("exametrika", "nominalBiclustering"))

  return(ret)
}
