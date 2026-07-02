#' @rdname Biclustering
#' @param ncls Number of latent classes/ranks to identify (between 2 and 20).
#' @param nfld Number of latent fields (item clusters) to identify.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#' observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @param method Analysis method to use (character string):
#'   * For nominal data, only "Biclustering" is available.
#' @param conf Confirmatory parameter for pre-specified field assignments. Can be either:
#'   * A vector with items and corresponding fields in sequence
#'   * A field membership profile matrix (items × fields) with 0/1 values
#'   * NULL (default) for exploratory analysis where field memberships are estimated
#' @param maxiter Maximum number of EM algorithm iterations. Default is 100.
#' @param verbose Logical; if TRUE, displays progress during estimation. Default is FALSE.
#' @param alpha Dirichlet distribution concentration parameter for prior density of field reference probabilities. Default is 1.
#' @param ... Additional arguments passed to specific methods.
#' #'
#' @examples
#' \donttest{
#' # Perform Biclustering for nominal sample data()
#' # Analyze data with 5 fields and 6 classes
#' result.Bi <- Biclustering(J35S515, nfld = 5, ncls = 6, method = "B")
#' }
#' @export
Biclustering.nominal <- function(U,
                                 ncls = 2, nfld = 2,
                                 conf = NULL,
                                 conf_class = NULL,
                                 mic = FALSE,
                                 maxiter = 100,
                                 verbose = FALSE,
                                 alpha = 1, ...) {
  tmp <- U
  tmp$Q <- remap_category_codes(tmp$Q)
  nobs <- NROW(tmp$Q)
  nitems <- NCOL(tmp$Q)
  const <- exp(-nitems)
  test_log_lik <- -1 / const
  old_test_log_lik <- -2 / const
  emt <- 0
  maxemt <- maxiter

  ncat <- as.vector(tmp$categories)
  maxQ <- max(ncat)

  ## confirmatory
  # set conf_mat for confirmatory clustering
  if (!is.null(conf)) {
    if (verbose) {
      message("Confirmatory Clustering is chosen.")
    }
    conf_mat <- build_conf_mat(conf, NCOL(U$Q))
    nfld <- NCOL(conf_mat)
  } else {
    conf_mat <- NULL
  }

  # set conf_class_mat for class-side confirmatory clustering
  if (!is.null(conf_class)) {
    if (verbose) {
      message("Class-side Confirmatory Clustering is chosen.")
    }
    if (is.vector(conf_class)) {
      if (length(conf_class) != nobs) {
        stop("conf_class vector size does NOT match with the number of respondents.")
      }
      conf_class_mat <- matrix(0, nrow = nobs, ncol = max(conf_class))
      for (i in 1:NROW(conf_class_mat)) {
        conf_class_mat[i, conf_class[i]] <- 1
      }
    } else if (is.matrix(conf_class) | is.data.frame(conf_class)) {
      if (NROW(conf_class) != nobs) {
        stop("conf_class matrix size does NOT match with the number of respondents.")
      }
      if (any(!conf_class %in% c(0, 1))) {
        stop("The conf_class matrix should only contain 0s and 1s.")
      }
      if (any(rowSums(conf_class) > 1)) {
        stop("The row sums of the conf_class matrix must be equal to 1.")
      }
      conf_class_mat <- as.matrix(conf_class)
    } else {
      stop("conf_class is not set properly.")
    }
    ncls <- NCOL(conf_class_mat)
  } else {
    conf_class_mat <- NULL
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  if (alpha <= 0) {
    stop("alpha must be positive (alpha > 0)")
  }

  ##
  fld0 <- pmin(ceiling(1:nitems / (nitems / nfld)), nfld)
  med_order <- order(apply(tmp$Q, 2, median), decreasing = TRUE)
  fld <- fld0[match(1:nitems, med_order)]
  fldmemb <- matrix(0, nrow = nitems, ncol = nfld)
  for (i in 1:nitems) {
    fldmemb[i, fld[i]] <- 1
  }
  ## Confirmatory Biclustering
  if (!any(is.null(conf_mat))) {
    fldmemb <- conf_mat
  }

  ## 3-dimensiona allay(nfld x ncls x Q)
  BCRM <- array(NA, dim = c(nfld, ncls, maxQ))
  for (i in 1:nfld) {
    for (j in 1:ncls) {
      class_effect <- j / ncls
      field_effect <- (nfld - i + 1) / nfld
      BCRM[i, j, ] <- init_field_membership_probs(maxQ, class_effect, field_effect)
    }
  }

  # One-hot encode tmp$Q into Uq[i, j, tmp$Q[i,j]] = 1 using matrix indexing.
  # Replaces a nobs*nitems R-level double loop with a single C-level write
  # into the flat backing storage of the 3-D array.
  # Missing entries (tmp$Z == 0) are left at zero; every downstream use of
  # Uq is masked by tmp$Z so the missing-cell values are never read.
  Uq <- array(0, dim = c(nobs, nitems, maxQ))
  valid <- as.vector(tmp$Z) == 1
  Uq[cbind(
    rep(seq_len(nobs), times = nitems)[valid],
    rep(seq_len(nitems), each = nobs)[valid],
    as.vector(tmp$Q)[valid]
  )] <- 1

  # iteration -------------------------------------------------------
  converge <- TRUE
  FLG <- TRUE
  while (FLG) {
    if (!is.finite(test_log_lik) ||
      test_log_lik - old_test_log_lik < 1e-8 * abs(old_test_log_lik)) {
      if (!is.finite(test_log_lik)) converge <- FALSE
      FLG <- FALSE
      break
    }
    if (emt == maxemt) {
      message("\nReached the maximum number of iterations (", maxemt, ").")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
      converge <- FALSE
      FLG <- FALSE
    }

    emt <- emt + 1
    old_test_log_lik <- test_log_lik

    tmpL <- matrix(0, nrow = nobs, ncol = ncls)
    for (q in 1:maxQ) {
      tmpL <- tmpL + (tmp$Z * Uq[, , q]) %*% fldmemb %*% log(BCRM[, , q] + const)
    }

    minllsr <- apply(tmpL, 1, min)
    expllsr <- exp(pmin(tmpL - minllsr, 700))
    clsmemb <- round(expllsr / rowSums(expllsr), 1e8)

    if (!is.null(conf_class_mat)) {
      clsmemb <- conf_class_mat
    }

    tmpH <- matrix(0, nrow = nitems, ncol = nfld)
    for (q in 1:maxQ) {
      tmpH <- tmpH + (t(tmp$Z * Uq[, , q]) %*% clsmemb) %*% t(log(BCRM[, , q] + const))
    }

    minllsr <- apply(tmpH, 1, min)
    expllsr <- exp(pmin(tmpH - minllsr, 700))
    fldmemb <- round(expllsr / rowSums(expllsr), 1e8)

    if (!any(is.null(conf_mat))) {
      fldmemb <- conf_mat
    }

    ## Maximization
    oldBCRM <- BCRM
    Ufcq <- array(0, dim = c(nfld, ncls, maxQ))
    for (q in 1:maxQ) {
      Ufcq[, , q] <- (t(fldmemb) %*% t(tmp$Z * Uq[, , q])) %*% clsmemb
    }

    # Apply Dirichlet prior (alpha parameter).
    # rowSums(Ufcq, dims=2) sums over the 3rd dim (categories), replacing
    # apply(Ufcq, c(1,2), sum) at C-level instead of per-cell apply.
    BCRM <- (Ufcq + alpha - 1) / array(rowSums(Ufcq, dims = 2) + maxQ * alpha - maxQ, dim = dim(BCRM))

    test_log_lik <- 0
    for (q in 1:maxQ) {
      pred_prob <- t(fldmemb %*% BCRM[, , q] %*% t(clsmemb))
      observed_mask <- (tmp$Z * Uq[, , q]) == 1
      test_log_lik <- test_log_lik + sum(log(pmax(pred_prob[observed_mask], const)))
    }

    if (verbose) {
      message(
        sprintf(
          "\n%-80s",
          paste0(
            "iter ", emt, " log_lik ", format(test_log_lik, digits = 6)
          )
        ),
        appendLF = FALSE
      )
    }

    if (!is.finite(test_log_lik) || test_log_lik - old_test_log_lik <= 0) {
      BCRM <- oldBCRM
      if (!is.finite(test_log_lik)) converge <- FALSE
      break
    }
  }


  # output ----------------------------------------------------------
  cls <- apply(clsmemb, 1, which.max)
  fld <- apply(fldmemb, 1, which.max)
  check_empty_fields(fld, nfld)
  fldmemb01 <- sign(fldmemb - apply(fldmemb, 1, max)) + 1
  flddist <- colSums(fldmemb01)
  clsmemb01 <- sign(clsmemb - apply(clsmemb, 1, max)) + 1
  clsdist <- colSums(clsmemb01)
  StudentRank <- cbind(clsmemb, Estimate = cls)
  rownames(StudentRank) <- tmp$ID
  colnames(StudentRank) <- c(paste("Membership", 1:ncls), "Estimate")


  # model fit -------------------------------------------------------
  testell <- 0
  for (q in 1:maxQ) {
    pred_prob <- t(fldmemb %*% BCRM[, , q] %*% t(clsmemb))
    observed_mask <- (tmp$Z * Uq[, , q]) == 1
    testell <- testell + sum(log(pmax(pred_prob[observed_mask], const)))
  }
  nparam <- ncls * nfld * (maxQ - 1)

  # Null model.
  # Zrep * Uq = Z * Uq[,,q] elementwise; compute via column-major recycling
  # (Uq * as.vector(Z)) which avoids the replicate() allocation.
  # colSums(X, dims=1) sums over the 1st dim, replacing apply(X, c(2,3), sum).
  ZU <- Uq * as.vector(tmp$Z)
  ZU_col_sums <- colSums(ZU, dims = 1)
  NullFRQ <- ZU_col_sums / colSums(tmp$Z)
  ell_N <- sum(ZU_col_sums * log(NullFRQ + const))

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

  msg <- "Class"
  ret <- structure(list(
    Q = tmp$Q,
    Z = tmp$Z,
    testlength = nitems,
    msg = msg,
    converge = converge,
    nobs = nobs,
    n_class = ncls,
    n_field = nfld,
    n_cycle = emt,
    Nclass = ncls,
    Nfield = nfld,
    N_Cycle = emt,
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
    log_lik = testell, # New naming convention
    # Deprecated fields (for backward compatibility)
    LogLik = testell
  ), class = c("exametrika", "nominalBiclustering"))

  return(ret)
}
