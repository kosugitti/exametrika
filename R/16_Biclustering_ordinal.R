#' @rdname Biclustering
#' @param ncls Number of latent classes/ranks to identify (between 2 and 20).
#' @param nfld Number of latent fields (item clusters) to identify.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#' observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @param method Analysis method to use (character string):
#'   * "B" or "Biclustering": Standard biclustering (default)
#'   * "R" or "Ranklustering": Ranklustering with ordered class structure
#' @param conf Confirmatory parameter for pre-specified field assignments. Can be either:
#'   * A vector with items and corresponding fields in sequence
#'   * A field membership profile matrix (items × fields) with 0/1 values
#'   * NULL (default) for exploratory analysis where field memberships are estimated
#' @param mic Logical; if TRUE, forces Field Reference Profiles to be monotonically
#' increasing. Default is FALSE.
#' @param maxiter Maximum number of EM algorithm iterations. Default is 100.
#' @param verbose Logical; if TRUE, displays progress during estimation. Default is TRUE.
#' @param alpha Dirichlet distribution concentration parameter for prior density of field reference probabilities. Default is 1.
#' @param ... Additional arguments passed to specific methods.
#' @export
Biclustering.ordinal <- function(U,
                                 ncls = 2, nfld = 2,
                                 method = "B",
                                 conf = NULL,
                                 conf_class = NULL,
                                 mic = FALSE,
                                 maxiter = 100,
                                 verbose = TRUE,
                                 alpha = 1, ...) {
  tmp <- U
  maxQ <- max(tmp$Q)
  nobs <- NROW(tmp$Q)
  nitems <- NCOL(tmp$Q)
  ncat <- tmp$categories
  const <- exp(-nitems)
  test_log_lik <- -1 / const
  old_test_log_lik <- -2 / const
  emt <- 0
  maxemt <- maxiter
  ncat <- as.vector(tmp$categories)
  # if (length(unique(ncat)) > 1) {
  #   stop("Error: Variables have different numbers of categories. Nominal data processing requires the same number of categories for all variables.")
  # }

  if (method == "B" | method == "Biclustering") {
    if (verbose) {
      message("Biclustering is chosen.")
    }
    model <- 1
  } else if (method == "R" | method == "Ranklustering") {
    if (verbose) {
      message("Ranklustering is chosen.")
    }
    model <- 2
  } else {
    stop("The method must be selected as either Biclustering or Ranklustering.")
  }

  if (alpha <= 0) {
    stop("alpha must be positive (alpha > 0)")
  }

  ## confirmatory
  # set conf_mat for confirmatory clustering
  if (!is.null(conf)) {
    if (verbose) {
      message("Confirmatory Clustering is chosen.")
    }
    if (is.vector(conf)) {
      # check size
      if (length(conf) != NCOL(U$Q)) {
        stop("conf vector size does NOT match with data.")
      }
      conf_mat <- matrix(0, nrow = NCOL(U$Q), ncol = max(conf))
      for (i in 1:NROW(conf_mat)) {
        conf_mat[i, conf[i]] <- 1
      }
    } else if (is.matrix(conf) | is.data.frame(conf)) {
      if (NROW(conf) != NCOL(U$Q)) {
        stop("conf matrix size does NOT match with data.")
      }
      if (any(!conf %in% c(0, 1))) {
        stop("The conf matrix should only contain 0s and 1s.")
      }
      if (any(rowSums(conf) > 1)) {
        stop("The row sums of the conf matrix must be equal to 1.")
      }
      conf_mat <- as.matrix(conf)
    } else {
      stop("conf matrix is not set properly.")
    }
    ###
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

  # Bicluster Category Reference Matrix(FxCxQ)
  BCRM <- array(NA, dim = c(nfld, ncls, maxQ))
  for (i in 1:nfld) {
    for (j in 1:ncls) {
      class_effect <- j / ncls
      field_effect <- (nfld - i + 1) / nfld
      BCRM[i, j, ] <- init_field_membership_probs(maxQ, class_effect, field_effect)
    }
  }

  # Bicluter Boundary Reference Matrix(FxCxQ+1)
  BBRM <- array(NA, dim = c(nfld, ncls, maxQ + 1))
  BBRM[, , 1] <- 1
  BBRM[, , maxQ + 1] <- 0
  for (q in maxQ:2) {
    BBRM[, , q] <- BCRM[, , q] + BBRM[, , q + 1]
  }

  # One-hot encode tmp$Q into Uq[i, j, tmp$Q[i,j]] = 1 using matrix indexing.
  # Replaces a nobs*nitems R-level double loop with a single C-level write
  # into the flat backing storage of the 3-D array.
  # Missing entries (tmp$Z == 0, flagged as tmp$Q == -1 by dataFormat) are
  # left at zero; every downstream use of Uq is masked by tmp$Z so the
  # missing-cell values are never read.
  Uq <- array(0, dim = c(nobs, nitems, maxQ))
  valid <- as.vector(tmp$Z) == 1
  Uq[cbind(
    rep(seq_len(nobs), times = nitems)[valid],
    rep(seq_len(nitems), each = nobs)[valid],
    as.vector(tmp$Q)[valid]
  )] <- 1

  # Precompute Z * Uq[,,q] once; neither Z nor Uq changes after this point.
  # Uq is stored column-major with dim1 (nobs) varying fastest, so multiplying
  # by as.vector(Z) recycles Z across the maxQ slices, giving ZU[i,j,q] =
  # Z[i,j] * Uq[i,j,q]. This replaces repeated elementwise products inside
  # the EM loop and the post-EM fit blocks.
  ZU <- Uq * as.vector(tmp$Z)

  if (model != 2) {
    Fil <- diag(rep(1, ncls))
  } else {
    Fil <- create_filter_matrix(ncls)
  }
  # Initialize smoothed_memb so that it is defined even when the EM loop
  # exits before reaching the smoothing step (e.g. immediate convergence).
  smoothed_memb <- matrix(0, nrow = nobs, ncol = ncls)
  # iteration start ---------------------------------------------------------
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
      converge <- FALSE
      FLG <- FALSE
      message("\nReached the maximum number of iterations.")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
    }

    emt <- emt + 1
    old_test_log_lik <- test_log_lik

    # Precompute log(BBRM[,,q] - BBRM[,,q+1] + const) once per EM iteration.
    # Both E-steps (class and field) use the same values across all q; the old
    # code recomputed this 2*maxQ times per iteration. Using drop=FALSE on
    # array slicing keeps the 3D shape even when nfld == 1.
    log_delta <- log(BBRM[, , seq_len(maxQ), drop = FALSE] -
      BBRM[, , seq_len(maxQ) + 1, drop = FALSE] + const)

    ## Msc <- Pi, Mjf
    tmpL <- matrix(0, nrow = nobs, ncol = ncls)
    for (q in 1:maxQ) {
      log_probs <- matrix(log_delta[, , q], nrow = nfld, ncol = ncls)
      tmpL <- tmpL + ZU[, , q] %*% fldmemb %*% log_probs
    }
    # Row-wise min via C-level pmin.int instead of apply (one R-level call per row):
    # do.call(pmin.int, as.data.frame(X)) passes each column as a separate argument,
    # and pmin.int(col1, col2, ...) returns the element-wise min across columns.
    minllsr <- do.call(pmin.int, as.data.frame(tmpL))
    expllsr <- exp(pmin(tmpL - minllsr, 700))
    clsmemb <- round(expllsr / rowSums(expllsr), 1e8)

    if (!is.null(conf_class_mat)) {
      clsmemb <- conf_class_mat
      smoothed_memb <- clsmemb
    } else {
      # For Ranklustering
      smoothed_memb <- clsmemb %*% Fil
    }

    ## Mjf <- Pi, Msc
    tmpH <- matrix(0, nrow = nitems, ncol = nfld)
    for (q in 1:maxQ) {
      log_probs <- matrix(log_delta[, , q], nrow = nfld, ncol = ncls)
      tmpH <- tmpH + (t(ZU[, , q]) %*% clsmemb) %*% t(log_probs)
    }

    minllsr <- do.call(pmin.int, as.data.frame(tmpH))
    expllsr <- exp(pmin(tmpH - minllsr, 700)) # 700 is approx upper limit for exp()
    fldmemb <- round(expllsr / rowSums(expllsr), 1e8)

    if (!any(is.null(conf_mat))) {
      fldmemb <- conf_mat
    }

    ## Maximization
    oldBCRM <- BCRM
    Ufcq <- array(0, dim = c(nfld, ncls, maxQ))
    cUfcq <- array(0, dim = c(nfld, ncls, maxQ))
    for (q in 1:maxQ) {
      Ufcq[, , q] <- (t(fldmemb) %*% t(ZU[, , q])) %*% clsmemb
    }
    # Apply Dirichlet prior (alpha parameter)
    Ufcq_prior <- Ufcq + alpha - 1
    Ufcq_prior <- pmax(Ufcq_prior, 1e-10)
    # Reverse cumulative sum along the 3rd axis:
    # cUfcq[, , q] = sum over k >= q of Ufcq_prior[, , k].
    # Equivalent to aperm(apply(., c(1,2), function(x) rev(cumsum(rev(x)))), c(2,3,1))
    # but vectorized: O(maxQ) array additions instead of nfld*ncls R-level calls.
    cUfcq <- Ufcq_prior
    for (q in rev(seq_len(maxQ - 1))) {
      cUfcq[, , q] <- cUfcq[, , q] + cUfcq[, , q + 1]
    }

    for (q in 1:maxQ) {
      BBRM[, , q] <- cUfcq[, , q] / cUfcq[, , 1]
    }

    ## Forced Ordering
    if (mic) {
      overall_order <- array(0, dim = ncls)
      for (i in 1:ncls) {
        total_expected <- 0
        for (j in 1:nfld) {
          field_expected <- sum(BBRM[j, i, 1:maxQ])
          total_expected <- total_expected + field_expected
        }
        overall_order[i] <- total_expected
      }
      overall_order <- order(overall_order)
      BBRM <- BBRM[, overall_order, ]
    }
    for (q in 1:maxQ) {
      BCRM[, , q] <- BBRM[, , q] - BBRM[, , q + 1]
    }

    test_log_lik <- 0
    for (q in 1:maxQ) {
      observed_mask <- ZU[, , q] == 1
      prob_leq_q1 <- t(fldmemb %*% BBRM[, , q] %*% t(clsmemb))
      prob_leq_q2 <- t(fldmemb %*% BBRM[, , q + 1] %*% t(clsmemb))
      prob_exact <- prob_leq_q1 - prob_leq_q2
      test_log_lik <- test_log_lik + sum(log(pmax(prob_exact[observed_mask], const)))
    }

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

    if (!is.finite(test_log_lik) || test_log_lik - old_test_log_lik <= 0) {
      BCRM <- oldBCRM
      if (!is.finite(test_log_lik)) converge <- FALSE
      break
    }
  }


  cls <- max.col(clsmemb, ties.method = "first")
  fld <- max.col(fldmemb, ties.method = "first")


  ### Model Fit
  testell <- 0
  for (q in 1:maxQ) {
    pred_prob <- t(fldmemb %*% BCRM[, , q] %*% t(clsmemb))
    observed_mask <- ZU[, , q] == 1
    testell <- testell + sum(log(pmax(pred_prob[observed_mask], const)))
  }

  if (model == 2) {
    nparam <- sum(diag(Fil)) * nfld * (maxQ - 1)
  } else {
    nparam <- ncls * nfld * (maxQ - 1)
  }

  ptn <- apply(tmp$Q, 1, function(x) paste(x, collapse = ""))
  benchG <- as.numeric(as.factor(ptn))

  fullG <- length(unique(benchG))
  benchmemb <- matrix(0, nrow = nobs, ncol = fullG)
  for (i in 1:nobs) {
    benchmemb[i, benchG[i]] <- 1
  }

  BenchFRQ <- array(NA, dim = c(nitems, fullG, maxQ))
  Bfcq <- array(0, dim = c(nitems, fullG, maxQ))
  for (q in 1:maxQ) {
    Bfcq[, , q] <- t(ZU[, , q]) %*% benchmemb
  }

  # rowSums(X, dims=2) sums over the 3rd dim, replacing apply(X, c(1,2), sum)
  BenchFRQ <- Bfcq / array(rowSums(Bfcq, dims = 2), dim = dim(BenchFRQ))
  BenchFRQ[is.nan(BenchFRQ)] <- const

  ell_B <- 0
  for (q in 1:maxQ) {
    ell_B <- ell_B + sum(t(ZU[, , q]) %*% benchmemb * log(BenchFRQ[, , q] + const))
  }
  bench_nparam <- nitems * fullG

  # Zrep * Uq == ZU elementwise; avoid the replicate() allocation by reusing ZU.
  # colSums(X, dims=1) sums over the 1st dim, replacing apply(X, c(2,3), sum).
  ZU_col_sums <- colSums(ZU, dims = 1)
  NullFRQ <- ZU_col_sums / colSums(tmp$Z)
  ell_N <- sum(ZU_col_sums * log(NullFRQ + const))
  null_nparam <- nitems


  df_B <- bench_nparam - null_nparam
  chi_B <- 2 * (ell_B - ell_N)
  # Analysis model
  chi_A <- 2 * (ell_B - test_log_lik)
  df_A <- bench_nparam - nparam
  FitIndices <- structure(
    c(list(
      model_log_like = test_log_lik,
      bench_log_like = ell_B,
      null_log_like = ell_N,
      model_Chi_sq = chi_A,
      null_Chi_sq = chi_B,
      model_df = df_A,
      null_df = df_B
    ), calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)),
    class = c("exametrika", "ModelFit")
  )

  # output ----------------------------------------------------------
  cls <- max.col(clsmemb, ties.method = "first")
  fld <- max.col(fldmemb, ties.method = "first")
  check_empty_fields(fld, nfld)
  fldmemb01 <- sign(fldmemb - do.call(pmax.int, as.data.frame(fldmemb))) + 1
  flddist <- colSums(fldmemb01)
  clsmemb01 <- sign(clsmemb - do.call(pmax.int, as.data.frame(clsmemb))) + 1
  clsdist <- colSums(clsmemb01)
  StudentRank <- cbind(clsmemb, Estimate = cls)
  rownames(StudentRank) <- tmp$ID
  colnames(StudentRank) <- c(paste("Membership", 1:ncls), "Estimate")
  ## Expected score
  BFRP1 <- BFRP2 <- matrix(0, nrow = nfld, ncol = ncls)
  weights <- matrix(0, nrow = nfld, ncol = ncls)

  for (q in 1:maxQ) {
    contrib <- (t(fldmemb) %*% t(ZU[, , q]) %*% clsmemb) * (BCRM[, , q])
    BFRP1 <- BFRP1 + q * contrib
    weights <- weights + contrib
  }
  BFRP1 <- BFRP1 / weights

  for (i in 1:ncls) {
    for (j in 1:nfld) {
      BFRP2[j, i] <- mean(tmp$Z[cls == i, fld == j] * tmp$Q[cls == i, fld == j])
    }
  }

  TRP <- colSums(BFRP1)
  TRPlag <- TRP[2:ncls]
  TRPmic <- sum(TRPlag[1:(ncls - 1)] - TRP[1:(ncls - 1)] < 0, na.rm = TRUE)

  model_esp <- matrix(0, nrow = nfld, ncol = ncls)
  for (f in 1:nfld) {
    for (cc in 1:ncls) {
      model_esp[f, cc] <- sum((1:maxQ) * BCRM[f, cc, ])
    }
  }
  norm_frp <- (model_esp - 1) / (maxQ - 1)
  FRPIndex <- IRPindex(norm_frp)
  FRPmic <- sum(abs(FRPIndex$C))

  SOACflg <- WOACflg <- FALSE
  if (TRPmic == 0) {
    WOACflg <- TRUE
    if (FRPmic == 0) {
      SOACflg <- TRUE
    }
  }
  if (verbose) {
    if (SOACflg & WOACflg) {
      message("Strongly ordinal alignment condition was satisfied.")
    }
    if (!SOACflg & WOACflg) {
      message("Weakly ordinal alignment condition was satisfied.")
    }
  }


  msg <- ifelse(model == 1, "Class", "Rank")
  ret <- structure(list(
    Q = tmp$Q,
    Z = tmp$Z,
    testlength = nitems,
    msg = msg,
    model = model,
    mic = mic,
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
    FRPIndex = FRPIndex,
    TRP = TRP,
    CMD = colSums(clsmemb),
    RMD = colSums(clsmemb),
    FieldMembership = fldmemb,
    ClassMembership = clsmemb,
    SmoothedMembership = smoothed_memb,
    FieldEstimated = fld,
    ClassEstimated = cls,
    Students = StudentRank,
    BFRP = list(Weighted = BFRP1, Observed = BFRP2),
    TestFitIndices = FitIndices,
    log_lik = test_log_lik, # New naming convention
    SOACflg = SOACflg,
    WOACflg = WOACflg,
    # Deprecated fields (for backward compatibility)
    LogLik = test_log_lik
  ), class = c("exametrika", "ordinalBiclustering"))

  return(ret)
}
