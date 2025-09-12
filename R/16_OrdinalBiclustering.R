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
#' @param ... Additional arguments passed to specific methods.
#' @export
Biclustering.ordinal <- function(U,
                                 ncls = 2, nfld = 2,
                                 method = "B",
                                 conf = NULL,
                                 mic = FALSE,
                                 maxiter = 100,
                                 verbose = TRUE, ...) {
  tmp <- U
  maxQ <- max(tmp$Q)
  nobs <- NROW(tmp$Q)
  nitems <- NCOL(tmp$Q)
  ncat <- tmp$categories
  const <- exp(-nitems)
  testell <- -1 / const
  oldtestell <- -2 / const
  emt <- 0
  maxemt <- 100
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


  ## confirmatory
  # set conf_mat for confirmatory clustering
  if (!is.null(conf)) {
    if (verbose) {
      message("Confirmatory Clustering is chosen.")
    }
    if (is.vector(conf)) {
      # check size
      if (length(conf) != NCOL(U)) {
        stop("conf vector size does NOT match with data.")
      }
      conf_mat <- matrix(0, nrow = NCOL(U), ncol = max(conf))
      for (i in 1:NROW(conf_mat)) {
        conf_mat[i, conf[i]] <- 1
      }
    } else if (is.matrix(conf) | is.data.frame(conf)) {
      if (NROW(conf) != NCOL(U)) {
        stop("conf matrix size does NOT match with data.")
      }
      if (any(!conf %in% c(0, 1))) {
        stop("The conf matrix should only contain 0s and 1s.")
      }
      if (any(rowSums(conf) > 1)) {
        stop("The row sums of the conf matrix must be equal to 1.")
      }
    } else {
      stop("conf matrix is not set properly.")
    }
    ###
    nfld <- NCOL(conf_mat)
  } else {
    conf_mat <- NULL
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  ##
  fld0 <- ceiling(1:nitems / (nitems / nfld))
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

  Uq <- array(0, dim = c(nobs, nitems, maxQ))
  for (i in 1:nobs) {
    for (j in 1:nitems) {
      q <- tmp$Q[i, j]
      Uq[i, j, q] <- 1
    }
  }

  if (model != 2) {
    Fil <- diag(rep(1, ncls))
  } else {
    Fil <- create_filter_matrix(ncls)
  }
  # iteration start ---------------------------------------------------------
  converge <- TRUE
  FLG <- TRUE
  while (FLG) {
    if (testell - oldtestell < 1e-8 * abs(oldtestell)) {
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
    oldtestell <- testell
    alpha <- array(1, maxQ)

    ## Msc <- Pi, Mjf
    tmpL <- matrix(0, nrow = nobs, ncol = ncls)
    for (q in 1:maxQ) {
      tmpL <- tmpL + (tmp$Z * Uq[, , q]) %*% fldmemb %*% log((BBRM[, , q] - BBRM[, , q + 1]) + const)
    }
    minllsr <- apply(tmpL, 1, min)
    expllsr <- exp(pmin(tmpL - minllsr, 700))
    clsmemb <- round(expllsr / rowSums(expllsr), 1e8)

    # For Ranklustering
    smoothed_memb <- clsmemb %*% Fil

    ## Mjf <- Pi, Msc
    tmpH <- matrix(0, nrow = nitems, ncol = nfld)
    for (q in 1:maxQ) {
      tmpH <- tmpH + (t(tmp$Z * Uq[, , q]) %*% clsmemb) %*% t(log((BBRM[, , q] - BBRM[, , q + 1]) + const))
    }

    minllsr <- apply(tmpH, 1, min)
    expllsr <- exp(pmin(tmpH - minllsr, 700)) # 700はおよそexp関数の実数範囲の上限
    fldmemb <- round(expllsr / rowSums(expllsr), 1e8)

    ## Maximization
    oldBCRM <- BCRM
    Ufcq <- array(0, dim = c(nfld, ncls, maxQ))
    cUfcq <- array(0, dim = c(nfld, ncls, maxQ))
    for (q in 1:maxQ) {
      Ufcq[, , q] <- (t(fldmemb) %*% t(tmp$Z * Uq[, , q])) %*% clsmemb
    }
    cUfcq <- aperm(apply(Ufcq, c(1, 2), function(x) rev(cumsum(rev(x)))), c(2, 3, 1))

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

    testell <- 0
    for (q in 1:maxQ) {
      observed_mask <- (tmp$Z * Uq[, , q]) == 1
      prob_leq_q1 <- t(fldmemb %*% BBRM[, , q] %*% t(clsmemb))
      prob_leq_q2 <- t(fldmemb %*% BBRM[, , q + 1] %*% t(clsmemb))
      prob_exact <- prob_leq_q1 - prob_leq_q2
      testell <- testell + sum(log(pmax(prob_exact[observed_mask], const)))
    }

    if (verbose) {
      message(
        sprintf(
          "\r%-80s",
          paste0(
            "iter ", emt, " logLik ", format(testell, digits = 6)
          )
        ),
        appendLF = FALSE
      )
    }

    if (testell - oldtestell <= 0) {
      BCRM <- oldBCRM
      break
    }
  }


  cls <- apply(clsmemb, 1, which.max)
  fld <- apply(fldmemb, 1, which.max)


  ### Model Fit
  testell <- 0
  for (q in 1:maxQ) {
    pred_prob <- t(fldmemb %*% BCRM[, , q] %*% t(clsmemb))
    observed_mask <- (tmp$Z * Uq[, , q]) == 1
    testell <- testell + sum(log(pmax(pred_prob[observed_mask], const)))
  }

  if (model == 2) {
    nparam <- sum(diag(Fil)) * nfld * (maxQ - 1)
  } else {
    nparam <- ncls * nfld * (maxQ - 1)
  }

  ptn <- apply(tmp$Q, 1, function(x) paste(x, collapse = ""))
  benchG <- as.numeric(as.factor(ptn))

  fullG <- length(benchG)
  benchmemb <- matrix(0, nrow = nobs, ncol = fullG)
  for (i in 1:nobs) {
    benchmemb[i, benchG[i]] <- 1
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

  Zrep <- replicate(maxQ, tmp$Z)
  NullFRQ <- apply(Zrep * Uq, c(2, 3), sum) / apply(tmp$Z, 2, sum)
  ell_N <- sum(apply(Zrep * Uq, c(2, 3), sum) * log(NullFRQ + const))
  null_nparam <- nitems


  df_B <- bench_nparam - null_nparam
  chi_B <- 2 * (ell_B - ell_N)
  # Analysis model
  chi_A <- 2 * (ell_B - testell)
  df_A <- bench_nparam - nparam
  FitIndices <- calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)

  # output ----------------------------------------------------------
  cls <- apply(clsmemb, 1, which.max)
  fld <- apply(fldmemb, 1, which.max)
  fldmemb01 <- sign(fldmemb - apply(fldmemb, 1, max)) + 1
  flddist <- colSums(fldmemb01)
  clsmemb01 <- sign(clsmemb - apply(clsmemb, 1, max)) + 1
  clsdist <- colSums(clsmemb01)
  StudentRank <- clsmemb
  rownames(StudentRank) <- tmp$ID
  ## Expected score
  BFRP1 <- BFRP2 <- matrix(0, nrow = nfld, ncol = ncls)
  weights <- matrix(0, nrow = nfld, ncol = ncls)

  for (q in 1:maxQ) {
    contrib <- (t(fldmemb) %*% t(tmp$Z * Uq[, , q]) %*% clsmemb) * (BCRM[, , q])
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
  WOACflg <- FALSE
  if (TRPmic == 0) {
    WOACflg <- TRUE
    if (verbose) {
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
    Nclass = ncls,
    Nfield = nfld,
    N_Cycle = emt,
    LFD = flddist,
    LRD = clsdist,
    LCD = clsdist,
    FRP = BCRM,
    TRP = TRP,
    CMD = colSums(clsmemb),
    RMD = colSums(clsmemb),
    FieldMembership = fldmemb,
    ClassMembership = clsmemb,
    FieldEstimated = fld,
    ClassEstimated = cls,
    Students = StudentRank,
    BFRP = list(Weighted = BFRP1, Observed = BFRP2),
    TestFitIndices = FitIndices,
    LogLik = testell,
    WOACflg = WOACflg
  ), class = c("exametrika", "ordinalBiclustering"))

  return(ret)
}
