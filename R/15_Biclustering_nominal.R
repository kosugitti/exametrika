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
#' @param verbose Logical; if TRUE, displays progress during estimation. Default is TRUE.
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
                                 mic = FALSE,
                                 maxiter = 100,
                                 verbose = TRUE,
                                 alpha = 1, ...) {
  tmp <- U
  maxQ <- max(tmp$Q)
  nobs <- NROW(tmp$Q)
  nitems <- NCOL(tmp$Q)
  const <- exp(-nitems)
  test_log_lik <- -1 / const
  old_test_log_lik <- -2 / const
  emt <- 0
  maxemt <- 100

  ncat <- as.vector(tmp$categories)

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

  if (alpha <= 0) {
    stop("alpha must be positive (alpha > 0)")
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

  ## 3-dimensiona allay(nfld x ncls x Q)
  BCRM <- array(NA, dim = c(nfld, ncls, maxQ))
  for (i in 1:nfld) {
    for (j in 1:ncls) {
      class_effect <- j / ncls
      field_effect <- (nfld - i + 1) / nfld
      BCRM[i, j, ] <- init_field_membership_probs(maxQ, class_effect, field_effect)
    }
  }

  Uq <- array(0, dim = c(nobs, nitems, maxQ))
  for (i in 1:nobs) {
    for (j in 1:nitems) {
      q <- tmp$Q[i, j]
      Uq[i, j, q] <- 1
    }
  }

  # iteration -------------------------------------------------------
  converge <- TRUE
  FLG <- TRUE
  while (FLG) {
    if (test_log_lik - old_test_log_lik < 1e-8 * abs(old_test_log_lik)) {
      FLG <- FALSE
      break
    }
    if (emt == maxemt) {
      message("\nReached ten times the maximum number of iterations.")
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

    # Apply Dirichlet prior (alpha parameter)
    BCRM <- (Ufcq + alpha - 1) / array(apply(Ufcq, c(1, 2), sum) + maxQ * alpha - maxQ, dim = dim(BCRM))

    test_log_lik <- 0
    for (q in 1:maxQ) {
      pred_prob <- t(fldmemb %*% BCRM[, , q] %*% t(clsmemb))
      observed_mask <- (tmp$Z * Uq[, , q]) == 1
      test_log_lik <- test_log_lik + sum(log(pmax(pred_prob[observed_mask], const)))
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

    if (test_log_lik - old_test_log_lik <= 0) {
      BCRM <- oldBCRM
      break
    }
  }


  # output ----------------------------------------------------------
  cls <- apply(clsmemb, 1, which.max)
  fld <- apply(fldmemb, 1, which.max)
  fldmemb01 <- sign(fldmemb - apply(fldmemb, 1, max)) + 1
  flddist <- colSums(fldmemb01)
  clsmemb01 <- sign(clsmemb - apply(clsmemb, 1, max)) + 1
  clsdist <- colSums(clsmemb01)
  StudentRank <- clsmemb
  rownames(StudentRank) <- tmp$ID


  # model fit -------------------------------------------------------
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
  fullG <- length(benchGroup)
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
  # Analysis model
  chi_A <- 2 * (ell_B - test_log_lik)
  df_A <- bench_nparam - nparam
  FitIndices <- calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)

  msg <- "Class"
  ret <- structure(list(
    Q = tmp$Q,
    Z = tmp$Z,
    testlength = nitems,
    msg = msg,
    converge = converge,
    nobs = nobs,
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
    log_lik = test_log_lik, # New naming convention
    # Deprecated fields (for backward compatibility)
    LogLik = test_log_lik
  ), class = c("exametrika", "nominalBiclustering"))

  return(ret)
}
