#' @rdname Biclustering
#' @section Nominal Data Method:
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
Biclustering.nominal <- function(U,
                                 ncls = 2, nfld = 2,
                                 method = NULL,
                                 conf = NULL,
                                 mic = FALSE,
                                 maxiter = 100,
                                 verbose = TRUE, ...) {
  tmp <- U
  Q <- max(tmp$Q)
  nobs <- NROW(tmp$Q)
  nitems <- NCOL(tmp$Q)
  Z <- tmp$Z
  const <- exp(-nitems)
  testell <- -1 / const
  oldtestell <- -2 / const
  emt <- 0
  maxemt <- 100

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

  ## inner function
  calc_probs <- function(Q, class_effect, field_effect) {
    q_vals <- 1:Q / Q
    high_influence <- q_vals * class_effect
    low_influence <- q_vals[order(q_vals, decreasing = TRUE)] * field_effect
    probs <- high_influence + low_influence
    return(probs / sum(probs))
  }

  ## 3-dimensiona allay(nfld x ncls x Q)
  PiFRQ <- array(NA, dim = c(nfld, ncls, Q))
  for (i in 1:nfld) {
    for (j in 1:ncls) {
      class_effect <- j / ncls
      field_effect <- (nfld - i + 1) / nfld
      PiFRQ[i, j, ] <- calc_probs(Q, class_effect, field_effect)
    }
  }
  Uq <- array(NA, dim = c(nobs, nitems, Q))
  Uq <- simplify2array(lapply(1:Q, function(q) {
    (U == q) * 1 * Z
  }))


  # iteration -------------------------------------------------------
  FLG <- TRUE
  while (FLG) {
    if (testell - oldtestell < 1e-8 * abs(oldtestell)) {
      FLG <- FALSE
      break
    }
    if (emt == maxemt) {
      message("\nReached ten times the maximum number of iterations.")
      FLG <- FALSE
    }

    emt <- emt + 1
    oldtestell <- testell
    alpha <- array(1, Q)

    tmpL <- matrix(0, nrow = nobs, ncol = ncls)
    for (q in 1:Q) {
      tmpL <- tmpL + (Z * Uq[, , q]) %*% fldmemb %*% log(PiFRQ[, , q] + const)
    }

    minllsr <- apply(tmpL, 1, min)
    expllsr <- exp(pmin(tmpL - minllsr, 700))
    clsmemb <- round(expllsr / rowSums(expllsr), 1e8)

    tmpH <- matrix(0, nrow = nitems, ncol = nfld)
    for (q in 1:Q) {
      tmpH <- tmpH + (t(Z * Uq[, , q]) %*% clsmemb) %*% t(log(PiFRQ[, , q] + const))
    }

    minllsr <- apply(tmpH, 1, min)
    expllsr <- exp(pmin(tmpH - minllsr, 700))
    fldmemb <- round(expllsr / rowSums(expllsr), 1e8)

    if (!any(is.null(conf_mat))) {
      fldmemb <- conf_mat
    }

    ## Maximization
    oldPiFRQ <- PiFRQ
    Ufcq <- array(0, dim = c(nfld, ncls, Q))
    for (q in 1:Q) {
      Ufcq[, , q] <- (t(fldmemb) %*% t(Z * Uq[, , q])) %*% clsmemb
    }

    PiFRQ <- Ufcq / array(apply(Ufcq, c(1, 2), sum), dim = dim(PiFRQ))

    testell <- sum(Ufcq * log(PiFRQ + const))

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
      PiFRQ <- oldPiFRQ
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
  for (q in 1:Q) {
    testell <- testell + sum(t(fldmemb) %*% t(Z * Uq[, , q]) %*% clsmemb * log(PiFRQ[, , q] + const))
  }
  nparam <- ncls * nfld

  # Full model
  ptn <- apply(U, 1, function(x) paste(x, collapse = ""))
  benchGroup <- as.numeric(as.factor(ptn))
  fullG <- length(benchGroup)
  benchmemb <- matrix(0, nrow = nobs, ncol = fullG)
  for (i in 1:nobs) {
    benchmemb[i, benchGroup[i]] <- 1
  }

  BenchFRQ <- array(NA, dim = c(nitems, fullG, Q))
  Bfcq <- array(0, dim = c(nitems, fullG, Q))
  for (q in 1:Q) {
    Bfcq[, , q] <- (t(Z * Uq[, , q])) %*% benchmemb
  }

  BenchFRQ <- Bfcq / array(apply(Bfcq, c(1, 2), sum), dim = dim(BenchFRQ))

  ell_B <- 0
  for (q in 1:Q) {
    ell_B <- ell_B + sum(t(Z * Uq[, , q]) %*% benchmemb * log(BenchFRQ[, , q] + const))
  }
  bench_nparam <- nitems * fullG

  # Null model
  Zrep <- replicate(Q, Z)
  NullFRQ <- apply(Zrep * Uq, c(2, 3), sum) / apply(Z, 2, sum)
  ell_N <- sum(apply(Zrep * Uq, c(2, 3), sum) * log(NullFRQ + const))
  null_nparam <- nitems


  df_B <- bench_nparam - null_nparam
  chi_B <- 2 * (ell_B - ell_N)
  # Analysis model
  chi_A <- 2 * (ell_B - testell)
  df_A <- bench_nparam - nparam
  FitIndices <- calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)


  ret <- structure(list(
    Q = Q,
    testlength = nitems,
    nobs = nobs,
    Nclass = ncls,
    Nfield = nfld,
    N_Cycle = emt,
    LFD = flddist,
    LRD = clsdist,
    LCD = clsdist,
    FRP = PiFRQ,
    CMD = colSums(clsmemb),
    RMD = colSums(clsmemb),
    FieldMembership = fldmemb,
    ClassMembership = clsmemb,
    FieldEstimated = fld,
    ClassEstimated = cls,
    Students = StudentRank,
    TestFitIndices = FitIndices
  ), class = c("exametrika", "Biclustering", "nominal"))

  return(ret)
}
