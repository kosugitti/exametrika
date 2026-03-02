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

  # Helper functions -------------------------------------------------------

  ## Compute U_fcq: sufficient statistics (nfld x ncls x maxQ)
  calc_Ufcq <- function(Uq, Z, cls01, fld01, maxQ) {
    nfld_l <- ncol(fld01)
    ncls_l <- ncol(cls01)
    U_fcq <- array(0, dim = c(nfld_l, ncls_l, maxQ))
    for (q in 1:maxQ) {
      U_fcq[, , q] <- t(fld01) %*% t(Z * Uq[, , q]) %*% cls01
    }
    return(U_fcq)
  }

  ## Log multivariate Beta function
  lmvbeta <- function(a) {
    sum(lgamma(a)) - lgamma(sum(a))
  }

  ## Log-sum-exp trick for numerical stability
  log_sum_exp <- function(x) {
    m <- max(x)
    m + log(sum(exp(x - m)))
  }

  ## Convert log-probabilities to probabilities
  log_to_prob <- function(log_p) {
    exp(log_p - log_sum_exp(log_p))
  }

  ## Expand 3D array by appending one zero-slice along dimension 1 (field axis)
  expand_dim1 <- function(A) {
    d <- dim(A)
    result <- array(0, dim = c(d[1] + 1, d[2], d[3]))
    result[1:d[1], , ] <- A
    result
  }

  ## Expand 3D array by appending one zero-slice along dimension 2 (class axis)
  expand_dim2 <- function(A) {
    d <- dim(A)
    result <- array(0, dim = c(d[1], d[2] + 1, d[3]))
    result[, 1:d[2], ] <- A
    result
  }

  ## BIC calculation for model selection
  bic_calc <- function(Uq, Z, fld01, cls01, BCRM, maxQ) {
    ncls_l <- ncol(cls01)
    nfld_l <- ncol(fld01)
    nobs_l <- nrow(cls01)
    testell <- 0
    for (q in 1:maxQ) {
      pred_prob <- t(fld01 %*% BCRM[, , q] %*% t(cls01))
      observed_mask <- (Z * Uq[, , q]) == 1
      testell <- testell + sum(log(pmax(pred_prob[observed_mask], const)))
    }
    nparam <- ncls_l * nfld_l * (maxQ - 1)
    bic <- -2 * testell + nparam * log(nobs_l)
    return(bic)
  }

  # Initialize -------------------------------------------------------------
  if (!is.null(seed)) {
    set.seed(seed)
  }
  limit_count <- 0
  iter <- 1

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
  colnames(cls01) <- paste("Class", 1:ncls)
  rownames(cls01) <- tmp$ID
  cls <- cls01 %*% (1:ncls)
  Nc <- colSums(cls01)

  ## Initial Field: one item per field
  nfld <- nitems
  fld01 <- matrix(0, nrow = nitems, ncol = nfld)
  for (i in 1:nitems) {
    fld01[i, i] <- 1
  }
  fld <- fld01 %*% (1:nfld)
  Nf <- colSums(fld01)

  # IRM Iteration (Collapsed Gibbs Sampler) --------------------------------
  IRM_FLG <- TRUE
  while (IRM_FLG) {
    iRand <- sample(1:nobs, nobs, replace = FALSE)

    # Compute U_fcq once before the class-side loop
    U_fcq <- calc_Ufcq(Uq, tmp$Z, cls01, fld01, maxQ)

    ## Gibbs sampler for Class c
    for (target in iRand) {
      # Compute v_fq: contribution of student "target" (class-independent)
      v_fq <- matrix(0, nrow = nfld, ncol = maxQ)
      for (q in 1:maxQ) {
        v_fq[, q] <- t(fld01) %*% (tmp$Z[target, ] * Uq[target, , q])
      }

      # Subtract target's contribution: U_fcq becomes U_fcq^{-s*}
      old_cls <- cls[target]
      for (q in 1:maxQ) {
        U_fcq[, old_cls, q] <- U_fcq[, old_cls, q] - v_fq[, q]
      }
      Nc[old_cls] <- Nc[old_cls] - 1

      # If the class disappeared
      if (Nc[old_cls] == 0) {
        ncls <- ncls - 1
        cls01 <- cls01[, -old_cls, drop = FALSE]
        cls <- cls01 %*% (1:ncls)
        Nc <- Nc[-old_cls]
        U_fcq <- U_fcq[, -old_cls, , drop = FALSE]
      }

      # CRP likelihood for existing classes
      exist_tab <- numeric(ncls)
      for (c in 1:ncls) {
        exist_tab[c] <- log(Nc[c] / (nobs - 1 + gamma_c) + const)
        for (f in 1:nfld) {
          nume <- U_fcq[f, c, ] + v_fq[f, ] + alpha_vec
          deno <- U_fcq[f, c, ] + alpha_vec
          exist_tab[c] <- exist_tab[c] + lmvbeta(nume) - lmvbeta(deno)
        }
      }

      # CRP likelihood for new class
      new_tab1 <- log(gamma_c / (nobs - 1 + gamma_c))
      new_tab2 <- 0
      for (f in 1:nfld) {
        new_tab2 <- new_tab2 + lmvbeta(v_fq[f, ] + alpha_vec) - lmvbeta(alpha_vec)
      }
      new_tab <- new_tab1 + new_tab2

      # Sample class assignment
      ptab <- log_to_prob(c(exist_tab, new_tab))
      sampled_value <- rmultinom(1, 1, ptab)
      delpos <- sampled_value[-(ncls + 1)]
      if (sampled_value[ncls + 1] == 0) {
        # Student s belongs to an existing class
        new_cls <- which.max(delpos)
        cls01[target, ] <- delpos
        cls[target] <- new_cls
      } else {
        # Student s belongs to a new class
        ncls <- ncls + 1
        cls01 <- cbind(cls01, 0)
        cls01[target, ] <- 0
        cls01[target, ncls] <- 1
        cls <- cls01 %*% (1:ncls)
        new_cls <- ncls
        U_fcq <- expand_dim2(U_fcq)
      }

      # Add back target's contribution to new class
      for (q in 1:maxQ) {
        U_fcq[, new_cls, q] <- U_fcq[, new_cls, q] + v_fq[, q]
      }
      Nc <- colSums(cls01)
    }

    ## Gibbs sampler for Field f
    oldfld <- fld
    jRand <- sample(1:nitems, nitems, replace = FALSE)

    # U_fcq is already up-to-date from class-side loop

    for (target in jRand) {
      # Compute v_cq: contribution of item "target" (field-independent)
      v_cq <- matrix(0, nrow = ncls, ncol = maxQ)
      for (q in 1:maxQ) {
        v_cq[, q] <- t(cls01) %*% (tmp$Z[, target] * Uq[, target, q])
      }

      # Subtract target's contribution: U_fcq becomes U_fcq^{-j*}
      old_fld <- fld[target]
      for (q in 1:maxQ) {
        U_fcq[old_fld, , q] <- U_fcq[old_fld, , q] - v_cq[, q]
      }
      Nf[old_fld] <- Nf[old_fld] - 1

      # If the field disappeared
      if (Nf[old_fld] == 0) {
        nfld <- nfld - 1
        fld01 <- fld01[, -old_fld, drop = FALSE]
        fld <- fld01 %*% (1:nfld)
        Nf <- Nf[-old_fld]
        U_fcq <- U_fcq[-old_fld, , , drop = FALSE]
      }

      # CRP likelihood for existing fields
      exist_tab <- numeric(nfld)
      for (f in 1:nfld) {
        exist_tab[f] <- log(Nf[f] / (nitems - 1 + gamma_f) + const)
        for (c in 1:ncls) {
          nume <- U_fcq[f, c, ] + v_cq[c, ] + alpha_vec
          deno <- U_fcq[f, c, ] + alpha_vec
          exist_tab[f] <- exist_tab[f] + lmvbeta(nume) - lmvbeta(deno)
        }
      }

      # CRP likelihood for new field
      new_tab1 <- log(gamma_f / (nitems - 1 + gamma_f))
      new_tab2 <- 0
      for (c in 1:ncls) {
        new_tab2 <- new_tab2 + lmvbeta(v_cq[c, ] + alpha_vec) - lmvbeta(alpha_vec)
      }
      new_tab <- new_tab1 + new_tab2

      # Sample field assignment
      ptab <- log_to_prob(c(exist_tab, new_tab))
      sampled_value <- rmultinom(1, 1, ptab)
      delpos <- sampled_value[-(nfld + 1)]
      if (sampled_value[nfld + 1] == 0) {
        # Item j belongs to an existing field
        new_fld <- which.max(delpos)
        fld01[target, ] <- delpos
        fld[target] <- new_fld
      } else {
        # Item j belongs to a new field
        nfld <- nfld + 1
        fld01 <- cbind(fld01, 0)
        fld01[target, ] <- 0
        fld01[target, nfld] <- 1
        fld <- fld01 %*% (1:nfld)
        new_fld <- nfld
        U_fcq <- expand_dim1(U_fcq)
      }

      # Add back target's contribution to new field
      for (q in 1:maxQ) {
        U_fcq[new_fld, , q] <- U_fcq[new_fld, , q] + v_cq[, q]
      }
      Nf <- colSums(fld01)
    }

    # Check convergence
    limit_count <- if (sum(abs(oldfld - fld)) == 0) {
      limit_count <- limit_count + 1
    } else {
      limit_count <- 0
    }

    if (verbose) {
      message(
        sprintf(
          "iter %d: match=%d nfld=%d ncls=%d",
          iter, limit_count, nfld, ncls
        )
      )
    }

    if (limit_count == stable_limit || iter == max_iter) {
      IRM_FLG <- FALSE
    } else {
      iter <- iter + 1
    }
  }

  colnames(cls01) <- paste("Class", 1:ncls)
  colnames(fld01) <- paste("Field", 1:nfld)

  # BCRM (Pi_fcq) estimation ------------------------------------------------
  U_fcq <- calc_Ufcq(Uq, tmp$Z, cls01, fld01, maxQ)
  BCRM <- sweep(U_fcq, 3, alpha_vec, "+")
  denom <- apply(BCRM, c(1, 2), sum)
  BCRM <- sweep(BCRM, c(1, 2), denom, "/")

  # Reorganizing small-sized classes ----------------------------------------
  bic <- bic_calc(Uq, tmp$Z, fld01, cls01, BCRM, maxQ)
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
      prob_S <- t(apply(log_S, 1, log_to_prob))
      cls <- apply(prob_S, 1, which.max)
      cls01 <- matrix(0, ncol = ncls, nrow = nobs)
      for (i in 1:nobs) {
        cls01[i, cls[i]] <- 1
      }

      # M-step
      U_fcq <- calc_Ufcq(Uq, tmp$Z, cls01, fld01, maxQ)
      BCRM <- sweep(U_fcq, 3, alpha_vec, "+")
      denom <- apply(BCRM, c(1, 2), sum)
      BCRM <- sweep(BCRM, c(1, 2), denom, "/")

      # Model fit
      bic <- bic_calc(Uq, tmp$Z, fld01, cls01, BCRM, maxQ)

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
  clsmemb <- t(apply(log_S, 1, log_to_prob))
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
  fldmemb <- t(apply(log_J, 1, log_to_prob))
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
  msg <- "Class"
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
