# IRM Gibbs Sampler Core and Helper Functions
#
# Internal functions shared by Biclustering_IRM.nominal and Biclustering_IRM.ordinal.
# These are not exported.

# Helper functions -------------------------------------------------------

#' Compute U_fcq: sufficient statistics (nfld x ncls x maxQ)
#' @noRd
irm_calc_Ufcq <- function(Uq, Z, cls01, fld01, maxQ) {
  nfld_l <- ncol(fld01)
  ncls_l <- ncol(cls01)
  U_fcq <- array(0, dim = c(nfld_l, ncls_l, maxQ))
  for (q in 1:maxQ) {
    U_fcq[, , q] <- t(fld01) %*% t(Z * Uq[, , q]) %*% cls01
  }
  return(U_fcq)
}

#' Log multivariate Beta function
#' @noRd
irm_lmvbeta <- function(a) {
  sum(lgamma(a)) - lgamma(sum(a))
}

#' Log-sum-exp trick for numerical stability
#' @noRd
irm_log_sum_exp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}

#' Convert log-probabilities to probabilities
#' @noRd
irm_log_to_prob <- function(log_p) {
  exp(log_p - irm_log_sum_exp(log_p))
}

#' Expand 3D array by appending one zero-slice along dimension 1 (field axis)
#' @noRd
irm_expand_dim1 <- function(A) {
  d <- dim(A)
  result <- array(0, dim = c(d[1] + 1, d[2], d[3]))
  result[1:d[1], , ] <- A
  result
}

#' Expand 3D array by appending one zero-slice along dimension 2 (class axis)
#' @noRd
irm_expand_dim2 <- function(A) {
  d <- dim(A)
  result <- array(0, dim = c(d[1], d[2] + 1, d[3]))
  result[, 1:d[2], ] <- A
  result
}

#' BIC calculation for IRM model selection
#' @noRd
irm_bic_calc <- function(Uq, Z, fld01, cls01, BCRM, maxQ, const) {
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

# Gibbs Sampler Core -----------------------------------------------------

#' Collapsed Gibbs Sampler for IRM (Chinese Restaurant Process)
#'
#' Shared core for nominal and ordinal IRM. Performs CRP-based collapsed
#' Gibbs sampling with Dirichlet-Multinomial likelihood. Data type
#' (nominal/ordinal) does not affect the Gibbs phase.
#'
#' @param Uq One-hot encoded response array (nobs x nitems x maxQ).
#' @param Z Missing indicator matrix (nobs x nitems).
#' @param cls01 Initial class membership matrix (nobs x ncls).
#' @param fld01 Initial field membership matrix (nitems x nfld).
#' @param gamma_c CRP concentration parameter for classes.
#' @param gamma_f CRP concentration parameter for fields.
#' @param alpha_vec Dirichlet prior parameter vector (length maxQ).
#' @param max_iter Maximum number of Gibbs iterations.
#' @param stable_limit Number of consecutive stable iterations for convergence.
#' @param verbose If TRUE, display progress messages.
#' @return A list with:
#'   \describe{
#'     \item{cls01}{Final class membership matrix (nobs x ncls).}
#'     \item{fld01}{Final field membership matrix (nitems x nfld).}
#'     \item{U_fcq}{Final sufficient statistics array (nfld x ncls x maxQ).}
#'     \item{ncls}{Number of classes.}
#'     \item{nfld}{Number of fields.}
#'     \item{cls}{Class assignment vector (length nobs).}
#'     \item{fld}{Field assignment vector (length nitems).}
#'     \item{Nc}{Class sizes (length ncls).}
#'     \item{Nf}{Field sizes (length nfld).}
#'     \item{n_gibbs_iter}{Number of Gibbs iterations performed.}
#'   }
#' @noRd
irm_gibbs_core <- function(Uq, Z, cls01, fld01,
                           gamma_c, gamma_f, alpha_vec,
                           max_iter, stable_limit, verbose) {
  nobs <- nrow(cls01)
  nitems <- nrow(fld01)
  ncls <- ncol(cls01)
  nfld <- ncol(fld01)
  maxQ <- length(alpha_vec)
  const <- exp(-nitems)

  cls <- as.vector(cls01 %*% (1:ncls))
  fld <- as.vector(fld01 %*% (1:nfld))
  Nc <- colSums(cls01)
  Nf <- colSums(fld01)

  limit_count <- 0
  iter <- 1

  IRM_FLG <- TRUE
  while (IRM_FLG) {
    iRand <- sample(1:nobs, nobs, replace = FALSE)

    # Compute U_fcq once before the class-side loop
    U_fcq <- irm_calc_Ufcq(Uq, Z, cls01, fld01, maxQ)

    ## Gibbs sampler for Class c
    for (target in iRand) {
      # Compute v_fq: contribution of student "target" (class-independent)
      v_fq <- matrix(0, nrow = nfld, ncol = maxQ)
      for (q in 1:maxQ) {
        v_fq[, q] <- t(fld01) %*% (Z[target, ] * Uq[target, , q])
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
        cls <- as.vector(cls01 %*% (1:ncls))
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
          exist_tab[c] <- exist_tab[c] + irm_lmvbeta(nume) - irm_lmvbeta(deno)
        }
      }

      # CRP likelihood for new class
      new_tab1 <- log(gamma_c / (nobs - 1 + gamma_c))
      new_tab2 <- 0
      for (f in 1:nfld) {
        new_tab2 <- new_tab2 + irm_lmvbeta(v_fq[f, ] + alpha_vec) - irm_lmvbeta(alpha_vec)
      }
      new_tab <- new_tab1 + new_tab2

      # Sample class assignment
      ptab <- irm_log_to_prob(c(exist_tab, new_tab))
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
        cls <- as.vector(cls01 %*% (1:ncls))
        new_cls <- ncls
        U_fcq <- irm_expand_dim2(U_fcq)
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
        v_cq[, q] <- t(cls01) %*% (Z[, target] * Uq[, target, q])
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
        fld <- as.vector(fld01 %*% (1:nfld))
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
          exist_tab[f] <- exist_tab[f] + irm_lmvbeta(nume) - irm_lmvbeta(deno)
        }
      }

      # CRP likelihood for new field
      new_tab1 <- log(gamma_f / (nitems - 1 + gamma_f))
      new_tab2 <- 0
      for (c in 1:ncls) {
        new_tab2 <- new_tab2 + irm_lmvbeta(v_cq[c, ] + alpha_vec) - irm_lmvbeta(alpha_vec)
      }
      new_tab <- new_tab1 + new_tab2

      # Sample field assignment
      ptab <- irm_log_to_prob(c(exist_tab, new_tab))
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
        fld <- as.vector(fld01 %*% (1:nfld))
        new_fld <- nfld
        U_fcq <- irm_expand_dim1(U_fcq)
      }

      # Add back target's contribution to new field
      for (q in 1:maxQ) {
        U_fcq[new_fld, , q] <- U_fcq[new_fld, , q] + v_cq[, q]
      }
      Nf <- colSums(fld01)
    }

    # Check convergence: field assignment stability (matching Mathematica original)
    limit_count <- if (sum(abs(oldfld - fld)) == 0) {
      limit_count + 1
    } else {
      0
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

  colnames(cls01) <- paste("Rank", 1:ncls)
  colnames(fld01) <- paste("Field", 1:nfld)

  list(
    cls01 = cls01, fld01 = fld01,
    U_fcq = U_fcq,
    ncls = ncls, nfld = nfld,
    cls = cls, fld = fld,
    Nc = Nc, Nf = Nf,
    n_gibbs_iter = iter
  )
}
