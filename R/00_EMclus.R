#' @title emclus function
#' @description
#' This function is used for both LCA (Latent Class Analysis) and
#' LRA (Latent Rank Analysis). LCA is considered a special case of LRA,
#' in which the filtering matrix is reduced to the identity matrix.
#' This function takes a dataset, the number of classes, and a filtering
#' matrix as input and returns the latent rank.
#' When a confirmatory IRP matrix (conf) is provided, non-NA cells are
#' held fixed throughout estimation and only NA cells are freely estimated.
#' @param U response matrix U of the examData class.
#' @param Z missing indicator matrix Z of the examData class.
#' @param Fil Filter matrix
#' @param ncls number of latent class
#' @param beta1 beta distribution parameter1 as prior density of rank reference matrix
#' @param beta2 beta distribution parameter2 as prior density of rank reference matrix
#' @param maxiter Maximum number of iterations.
#' @param mic Monotonic increasing IRP option
#' @param verbose verbose output Flag. default is FALSE
#' @param conf Confirmatory IRP matrix (ncls x testlength). Non-NA values are
#'   fixed, NA values are freely estimated. NULL means fully exploratory.
#' @noRd

emclus <- function(U, Z, ncls, Fil, beta1, beta2, maxiter = 100, mic = FALSE,
                   verbose = FALSE, conf = NULL) {
  # Initialize
  testlength <- NCOL(U)
  const <- exp(-testlength)
  test_log_lik <- -1 / const
  old_test_log_lik <- -2 / const
  item_log_lik <- rep(test_log_lik / testlength, testlength)
  classRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)

  # Prepare confirmatory constraints
  if (!is.null(conf)) {
    fixed <- !is.na(conf)
    classRefMat[fixed] <- conf[fixed]
  }

  ## EM algorithm
  emt <- 0
  converge <- TRUE
  FLG <- TRUE

  while (FLG) {
    emt <- emt + 1
    old_test_log_lik <- test_log_lik

    llmat <- U %*% t(log(classRefMat + const)) + (Z * (1 - U)) %*% t(log(1 - classRefMat + const))
    exp_llmat <- exp(llmat)
    postDist <- exp_llmat / rowSums(exp_llmat)

    smoothPost <- postDist %*% Fil
    correct_cls <- t(smoothPost) %*% U
    incorrect_cls <- t(smoothPost) %*% (Z * (1 - U))

    old_classRefMat <- classRefMat
    classRefMat <- (correct_cls + beta1 - 1) / (correct_cls + incorrect_cls + beta1 + beta2 - 2)
    classRefMat <- pmax(pmin(classRefMat, 1 - const), const)

    # Apply confirmatory constraints after M-step, before mic sort
    if (!is.null(conf)) {
      classRefMat[fixed] <- conf[fixed]
    }

    if (mic) {
      classRefMat <- apply(classRefMat, 2, sort)
    }

    item_log_lik <- colSums(correct_cls * log(classRefMat + const) + incorrect_cls * log(1 - classRefMat + const))
    test_log_lik <- sum(item_log_lik)
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
      classRefMat <- old_classRefMat
      FLG <- FALSE
    }
    if ((test_log_lik - old_test_log_lik) <= 0.0001 * abs(old_test_log_lik)) {
      FLG <- FALSE
    }
    if (emt == maxiter) {
      message("\nReached the maximum number of iterations.")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
      converge <- FALSE
      FLG <- FALSE
    }
  }

  ret <- list(
    iter = emt,
    converge = converge,
    postDist = postDist,
    classRefMat = classRefMat
  )
  return(ret)
}


#' @title SOM-based Latent Rank Estimation
#' @description
#' Estimates latent ranks using Self-Organizing Maps (SOM).
#' This function is used internally by LRA.binary when method = "SOM".
#' Unlike the GTM method (emclus), SOM uses online learning with
#' neighborhood functions to update the rank reference matrix.
#' @param U response matrix U of the examData class.
#' @param Z missing indicator matrix Z of the examData class.
#' @param ncls number of latent ranks
#' @param mic Monotonic increasing IRP option
#' @param maxiter Maximum number of iterations.
#' @param BIC.check If TRUE, convergence is checked using BIC values.
#' @param seed Random seed for reproducibility.
#' @param verbose verbose output Flag. default is FALSE
#' @param conf Confirmatory IRP matrix (ncls x testlength). Non-NA values are
#'   fixed, NA values are freely estimated. NULL means fully exploratory.
#' @return A list with iter, converge, postDist, classRefMat (same structure as emclus)
#' @noRd

somclus <- function(U, Z, ncls, mic = FALSE, maxiter = 100,
                    BIC.check = FALSE, seed = NULL, verbose = FALSE,
                    conf = NULL) {
  testlength <- NCOL(U)
  samplesize <- NROW(U)
  const <- exp(-testlength)

  # Prepare confirmatory constraint (transposed to match RefMat: testlength x ncls)
  if (!is.null(conf)) {
    conf_t <- t(conf)
    fixed_t <- !is.na(conf_t)
  }

  somt <- 0
  alpha1 <- 1
  alphaT <- 0.01
  sigma1 <- 1
  sigmaT <- 0.12

  alpha_list <- ((maxiter - 1:maxiter) * alpha1 + (1:maxiter - 1) * alphaT) / (maxiter - 1)
  sigma_list <- ((maxiter - 1:maxiter) * sigma1 + (1:maxiter - 1) * sigmaT) / (maxiter - 1)

  kappa1 <- 0.01
  kappaT <- 0.0001

  kappa_list <- ((maxiter - 1:maxiter) * kappa1 + (1:maxiter - 1) * kappaT) / (maxiter - 1)

  prior_list <- rep(1 / ncls, ncls)

  r_list <- seq(-ncls + 1, ncls - 1)
  hhhmat <- array(NA, c(maxiter, length(r_list)))
  for (t in 1:maxiter) {
    hhhmat[t, ] <- alpha_list[t] * ncls / samplesize * exp(-(r_list)^2 / (2 * ncls^2 * sigma_list[t]^2))
  }

  clsRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)
  RefMat <- t(clsRefMat)

  # Apply confirmatory constraints to initial values
  if (!is.null(conf)) {
    RefMat[fixed_t] <- conf_t[fixed_t]
  }

  oldBIC <- 1e5
  converge <- TRUE
  FLG <- TRUE
  while (FLG) {
    somt <- somt + 1

    if (somt <= maxiter) {
      h_count <- somt
    } else {
      h_count <- maxiter
    }

    loglike <- 0

    if (is.null(seed)) {
      set.seed(sum(U) + somt)
    } else {
      set.seed(seed)
    }

    is <- order(runif(samplesize, 1, 100))

    for (s in 1:samplesize) {
      ss <- is[s]
      mlrank <- U[ss, ] %*% log(RefMat + const) + (1 - U[ss, ]) %*% log(1 - RefMat + const) + log(prior_list)
      winner <- which.max(mlrank)
      loglike <- loglike + mlrank[winner]
      hhh <- matrix(rep(hhhmat[h_count, (ncls + 1 - winner):(2 * ncls - winner)], testlength),
        nrow = testlength, byrow = T
      )
      RefMat <- RefMat + hhh * (U[ss, ] - RefMat)
      prior_list <- prior_list + (kappa_list[h_count] / ncls)
      prior_list[winner] <- prior_list[winner] - kappa_list[h_count]
      prior_list[prior_list > 1] <- 1
      prior_list[prior_list < const] <- const
    }

    # Apply confirmatory constraints before mic sort
    if (!is.null(conf)) {
      RefMat[fixed_t] <- conf_t[fixed_t]
    }

    if (mic) {
      RefMat <- t(apply(RefMat, 1, sort))
    }
    llmat <- U %*% t(log(t(RefMat) + const)) + (Z * (1 - U)) %*%
      t(log(1 - t(RefMat) + const))
    expllmat <- exp(llmat)
    postdist <- expllmat / rowSums(expllmat)
    item_ell <- item_log_lik(U, Z, postdist, t(RefMat))
    if (BIC.check) {
      if (somt > maxiter * 10) {
        message("\nReached ten times the maximum number of iterations.")
        message("Warning: Algorithm may not have converged. Interpret results with caution.")
        converge <- FALSE
        FLG <- FALSE
        break
      }
      FI <- ItemFit(U, Z, item_ell, ncls)
      diff <- abs(oldBIC - FI$test$BIC)
      oldBIC <- FI$test$BIC
      if (diff < 1e-4) {
        message("\nConverged before reaching maximum iterations.")
        FLG <- FALSE
        break
      }
    } else {
      if (somt == maxiter) {
        message("\nReached the maximum number of iterations.")
        message("Warning: Algorithm may not have converged. Interpret results with caution.")
        converge <- FALSE
        FLG <- FALSE
      }
    }
  }

  ret <- list(
    iter = somt,
    converge = converge,
    postDist = postdist,
    classRefMat = t(RefMat)
  )
  return(ret)
}


#' @title calc final item log-likelihood
#' @description
#' Using the original data, class membership matrix, and class reference matrix,
#'  the log-likelihood for each item is calculated.
#' @param U response matrix U of the examData class.
#' @param Z missing indicator matrix Z of the examData class.
#' @param postDist class membership matrix
#' @param classRefMat class reference matrix
#' @noRd

#' @title Validate and align confirmatory IRP matrix
#' @description
#' Validates the confirmatory IRP matrix (conf) for use in LCA/LRA.
#' The user-facing format is items (rows) x ncls (columns), matching
#' the IRP output format. When conf has row names, matches them against
#' the item labels of the dataset and reorders/expands accordingly.
#' Items in the data but not in conf are set to NA (freely estimated).
#' When conf has no row names, validates dimension match by position.
#' Returns the transposed matrix (ncls x testlength) for internal use.
#' @param conf Confirmatory IRP matrix (items x ncls). Non-NA values are
#'   fixed, NA values are freely estimated. Row names, if present,
#'   are used for label-based matching against item_labels.
#' @param ncls Number of latent classes/ranks.
#' @param item_labels Character vector of item labels from the dataset.
#' @return The validated (and possibly reordered/expanded) conf matrix
#'   transposed to dimension ncls x length(item_labels) for internal use.
#' @noRd

validate_conf <- function(conf, ncls, item_labels) {
  testlength <- length(item_labels)

  if (!is.matrix(conf)) {
    stop("conf must be a matrix.")
  }
  if (ncol(conf) != ncls) {
    stop(sprintf(
      "conf must have %d columns (ncls/nrank), but got %d.",
      ncls, ncol(conf)
    ))
  }

  # Label-based matching when conf has row names
  if (!is.null(rownames(conf))) {
    conf_labels <- rownames(conf)
    # Check for unknown labels
    unknown <- setdiff(conf_labels, item_labels)
    if (length(unknown) > 0) {
      stop(sprintf(
        "conf contains item labels not found in the data: %s",
        paste(unknown, collapse = ", ")
      ))
    }
    # Build aligned matrix (NA for items not in conf)
    aligned <- matrix(NA, nrow = testlength, ncol = ncls)
    rownames(aligned) <- item_labels
    matched <- match(conf_labels, item_labels)
    aligned[matched, ] <- conf
    conf <- aligned
  } else {
    # Position-based: require exact dimension match
    if (nrow(conf) != testlength) {
      stop(sprintf(
        "conf must have %d rows (testlength), but got %d. Set row names for label-based matching.",
        testlength, nrow(conf)
      ))
    }
  }

  # Value range check
  non_na <- conf[!is.na(conf)]
  if (length(non_na) > 0 && any(non_na <= 0 | non_na >= 1)) {
    stop("Non-NA values in conf must be in the open interval (0, 1).")
  }

  # Transpose to ncls x testlength for internal use
  return(t(conf))
}


item_log_lik <- function(U, Z, postDist, classRefMat) {
  const <- exp(-NCOL(U))
  correct_cls <- t(postDist) %*% U
  incorrect_cls <- t(postDist) %*% (Z * (1 - U))
  item_ll <- colSums(correct_cls * log(classRefMat + const) + incorrect_cls * log(1 - classRefMat + const))
  return(item_ll)
}
