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
                   verbose = FALSE, conf = NULL, tol = 1e-8) {
  # Initialize
  testlength <- NCOL(U)
  const <- exp(-testlength)
  # The starting value must be below any attainable log-likelihood. The old
  # sentinel -1/const = -exp(J) is not: for a short test with many
  # respondents it sits ABOVE the real log-likelihood, so the first cycle
  # looked like a decrease and the loop exited after one iteration while
  # still reporting convergence. -Inf is unconditionally below, and the
  # comparisons are skipped on the first pass instead.
  test_log_lik <- -Inf
  old_test_log_lik <- -Inf
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
    postDist <- row_softmax(llmat)

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
    # Convergence is judged on the OBSERVED-data log-likelihood
    #   sum_s log sum_c (1/C) prod_j p_cj^u (1-p_cj)^(1-u),
    # which is the quantity EM is guaranteed to increase. The expected
    # log-posterior that Shojima (2022, eq. 5.11-5.12) monitors instead is
    # Q(theta_t | theta_{t-1}), whose successive values condition on a moving
    # point and therefore need not increase; on short tests it does decrease,
    # and the "decreased -> revert and stop" rule then fires on a healthy
    # iteration. See NEWS for the numerical consequences.
    llmat_new <- U %*% t(log(classRefMat + const)) +
      (Z * (1 - U)) %*% t(log(1 - classRefMat + const))
    row_max <- apply(llmat_new, 1, max)
    test_log_lik <- sum(row_max + log(rowSums(exp(llmat_new - row_max)))) -
      NROW(U) * log(ncls)
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
    if (is.finite(old_test_log_lik)) {
      if (test_log_lik - old_test_log_lik <= 0) {
        classRefMat <- old_classRefMat
        FLG <- FALSE
      }
      if ((test_log_lik - old_test_log_lik) <= tol * abs(old_test_log_lik)) {
        FLG <- FALSE
      }
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
#' @param BIC.check If TRUE, stop early once the change in BIC falls below a
#'   threshold. This is an early-stopping option, not a convergence test: SOM
#'   has no convergence criterion and normally runs the full annealing schedule.
#' @param seed Random seed for reproducibility. It does not change the algorithm;
#'   the presentation order is redrawn every epoch either way.
#' @param verbose verbose output Flag. default is FALSE
#' @param conf Confirmatory IRP matrix (ncls x testlength). Non-NA values are
#'   fixed, NA values are freely estimated. NULL means fully exploratory.
#' @return A list with iter, converge, postDist, classRefMat (same structure as emclus).
#'   For SOM, converge is TRUE unless BIC.check early stopping was requested and
#'   failed to trigger within ten times maxiter.
#' @noRd

somclus <- function(U, Z, ncls, mic = FALSE, maxiter = 100,
                    BIC.check = FALSE, seed = NULL, verbose = FALSE,
                    conf = NULL) {
  testlength <- NCOL(U)
  samplesize <- NROW(U)
  const <- exp(-testlength)

  # Restore the caller's .Random.seed on exit; this function reseeds every epoch
  if (exists(".Random.seed", envir = globalenv())) {
    old_rng_state <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_rng_state, envir = globalenv()), add = TRUE)
  }

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

  # Base value for the per-epoch seed. `seed` only fixes which orders are drawn;
  # it does not alter the algorithm.
  seed_base <- if (is.null(seed)) sum(U) else seed

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

    # Presentation order. The original implementation reseeds every epoch with
    # SeedRandom[Total[uuu] + somt]. Adding somt keeps the run reproducible while
    # letting the order change from epoch to epoch; without it every epoch would
    # reuse one frozen order, which breaks the premise of online learning.
    set.seed(seed_base + somt)

    is <- order(runif(samplesize, 1, 100))

    # One epoch of online updates. The heavy inner loop over students lives in
    # src/som_core.cpp; the presentation order is drawn above so that set.seed()
    # still governs reproducibility.
    epoch <- som_epoch_cpp(
      RefMat = RefMat,
      prior_list = prior_list,
      U = U,
      order = as.integer(is),
      hhh_row = hhhmat[h_count, ],
      kappa = kappa_list[h_count],
      cnst = const,
      mic = mic,
      conf_t_ = if (is.null(conf)) NULL else conf_t,
      fixed_t_ = if (is.null(conf)) NULL else fixed_t
    )
    RefMat <- epoch$RefMat
    prior_list <- epoch$prior

    if (BIC.check) {
      # Only the early-stopping path needs the posterior every epoch; the default
      # path computes it once after the schedule is done (see below).
      llmat <- U %*% t(log(t(RefMat) + const)) + (Z * (1 - U)) %*%
        t(log(1 - t(RefMat) + const))
      postdist <- row_softmax(llmat)
      item_ell <- item_log_lik(U, Z, postdist, t(RefMat))
      if (somt > maxiter * 10) {
        message("\nEarly stopping did not trigger within ten times maxiter; estimation was cut off.")
        converge <- FALSE
        FLG <- FALSE
        break
      }
      FI <- ItemFit(U, Z, item_ell, ncls)
      diff <- abs(oldBIC - FI$test$BIC)
      oldBIC <- FI$test$BIC
      if (diff < 1e-4) {
        message("\nEarly stopping: the change in BIC fell below the threshold.")
        FLG <- FALSE
        break
      }
    } else {
      # SOM is designed to run its annealing schedule for maxiter epochs, and the
      # original (Module_LRA.wl) has no convergence test either. Finishing the
      # schedule is normal termination, not a failure to converge, so no warning.
      if (somt == maxiter) {
        FLG <- FALSE
      }
    }
  }

  # Posterior over ranks for the final reference matrix.
  llmat <- U %*% t(log(t(RefMat) + const)) + (Z * (1 - U)) %*%
    t(log(1 - t(RefMat) + const))
  postdist <- row_softmax(llmat)

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
