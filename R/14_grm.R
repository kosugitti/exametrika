#' @title cumulative probability of GRM
#' @param theta latent score of subject
#' @param a discriminant parameter of IRF
#' @param b difficulty parameter of IRF
#' @keywords internal

grm_cumprob <- function(theta, a, b) {
  return(1 / (1 + exp(-a * (theta - b))))
}

#' @title Probability function for GRM
#' @description
#' Calculates the probability of selecting each category given a latent trait value and item parameters.
#' @param theta Latent trait value of the subject
#' @param a Discrimination parameter of IRF
#' @param b Vector of difficulty parameters (thresholds) of IRF
#' @return Vector of category selection probabilities
#' @examples
#' \dontrun{
#' # Example for an item with 3 categories
#' a <- 1.5
#' b <- c(-1.0, 1.0)
#' theta <- 0
#' grm_prob(theta, a, b)
#' }
#' @export

grm_prob <- function(theta, a, b) {
  K <- length(b) + 1
  cum_p <- numeric(K + 1)
  cum_p[1] <- 1
  cum_p[K + 1] <- 0

  for (k in 1:(K - 1)) {
    cum_p[k + 1] <- grm_cumprob(theta, a, b[k])
  }
  p <- numeric(K)
  for (k in 1:K) {
    p[k] <- cum_p[k] - cum_p[k + 1]
  }
  return(p)
}

#' @title Item Information Function for GRM
#' @description
#' Calculates the value of the Item Information Function for the Graded Response Model.
#' @details
#' The information is Samejima's (1969) item information for the GRM,
#' \deqn{I(\theta) = \sum_{k=1}^{K} \frac{[P_k'(\theta)]^2}{P_k(\theta)},}
#' where \eqn{P_k(\theta) = P_{k-1}^*(\theta) - P_k^*(\theta)} is the
#' category response probability, \eqn{P_k^*(\theta)} is the cumulative
#' (boundary) probability with \eqn{P_0^* = 1} and \eqn{P_K^* = 0}, and
#' \eqn{P_k^{*\prime}(\theta) = a P_k^*(\theta) [1 - P_k^*(\theta)]}.
#' The logistic metric of the estimation routine is used as is (no
#' 1.702 scaling constant), so the information is consistent with the
#' parameters returned by \code{\link{GRM}} and with the posterior
#' standard deviations (PSD).
#' @param theta Latent trait value of the subject
#' @param a Discrimination parameter of IRF
#' @param b Vector of difficulty parameters (thresholds) of IRF
#' @return Value of the Item Information Function
#' @examples
#' \dontrun{
#' # Example for an item with 3 categories
#' a <- 1.5
#' b <- c(-1.0, 1.0)
#' thetas <- seq(-3, 3, by = 0.1)
#' info <- sapply(thetas, function(t) grm_iif(t, a, b))
#' plot(thetas, info, type = "l", xlab = "Theta", ylab = "Information")
#' }
#' @export


grm_iif <- function(theta, a, b) {
  K <- length(b) + 1
  # Boundary probabilities P*_0 = 1 > P*_1 > ... > P*_{K-1} > P*_K = 0
  cum_p <- c(1, grm_cumprob(theta, a, b), 0)
  # Category probabilities P_k = P*_{k-1} - P*_k
  p <- cum_p[1:K] - cum_p[2:(K + 1)]
  # dP*_k/dtheta = a P*_k (1 - P*_k); zero at both endpoints
  d_cum <- a * cum_p * (1 - cum_p)
  d_p <- d_cum[1:K] - d_cum[2:(K + 1)]
  info <- sum(d_p^2 / pmax(p, 1e-10))
  return(unname(info))
}

#' @title generate start values for optimize
#' @param tmp dataset
#' @keywords internal
generate_start_values <- function(tmp) {
  nobs <- nrow(tmp)
  nitems <- ncol(tmp)
  ncat <- apply(tmp, 2, max, na.rm = TRUE)

  cor_mat <- cor(tmp, use = "pairwise.complete.obs")
  diag(cor_mat) <- 1
  eigen_result <- eigen(cor_mat)
  loadings <- abs(eigen_result$vectors[, 1]) * sqrt(abs(eigen_result$values[1]))
  a_init <- loadings / sqrt(1 - loadings^2)
  a_init <- pmax(pmin(a_init, 2.5), 0.3)

  b_init <- vector("list", nitems)
  for (j in 1:nitems) {
    valid_resp <- tmp[!is.na(tmp[, j]), j]
    n_thresh <- ncat[j] - 1
    if (n_thresh == 1) {
      b_init[[j]] <- quantile(valid_resp, 0.5, na.rm = TRUE) - mean(valid_resp, na.rm = TRUE)
    } else {
      probs <- seq(0.1, 0.9, length.out = n_thresh)
      thresholds <- quantile(valid_resp, probs, na.rm = TRUE)
      thresholds <- thresholds - median(thresholds)
      thresholds <- sort(thresholds)
      for (k in 2:length(thresholds)) {
        if (thresholds[k] <= thresholds[k - 1]) {
          thresholds[k] <- thresholds[k - 1] + 0.1
        }
      }
      b_init[[j]] <- thresholds
    }
  }
  return(list(a = a_init, b = b_init))
}


#' @title convert parameters to optimization target
#' @param a_vec discrimination parameter vector
#' @param b_list threshold parameter list
#' @keywords internal
params_to_target <- function(a_vec, b_list) {
  nitems <- length(a_vec)
  target_a <- log(a_vec)
  target_b <- c()
  for (j in 1:nitems) {
    thresholds <- b_list[[j]]
    n_thresholds <- length(thresholds)
    if (n_thresholds == 1) {
      target_b <- c(target_b, thresholds)
    } else {
      target_b <- c(target_b, thresholds[1])
      for (k in 2:n_thresholds) {
        delta <- max(thresholds[k] - thresholds[k - 1], 0.01)
        target_b <- c(target_b, log(delta))
      }
    }
  }
  return(c(target_a, target_b))
}

#' @title Graded Response Model (GRM)
#' @description
#' Implements Samejima's (1969) Graded Response Model (GRM), which is an Item Response Theory
#' model for ordered categorical response data. The model estimates discrimination parameters
#' and category threshold parameters for each item. It is widely used in psychological measurement,
#' educational assessment, and other fields that deal with multi-step rating scales.
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class using the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. 1 indicates observed values, 0 indicates missing values.
#' @param w Item weight vector
#' @param na Specifies numbers or characters to be treated as missing values.
#' @param verbose Logical; if TRUE, shows progress of iterations (default: FALSE)
#'
#' @return A list of class "exametrika" and "GRM" containing the following elements:
#' \describe{
#'   \item{testlength}{Length of the test (number of items)}
#'   \item{nobs}{Sample size (number of rows in the dataset)}
#'   \item{log_lik}{Log-likelihood value at convergence}
#'   \item{iterations}{Number of iterations and function evaluations from optimization}
#'   \item{params}{Matrix containing the estimated item parameters}
#'   \item{EAP}{Ability parameters of examinees estimated by EAP method}
#'   \item{MAP}{Ability parameters of examinees estimated by MAP method}
#'   \item{PSD}{Posterior standard deviation of the ability parameters}
#'   \item{ItemFitIndices}{Fit indices for each item. See also \code{\link{ItemFit}}}
#'   \item{TestFitIndices}{Overall fit indices for the test. See also \code{\link{TestFit}}}
#' }
#'
#' @references
#' Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores.
#' Psychometrika Monograph Supplement, 34(4, Pt. 2), 1-100.
#'
#' @importFrom stats optim dnorm
#' @useDynLib exametrika, .registration=TRUE
#' @importFrom Rcpp evalCpp
#'
#' @examples
#' \dontrun{
#' # Apply GRM to example data
#' result <- GRM(J5S1000)
#' print(result)
#' plot(result, type = "IRF")
#' plot(result, type = "IIF")
#' plot(result, type = "TIF")
#' }
#' @export
#'
#'
GRM <- function(U, na = NULL, Z = NULL, w = NULL, verbose = FALSE) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (tmp$response.type != "ordinal") {
    response_type_error(tmp$response.type, "GRM")
  }

  tmp$Q[tmp$Z == 0] <- NA
  dat <- tmp$Q
  nitems <- NCOL(tmp$Z)
  nobs <- NROW(tmp$Z)

  # GRM internals (R and C++) index categories from 1..K, so remap each item's
  # responses to contiguous 1-based codes. Handles 0-based coding (e.g., 0..3)
  # and gaps (e.g., 1,2,4). ncat becomes the count of distinct valid responses.
  cat_levels <- lapply(seq_len(nitems), function(j) {
    sort(unique(dat[!is.na(dat[, j]), j]))
  })
  ncat <- vapply(cat_levels, length, integer(1))
  for (j in seq_len(nitems)) {
    dat[, j] <- match(dat[, j], cat_levels[[j]])
  }
  n_quad_points <- 51

  start_vals <- generate_start_values(dat)
  target <- params_to_target(start_vals$a, start_vals$b)
  if (verbose) {
    cat(
      "Parameters:", length(target), "| Initial LL:",
      round(log_lik_grm_cpp(target, dat, n_quad_points), 3), "\n"
    )
  }

  result <- optim(
    par = target,
    fn = function(x) -log_lik_grm_cpp(x, dat, n_quad_points),
    gr = function(x) -score_function_analytical_grm(x, dat, n_quad_points),
    method = "BFGS",
    control = list(
      maxit = 500,
      reltol = 1e-8,
      trace = if (verbose) 1 else 0
    )
  )
  final_params <- target_to_params_grm(result$par, nitems, ncat)

  est_a <- final_params$a
  est_b <- final_params$b
  est_mat <- matrix(NA, nrow = nitems, ncol = max(ncat))
  est_mat[, 1] <- est_a
  for (j in 1:nitems) {
    est_mat[j, 2:ncat[j]] <- est_b[[j]]
  }
  colnames(est_mat) <- c("Slope", paste0("Threshold", 1:(max(ncat) - 1)))
  rownames(est_mat) <- tmp$ItemLabel

  # subject params
  quadrature <- seq(-6, 6, 0.01)
  quad_weight <- dnorm(quadrature)
  quad_weight <- quad_weight / sum(quad_weight)
  nq <- length(quadrature)

  # Posterior over the quadrature grid:
  #   log P(theta_q | x_i) = log w(q) + sum_j log P_j(x_ij | theta_q) + const
  # (product of item category probabilities, accumulated in log space;
  # missing responses are simply skipped)
  log_post <- matrix(log(quad_weight), nrow = nobs, ncol = nq, byrow = TRUE)
  for (j in 1:nitems) {
    # K x nq matrix of category probabilities for item j
    Pj <- vapply(
      quadrature,
      function(t) grm_prob(t, est_a[j], est_b[[j]]),
      numeric(ncat[j])
    )
    lPj <- log(pmax(Pj, 1e-300))
    resp <- dat[, j]
    ok <- which(!is.na(resp))
    log_post[ok, ] <- log_post[ok, ] + lPj[resp[ok], , drop = FALSE]
  }

  # Normalize per subject (log-sum-exp for numerical stability)
  log_post <- log_post - apply(log_post, 1, max)
  post_theta <- exp(log_post)
  post_theta <- post_theta / rowSums(post_theta)
  score_idx <- apply(post_theta, 1, which.max)
  MAP <- quadrature[score_idx]
  # EAP
  theta_matrix <- matrix(quadrature, nrow = nobs, ncol = nq, byrow = TRUE)
  EAP <- rowSums(post_theta * theta_matrix)
  # PSD
  theta_squared_matrix <- matrix(quadrature^2, nrow = nobs, ncol = nq, byrow = TRUE)
  second_moment <- rowSums(post_theta * theta_squared_matrix)
  posterior_variance <- second_moment - EAP^2
  PSD <- sqrt(posterior_variance)

  # model fit
  const <- 1e-16
  ### Analysis model
  ell_A <- rep(0, nitems)
  for (j in 1:nitems) {
    for (i in 1:nobs) {
      theta_i <- EAP[i]
      resp <- dat[i, j]
      if (!is.na(resp)) {
        prob <- grm_prob(theta_i, est_a[j], est_b[[j]])[resp]
        ell_A[j] <- ell_A[j] + log(prob + const)
      }
    }
  }
  ### Null model
  response_list <- lapply(seq_len(nitems), function(j) {
    table(factor(dat[, j], levels = seq_len(ncat[j])))
  })
  valid_response <- apply(tmp$Z, 2, sum)
  ell_N <- rep(0, nitems)
  for (j in 1:nitems) {
    cat_probs <- response_list[[j]] / valid_response[j]
    nz <- response_list[[j]] > 0
    if (any(nz)) {
      ell_N[j] <- sum(response_list[[j]][nz] * log(cat_probs[nz]))
    }
  }
  ### Bench model
  patterns <- rowSums(dat, na.rm = TRUE)
  unique_ptn <- unique(patterns)
  n_pattern <- length(unique_ptn)
  pattern_groups <- match(patterns, unique_ptn)

  MsG <- matrix(0, nrow = nobs, ncol = n_pattern)
  for (i in 1:nobs) {
    MsG[i, pattern_groups[i]] <- 1
  }
  group_size <- colSums(MsG)
  ell_B <- rep(0, nitems)
  for (j in 1:nitems) {
    for (g in 1:n_pattern) {
      group_memb <- which(pattern_groups == g)
      valid_memb <- group_memb[tmp$Z[group_memb, j] == 1]
      if (length(valid_memb) > 0) {
        cat_counts <- table(factor(dat[valid_memb, j], levels = seq_len(ncat[j])))
        cat_probs <- cat_counts / sum(cat_counts)
        nz <- cat_counts > 0
        if (any(nz)) {
          ell_B[j] <- ell_B[j] + sum(cat_counts[nz] * log(cat_probs[nz]))
        }
      }
    }
  }

  chi_A <- 2 * (ell_B - ell_A)
  chi_B <- 2 * (ell_B - ell_N)
  # Free-parameter counts per item (matches exametrika convention in
  # Biclustering.ordinal: df = bench_nparam - restricted_nparam):
  #   bench: per raw-score group a multinomial over ncat categories
  #          -> n_pattern * (ncat - 1)
  #   null:  single multinomial across all examinees
  #          -> (ncat - 1)
  #   GRM:   one slope plus (ncat - 1) thresholds
  #          -> ncat
  bench_nparam <- n_pattern * (ncat - 1)
  null_nparam <- ncat - 1
  model_nparam <- ncat
  df_A <- bench_nparam - model_nparam
  df_B <- bench_nparam - null_nparam
  ItemFitIndices <- structure(
    c(list(
      model_log_like = ell_A,
      bench_log_like = ell_B,
      null_log_like = ell_N,
      model_Chi_sq = chi_A,
      null_Chi_sq = chi_B,
      model_df = df_A,
      null_df = df_B
    ), calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)),
    class = c("exametrika", "ModelFit")
  )
  TestFitIndices <- structure(
    c(list(
      model_log_like = sum(ell_A),
      bench_log_like = sum(ell_B),
      null_log_like = sum(ell_N),
      model_Chi_sq = sum(chi_A),
      null_Chi_sq = sum(chi_B),
      model_df = sum(df_A),
      null_df = sum(df_B)
    ), calcFitIndices(sum(chi_A), sum(chi_B), sum(df_A), sum(df_B), nobs)),
    class = c("exametrika", "ModelFit")
  )

  ret <- structure(list(
    testlength = nitems,
    nobs = nobs,
    log_lik = -result$value,
    iterations = result$counts,
    params = as.data.frame(est_mat),
    EAP = EAP,
    MAP = MAP,
    PSD = PSD,
    ItemFitIndices = ItemFitIndices,
    TestFitIndices = TestFitIndices
  ), class = c("exametrika", "GRM"))
  return(ret)
}
