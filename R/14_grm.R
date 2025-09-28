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
  p <- grm_prob(theta, a, b)
  info <- 1
  for (c in 1:(K - 1)) {
    p_c <- grm_cumprob(theta, a, b)[c]
    q_c <- 1 - p_c

    p_c_plus1 <- grm_cumprob(theta, a, b)[c + 1]
    if (is.na(p_c_plus1)) {
      p_c_plus1 <- 0
    }
    q_c_plus1 <- 1 - p_c_plus1

    num <- (p_c * q_c - p_c_plus1 * q_c_plus1)^2
    den <- p_c
    info <- info + (1.702^2 * a^2 * num) / den
  }
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
#' @param verbose Logical; if TRUE, shows progress of iterations (default: TRUE)
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
#' \donttest{
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
GRM <- function(U, na = NULL, Z = NULL, w = NULL, verbose = TRUE) {
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
  ncat <- apply(dat, 2, function(x) max(x, na.rm = TRUE))
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
  # W(SxP), L(PxQ)
  W <- matrix(nrow = nobs, ncol = sum(ncat))
  L <- matrix(nrow = sum(ncat), ncol = nq)

  for (i in 1:nobs) {
    vec <- c()
    for (j in 1:nitems) {
      resp <- dat[i, j]
      v <- rep(0, ncat[j])
      v[resp] <- 1
      vec <- c(vec, v)
    }
    W[i, ] <- vec
  }

  for (j in 1:nitems) {
    a <- est_a[j]
    b <- est_b[[j]]
    start_pos <- c(0, cumsum(ncat)) + 1
    end_pos <- start_pos - 1
    start_pos <- start_pos[1:nitems]
    end_pos <- end_pos[-1]
    for (q in 1:nq) {
      L[start_pos[j]:end_pos[j], q] <- grm_prob(quadrature[q], a, b)
    }
  }

  L_weighted <- t(t(L) * quad_weight)
  post_theta <- W %*% L_weighted
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
  response_list <- apply(tmp$Q, 2, table)
  valid_response <- apply(tmp$Z, 2, sum)
  ell_N <- rep(0, nitems)
  for (j in 1:nitems) {
    cat_probs <- response_list[[j]] / valid_response[j]
    ell_N[j] <- sum(response_list[[j]] * log(cat_probs + const))
  }
  ### Bench model
  patterns <- rowSums(tmp$Q * tmp$Z, na.rm = TRUE)
  unique_ptn <- unique(patterns)
  n_pattern <- length(unique_ptn)
  pattern_groups <- match(patterns, unique_ptn)

  MsG <- matrix(0, nrow = nobs, ncol = n_pattern)
  for (i in 1:nobs) {
    MsG[i, pattern_groups[i]] <- 1
  }
  group_size <- colSums(MsG)
  ell_B <- rep(0, nitems)
  const <- exp(-nitems * 100)
  for (j in 1:nitems) {
    for (g in 1:n_pattern) {
      group_memb <- which(pattern_groups == g)
      valid_memb <- group_memb[tmp$Z[group_memb, j] == 1]
      if (length(valid_memb) > 0) {
        cat_counts <- table(factor(tmp$Q[valid_memb, j], levels = 0:ncat[j]))
        cat_probs <- cat_counts / sum(cat_counts)
        ell_B[j] <- ell_B[j] + sum(cat_counts * log(cat_probs + const))
      }
    }
  }

  chi_A <- 2 * (ell_B - ell_A)
  chi_B <- 2 * (ell_B - ell_N)
  df_B <- n_pattern * (ncat - 1)
  df_A <- df_B + 1
  ItemFitIndices <- calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)
  TestFitIndices <- calcFitIndices(sum(chi_A), sum(chi_B), sum(df_A), sum(df_B), nobs)

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
