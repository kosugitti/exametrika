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


#' @title parameter transformation target_to_params
#' @param target optimize target vector
#' @param nitems number of items
#' @param ncat number of categories for each items
#' @keywords interenal

target_to_params_jac <- function(target, nitems, ncat) {
  a_vec <- exp(target[1:nitems])
  b_list <- vector("list", nitems)

  pos <- nitems
  for (j in 1:nitems) {
    n_thresholds <- ncat[j] - 1

    if (n_thresholds == 1) {
      b_list[[j]] <- target[pos + 1]
    } else {
      first_threshold <- target[pos + 1]
      deltas <- exp(target[(pos + 2):(pos + n_thresholds)])
      b_list[[j]] <- c(first_threshold, first_threshold + cumsum(deltas))
    }

    pos <- pos + n_thresholds
  }

  return(list(a = a_vec, b = b_list))
}

#' @title parameter transformation params_to_target
#' @param a_vec vector of descriminant parameters
#' @param b_list lists of difficuluty parameter vec
#' @keywords internal

params_to_target_jac <- function(a_vec, b_list) {
  nitems <- length(a_vec)
  target_a <- log(a_vec)

  target_b <- c()
  target_b <- c()

  for (j in 1:nitems) {
    thresholds <- b_list[[j]]
    n_thresholds <- length(thresholds)

    if (n_thresholds == 1) {
      target_b <- c(target_b, thresholds)
    } else {
      target_b <- c(target_b, thresholds[1])

      for (k in 2:n_thresholds) {
        delta <- thresholds[k] - thresholds[k - 1]
        if (delta <= 0) delta <- 0.01
        target_b <- c(target_b, log(delta))
      }
    }
  }

  return(c(target_a, target_b))
}

#' @title Jacobian function for GRM
#' @param target target vector
#' @param nitems number of items
#' @param ncat number of categories for each items
#' @keywords internal
Jacobian_grm <- function(target, nitems, ncat) {
  jac_list <- vector("list", nitems)
  pos <- nitems
  for (j in 1:nitems) {
    n_thresholds <- ncat[j] - 1
    jac_matrix <- matrix(0, n_thresholds, n_thresholds)

    if (n_thresholds == 1) {
      jac_matrix[1, 1] <- 1
    } else {
      jac_matrix[1, 1] <- 1

      for (i in 2:n_thresholds) {
        jac_matrix[i, 1] <- 1
        for (k in 2:i) {
          jac_matrix[i, k] <- exp(target[pos + k])
        }
      }
    }

    a_jacobian <- exp(target[j])

    jac_list[[j]] <- list(
      a_jacobian = a_jacobian,
      b_jacobian = jac_matrix
    )

    pos <- pos + n_thresholds
  }

  return(jac_list)
}

#' @title log_lik function for grm
#' @param target target vector
#' @param dat data set
#' @keywords internal
#'
log_lik_grm <- function(target, dat, verbose) {
  nobs <- NROW(dat)
  nitems <- NCOL(dat)
  ncat <- apply(dat, 2, max, na.rm = TRUE)
  tmp <- target_to_params_jac(target, nitems, ncat)
  a_vec <- tmp$a
  b_list <- tmp$b

  quadrature <- seq(-6, 6, length.out = 51)
  weights <- dnorm(quadrature)
  weights <- weights / sum(weights)
  nq <- length(quadrature)

  cum_pr_list <- list()
  cat_pr_list <- list()

  for (j in 1:nitems) {
    ncats <- ncat[j]
    cum_pr <- matrix(0, nrow = ncats + 1, ncol = nq)
    cum_pr[1, ] <- 1
    cum_pr[ncats + 1, ] <- 0
    for (k in 1:length(b_list[[j]])) {
      for (q in 1:nq) {
        cum_pr[k + 1, q] <- 1 / (1 + exp(-a_vec[j] * (quadrature[q] - b_list[[j]][k])))
      }
    }

    cat_pr <- matrix(0, nrow = ncats, ncol = nq)
    for (k in 1:ncats) {
      cat_pr[k, ] <- pmax(cum_pr[k, ] - cum_pr[k + 1, ], 1e-10)
    }
    cum_pr_list[[j]] <- cum_pr
    cat_pr_list[[j]] <- cat_pr
  }
  ll <- 0
  for (i in 1:nobs) {
    log_p <- numeric(nq)
    for (q in 1:nq) {
      log_p_iq <- 0
      for (j in 1:nitems) {
        resp <- dat[i, j]
        if (!is.na(resp)) {
          log_p_iq <- log_p_iq + log(cat_pr_list[[j]][resp, q])
        }
      }
      log_p[q] <- log_p_iq + log(weights[q])
    }
    log_p_max <- max(log_p)
    ll <- ll + log_p_max + log(sum(exp(log_p - log_p_max)))
  }
  return(ll)
}

#' @title score function for grm
#' @param target target vector
#' @param dat data set
#' @keywords internal
#'
score_function_with_Jacobian <- function(target, dat) {
  nobs <- NROW(dat)
  nitems <- NCOL(dat)
  ncat <- apply(dat, 2, max, na.rm = T)

  tmp <- target_to_params_jac(target, nitems, ncat)
  a_vec <- tmp$a
  b_list <- tmp$b

  jacobians <- Jacobian_grm(target, nitems, ncat)
  quadrature <- seq(-6, 6, length.out = 51)
  weights <- dnorm(quadrature)
  weights <- weights / sum(weights)
  nq <- length(quadrature)

  # prepare prob matrix size<PxQ>
  cat_pr_list <- list()
  cum_pr_list <- list()
  prod_pr_list <- list()

  for (j in 1:nitems) {
    ncats <- ncat[j]
    cum_pr <- matrix(0, nrow = ncats + 1, ncol = nq)
    cum_pr[1, ] <- 1
    cum_pr[ncats + 1, ] <- 0

    for (k in 1:length(b_list[[j]])) {
      for (q in 1:nq) {
        cum_pr[k + 1, q] <- 1 / (1 + exp(-a_vec[j] * (quadrature[q] - b_list[[j]][k])))
      }
    }

    cat_pr <- matrix(0, nrow = ncats, ncol = nq)
    for (k in 1:ncats) {
      cat_pr[k, ] <- pmax(cum_pr[k, ] - cum_pr[k + 1, ], 1e-10)
    }

    prod_pr <- cum_pr * (1 - cum_pr)

    cum_pr_list[[j]] <- cum_pr
    cat_pr_list[[j]] <- cat_pr
    prod_pr_list[[j]] <- prod_pr
  }

  p_xz <- matrix(0, nobs, nq)
  for (i in 1:nobs) {
    for (q in 1:nq) {
      log_lik_q <- 0
      for (j in 1:nitems) {
        resp <- dat[i, j]
        if (!is.na(resp)) {
          log_lik_q <- log_lik_q + log(cat_pr_list[[j]][resp, q])
        }
      }
      p_xz[i, q] <- exp(log_lik_q)
    }
  }
  # likelihood
  p_x <- rowSums(p_xz * rep(weights, each = nobs))
  # posterior distribution
  p_z_x <- p_xz / p_x


  model_score_a <- numeric(nitems)
  model_score_b <- vector("list", nitems)
  for (j in 1:nitems) {
    model_score_b[[j]] <- numeric(length(b_list[[j]]))
  }

  for (i in 1:nobs) {
    for (j in 1:nitems) {
      resp <- dat[i, j]
      if (!is.na(resp)) {
        for (q in 1:nq) {
          theta_q <- quadrature[q]
          p_q <- p_z_x[i, q] * weights[q]

          da_j <- 0
          if (resp <= length(b_list[[j]])) {
            gamma_k1 <- cum_pr_list[[j]][resp + 1, q]
            prod_k1 <- prod_pr_list[[j]][resp + 1, q]
            da_j <- da_j - (theta_q - b_list[[j]][resp]) * prod_k1 / cat_pr_list[[j]][resp, q]
          }
          if (resp > 1 && resp - 1 <= length(b_list[[j]])) {
            gamma_k <- cum_pr_list[[j]][resp, q]
            prod_k <- prod_pr_list[[j]][resp, q]
            da_j <- da_j + (theta_q - b_list[[j]][resp - 1]) * prod_k / cat_pr_list[[j]][resp, q]
          }
          model_score_a[j] <- model_score_a[j] + p_q * da_j

          for (k in 1:length(b_list[[j]])) {
            db_jk <- 0
            if (k == resp - 1 && resp > 0) {
              prod_k <- prod_pr_list[[j]][resp, q]
              db_jk <- db_jk + a_vec[j] * prod_k / cat_pr_list[[j]][resp, q]
            }
            if (k == resp && resp <= length(b_list[[j]])) {
              prod_k1 <- prod_pr_list[[j]][resp + 1, q]
              db_jk <- db_jk - a_vec[j] * prod_k1 / cat_pr_list[[j]][resp, q]
            }
            model_score_b[[j]][k] <- model_score_b[[j]][k] + p_q * db_jk
          }
        }
      }
    }
  }

  # set 0 for stability
  for (j in 1:nitems) {
    if (abs(model_score_a[j]) < 1e-8) model_score_a[j] <- 0
    for (k in 1:length(model_score_b[[j]])) {
      if (abs(model_score_b[[j]][k]) < 1e-8) model_score_b[[j]][k] <- 0
    }
  }

  target_score <- numeric(length(target))
  for (j in 1:nitems) {
    target_score[j] <- -1 * model_score_a[j] * jacobians[[j]]$a_jacobian
  }
  pos <- nitems
  for (j in 1:nitems) {
    n_thresholds <- length(b_list[[j]])
    target_grad_b <- as.vector(t(jacobians[[j]]$b_jacobian) %*% model_score_b[[j]])
    target_score[(pos + 1):(pos + n_thresholds)] <- target_grad_b
    pos <- pos + n_thresholds
  }

  return(-target_score)
}

#' @title generate start values for optimize
#' @param tmp dataset
#' @keywords internal

generate_start_values <- function(tmp) {
  nobs <- NROW(tmp$Z)
  nitems <- NCOL(tmp$Q)
  a <- numeric(nitems)
  b <- vector("list", nitems)

  cor <- PolychoricCorrelationMatrix(tmp)
  eV <- eigen(cor)
  loadings <- abs(eV$vectors[, 1]) * sqrt(eV$values[1])
  a <- loadings / (1 - loadings^2)
  a <- pmax(pmin(a, 2.5), 0.3)


  for (j in 1:nitems) {
    counts <- tabulate(tmp$Q[, j])
    probs <- cumsum(counts / sum(counts))
    thresholds <- qnorm(probs[-length(probs)])

    thresholds <- pmax(pmin(thresholds, 4), -4)
    for (k in 2:length(thresholds)) {
      if (thresholds[k] <= thresholds[k - 1]) {
        thresholds[k] <- thresholds[k - 1] + 0.1
      }
    }
    b[[j]] <- thresholds
  }
  return(list(a = a, b = b))
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
#' @importFrom stats rnorm optim dnorm
#' @importFrom graphics plot lines
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

  if (U$response.type != "ordinal") {
    response_type_error(U$response.type, "GRM")
  }

  tmp$Q[tmp$Z == 0] <- NA
  dat <- tmp$Q
  nitems <- NCOL(tmp$Z)
  nobs <- NROW(tmp$Z)
  ncat <- apply(dat, 2, function(x) max(x, na.rm = TRUE))
  start_vals <- generate_start_values(tmp)
  target <- target <- params_to_target_jac(start_vals$a, start_vals$b)
  # item params
  fit <- optim(
    par = target,
    fn = log_lik_grm,
    gr = score_function_with_Jacobian,
    method = "BFGS",
    control = list(
      maxit = 300,
      reltol = 1e-16,
      fnscale = -1,
      trace = if (verbose) 1 else 0
    ),
    dat = tmp$Q
  )


  est_params <- target_to_params_jac(fit$par, nitems, ncat)
  est_a <- esvalueest_a <- est_params$a
  est_b <- lapply(1:nitems, function(i) {
    est_params$b[[i]] * est_a[i]
  })

  est_mat <- matrix(NA, nrow = nitems, ncol = max(ncat))
  est_mat[, 1] <- est_a
  for (j in 1:nitems) {
    est_mat[j, 2:ncat[j]] <- est_b[[j]]
  }
  colnames(est_mat) <- c("Descriminate", paste0("Threshold", 1:(max(ncat) - 1)))
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
    params = as.data.frame(est_mat),
    EAP = EAP,
    MAP = MAP,
    PSD = PSD,
    ItemFitIndices = ItemFitIndices,
    TestFitIndices = TestFitIndices
  ), class = c("exametrika", "GRM"))
  return(ret)
}
