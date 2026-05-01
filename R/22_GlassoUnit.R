#' @title Soft thresholding operator
#' @param y Input value
#' @param lambda Threshold value
#' @return Soft-thresholded value
#' @noRd

soft_thresholding <- function(y, lambda) {
  sign(y) * pmax(abs(y) - lambda, 0)
}


#' @title Single-lambda Graphical Lasso
#' @description
#' Estimates a precision matrix at a single lambda value via block coordinate
#' descent with cyclical coordinate descent for the inner lasso step.
#' @param S Sample covariance or correlation matrix (p x p)
#' @param lambda Regularization parameter (scalar)
#' @param W_init Initial working covariance matrix. If NULL, initialized as S + diag(lambda, p)
#' @param Beta_init Initial beta cache (p-1 x p matrix). If NULL, initialized as zeros
#' @param eps Convergence tolerance for both inner and outer loops
#' @param max_iter Maximum number of outer iterations
#' @return A list with elements Theta (precision matrix), W (working covariance),
#'   Beta (beta cache), niter (number of outer iterations), converged (logical)
#' @noRd

glasso_one <- function(S, lambda, W_init = NULL, Beta_init = NULL, eps = 1e-6, max_iter = 1000, ...) {
  niter <- 0
  p <- ncol(S)

  if (is.null(W_init)) {
    W_init <- S + diag(lambda, ncol(S))
  }
  if (is.null(Beta_init)) {
    Beta_init <- matrix(0, nrow = nrow(S) - 1, ncol = ncol(S))
  }

  ## Size check

  if (sum(dim(S) != dim(W_init)) != 0) {
    stop("wrong size Winit")
  }
  if (sum(dim(Beta_init) != c(p - 1, p)) != 0) {
    stop("wrong size Beta init")
  }

  W <- W_init
  beta_cache <- Beta_init


  W_old <- W
  W_FLG <- TRUE
  Theta <- matrix(0, nrow = p, ncol = p)

  while (W_FLG) {
    niter <- niter + 1
    for (j in 1:p) {
      W11 <- W[-j, -j]
      s12 <- S[-j, j]
      beta <- beta_cache[, j]
      beta_FLG <- TRUE
      while (beta_FLG) {
        beta_old <- beta
        for (k in 1:(p - 1)) {
          r <- s12[k] - W11[k, ] %*% beta + W11[k, k] * beta[k]
          beta[k] <- soft_thresholding(r, lambda) / W11[k, k]
        }
        diff <- sum(abs(beta - beta_old))
        if (diff < eps) {
          beta_cache[, j] <- beta
          beta_FLG <- FALSE
        }
      }
      W12 <- W11 %*% beta
      W[-j, j] <- W[j, -j] <- W12

      theta22 <- 1 / (W[j, j] - sum(W12 * beta))
      theta12 <- -beta * as.numeric(theta22)
      Theta[j, j] <- theta22
      Theta[-j, j] <- theta12
      Theta[j, -j] <- theta12
    }
    if (max(abs(W - W_old)) < eps) {
      W_FLG <- FALSE
    } else {
      W_old <- W
    }
    if (niter > max_iter) {
      break
    }
  }

  return(list(
    Theta = Theta,
    W = W,
    Beta = beta_cache,
    niter = niter,
    converged = (niter <= max_iter)
  ))
}

#' @title Extended Bayesian Information Criterion for Graphical Lasso
#' @description
#' Computes the EBIC of Foygel and Drton (2010) for a given precision matrix estimate.
#' @param S Sample covariance or correlation matrix
#' @param Theta Estimated precision matrix
#' @param n Sample size
#' @param p Number of variables
#' @param gamma EBIC tuning parameter (default 0.5)
#' @return Scalar EBIC value
#' @noRd

compute_EBIC_glasso <- function(S, Theta, n, p, gamma = 0.5) {
  term1 <- n * sum(S * Theta) - n * as.numeric(determinant(Theta, logarithm = TRUE)$modulus)
  E <- sum(abs(Theta[upper.tri(Theta)]) > 1e-6)
  term2 <- E * log(n)
  term3 <- 4 * gamma * E * log(p)
  EBIC <- term1 + term2 + term3
  return(EBIC)
}


#' @title Graphical Lasso for Gaussian Graphical Models
#' @description
#' Estimates a sparse precision matrix from ordinal item response data via
#' the Graphical Lasso (Friedman, Hastie, Tibshirani 2008). The polychoric
#' correlation matrix is used as input. The optimal regularization parameter
#' lambda is selected by the Extended Bayesian Information Criterion
#' (Foygel and Drton 2010).
#' @details
#' The Graphical Lasso estimates the precision matrix Theta by maximizing
#' the penalized log-likelihood:
#'
#'   log det(Theta) - tr(S Theta) - lambda * ||Theta||_1
#'
#' subject to Theta being positive definite. Optimization is performed by
#' block coordinate descent (Algorithm 17.2 of Hastie, Tibshirani, Friedman
#' 2009) with cyclical coordinate descent for the inner lasso step.
#'
#' Lambda is searched on a log-scale grid from lambda_max (the maximum
#' absolute off-diagonal of S) down to lambda_max * lambda_ratio. For each
#' lambda, the EBIC is computed and the lambda minimizing EBIC is returned.
#' Warm-starting (W and beta cache from the previous lambda) is used to
#' accelerate the search.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#'   it is converted to the exametrika class with the [dataFormat] function.
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param gamma EBIC tuning parameter in `[0, 1]`. Default is 0.5.
#' @param n_lambda Number of lambda values in the search grid. Default is 50.
#' @param lambda_ratio Ratio of lambda_min to lambda_max. Default is 0.01.
#' @param penalize_diagonal Logical. If TRUE, the L1 penalty is applied to
#'   the diagonal of Theta. Default is TRUE.
#' @param max_iter Maximum number of outer iterations for the block
#'   coordinate descent algorithm. Default is 100.
#' @param eps Convergence tolerance. Default is 1e-6.
#' @param edge_tol Threshold below which an off-diagonal element of Theta is
#'   considered zero (no edge). Default is 1e-8.
#' @param verbose Logical. If TRUE, progress messages are displayed.
#' @param ... Additional arguments (currently unused; reserved for future extensions).
#' @return
#' \describe{
#'   \item{theta}{Estimated precision matrix at the optimal lambda.}
#'   \item{W}{Working covariance matrix at the optimal lambda.}
#'   \item{lambda_opt}{Selected lambda value.}
#'   \item{gamma}{EBIC tuning parameter used.}
#'   \item{ebic_opt}{Minimum EBIC value.}
#'   \item{n_edge}{Number of edges in the selected model.}
#'   \item{path}{Data frame with columns lambda, ebic, n_edge over the search grid.}
#' }
#' @references
#' Friedman, J., Hastie, T., and Tibshirani, R. (2008). Sparse inverse
#' covariance estimation with the graphical lasso. Biostatistics, 9(3), 432-441.
#'
#' Foygel, R., and Drton, M. (2010). Extended Bayesian Information Criteria
#' for Gaussian Graphical Models. Advances in Neural Information Processing
#' Systems 23.
#' @seealso [PolychoricCorrelationMatrix]
#' @examples
#' \donttest{
#' # Estimate a sparse precision matrix from ordinal data
#' result.Glasso <- Glasso(J15S3810)
#' result.Glasso$lambda_opt
#' result.Glasso$n_edge
#' }
#' @export

Glasso <- function(U,
                   na = NULL,
                   Z = NULL,
                   w = NULL,
                   gamma = 0.5,
                   n_lambda = 50,
                   lambda_ratio = 0.01,
                   penalize_diagonal = TRUE,
                   max_iter = 100,
                   eps = 1e-6,
                   edge_tol = 1e-8,
                   verbose = FALSE,
                   ...) {
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (tmp$response.type != "ordinal") {
    response_type_error(tmp$response.type, "Glasso")
  }

  p <- NCOL(tmp$Q)
  nobs <- NROW(tmp$Q)
  if (verbose) message("Computing polychoric correlation matrix...")
  S <- PolychoricCorrelationMatrix(tmp)

  lambda_max <- max(abs(S - diag(diag(S))))
  lambda_min <- lambda_max * lambda_ratio
  lambda_set <- exp(seq(log(lambda_max), log(lambda_min), length.out = n_lambda))

  if (verbose) message("Searching optimal lambda by EBIC...")

  best_ebic <- Inf
  best_Theta <- NULL
  best_W <- NULL
  best_lambda <- NA
  best_index <- NA

  EBICs <- numeric(length = n_lambda)
  n_edges <- integer(length = n_lambda)

  W_prev <- NULL
  Beta_prev <- NULL
  for (k in seq_along(lambda_set)) {
    l <- lambda_set[k]
    if (k == 1) {
      if (penalize_diagonal) {
        W_init <- S + diag(l, p)
      } else {
        W_init <- S
      }
      Beta_init <- matrix(0, p - 1, p)
    } else {
      W_init <- W_prev
      if (penalize_diagonal) {
        diag(W_init) <- diag(S) + l
      } else {
        diag(W_init) <- diag(S)
      }
      Beta_init <- Beta_prev
    }

    res <- glasso_one(S,
      lambda = l, W_init = W_init, Beta_init = Beta_init,
      eps = eps, max_iter = max_iter
    )
    if (!res$converged) {
      warning(sprintf("Algorithm did not converge at lambda = %.4f.", l))
      stop("Glasso could not be computed. Try increasing max_iter.")
    }
    ebic_k <- compute_EBIC_glasso(S, res$Theta, n = nobs, p = p, gamma = gamma)
    edges_k <- sum(abs(res$Theta[upper.tri(res$Theta)]) > edge_tol)

    EBICs[k] <- ebic_k
    n_edges[k] <- edges_k

    if (ebic_k < best_ebic) {
      best_ebic <- ebic_k
      best_Theta <- res$Theta
      best_W <- res$W
      best_lambda <- l
      best_index <- k
    }


    W_prev <- res$W
    Beta_prev <- res$Beta
  }

  ret <- structure(
    list(
      theta = best_Theta,
      W = best_W,
      lambda_opt = best_lambda,
      gamma = gamma,
      ebic_opt = best_ebic,
      n_edge = n_edges[best_index],
      path = data.frame(
        lambda = lambda_set,
        ebic = EBICs,
        n_edge = n_edges
      )
    ),
    class = c("exametrika", "Glasso")
  )

  return(ret)
}
