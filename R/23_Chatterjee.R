#' @title Chatterjee's xi correlation coefficient
#' @description
#' Computes Chatterjee's (2021) rank-based correlation coefficient xi.
#' Unlike Pearson or Spearman, xi is asymmetric: xi(x, y) and xi(y, x)
#' may differ. This asymmetry is the basis for direction-detection in
#' graphical models.
#' @details
#' For tied x values, ranks are broken using \code{ties_method}. With
#' \code{"random"} (the default), each call may produce a slightly
#' different value due to random tie-breaking. Use \code{xi_stable()}
#' to average over many randomizations.
#' @param x Numeric or ordered factor (predictor)
#' @param y Numeric or ordered factor (response)
#' @param ties_method How to break ties in x. Default "random".
#' @return Numeric scalar: Chatterjee's xi value.
#' @references
#' Chatterjee, S. (2021). A new coefficient of correlation. Journal of
#' the American Statistical Association, 116(536), 2009-2022.
#' @examples
#' \donttest{
#' x <- rnorm(100)
#' y <- x^2 + rnorm(100, sd = 0.1)
#' chatterjee_xi(x, y) # near 1, since y is determined by x
#' chatterjee_xi(y, x) # smaller, since x is not determined by y
#' }
#' @export
chatterjee_xi <- function(x, y, ties_method = "random") {
  n <- as.numeric(length(x))
  ord <- order(rank(x, ties.method = ties_method))
  sorted.y <- y[ord]
  r_i <- rank(sorted.y, ties.method = "max")
  l_i <- rank(-sorted.y, ties.method = "max")
  1 - n * sum(abs(diff(r_i))) / (2 * sum(l_i * (n - l_i)))
}


#' @title Bootstrap-averaged Chatterjee's xi
#' @description
#' Computes Chatterjee's xi B times with random tie-breaking and returns
#' the average. This stabilizes the estimate against tie-breaking
#' variability, which is important for ordinal data with many ties (e.g.,
#' Likert-scale items).
#' @details
#' For each of the B replications, ties in x are broken randomly via
#' \code{rank(x, ties.method = "random")} inside \code{chatterjee_xi()}.
#' The standard error of the mean shrinks as 1 / sqrt(B); B = 1000 typically
#' yields SE around 0.001 for moderately tied data.
#' @param x Numeric or ordered factor (predictor)
#' @param y Numeric or ordered factor (response)
#' @param B Number of bootstrap replications. Default 1000.
#' @param seed Optional integer seed for reproducibility.
#' @return A list with elements:
#' \describe{
#'   \item{xi}{Mean of the B Chatterjee xi values.}
#'   \item{sd}{Standard deviation of the B values (tie-breaking spread).}
#'   \item{se}{Standard error of the mean (sd / sqrt(B)).}
#'   \item{B}{Number of bootstrap replications used.}
#' }
#' @references
#' Chatterjee, S. (2021). A new coefficient of correlation. Journal of
#' the American Statistical Association, 116(536), 2009-2022.
#' @examples
#' \donttest{
#' x <- rnorm(100)
#' y <- x^2 + rnorm(100, sd = 0.1)
#' xi_stable(x, y, B = 500, seed = 42)
#' }
#' @export
xi_stable <- function(x, y, B = 1000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  xi_vec <- replicate(B, chatterjee_xi(x, y))
  list(
    xi = mean(xi_vec),
    sd = stats::sd(xi_vec),
    se = stats::sd(xi_vec) / sqrt(B),
    B = B
  )
}


#' @title Pairwise Chatterjee's xi correlation matrix
#' @description
#' Computes the p x p asymmetric matrix of Chatterjee's xi correlations
#' between all item pairs. Entry `[j, k]` is xi(item_j, item_k), which in
#' general differs from xi(item_k, item_j); this asymmetry is what
#' enables direction detection in subsequent graph-construction steps.
#' Each off-diagonal entry is computed by \code{xi_stable()} to average
#' over tie-breaking.
#' @details
#' Pairs are computed with pairwise-complete observations: for each
#' pair (j, k), rows where either `Q[i, j]` or `Q[i, k]` is missing are
#' excluded. The diagonal is set to 1.
#' @param U Either an exametrika object or raw data.
#' @param na Values to be treated as missing.
#' @param Z Missing indicator matrix.
#' @param w Item weight vector.
#' @param B Number of bootstrap replications per pair. Default 1000.
#' @param seed Optional integer seed for reproducibility.
#' @param verbose Logical. If TRUE, progress messages are displayed.
#' @return A p x p numeric matrix with item labels as row/column names.
#'   The diagonal is 1; off-diagonal entries are asymmetric.
#' @references
#' Chatterjee, S. (2021). A new coefficient of correlation. Journal of
#' the American Statistical Association, 116(536), 2009-2022.
#' @examples
#' \donttest{
#' xi_mat <- chatterjee_matrix(J5S1000, B = 100, seed = 42)
#' }
#' @export
chatterjee_matrix <- function(U,
                              na = NULL, Z = NULL, w = NULL,
                              B = 1000, seed = NULL,
                              verbose = FALSE) {
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  if (tmp$response.type != "ordinal") {
    response_type_error(tmp$response.type, "chatterjee_matrix")
  }

  if (!is.null(seed)) set.seed(seed)
  Q <- tmp$Q
  Q[tmp$Z == 0] <- NA
  p <- ncol(Q)

  if (verbose) {
    message(sprintf("Computing %d x %d Chatterjee xi matrix (B = %d)...", p, p, B))
  }

  M <- matrix(1, nrow = p, ncol = p)
  for (j in seq_len(p)) {
    for (k in seq_len(p)) {
      if (j == k) next
      valid <- !is.na(Q[, j]) & !is.na(Q[, k])
      M[j, k] <- xi_stable(Q[valid, j], Q[valid, k], B = B)$xi
    }
  }

  rownames(M) <- tmp$ItemLabel
  colnames(M) <- tmp$ItemLabel
  M
}
