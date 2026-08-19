# The M2 whitener on a rank-deficient margin covariance (2026-07-27, v2.0.0)
#
# Xi is singular whenever some margin direction carries no usable variance,
# which is routine once the margin set gets large. The fallback used to be an
# eigendecomposition; it is now a pivoted Cholesky, which costs the same
# m^3/3 as the plain factorisation instead of an order of magnitude more
# (m = 12,640: 16 seconds against roughly 80 minutes). These tests pin the two
# properties that swap relies on: the reported rank, and the quadratic form.

make_psd <- function(m, rank) {
  set.seed(20260727)
  B <- matrix(stats::rnorm(m * rank), nrow = m, ncol = rank)
  return(tcrossprod(B))
}

test_that("the whitener reports the rank of a deficient Xi", {
  Xi <- make_psd(40, 25)

  W <- exametrika:::m2_whitener(Xi)

  expect_equal(W$dim, 25)
})

test_that("a full-rank Xi goes through the plain Cholesky untouched", {
  Xi <- make_psd(30, 30) + diag(30)

  W <- exametrika:::m2_whitener(Xi)
  e <- stats::rnorm(30)

  expect_equal(W$dim, 30)
  # The whitened residual must reproduce the Mahalanobis form exactly.
  expect_equal(sum(W$apply(e)^2), as.numeric(crossprod(e, solve(Xi, e))))
})

test_that("the pivoted path reproduces the eigen path's quadratic form", {
  # A residual in the column space of Xi is the case the statistic is built on;
  # every generalised inverse agrees there, so the two decompositions must too.
  m <- 60
  rank <- 40
  set.seed(20260727)
  B <- matrix(stats::rnorm(m * rank), nrow = m, ncol = rank)
  Xi <- tcrossprod(B)
  e <- as.vector(B %*% stats::rnorm(rank))

  W <- exametrika:::m2_whitener(Xi)

  ev <- eigen(Xi, symmetric = TRUE)
  keep <- ev$values > max(ev$values) * 1e-10
  eigen_form <- sum(((t(ev$vectors[, keep, drop = FALSE]) %*% e) /
    sqrt(ev$values[keep]))^2)

  expect_equal(W$dim, rank)
  expect_equal(sum(W$apply(e)^2), eigen_form, tolerance = 1e-6)
})

test_that("the whitener accepts a vector and a matrix alike", {
  Xi <- make_psd(40, 25)
  W <- exametrika:::m2_whitener(Xi)
  Delta <- matrix(stats::rnorm(40 * 3), nrow = 40)

  expect_equal(nrow(W$apply(Delta)), 25)
  expect_equal(ncol(W$apply(Delta)), 3)
  expect_equal(nrow(W$apply(stats::rnorm(40))), 25)
})

test_that("M2 on a real fit is unchanged by the decomposition swap", {
  # J35S500 biclustering: small enough that the eigendecomposition is still
  # affordable, large enough that Xi is rank-deficient in practice.
  skip_on_cran()
  dat <- dataFormat(J35S500, response.type = "ordinal")
  fit <- suppressWarnings(suppressMessages(
    Biclustering(dat, ncls = 3, nfld = 2, method = "B")
  ))

  res <- M2(fit, verbose = FALSE)

  expect_true(is.finite(res$M2))
  expect_gt(res$M2, 0)
  expect_gt(res$df, 0)
  expect_lte(res$m, res$m + res$m_dropped)
})
