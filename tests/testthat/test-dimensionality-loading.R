# first_loading in Dimensionality() (2.3.0): loadings on the first principal
# component, oriented to sum to a positive value, so that a reverse-keyed item
# shows up with the opposite sign.

test_that("first_loading is v1 * sqrt(lambda1), oriented and named", {
  set.seed(41)
  n <- 120
  th <- stats::rnorm(n)
  X <- vapply(1:4, function(j) {
    findInterval(th + stats::rnorm(n), c(-1, 0, 1)) + 1L
  }, integer(n))
  colnames(X) <- paste0("I", 1:4)
  d <- suppressMessages(dataFormat(X, response.type = "ordinal"))
  res <- Dimensionality(d)

  R <- unclass(PolychoricCorrelationMatrix(d))
  e <- eigen(R, symmetric = TRUE)
  expected <- abs(e$vectors[, 1]) * sqrt(e$values[1])
  expect_equal(unname(res$first_loading), expected, tolerance = 1e-10)
  expect_named(res$first_loading, paste0("I", 1:4))
  expect_gt(sum(res$first_loading), 0)

  # A reverse-keyed item gets the opposite sign
  Xr <- X
  Xr[, 4] <- 5L - Xr[, 4]
  dr <- suppressMessages(dataFormat(Xr, response.type = "ordinal"))
  lr <- Dimensionality(dr)$first_loading
  expect_lt(lr[4], 0)
  expect_true(all(lr[1:3] > 0))
})
