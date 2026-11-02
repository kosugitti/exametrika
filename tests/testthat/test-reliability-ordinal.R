# AlphaCoefficient() / OmegaCoefficient() on ordinal data (2.3.0).
# Before 2.3.0 both stopped with "PhiCoefficient is not applicable to ordinal
# response data", because the data-matrix path always built phi and tetrachoric
# matrices. Ordinal data now use the covariance and polychoric matrices.

make_ordinal_rel <- function() {
  set.seed(31)
  n <- 80
  th <- stats::rnorm(n)
  X <- vapply(1:4, function(j) {
    findInterval(th + stats::rnorm(n), c(-1, 0, 1)) + 1L
  }, integer(n))
  X[1:5, 2] <- NA
  return(suppressMessages(dataFormat(X, response.type = "ordinal")))
}

test_that("alpha and omega accept ordinal data", {
  d <- make_ordinal_rel()
  a <- AlphaCoefficient(d)
  o <- OmegaCoefficient(d)
  expect_named(a, c("AlphaCov", "AlphaPolychoric"))
  expect_named(o, c("OmegaCov", "OmegaPolychoric"))

  # Each value equals the matrix-input result on the matrix it names
  Qx <- d$Q
  Qx[d$Z == 0] <- NA
  V <- stats::cov(Qx, use = "pairwise")
  R <- unclass(PolychoricCorrelationMatrix(d))
  expect_equal(a$AlphaCov, AlphaCoefficient(V))
  expect_equal(a$AlphaPolychoric, AlphaCoefficient(R))
  expect_equal(o$OmegaCov, OmegaCoefficient(V))
  expect_equal(o$OmegaPolychoric, OmegaCoefficient(R))

  # Polychoric values exceed the Pearson-based ones on coarse categories
  expect_gt(a$AlphaPolychoric, a$AlphaCov)
})

test_that("binary data keep the three historical components", {
  set.seed(5)
  th <- stats::rnorm(100)
  B <- vapply(1:5, function(j) as.integer(th + stats::rnorm(100) > 0), integer(100))
  d <- suppressMessages(dataFormat(B))
  o <- OmegaCoefficient(d)
  expect_named(o, c("OmegaCov", "OmegaPhi", "OmegaTetrachoric"))
  a <- AlphaCoefficient(d)
  expect_named(a, c("AlphaCov", "AlphaPhi", "AlphaTetrachoric"))
})

test_that("nominal data are rejected with a message naming the function", {
  set.seed(2)
  X <- matrix(sample(1:4, 200, replace = TRUE), 50, 4)
  d <- suppressMessages(dataFormat(X, response.type = "nominal"))
  expect_error(OmegaCoefficient(d), "OmegaCoefficient is not applicable")
  expect_error(AlphaCoefficient(d), "AlphaCoefficient is not applicable")
})
