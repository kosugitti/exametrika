# Tests for Chatterjee's xi correlation

### Setup
set.seed(1234)
test_data <- J15S3810$Q[sample(nrow(J15S3810$Q), 200), ]
tmp <- dataFormat(test_data)


test_that("chatterjee_xi returns a numeric scalar", {
  x <- rnorm(100)
  y <- rnorm(100)
  xi <- chatterjee_xi(x, y)
  expect_length(xi, 1)
  expect_true(is.numeric(xi))
})


test_that("chatterjee_xi detects functional dependence", {
  set.seed(42)
  x <- rnorm(500)
  y <- x^2 + rnorm(500, sd = 0.05)
  # y is nearly determined by x, so xi(x, y) should be high
  expect_gt(chatterjee_xi(x, y), 0.7)
  # x is not determined by y (sign ambiguity), so xi(y, x) should be smaller
  expect_lt(chatterjee_xi(y, x), 0.5)
})


test_that("chatterjee_xi handles large n without integer overflow", {
  set.seed(42)
  n <- 5000
  x <- sample(1:4, n, replace = TRUE)
  y <- sample(1:4, n, replace = TRUE)
  xi <- chatterjee_xi(x, y)
  expect_false(is.na(xi))
  expect_true(is.numeric(xi))
})


test_that("xi_stable returns the expected list structure", {
  x <- rnorm(100)
  y <- rnorm(100)
  res <- xi_stable(x, y, B = 50, seed = 42)
  expect_named(res, c("xi", "sd", "se", "B"))
  expect_equal(res$B, 50)
  expect_equal(res$se, res$sd / sqrt(50))
})


test_that("xi_stable is reproducible with seed", {
  x <- rnorm(100)
  y <- rnorm(100)
  res1 <- xi_stable(x, y, B = 50, seed = 42)
  res2 <- xi_stable(x, y, B = 50, seed = 42)
  expect_equal(res1$xi, res2$xi)
})


test_that("chatterjee_matrix returns a labeled p x p matrix", {
  M <- chatterjee_matrix(tmp, B = 30, seed = 42)
  expect_true(is.matrix(M))
  expect_equal(dim(M), c(15, 15))
  expect_equal(rownames(M), tmp$ItemLabel)
  expect_equal(colnames(M), tmp$ItemLabel)
})


test_that("chatterjee_matrix has unit diagonal", {
  M <- chatterjee_matrix(tmp, B = 30, seed = 42)
  expect_equal(unname(diag(M)), rep(1, 15))
})


test_that("chatterjee_matrix is generally asymmetric", {
  M <- chatterjee_matrix(tmp, B = 30, seed = 42)
  # off-diagonal should not be perfectly symmetric
  off_diff <- max(abs(M - t(M)))
  expect_gt(off_diff, 0.01)
})


test_that("chatterjee_matrix rejects non-ordinal data", {
  expect_error(chatterjee_matrix(J5S10))
})
