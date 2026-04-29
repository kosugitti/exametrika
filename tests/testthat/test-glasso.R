# Tests for Graphical Lasso

### Setup (full J15S3810 for numerical stability)
tmp <- J15S3810
S_test <- PolychoricCorrelationMatrix(tmp)


test_that("soft_thresholding works correctly", {
  expect_equal(soft_thresholding(0.5, 0.3), 0.2)
  expect_equal(soft_thresholding(-0.5, 0.3), -0.2)
  expect_equal(soft_thresholding(0.2, 0.3), 0)
  expect_equal(soft_thresholding(c(0.5, -0.5, 0.2), 0.3), c(0.2, -0.2, 0))
})


test_that("glasso_one at lambda = 0 matches solve(S)", {
  res <- glasso_one(S_test, lambda = 0, eps = 1e-8, max_iter = 200)
  expect_true(res$converged)
  expect_equal(res$Theta, unname(solve(S_test)), tolerance = 1e-4)
})


test_that("glasso_one at large lambda yields diagonal Theta", {
  big_lambda <- max(abs(S_test - diag(diag(S_test)))) * 2
  res <- glasso_one(S_test, lambda = big_lambda)
  off_diag <- res$Theta[upper.tri(res$Theta)]
  expect_true(all(abs(off_diag) < 1e-6))
})


test_that("glasso_one returns expected structure", {
  res <- glasso_one(S_test, lambda = 0.1)
  expect_named(res, c("Theta", "W", "Beta", "niter", "converged"))
  expect_true(is.matrix(res$Theta))
  expect_true(is.logical(res$converged))
})


test_that("compute_EBIC_glasso returns scalar", {
  Theta_hat <- glasso_one(S_test, lambda = 0.1)$Theta
  ebic <- compute_EBIC_glasso(S_test, Theta_hat, n = 200, p = 15, gamma = 0.5)
  expect_length(ebic, 1)
  expect_true(is.numeric(ebic))
})


test_that("Glasso returns expected structure", {
  res <- Glasso(tmp, n_lambda = 10)
  expect_s3_class(res, "exametrika")
  expect_named(res, c("theta", "W", "lambda_opt", "gamma", "ebic_opt", "n_edge", "path"))
  expect_true(is.matrix(res$theta))
  expect_equal(dim(res$theta), c(15, 15))
})


test_that("Glasso theta is symmetric", {
  res <- Glasso(tmp, n_lambda = 10)
  expect_lt(max(abs(res$theta - t(res$theta))), 1e-8)
})


test_that("Glasso path has the expected length", {
  res <- Glasso(tmp, n_lambda = 10)
  expect_equal(nrow(res$path), 10)
  expect_named(res$path, c("lambda", "ebic", "n_edge"))
})


test_that("Glasso rejects non-ordinal data", {
  binary_data <- J5S10
  expect_error(Glasso(binary_data))
})
