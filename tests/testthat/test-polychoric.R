# Tests for Polychoric Correlation C++ Implementation

test_that("qBiNormal_cpp works correctly", {
  # Test basic functionality
  expect_equal(qBiNormal_cpp(0, 0, 0), 0.25, tolerance = 1e-10)

  # Test infinite bounds
  expect_equal(qBiNormal_cpp(-Inf, 0, 0.2), 0, tolerance = 1e-10)
  expect_equal(qBiNormal_cpp(0, Inf, 0.4), 0.5, tolerance = 1e-10)
  expect_equal(qBiNormal_cpp(Inf, Inf, 0.5), 1, tolerance = 1e-10)

  # Test edge cases
  expect_equal(qBiNormal_cpp(-1, -1, 0.5), 0.062514, tolerance = 1e-5)
  expect_equal(qBiNormal_cpp(1, 1, -0.3), 0.693007, tolerance = 1e-5)
})

test_that("polychoric_likelihood_cpp works correctly", {
  # Create test contingency table
  test_mat <- matrix(c(
    20, 15, 5,
    10, 25, 15,
    5, 10, 30
  ), nrow = 3, ncol = 3, byrow = TRUE)

  # Test different correlation values
  ll_0 <- polychoric_likelihood_cpp(0, test_mat)
  ll_pos <- polychoric_likelihood_cpp(0.5, test_mat)
  ll_neg <- polychoric_likelihood_cpp(-0.5, test_mat)

  expect_type(ll_0, "double")
  expect_type(ll_pos, "double")
  expect_type(ll_neg, "double")

  expect_true(is.finite(ll_0))
  expect_true(is.finite(ll_pos))
  expect_true(is.finite(ll_neg))
})

test_that("polychoric_cpp individual correlation works", {
  # Test data
  x <- c(1, 2, 3, 2, 1, 3, 2, 2, 1, 3)
  y <- c(2, 3, 1, 2, 3, 1, 2, 1, 3, 2)

  result <- polychoric_cpp(as.integer(x), as.integer(y))

  expect_type(result, "double")
  expect_true(abs(result) <= 1)
  expect_false(is.na(result))

  # Test with insufficient data
  expect_true(is.na(polychoric_cpp(c(1L, 2L), c(1L, 2L))))
})

test_that("polychoric_matrix_cpp works correctly", {
  # Create test matrix
  set.seed(42)
  test_matrix <- matrix(sample(1:3, 30, replace = TRUE), ncol = 3)

  result <- polychoric_matrix_cpp(test_matrix)

  # Check dimensions
  expect_equal(dim(result), c(3, 3))

  # Check diagonal is 1
  expect_equal(diag(result), c(1, 1, 1), tolerance = 1e-10)

  # Check symmetry
  expect_equal(result[1, 2], result[2, 1], tolerance = 1e-10)
  expect_equal(result[1, 3], result[3, 1], tolerance = 1e-10)
  expect_equal(result[2, 3], result[3, 2], tolerance = 1e-10)

  # Check all values are valid correlations
  expect_true(all(abs(result) <= 1))
})

test_that("polychoric function integration works", {
  x <- c(1, 2, 3, 2, 1, 3, 2, 2, 1, 3, 1, 2, 3, 2, 1)
  y <- c(2, 3, 1, 2, 3, 1, 2, 1, 3, 2, 3, 1, 2, 3, 1)

  result <- polychoric(x, y)

  expect_type(result, "double")
  expect_true(abs(result) <= 1)
  expect_false(is.na(result))
})

test_that("Performance is significantly improved", {
  x <- sample(1:4, 50, replace = TRUE)
  y <- sample(1:3, 50, replace = TRUE)

  # Time the C++ implementation
  start_time <- Sys.time()
  result <- polychoric_cpp(as.integer(x), as.integer(y))
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 0.1) # Should be very fast
  expect_false(is.na(result))
})
