# Tests for GRM C++ Implementation

test_that("GRM helper functions work correctly", {
  # Test target_to_params_grm
  test_target <- c(0, 0.5, -1, 0, log(0.5), 0)
  test_nitems <- 2
  test_ncat <- c(3, 3)

  params_result <- target_to_params_grm(test_target, test_nitems, test_ncat)

  expect_type(params_result, "list")
  expect_named(params_result, c("a", "b"))
  expect_length(params_result$a, 2)
  expect_length(params_result$b, 2)
  expect_true(all(params_result$a > 0)) # Discrimination should be positive

  # Test simple_quadrature_grm
  quad_result <- simple_quadrature_grm(21)

  expect_type(quad_result, "list")
  expect_named(quad_result, c("nodes", "weights"))
  expect_length(quad_result$nodes, 21)
  expect_length(quad_result$weights, 21)
  expect_equal(sum(quad_result$weights), 1, tolerance = 1e-10)
})

test_that("log_lik_grm_cpp works correctly", {
  # Create small test dataset
  set.seed(123)
  small_data <- matrix(sample(1:3, 30, replace = TRUE), nrow = 10, ncol = 3)

  # Generate reasonable starting values
  start_vals <- generate_start_values(small_data)
  target <- params_to_target(start_vals$a, start_vals$b)

  ll_result <- log_lik_grm_cpp(target, small_data, 21)

  expect_type(ll_result, "double")
  expect_true(is.finite(ll_result))
  expect_true(ll_result < 0) # Log-likelihood should be negative
})

test_that("score_function_analytical_grm works correctly", {
  # Create small test dataset
  set.seed(456)
  small_data <- matrix(sample(1:4, 40, replace = TRUE), nrow = 10, ncol = 4)

  # Generate starting values
  start_vals <- generate_start_values(small_data)
  target <- params_to_target(start_vals$a, start_vals$b)

  # Test analytical gradient
  grad_result <- score_function_analytical_grm(target, small_data, 21)

  expect_type(grad_result, "double")
  expect_length(grad_result, length(target))
  expect_true(all(is.finite(grad_result)))
})

test_that("compare_gradients_grm works correctly", {
  # Create small test dataset
  set.seed(789)
  small_data <- matrix(sample(1:3, 24, replace = TRUE), nrow = 8, ncol = 3)

  # Generate starting values
  start_vals <- generate_start_values(small_data)
  target <- params_to_target(start_vals$a, start_vals$b)

  # Compare gradients
  comparison <- compare_gradients_grm(target, small_data, 21)

  expect_type(comparison, "list")
  expect_named(comparison, c("analytical", "numerical", "difference", "max_abs_diff", "rms_diff"))
  expect_length(comparison$analytical, length(target))
  expect_length(comparison$numerical, length(target))
  expect_type(comparison$max_abs_diff, "double")
  expect_type(comparison$rms_diff, "double")

  # Gradients should be reasonably close
  expect_true(comparison$max_abs_diff < 1e-2)
})

test_that("GRM model fitting works correctly", {
  # Use example data and ensure ordinal type
  data("J5S1000", package = "exametrika")
  small_data <- J5S1000$Q[1:50, 1:3]

  # Format data explicitly as ordinal
  formatted_data <- dataFormat(small_data, response.type = "ordinal")

  # Fit GRM model
  grm_result <- GRM(formatted_data, verbose = FALSE)

  expect_s3_class(grm_result, c("exametrika", "GRM"))
  expect_named(grm_result, c(
    "testlength", "nobs", "log_lik", "iterations",
    "params", "EAP", "MAP", "PSD",
    "ItemFitIndices", "TestFitIndices"
  ))

  # Check dimensions
  expect_equal(grm_result$testlength, 3)
  expect_equal(grm_result$nobs, 50)
  expect_equal(nrow(grm_result$params), 3)
  expect_length(grm_result$EAP, 50)
  expect_length(grm_result$MAP, 50)
  expect_length(grm_result$PSD, 50)

  # Check parameter estimates are reasonable
  expect_true(all(grm_result$params$Slope > 0))
  expect_true(all(is.finite(grm_result$params$Slope)))
  expect_true(is.finite(grm_result$log_lik))
  expect_true(grm_result$iterations[[1]] >= 1)
})

test_that("GRM performance is acceptable", {
  # Create medium-sized dataset
  set.seed(999)
  medium_data <- matrix(sample(1:4, 200, replace = TRUE), nrow = 50, ncol = 4)

  # Format as ordinal explicitly
  formatted_medium <- dataFormat(medium_data, response.type = "ordinal")

  # Time the fitting
  start_time <- Sys.time()
  grm_result <- GRM(formatted_medium, verbose = FALSE)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 10) # Should complete within 10 seconds
  expect_s3_class(grm_result, "GRM")
  expect_true(is.finite(grm_result$log_lik))
})

test_that("GRM error handling works", {
  # Test with inappropriate data
  expect_error(GRM(matrix(letters[1:20], nrow = 5, ncol = 4)))

  # Test with insufficient data
  tiny_data <- matrix(c(1, 2, 1, 2), nrow = 2, ncol = 2)
  expect_error(GRM(tiny_data))
})
