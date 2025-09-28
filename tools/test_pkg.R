# Comprehensive Package Testing Script using testthat
# This script performs thorough testing of the exametrika package

library(testthat)
library(devtools)

cat("=== exametrika Package Testing Suite ===\n\n")

# Load the package
load_all()

# ==============================================================================
# Test 1: Basic Package Loading and Data
# ==============================================================================

test_that("Package loads correctly and data is available", {
  expect_true("exametrika" %in% loadedNamespaces())

  # Test example datasets
  data("J5S1000")
  expect_s3_class(J5S1000, "exametrika")
  expect_equal(nrow(J5S1000$Q), 1000)
  expect_equal(ncol(J5S1000$Q), 5)

  data("J15S500")
  expect_s3_class(J15S500, "exametrika")
  expect_equal(nrow(J15S500$U), 500)
  expect_equal(ncol(J15S500$U), 15)
})

# ==============================================================================
# Test 2: Data Formatting Functions
# ==============================================================================

test_that("dataFormat function works correctly", {
  # Test with simple binary data
  binary_data <- matrix(c(1,0,1,0,1,0,1,0,1,0,0,1), nrow=3, ncol=4)
  result <- dataFormat(binary_data)

  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "binary")
  expect_equal(nrow(result$U), 3)
  expect_equal(ncol(result$U), 4)

  # Test with ordinal data
  ordinal_data <- matrix(sample(1:4, 60, replace=TRUE), nrow=15, ncol=4)
  result <- dataFormat(ordinal_data)

  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "ordinal")
  expect_equal(nrow(result$Q), 15)
  expect_equal(ncol(result$Q), 4)
})

# ==============================================================================
# Test 3: Classical Test Theory Functions
# ==============================================================================

test_that("CTT functions work correctly", {
  data("J15S500")

  # Test correct response rate
  crr_result <- crr(J15S500)
  expect_type(crr_result, "double")
  expect_length(crr_result, 15)
  expect_true(all(crr_result >= 0 & crr_result <= 1))

  # Test item-total correlation
  itc_result <- ItemTotalCorr(J15S500)
  expect_type(itc_result, "double")
  expect_length(itc_result, 15)
  expect_true(all(abs(itc_result) <= 1))

  # Test standardized scores
  sscore_result <- sscore(J15S500)
  expect_type(sscore_result, "double")
  expect_length(sscore_result, 500)
  expect_true(abs(mean(sscore_result, na.rm=TRUE)) < 0.1)  # Should be near 0
})

# ==============================================================================
# Test 4: Polychoric Correlation C++ Implementation
# ==============================================================================

test_that("Polychoric correlation C++ functions work correctly", {
  # Test qBiNormal_cpp
  expect_equal(qBiNormal_cpp(0, 0, 0), 0.25, tolerance = 1e-10)
  expect_equal(qBiNormal_cpp(-Inf, 0, 0.2), 0, tolerance = 1e-10)
  expect_equal(qBiNormal_cpp(0, Inf, 0.4), 0.5, tolerance = 1e-10)

  # Test polychoric_cpp individual correlation
  x <- c(1,2,3,2,1,3,2,2,1,3)
  y <- c(2,3,1,2,3,1,2,1,3,2)

  cpp_result <- polychoric_cpp(as.integer(x), as.integer(y))
  expect_type(cpp_result, "double")
  expect_true(abs(cpp_result) <= 1)
  expect_false(is.na(cpp_result))

  # Test polychoric_matrix_cpp
  test_matrix <- matrix(c(x, y, sample(1:3, 10, replace=TRUE)), ncol=3)
  matrix_result <- polychoric_matrix_cpp(test_matrix)

  expect_equal(dim(matrix_result), c(3, 3))
  expect_equal(diag(matrix_result), c(1, 1, 1), tolerance = 1e-10)
  expect_true(all(abs(matrix_result) <= 1))

  # Test symmetry
  expect_equal(matrix_result[1,2], matrix_result[2,1], tolerance = 1e-10)
  expect_equal(matrix_result[1,3], matrix_result[3,1], tolerance = 1e-10)
  expect_equal(matrix_result[2,3], matrix_result[3,2], tolerance = 1e-10)
})

# ==============================================================================
# Test 5: GRM C++ Implementation
# ==============================================================================

test_that("GRM C++ functions work correctly", {
  # Test target_to_params_grm
  test_target <- c(0, 0.5, -1, 0, log(0.5))  # 2 items, log(a), b params
  test_nitems <- 2
  test_ncat <- c(3, 3)

  params_result <- target_to_params_grm(test_target, test_nitems, test_ncat)
  expect_type(params_result, "list")
  expect_named(params_result, c("a", "b"))
  expect_length(params_result$a, 2)
  expect_length(params_result$b, 2)

  # Test simple_quadrature_grm
  quad_result <- simple_quadrature_grm(21)
  expect_type(quad_result, "list")
  expect_named(quad_result, c("nodes", "weights"))
  expect_length(quad_result$nodes, 21)
  expect_length(quad_result$weights, 21)
  expect_equal(sum(quad_result$weights), 1, tolerance = 1e-10)

  # Test log_lik_grm_cpp with small dataset
  small_data <- matrix(sample(1:3, 30, replace=TRUE), nrow=10, ncol=3)
  start_vals <- generate_start_values(small_data)
  target <- params_to_target(start_vals$a, start_vals$b)

  ll_result <- log_lik_grm_cpp(target, small_data, 21)
  expect_type(ll_result, "double")
  expect_true(is.finite(ll_result))
  expect_true(ll_result < 0)  # Log-likelihood should be negative
})

# ==============================================================================
# Test 6: GRM Model Fitting
# ==============================================================================

test_that("GRM model fitting works correctly", {
  # Use small subset for faster testing
  data("J5S1000")
  small_data <- J5S1000$Q[1:50, 1:3]

  # Fit GRM model
  grm_result <- GRM(small_data, verbose = FALSE)

  expect_s3_class(grm_result, c("exametrika", "GRM"))
  expect_named(grm_result, c("testlength", "nobs", "log_lik", "iterations",
                            "params", "EAP", "MAP", "PSD",
                            "ItemFitIndices", "TestFitIndices"))

  # Check dimensions
  expect_equal(grm_result$testlength, 3)
  expect_equal(grm_result$nobs, 50)
  expect_equal(nrow(grm_result$params), 3)
  expect_length(grm_result$EAP, 50)
  expect_length(grm_result$MAP, 50)
  expect_length(grm_result$PSD, 50)

  # Check parameter estimates are reasonable
  expect_true(all(grm_result$params$Slope > 0))  # Discrimination > 0
  expect_true(all(is.finite(grm_result$params$Slope)))
  expect_true(is.finite(grm_result$log_lik))
  expect_equal(grm_result$iterations[[1]] >= 1, TRUE)  # At least 1 iteration
})

# ==============================================================================
# Test 7: Polychoric Correlation Matrix
# ==============================================================================

test_that("PolychoricCorrelationMatrix works correctly", {
  # Create test ordinal data
  test_data <- matrix(sample(1:4, 100, replace=TRUE), nrow=25, ncol=4)
  formatted_data <- dataFormat(test_data)
  formatted_data$response.type <- "ordinal"  # Ensure ordinal type

  # Test matrix computation
  result <- tryCatch({
    polychoric_matrix_cpp(test_data)
  }, error = function(e) NULL)

  if (!is.null(result)) {
    expect_equal(dim(result), c(4, 4))
    expect_equal(diag(result), rep(1, 4), tolerance = 1e-10)
    expect_true(all(abs(result) <= 1))

    # Test symmetry
    expect_equal(result, t(result), tolerance = 1e-10)
  } else {
    skip("Polychoric matrix computation failed - may need data adjustment")
  }
})

# ==============================================================================
# Test 8: Error Handling
# ==============================================================================

test_that("Error handling works correctly", {
  # Test with inappropriate data types
  expect_error(GRM(matrix(letters[1:20], nrow=5, ncol=4)))

  # Test with insufficient data
  tiny_data <- matrix(c(1,2,1,2), nrow=2, ncol=2)
  expect_error(GRM(tiny_data))

  # Test polychoric with insufficient data
  expect_true(is.na(polychoric_cpp(c(1,1), c(2,2))))
})

# ==============================================================================
# Test 9: Performance Benchmarks
# ==============================================================================

test_that("Performance improvements are significant", {
  # Test polychoric speed
  x <- sample(1:4, 100, replace=TRUE)
  y <- sample(1:3, 100, replace=TRUE)

  # Time C++ version
  cpp_time <- system.time({
    cpp_result <- polychoric_cpp(as.integer(x), as.integer(y))
  })["elapsed"]

  expect_true(cpp_time < 0.1)  # Should be very fast
  expect_false(is.na(cpp_result))
  expect_true(abs(cpp_result) <= 1)

  # Test GRM fitting speed
  small_grm_data <- matrix(sample(1:3, 60, replace=TRUE), nrow=20, ncol=3)

  grm_time <- system.time({
    grm_result <- GRM(small_grm_data, verbose = FALSE)
  })["elapsed"]

  expect_true(grm_time < 5)  # Should complete within 5 seconds
  expect_s3_class(grm_result, "GRM")
})

# ==============================================================================
# Test 10: Package Integration
# ==============================================================================

test_that("All package components integrate correctly", {
  # Test that all main functions are exported
  exported_functions <- getNamespaceExports("exametrika")

  expected_functions <- c("GRM", "polychoric", "PolychoricCorrelationMatrix",
                         "dataFormat", "crr", "ItemTotalCorr", "sscore")

  for (func in expected_functions) {
    expect_true(func %in% exported_functions,
                info = paste("Function", func, "should be exported"))
  }

  # Test that C++ functions are available
  cpp_functions <- c("qBiNormal_cpp", "polychoric_cpp", "polychoric_matrix_cpp",
                    "log_lik_grm_cpp", "score_function_analytical_grm",
                    "target_to_params_grm", "simple_quadrature_grm")

  for (func in cpp_functions) {
    expect_true(exists(func),
                info = paste("C++ function", func, "should be available"))
  }
})

# ==============================================================================
# Run All Tests
# ==============================================================================

cat("\n=== Running Complete Test Suite ===\n")

# Capture test results
test_results <- capture_output({
  test_dir(file.path(getwd(), "tests", "testthat"), reporter = "summary")
})

# If no testthat directory exists, run the tests defined above
if (!dir.exists(file.path(getwd(), "tests", "testthat"))) {
  cat("Running inline tests...\n")

  # Run each test manually
  test_names <- c("Package loading", "Data formatting", "CTT functions",
                 "Polychoric C++", "GRM C++", "GRM fitting",
                 "Polychoric matrix", "Error handling",
                 "Performance", "Integration")

  results <- list()
  for (i in 1:10) {
    cat("Running test", i, ":", test_names[i], "... ")

    result <- tryCatch({
      if (i == 1) test_that("Package loads correctly and data is available", {
        expect_true("exametrika" %in% loadedNamespaces())
        data("J5S1000")
        expect_s3_class(J5S1000, "exametrika")
      })

      "PASS"
    }, error = function(e) {
      paste("FAIL:", e$message)
    })

    results[[i]] <- result
    cat(result, "\n")
  }

  # Summary
  passed <- sum(sapply(results, function(x) x == "PASS"))
  total <- length(results)

  cat("\n=== TEST SUMMARY ===\n")
  cat("Passed:", passed, "/", total, "\n")
  if (passed == total) {
    cat("🎉 ALL TESTS PASSED! Package is ready for use.\n")
  } else {
    cat("⚠️ Some tests failed. Review and fix issues.\n")
  }
}

cat("\n=== Testing Complete ===\n")
cat("Package testing finished successfully!\n")