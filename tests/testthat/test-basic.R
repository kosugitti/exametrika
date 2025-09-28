# Basic Package Tests

test_that("Package loads and example data is available", {
  # Test that the package namespace is loaded
  expect_true("exametrika" %in% loadedNamespaces())

  # Test example datasets
  data("J5S1000")
  expect_s3_class(J5S1000, "exametrika")
  expect_equal(nrow(J5S1000$Q), 1000)
  expect_equal(ncol(J5S1000$Q), 5)
  expect_equal(J5S1000$response.type, "ordinal")

  data("J15S500")
  expect_s3_class(J15S500, "exametrika")
  expect_equal(nrow(J15S500$U), 500)
  expect_equal(ncol(J15S500$U), 15)
  expect_equal(J15S500$response.type, "binary")
})

test_that("dataFormat works correctly", {
  # Test with simple binary data (25 rows)
  set.seed(200)
  binary_data <- matrix(sample(c(0, 1), 25 * 4, replace = TRUE), nrow = 25, ncol = 4)
  result <- dataFormat(binary_data)

  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "binary")
  expect_equal(nrow(result$U), 25)
  expect_equal(ncol(result$U), 4)

  # Test with ordinal data (explicitly specified, 25 rows)
  set.seed(201)
  ordinal_data <- matrix(sample(1:4, 25 * 4, replace = TRUE), nrow = 25, ncol = 4)
  result <- dataFormat(ordinal_data, response.type = "ordinal")

  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "ordinal")
  expect_equal(nrow(result$Q), 25)
  expect_equal(ncol(result$Q), 4)

  # Test automatic ordinal detection (25 rows)
  set.seed(202)
  ordinal_auto <- matrix(sample(1:4, 25 * 4, replace = TRUE), nrow = 25, ncol = 4)
  result_auto <- dataFormat(ordinal_auto)

  expect_s3_class(result_auto, "exametrika")
  expect_equal(result_auto$response.type, "ordinal")

  # Test with missing values (25 rows)
  set.seed(203)
  data_with_na <- matrix(sample(c(1, 2, 3, NA), 25 * 2, replace = TRUE), nrow = 25, ncol = 2)
  result <- dataFormat(data_with_na)

  expect_s3_class(result, "exametrika")
  expect_true(any(result$Z == 0)) # Should have missing indicators
})

test_that("Essential package functions are exported", {
  exported_functions <- getNamespaceExports("exametrika")

  # Core functions should be exported
  essential_functions <- c(
    "GRM", "polychoric", "PolychoricCorrelationMatrix",
    "dataFormat", "crr", "ItemTotalCorr", "sscore",
    "nrs", "passage", "percentile", "stanine"
  )

  for (func in essential_functions) {
    expect_true(func %in% exported_functions,
      info = paste("Essential function", func, "should be exported")
    )
  }
})

test_that("C++ functions are available", {
  # Test that key C++ functions exist and work
  cpp_functions <- c(
    "qBiNormal_cpp", "polychoric_cpp", "polychoric_matrix_cpp",
    "log_lik_grm_cpp", "score_function_analytical_grm",
    "target_to_params_grm", "simple_quadrature_grm"
  )

  for (func in cpp_functions) {
    expect_true(exists(func, mode = "function"),
      info = paste("C++ function", func, "should be available")
    )
  }

  # Quick functionality test
  expect_type(qBiNormal_cpp(0, 0, 0), "double")
  expect_type(simple_quadrature_grm(5), "list")
})
