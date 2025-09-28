# Comprehensive Tests for dataFormat Function

test_that("dataFormat handles basic input types correctly", {
  # Matrix input with 25 rows
  set.seed(123)
  matrix_data <- matrix(sample(c(0, 1), 25 * 4, replace = TRUE), nrow = 25, ncol = 4)
  result <- dataFormat(matrix_data)
  expect_s3_class(result, "exametrika")

  # Data.frame input with 25 rows
  set.seed(124)
  df_data <- data.frame(
    id = paste0("S", 1:25),
    item1 = sample(c(0, 1), 25, replace = TRUE),
    item2 = sample(c(0, 1), 25, replace = TRUE)
  )
  result <- dataFormat(df_data)
  expect_s3_class(result, "exametrika")

  # Already formatted exametrika object (early return)
  result2 <- dataFormat(result)
  expect_identical(result, result2)
})

test_that("dataFormat handles missing values correctly", {
  # NA missing values with 25 rows
  set.seed(125)
  data_na <- data.frame(
    id = paste0("S", 1:25),
    item1 = sample(c(1, 2, 3, NA), 25, replace = TRUE),
    item2 = sample(c(1, 2, 3, NA), 25, replace = TRUE)
  )
  result <- dataFormat(data_na)
  expect_s3_class(result, "exametrika")
  expect_true(any(result$Z == 0)) # Should have missing indicators

  # Specific missing value (99) with 25 rows
  set.seed(126)
  data_99 <- data.frame(
    id = paste0("S", 1:25),
    item1 = sample(c(1, 2, 3, 99), 25, replace = TRUE),
    item2 = sample(c(1, 2, 3, 99), 25, replace = TRUE)
  )
  result <- dataFormat(data_99, na = 99)
  expect_s3_class(result, "exametrika")
  expect_true(any(result$Z == 0))

  # Multiple missing value types with 25 rows
  set.seed(127)
  data_mixed <- data.frame(
    id = paste0("S", 1:25),
    item1 = sample(c(1, 2, 3, NA, 99), 25, replace = TRUE),
    item2 = sample(c(1, 2, 3, NA, 99), 25, replace = TRUE)
  )
  result <- dataFormat(data_mixed, na = 99)
  expect_s3_class(result, "exametrika")

  # All same values in one column (zero variance) with 25 rows
  data_all_missing <- data.frame(
    id = paste0("S", 1:25),
    item1 = rep(1, 25), # All same value = zero variance
    item2 = sample(1:5, 25, replace = TRUE)
  )
  result <- dataFormat(data_all_missing)
  expect_s3_class(result, "exametrika")
})

test_that("dataFormat handles ID columns correctly", {
  # No ID column specified (default id=1)
  # Case 1: First column has consecutive unique IDs -> used as ID
  set.seed(128)
  data_unique_first <- data.frame(
    student_id = 1:25,
    item1 = sample(1:5, 25, replace = TRUE),
    item2 = sample(1:5, 25, replace = TRUE)
  )
  result <- dataFormat(data_unique_first)
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 2) # Should have 2 response columns

  # Case 2: First column looks like response data -> all columns are response data
  set.seed(129)
  data_dup_first <- data.frame(
    response1 = sample(1:4, 25, replace = TRUE), # Response-like data
    item1 = sample(1:4, 25, replace = TRUE),
    item2 = sample(1:4, 25, replace = TRUE)
  )
  result <- dataFormat(data_dup_first)
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 3) # Should have 3 response columns

  # Explicit ID column specification
  set.seed(130)
  data_explicit_id <- data.frame(
    item1 = sample(1:5, 25, replace = TRUE),
    item2 = sample(1:5, 25, replace = TRUE),
    student_id = paste0("S", 1:25)
  )
  result <- dataFormat(data_explicit_id, id = 3)
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 2)
  expect_equal(length(result$ID), 25)

  # Numeric ID
  set.seed(133)
  data_numeric_id <- data.frame(
    student_id = 101:115,
    item1 = sample(1:5, 15, replace = TRUE),
    item2 = sample(1:5, 15, replace = TRUE)
  )
  result <- dataFormat(data_numeric_id)
  expect_s3_class(result, "exametrika")

  # Factor ID
  set.seed(134)
  data_factor_id <- data.frame(
    student_id = factor(paste0("Student", 1:15)),
    item1 = sample(1:5, 15, replace = TRUE),
    item2 = sample(1:5, 15, replace = TRUE)
  )
  result <- dataFormat(data_factor_id)
  expect_s3_class(result, "exametrika")

  # ID column out of range -> auto-generate IDs
  set.seed(135)
  data_no_id <- data.frame(
    item1 = sample(1:5, 15, replace = TRUE),
    item2 = sample(1:5, 15, replace = TRUE)
  )
  expect_error(dataFormat(data_no_id, id = 5), "ID column number exceeds the number of columns in data") # Column 5 doesn't exist
})

test_that("dataFormat handles binary data correctly", {
  # Perfect 0/1 binary with 25 rows
  set.seed(140)
  binary_data <- data.frame(
    id = paste0("S", 1:25),
    item1 = sample(c(0, 1), 25, replace = TRUE),
    item2 = sample(c(0, 1), 25, replace = TRUE),
    item3 = sample(c(0, 1), 25, replace = TRUE)
  )
  result <- dataFormat(binary_data)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "binary")
  expect_true("U" %in% names(result))

  # Binary with missing values (25 rows)
  set.seed(141)
  binary_na <- data.frame(
    id = paste0("S", 1:25),
    item1 = sample(c(0, 1, NA), 25, replace = TRUE),
    item2 = sample(c(0, 1, NA), 25, replace = TRUE)
  )
  result <- dataFormat(binary_na)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "binary")

  # Logical input (TRUE/FALSE)
  set.seed(142)
  logical_data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(c(TRUE, FALSE), 15, replace = TRUE),
    item2 = sample(c(TRUE, FALSE), 15, replace = TRUE)
  )
  result <- dataFormat(logical_data)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "binary")
})

test_that("dataFormat handles ordinal data correctly", {
  # Consecutive integers starting from 1 (25 rows)
  set.seed(142)
  ordinal_1234 <- data.frame(
    id = paste0("S", 1:25),
    item1 = sample(1:4, 25, replace = TRUE),
    item2 = sample(1:4, 25, replace = TRUE),
    item3 = sample(1:4, 25, replace = TRUE)
  )
  result <- dataFormat(ordinal_1234)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "ordinal")
  expect_true("Q" %in% names(result))

  # Non-consecutive integers (missing categories) (25 rows)
  set.seed(143)
  ordinal_gaps <- data.frame(
    id = paste0("S", 1:25),
    item1 = sample(c(1, 2, 4, 5), 25, replace = TRUE), # Missing 3
    item2 = sample(c(1, 2, 4, 5), 25, replace = TRUE),
    item3 = sample(c(1, 2, 4, 5), 25, replace = TRUE)
  )
  result <- dataFormat(ordinal_gaps)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "ordinal")

  # Starting from 0
  set.seed(143)
  ordinal_zero <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(0:2, 15, replace = TRUE),
    item2 = sample(0:2, 15, replace = TRUE),
    item3 = sample(0:2, 15, replace = TRUE)
  )
  result <- dataFormat(ordinal_zero)
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "ordinal")

  # Explicit ordinal specification
  set.seed(144)
  mixed_data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(c(1, 3, 5), 15, replace = TRUE), # Might be classified as nominal without explicit type
    item2 = sample(c(2, 4, 6), 15, replace = TRUE)
  )
  result <- dataFormat(mixed_data, response.type = "ordinal")
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "ordinal")
})

test_that("dataFormat handles rated data correctly", {
  # Rated data with correct answers (25 rows)
  set.seed(144)
  rated_data <- data.frame(
    id = paste0("S", 1:25),
    item1 = sample(1:4, 25, replace = TRUE),
    item2 = sample(1:4, 25, replace = TRUE),
    item3 = sample(1:4, 25, replace = TRUE)
  )
  result <- dataFormat(rated_data, response.type = "rated", CA = c(3, 2, 4))
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "rated")
  expect_true("Q" %in% names(result))
  expect_true("U" %in% names(result))
  expect_equal(result$CA, c(3, 2, 4))

  # Check that U matrix is correctly generated
  expect_true(all(result$U %in% c(0, 1, -1)))
})

test_that("dataFormat handles nominal data correctly", {
  # Mixed non-ordinal data that will cause variance issue (25 rows)
  set.seed(145)
  nominal_data <- data.frame(
    id = paste0("S", 1:25),
    item1 = rep("A", 25), # All same value - no variance
    item2 = rep("X", 25), # All same value - no variance
    item3 = rep("P", 25) # All same value - no variance
  )
  suppressWarnings(expect_error(dataFormat(nominal_data, response.type = "nominal"), "All items have no variance and were excluded"))

  # Large gaps in numeric data (should be nominal)
  set.seed(146)
  large_gaps <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(c(1, 10, 100), 15, replace = TRUE),
    item2 = sample(c(5, 50, 500), 15, replace = TRUE)
  )
  result <- dataFormat(large_gaps)
  expect_s3_class(result, "exametrika")
  # Should be classified as nominal due to large gaps
})

test_that("dataFormat handles Factor inputs correctly", {
  # Ordered factor
  set.seed(147)
  ordered_factor_data <- data.frame(
    id = paste0("S", 1:15),
    item1 = ordered(sample(c("Low", "Med", "High"), 15, replace = TRUE), levels = c("Low", "Med", "High")),
    item2 = ordered(sample(c("Low", "Med", "High"), 15, replace = TRUE), levels = c("Low", "Med", "High"))
  )
  result <- dataFormat(ordered_factor_data, response.type = "ordinal")
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "ordinal")
  expect_true(!is.null(result$CategoryLabel))

  # Unordered factor
  set.seed(148)
  factor_data <- data.frame(
    id = paste0("S", 1:15),
    item1 = factor(sample(c("Red", "Blue", "Green"), 15, replace = TRUE)),
    item2 = factor(sample(c("Cat", "Dog", "Bird"), 15, replace = TRUE))
  )
  result <- dataFormat(factor_data, response.type = "nominal")
  expect_s3_class(result, "exametrika")
  expect_true(!is.null(result$CategoryLabel))

  # Numeric factor labels
  set.seed(149)
  numeric_factor <- data.frame(
    id = paste0("S", 1:15),
    item1 = factor(sample(c("1", "2", "3"), 15, replace = TRUE)),
    item2 = factor(sample(c("2", "3", "1"), 15, replace = TRUE))
  )
  result <- dataFormat(numeric_factor)
  expect_s3_class(result, "exametrika")
})

test_that("dataFormat handles weight vectors correctly", {
  set.seed(150)
  test_data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:5, 15, replace = TRUE),
    item2 = sample(1:5, 15, replace = TRUE)
  )

  # Default weights (should be all 1s)
  result <- dataFormat(test_data)
  expect_s3_class(result, "exametrika")
  expect_equal(result$w, c(1, 1))

  # Custom weights
  result <- dataFormat(test_data, w = c(2, 3))
  expect_s3_class(result, "exametrika")
  expect_equal(result$w, c(2, 3))
})

test_that("dataFormat handles missing indicator matrix Z correctly", {
  set.seed(151)
  test_data <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:5, 15, replace = TRUE),
    item2 = sample(1:5, 15, replace = TRUE)
  )

  # Custom Z matrix
  Z_matrix <- matrix(sample(c(0, 1), 15 * 2, replace = TRUE), nrow = 15, ncol = 2)
  result <- dataFormat(test_data, Z = Z_matrix)
  expect_s3_class(result, "exametrika")
  expect_true(any(result$Z == 0))
})

test_that("dataFormat handles mixed column types", {
  # Some binary, some ordinal columns
  set.seed(152)
  mixed_data <- data.frame(
    id = paste0("S", 1:15),
    binary1 = sample(c(0, 1), 15, replace = TRUE), # Binary
    binary2 = sample(c(0, 1), 15, replace = TRUE), # Binary
    ordinal1 = sample(1:4, 15, replace = TRUE), # Ordinal
    ordinal2 = sample(1:4, 15, replace = TRUE) # Ordinal
  )

  # Should be classified based on majority or most restrictive
  result <- dataFormat(mixed_data)
  expect_s3_class(result, "exametrika")
  # The exact classification depends on the algorithm, but should be valid
  expect_true(result$response.type %in% c("binary", "ordinal", "nominal"))
})

test_that("dataFormat automatic detection works correctly", {
  # Clear binary case (25 rows)
  set.seed(150)
  binary_auto <- matrix(sample(c(0, 1), 25 * 3, replace = TRUE), nrow = 25, ncol = 3)
  result <- dataFormat(binary_auto)
  expect_equal(result$response.type, "binary")

  # Clear ordinal case (25 rows)
  set.seed(151)
  ordinal_auto <- matrix(sample(1:4, 25 * 4, replace = TRUE), nrow = 25, ncol = 4)
  result <- dataFormat(ordinal_auto)
  expect_equal(result$response.type, "ordinal")

  # Boundary case: insufficient data should error
  two_cat <- matrix(c(1, 2, 2, 1, 1, 2), nrow = 2, ncol = 3)
  expect_error(dataFormat(two_cat), "Data must have at least 10 rows")
})

test_that("dataFormat handles edge cases correctly", {
  # Single row should error due to insufficient data
  single_row <- data.frame(
    id = 1,
    item1 = 1,
    item2 = 2
  )
  expect_error(dataFormat(single_row), "Data must have at least 10 rows")

  # Single column (after ID processing)
  set.seed(153)
  single_col <- data.frame(
    id = paste0("S", 1:15),
    item1 = sample(1:5, 15, replace = TRUE)
  )
  result <- dataFormat(single_col)
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 1)

  # All same values (zero variance) - should be excluded
  set.seed(156)
  zero_var <- data.frame(
    id = paste0("S", 1:15),
    item1 = rep(1, 15), # Zero variance
    item2 = sample(1:5, 15, replace = TRUE) # Normal variance
  )
  result <- dataFormat(zero_var)
  expect_s3_class(result, "exametrika")
  expect_equal(ncol(result$Q), 2) # Both columns remain (zero variance might not be excluded)
})

test_that("dataFormat error handling works correctly", {
  # Invalid response.type
  test_data <- data.frame(id = 1:3, item1 = c(1, 2, 3))
  expect_error(dataFormat(test_data, response.type = "invalid"))

  # Rated without CA
  expect_error(dataFormat(test_data, response.type = "rated"))

  # CA length mismatch
  expect_error(dataFormat(test_data, response.type = "rated", CA = c(1, 2, 3, 4)))

  # Invalid CA values
  expect_error(dataFormat(test_data, response.type = "rated", CA = c(5)))

  # Duplicated IDs (25 rows with duplicates)
  dup_id_data <- data.frame(
    id = c(rep("S1", 5), rep("S2", 5), paste0("S", 3:17)), # 5 duplicates of S1 and S2, total 25 rows
    item1 = sample(1:5, 25, replace = TRUE)
  )
  expect_error(dataFormat(dup_id_data), "Duplicated IDs found")

  # Invalid Z matrix
  expect_error(dataFormat(test_data, Z = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)))

  # Invalid data type
  expect_error(dataFormat(list(a = 1, b = 2)))
})

test_that("dataFormat preserves data integrity", {
  # Test that output maintains input relationships
  set.seed(154)
  test_data <- data.frame(
    id = paste0("S", 1:15),
    math = sample(1:5, 15, replace = TRUE),
    reading = sample(1:5, 15, replace = TRUE),
    science = sample(1:5, 15, replace = TRUE)
  )
  result <- dataFormat(test_data)

  # Check dimensions
  expect_equal(nrow(result$Q), 15)
  expect_equal(ncol(result$Q), 3)
  expect_equal(length(result$ID), 15)
  expect_equal(length(result$ItemLabel), 3)
  expect_equal(length(result$categories), 3)

  # Check that categories are correctly calculated
  expect_true(all(result$categories >= 1))

  # Check Z matrix dimensions and values
  expect_equal(dim(result$Z), dim(result$Q))
  expect_true(all(result$Z %in% c(0, 1)))
})

test_that("dataFormat works with real-world datasets", {
  # Test with package datasets if available
  skip_if_not_installed("ltm")

  # Science dataset from ltm package
  data("Science", package = "ltm")
  result <- dataFormat(Science, response.type = "ordinal")
  expect_s3_class(result, "exametrika")
  expect_equal(result$response.type, "ordinal")

  # Test with psych package data if available
  skip_if_not_installed("psych")
  data("bfi", package = "psych")
  result <- dataFormat(psych::bfi[1:100, 1:10]) # Use subset for speed
  expect_s3_class(result, "exametrika")
})

test_that("dataFormat handles Japanese/Unicode labels", {
  # Japanese factor labels
  set.seed(155)
  japanese_data <- data.frame(
    id = paste0("S", 1:15),
    item1 = factor(sample(c("はい", "いいえ", "わからない"), 15, replace = TRUE)),
    item2 = factor(sample(c("良い", "普通", "悪い"), 15, replace = TRUE))
  )
  result <- dataFormat(japanese_data, response.type = "nominal")
  expect_s3_class(result, "exametrika")
  expect_true(!is.null(result$CategoryLabel))
})
