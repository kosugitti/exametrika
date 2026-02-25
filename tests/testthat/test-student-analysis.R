library(exametrika)

### StudentAnalysis Tests

test_that("StudentAnalysis Basic Execution", {
  result <- StudentAnalysis(J15S500)

  # Result should be a data.frame
  expect_true(is.data.frame(result))

  # Expected columns: ID, NR, NRS, PR, SS, Percentile, Stanine
  expected_cols <- c("ID", "NR", "NRS", "PR", "SS", "Percentile", "Stanine")
  expect_equal(names(result), expected_cols)
})

test_that("StudentAnalysis Correct Dimensions", {
  result <- StudentAnalysis(J15S500)

  # J15S500 has 500 students and 15 items
  expect_equal(nrow(result), 500)
  expect_equal(ncol(result), 7)

  # NR should equal number of items (15) for complete-response students
  expect_true(all(result$NR <= 15))
  expect_true(all(result$NR > 0))
})

test_that("StudentAnalysis Value Ranges", {
  result <- StudentAnalysis(J15S500)

  # Passage Rate should be between 0 and 1
  expect_true(all(result$PR >= 0 & result$PR <= 1, na.rm = TRUE))

  # NRS should be non-negative and at most equal to NR
  expect_true(all(result$NRS >= 0, na.rm = TRUE))
  expect_true(all(result$NRS <= result$NR, na.rm = TRUE))

  # Stanine should be between 1 and 9 (returned as factor)
  expect_true(is.factor(result$Stanine))
  stanine_vals <- as.numeric(as.character(result$Stanine))
  expect_true(all(stanine_vals >= 1 & stanine_vals <= 9, na.rm = TRUE))

  # Percentile should be between 0 and 100
  expect_true(all(result$Percentile >= 0 & result$Percentile <= 100, na.rm = TRUE))
})
