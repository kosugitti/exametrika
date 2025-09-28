# Tests for Classical Test Theory Functions

test_that("CTT functions work with binary data", {
  data("J15S500", package = "exametrika")

  # Test correct response rate
  crr_result <- crr(J15S500)
  expect_type(crr_result, "double")
  expect_length(crr_result, 15)
  expect_true(all(crr_result >= 0 & crr_result <= 1))
  expect_true(all(is.finite(crr_result)))

  # Test item-total correlation
  itc_result <- ItemTotalCorr(J15S500)
  expect_type(itc_result, "double")
  expect_length(itc_result, 15)
  expect_true(all(abs(itc_result) <= 1))
  expect_true(all(is.finite(itc_result)))

  # Test number-right score
  nrs_result <- nrs(J15S500)
  expect_type(nrs_result, "double")
  expect_length(nrs_result, 500)
  expect_true(all(nrs_result >= 0))
  expect_true(all(is.finite(nrs_result)))

  # Test standardized scores
  sscore_result <- sscore(J15S500)
  expect_type(sscore_result, "double")
  expect_length(sscore_result, 500)
  expect_true(abs(mean(sscore_result, na.rm = TRUE)) < 0.1) # Should be near 0
  expect_true(abs(sd(sscore_result, na.rm = TRUE) - 1) < 0.1) # Should be near 1

  # Test passage rate
  passage_result <- passage(J15S500)
  expect_type(passage_result, "double")
  expect_length(passage_result, 500)
  expect_true(all(passage_result >= 0 & passage_result <= 1))

  # Test percentiles
  percentile_result <- percentile(J15S500)
  expect_length(percentile_result, 500)
  expect_true(all(percentile_result >= 1 & percentile_result <= 100))

  # Test stanines
  stanine_result <- stanine(J15S500)
  expect_type(stanine_result, "list")
  expect_named(stanine_result, c("stanine", "stanineScore"))
  expect_length(stanine_result$stanineScore, 500)
})

test_that("Item statistics work correctly", {
  data("J15S500", package = "exametrika")

  # Test item odds
  odds_result <- ItemOdds(J15S500)
  expect_type(odds_result, "double")
  expect_length(odds_result, 15)
  expect_true(all(odds_result > 0)) # Odds should be positive

  # Test item thresholds
  threshold_result <- ItemThreshold(J15S500)
  expect_type(threshold_result, "double")
  expect_length(threshold_result, 15)
  expect_true(all(is.finite(threshold_result)))

  # Test item entropy
  entropy_result <- ItemEntropy(J15S500)
  expect_type(entropy_result, "double")
  expect_length(entropy_result, 15)
  expect_true(all(entropy_result >= 0 & entropy_result <= 1))
})

test_that("Inter-item functions work correctly", {
  # Use smaller dataset for speed
  small_data <- J15S500$U[1:100, 1:5]
  formatted_data <- dataFormat(small_data)

  # Test joint sample size
  jss_result <- JointSampleSize(formatted_data)
  expect_s3_class(jss_result, c("exametrika", "matrix"))
  expect_equal(dim(jss_result), c(5, 5))
  expect_true(all(jss_result >= 0))

  # Test joint correct response rate
  jcrr_result <- JCRR(formatted_data)
  expect_s3_class(jcrr_result, c("exametrika", "matrix"))
  expect_equal(dim(jcrr_result), c(5, 5))
  expect_true(all(jcrr_result >= 0 & jcrr_result <= 1))

  # Test conditional correct response rate
  ccrr_result <- CCRR(formatted_data)
  expect_s3_class(ccrr_result, c("exametrika", "matrix"))
  expect_equal(dim(ccrr_result), c(5, 5))

  # Test phi coefficient
  phi_result <- PhiCoefficient(formatted_data)
  expect_s3_class(phi_result, c("exametrika", "matrix"))
  expect_equal(dim(phi_result), c(5, 5))
})

test_that("CTT error handling works", {
  # Test with wrong data type (25 rows)
  set.seed(300)
  ordinal_data <- matrix(sample(1:4, 25 * 4, replace = TRUE), nrow = 25, ncol = 4)
  formatted_ordinal <- dataFormat(ordinal_data)

  # These should fail with ordinal data
  expect_error(crr(formatted_ordinal))
  expect_error(nrs(formatted_ordinal))
  expect_error(passage(formatted_ordinal))
})

test_that("CTT performance is acceptable", {
  data("J15S500", package = "exametrika")

  # Time some key computations
  start_time <- Sys.time()
  crr_result <- crr(J15S500)
  itc_result <- ItemTotalCorr(J15S500)
  sscore_result <- sscore(J15S500)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 1) # Should be very fast
  expect_length(crr_result, 15)
  expect_length(itc_result, 15)
  expect_length(sscore_result, 500)
})
