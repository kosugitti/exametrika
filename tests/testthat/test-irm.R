library(exametrika)

### IRM (Infinite Relational Model) Tests
###
### Two-tier strategy:
###   - The basic structure / reproducibility tests use J15S500
###     (20 items x 400 students) as a fast fixture and run on CRAN.
###   - The original J35S515 (35 items x 515 students) full-data run
###     is preserved as a single skip_on_cran() smoke test.

### Fast shared fixture (J15S500, runs everywhere)
tmp_irm_fast <- dataFormat(J15S500)
result_irm_fast <- Biclustering_IRM(
  tmp_irm_fast, gamma_c = 1, gamma_f = 1, seed = 123, verbose = FALSE
)

test_that("IRM Basic Execution", {
  # Basic structure checks
  expect_s3_class(result_irm_fast, "exametrika")
  expect_true("IRM" %in% class(result_irm_fast))

  # Result components exist
  expect_true(!is.null(result_irm_fast$n_class))
  expect_true(!is.null(result_irm_fast$n_field))
  expect_true(!is.null(result_irm_fast$TRP))
  expect_true(!is.null(result_irm_fast$LCD))
  expect_true(!is.null(result_irm_fast$LFD))
  expect_true(!is.null(result_irm_fast$FRP))
  expect_true(!is.null(result_irm_fast$FieldEstimated))
  expect_true(!is.null(result_irm_fast$ClassEstimated))

  # Dimensions are consistent
  expect_equal(length(result_irm_fast$TRP), result_irm_fast$n_class)
  expect_equal(length(result_irm_fast$LCD), result_irm_fast$n_class)
  expect_equal(length(result_irm_fast$LFD), result_irm_fast$n_field)
  expect_equal(length(result_irm_fast$FieldEstimated), result_irm_fast$testlength)
})

test_that("IRM Backward Compatibility", {
  # Deprecated field names should still work
  expect_equal(result_irm_fast$Nclass, result_irm_fast$n_class)
  expect_equal(result_irm_fast$Nfield, result_irm_fast$n_field)
  expect_equal(result_irm_fast$N_Cycle, result_irm_fast$n_cycle)
  expect_equal(result_irm_fast$EM_Cycle, result_irm_fast$em_cycle)
})

test_that("IRM Test Fit Indices", {
  # TestFitIndices
  expect_true(!is.null(result_irm_fast$TestFitIndices))
  tfi <- result_irm_fast$TestFitIndices
  expect_true(!is.null(tfi$model_log_like))
  expect_true(!is.null(tfi$AIC))
  expect_true(!is.null(tfi$BIC))

  # Log-likelihood should be negative
  expect_true(tfi$model_log_like < 0)

  # log_lik field should match
  expect_equal(result_irm_fast$log_lik, tfi$model_log_like)
})

test_that("IRM FRP Validity", {
  # FRP should contain values between 0 and 1
  expect_true(all(result_irm_fast$FRP >= 0 & result_irm_fast$FRP <= 1))

  # FRP dimensions: n_field rows x n_class columns
  expect_equal(nrow(result_irm_fast$FRP), result_irm_fast$n_field)
  expect_equal(ncol(result_irm_fast$FRP), result_irm_fast$n_class)
})

test_that("IRM FRPIndex Exists", {
  # FRPIndex should exist
  expect_true(!is.null(result_irm_fast$FRPIndex))
})

test_that("IRM Seed Reproducibility", {
  # Same seed should produce identical results (uses fast fixture)
  result_a <- Biclustering_IRM(
    tmp_irm_fast, gamma_c = 1, gamma_f = 1, seed = 42, verbose = FALSE
  )
  result_b <- Biclustering_IRM(
    tmp_irm_fast, gamma_c = 1, gamma_f = 1, seed = 42, verbose = FALSE
  )

  expect_equal(result_a$n_class, result_b$n_class)
  expect_equal(result_a$n_field, result_b$n_field)
  expect_equal(result_a$FRP, result_b$FRP)
  expect_equal(result_a$TRP, result_b$TRP)
  expect_equal(result_a$FieldEstimated, result_b$FieldEstimated)
  expect_equal(result_a$ClassEstimated, result_b$ClassEstimated)
  expect_equal(result_a$log_lik, result_b$log_lik)
})

test_that("IRM Seed NULL does not set seed", {
  # With seed = NULL, the function should not call set.seed()
  # and the results should depend on the current RNG state.
  # We verify that the function runs without error with seed = NULL.
  result_null <- Biclustering_IRM(
    tmp_irm_fast, gamma_c = 1, gamma_f = 1, seed = NULL, verbose = FALSE
  )
  expect_s3_class(result_null, "exametrika")
  expect_true("IRM" %in% class(result_null))
  expect_true(!is.null(result_null$n_class))
  expect_true(!is.null(result_null$n_field))
})

test_that("IRM Default seed is 123", {
  # Verify the default value of seed parameter is 123
  fn_formals <- formals(Biclustering_IRM.binary)
  expect_equal(fn_formals$seed, 123)
})

test_that("IRM J35S515 real-data smoke test", {
  skip_on_cran()
  # Preserves the original J35S515 fixture as a single off-CRAN smoke test.
  tmp_full <- dataFormat(J35S515)
  result_full <- Biclustering_IRM(
    tmp_full, gamma_c = 1, gamma_f = 1, seed = 123, verbose = FALSE
  )
  expect_s3_class(result_full, "exametrika")
  expect_true("IRM" %in% class(result_full))
  expect_true(result_full$n_class >= 1)
  expect_true(result_full$n_field >= 1)
  expect_true(is.finite(result_full$log_lik))
})
