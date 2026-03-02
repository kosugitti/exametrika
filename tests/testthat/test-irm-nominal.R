library(exametrika)

### Nominal IRM (Biclustering_IRM.nominal) Tests
### Data: J20S600 (nominal, 20 items, 600 students, 4 categories)

### Setup - run model once and share across tests
result_nirm <- Biclustering_IRM(J20S600, gamma_c = 1, gamma_f = 1, seed = 123, verbose = FALSE)

test_that("Nominal IRM Basic Execution", {
  # Basic structure checks
  expect_s3_class(result_nirm, "exametrika")
  expect_true("nominalBiclustering" %in% class(result_nirm))

  # Result components exist
  expect_true(!is.null(result_nirm$n_class))
  expect_true(!is.null(result_nirm$n_field))
  expect_true(!is.null(result_nirm$n_cycle))
  expect_true(!is.null(result_nirm$FRP))
  expect_true(!is.null(result_nirm$LCD))
  expect_true(!is.null(result_nirm$LFD))
  expect_true(!is.null(result_nirm$FieldEstimated))
  expect_true(!is.null(result_nirm$ClassEstimated))
  expect_true(!is.null(result_nirm$FieldMembership))
  expect_true(!is.null(result_nirm$ClassMembership))
  expect_true(!is.null(result_nirm$Students))
  expect_true(!is.null(result_nirm$Q))
  expect_true(!is.null(result_nirm$Z))
})

test_that("Nominal IRM Dimensions Consistency", {
  nfld <- result_nirm$n_field
  ncls <- result_nirm$n_class
  nitems <- result_nirm$testlength
  nobs <- result_nirm$nobs

  # Check basic dimensions

  expect_equal(nitems, 20)
  expect_equal(nobs, 600)
  expect_true(nfld >= 1 && nfld <= nitems)
  expect_true(ncls >= 2)

  # FRP: 3D array (nfld x ncls x maxQ)
  expect_equal(length(dim(result_nirm$FRP)), 3)
  expect_equal(dim(result_nirm$FRP)[1], nfld)
  expect_equal(dim(result_nirm$FRP)[2], ncls)
  expect_equal(dim(result_nirm$FRP)[3], 4) # J20S600 has 4 categories

  # LCD and LFD
  expect_equal(length(result_nirm$LCD), ncls)
  expect_equal(length(result_nirm$LFD), nfld)

  # Field/Class Estimated
  expect_equal(length(result_nirm$FieldEstimated), nitems)
  expect_equal(length(result_nirm$ClassEstimated), nobs)

  # Field/Class Membership
  expect_equal(nrow(result_nirm$FieldMembership), nitems)
  expect_equal(ncol(result_nirm$FieldMembership), nfld)
  expect_equal(nrow(result_nirm$ClassMembership), nobs)
  expect_equal(ncol(result_nirm$ClassMembership), ncls)

  # Students matrix
  expect_equal(nrow(result_nirm$Students), nobs)
  expect_equal(ncol(result_nirm$Students), ncls + 1) # membership columns + Estimate
})

test_that("Nominal IRM FRP Probabilities Sum to 1", {
  nfld <- dim(result_nirm$FRP)[1]
  ncls <- dim(result_nirm$FRP)[2]
  for (f in 1:nfld) {
    for (c in 1:ncls) {
      expect_equal(sum(result_nirm$FRP[f, c, ]), 1.0, tolerance = 1e-10)
    }
  }
})

test_that("Nominal IRM FRP Values Are Valid Probabilities", {
  # All FRP values should be between 0 and 1
  expect_true(all(result_nirm$FRP >= 0))
  expect_true(all(result_nirm$FRP <= 1))
})

test_that("Nominal IRM Distribution Sums", {
  # LCD (Class Distribution) should sum to nobs
  expect_equal(sum(result_nirm$LCD), result_nirm$nobs)

  # LFD (Field Distribution) should sum to testlength
  expect_equal(sum(result_nirm$LFD), result_nirm$testlength)
})

test_that("Nominal IRM ClassMembership Rows Sum to 1", {
  row_sums <- unname(rowSums(result_nirm$ClassMembership))
  for (i in seq_along(row_sums)) {
    expect_equal(row_sums[i], 1.0, tolerance = 1e-10)
  }
})

test_that("Nominal IRM FieldMembership Rows Sum to 1", {
  row_sums <- unname(rowSums(result_nirm$FieldMembership))
  for (i in seq_along(row_sums)) {
    expect_equal(row_sums[i], 1.0, tolerance = 1e-10)
  }
})

test_that("Nominal IRM ClassEstimated Values Are Valid", {
  # Each estimated class should be within 1:n_class
  expect_true(all(result_nirm$ClassEstimated >= 1))
  expect_true(all(result_nirm$ClassEstimated <= result_nirm$n_class))
})

test_that("Nominal IRM FieldEstimated Values Are Valid", {
  # Each estimated field should be within 1:n_field
  expect_true(all(result_nirm$FieldEstimated >= 1))
  expect_true(all(result_nirm$FieldEstimated <= result_nirm$n_field))
})

test_that("Nominal IRM Test Fit Indices", {
  # TestFitIndices should exist and have proper components
  expect_true(!is.null(result_nirm$TestFitIndices))
  tfi <- result_nirm$TestFitIndices
  expect_s3_class(tfi, "exametrika")
  expect_true("ModelFit" %in% class(tfi))

  expect_true(!is.null(tfi$model_log_like))
  expect_true(!is.null(tfi$bench_log_like))
  expect_true(!is.null(tfi$null_log_like))
  expect_true(!is.null(tfi$AIC))
  expect_true(!is.null(tfi$BIC))
  expect_true(!is.null(tfi$RMSEA))

  # Log-likelihood should be negative
  expect_true(tfi$model_log_like < 0)

  # Log-likelihood ordering: model < bench (bench is closer to saturated)
  expect_true(tfi$model_log_like <= tfi$bench_log_like)

  # log_lik field should match
  expect_equal(result_nirm$log_lik, tfi$model_log_like)
})

test_that("Nominal IRM Backward Compatibility", {
  # Deprecated field names should still work
  expect_equal(result_nirm$Nclass, result_nirm$n_class)
  expect_equal(result_nirm$Nfield, result_nirm$n_field)
  expect_equal(result_nirm$N_Cycle, result_nirm$n_cycle)
  expect_equal(result_nirm$LogLik, result_nirm$log_lik)

  # LRD should equal LCD (alias)
  expect_equal(result_nirm$LRD, result_nirm$LCD)

  # RMD should equal CMD (alias)
  expect_equal(result_nirm$RMD, result_nirm$CMD)
})

test_that("Nominal IRM Seed Reproducibility", {
  # Same seed should produce identical results
  result_a <- Biclustering_IRM(J20S600, gamma_c = 1, gamma_f = 1, seed = 42, verbose = FALSE)
  result_b <- Biclustering_IRM(J20S600, gamma_c = 1, gamma_f = 1, seed = 42, verbose = FALSE)

  expect_equal(result_a$n_class, result_b$n_class)
  expect_equal(result_a$n_field, result_b$n_field)
  expect_equal(result_a$FRP, result_b$FRP)
  expect_equal(result_a$FieldEstimated, result_b$FieldEstimated)
  expect_equal(result_a$ClassEstimated, result_b$ClassEstimated)
  expect_equal(result_a$log_lik, result_b$log_lik)
  expect_equal(result_a$LCD, result_b$LCD)
  expect_equal(result_a$LFD, result_b$LFD)
})

test_that("Nominal IRM Seed NULL Does Not Set Seed", {
  # With seed = NULL, the function should not call set.seed()
  # and the results should depend on the current RNG state.
  result_null <- Biclustering_IRM(J20S600, gamma_c = 1, gamma_f = 1, seed = NULL, verbose = FALSE)
  expect_s3_class(result_null, "exametrika")
  expect_true("nominalBiclustering" %in% class(result_null))
  expect_true(!is.null(result_null$n_class))
  expect_true(!is.null(result_null$n_field))
})

test_that("Nominal IRM Default Seed Is 123", {
  # Verify the default value of seed parameter is 123
  fn_formals <- formals(Biclustering_IRM.nominal)
  expect_equal(fn_formals$seed, 123)
})

test_that("Nominal IRM S3 Dispatch Works", {
  # Biclustering_IRM should dispatch to .nominal method for nominal data
  # J20S600 has response.type == "nominal"
  expect_equal(J20S600$response.type, "nominal")
})

test_that("Nominal IRM Alpha Parameter Validation", {
  # alpha must be positive
  expect_error(
    Biclustering_IRM(J20S600, gamma_c = 1, gamma_f = 1, alpha = 0, seed = 123, verbose = FALSE),
    "alpha must be positive"
  )
  expect_error(
    Biclustering_IRM(J20S600, gamma_c = 1, gamma_f = 1, alpha = -1, seed = 123, verbose = FALSE),
    "alpha must be positive"
  )
})

test_that("Nominal IRM Q and Z Matrices Stored Correctly", {
  # Q and Z should be stored in the result
  expect_equal(dim(result_nirm$Q), c(600, 20))
  expect_equal(dim(result_nirm$Z), c(600, 20))

  # Z values should be 0 or 1
  expect_true(all(result_nirm$Z %in% c(0, 1)))

  # Q values for observed responses (Z=1) should be integers in 1:maxQ
  observed <- result_nirm$Z == 1
  expect_true(all(result_nirm$Q[observed] >= 1))
  expect_true(all(result_nirm$Q[observed] <= 4))
})

test_that("Nominal IRM Students Matrix Structure", {
  ncls <- result_nirm$n_class
  students <- result_nirm$Students

  # Last column should be "Estimate"
  expect_equal(colnames(students)[ncol(students)], "Estimate")

  # Membership columns should sum to approximately 1 per student
  membership_sums <- unname(rowSums(students[, 1:ncls]))
  for (i in 1:nrow(students)) {
    expect_equal(membership_sums[i], 1.0, tolerance = 1e-10)
  }

  # Estimate column should match ClassEstimated
  expect_equal(unname(as.numeric(students[, "Estimate"])), unname(result_nirm$ClassEstimated))
})
