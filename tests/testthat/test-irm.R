library(exametrika)

### IRM (Infinite Relational Model) Tests

### Setup - run model once and share across tests
tmp_irm <- dataFormat(J35S515)
result_irm <- Biclustering_IRM(tmp_irm, gamma_c = 1, gamma_f = 1, verbose = FALSE)

test_that("IRM Basic Execution", {
  # Basic structure checks
  expect_s3_class(result_irm, "exametrika")
  expect_true("IRM" %in% class(result_irm))

  # Result components exist
  expect_true(!is.null(result_irm$n_class))
  expect_true(!is.null(result_irm$n_field))
  expect_true(!is.null(result_irm$TRP))
  expect_true(!is.null(result_irm$LCD))
  expect_true(!is.null(result_irm$LFD))
  expect_true(!is.null(result_irm$FRP))
  expect_true(!is.null(result_irm$FieldEstimated))
  expect_true(!is.null(result_irm$ClassEstimated))

  # Dimensions are consistent
  expect_equal(length(result_irm$TRP), result_irm$n_class)
  expect_equal(length(result_irm$LCD), result_irm$n_class)
  expect_equal(length(result_irm$LFD), result_irm$n_field)
  expect_equal(length(result_irm$FieldEstimated), result_irm$testlength)
})

test_that("IRM Backward Compatibility", {
  # Deprecated field names should still work
  expect_equal(result_irm$Nclass, result_irm$n_class)
  expect_equal(result_irm$Nfield, result_irm$n_field)
  expect_equal(result_irm$N_Cycle, result_irm$n_cycle)
  expect_equal(result_irm$EM_Cycle, result_irm$em_cycle)
})

test_that("IRM Test Fit Indices", {
  # TestFitIndices
  expect_true(!is.null(result_irm$TestFitIndices))
  tfi <- result_irm$TestFitIndices
  expect_true(!is.null(tfi$model_log_like))
  expect_true(!is.null(tfi$AIC))
  expect_true(!is.null(tfi$BIC))

  # Log-likelihood should be negative
  expect_true(tfi$model_log_like < 0)

  # log_lik field should match
  expect_equal(result_irm$log_lik, tfi$model_log_like)
})

test_that("IRM FRP Validity", {
  # FRP should contain values between 0 and 1
  expect_true(all(result_irm$FRP >= 0 & result_irm$FRP <= 1))

  # FRP dimensions: n_field rows x n_class columns
  expect_equal(nrow(result_irm$FRP), result_irm$n_field)
  expect_equal(ncol(result_irm$FRP), result_irm$n_class)
})

test_that("IRM FRPIndex Exists", {
  # FRPIndex should exist
  expect_true(!is.null(result_irm$FRPIndex))
})
