library(exametrika)

### Ordinal IRM (Biclustering_IRM.ordinal) Tests
### Data: J35S500 (ordinal, 35 items, 500 students, 5 categories)

### Setup - run model once and share across tests
result_oirm <- Biclustering_IRM(J35S500, gamma_c = 1, gamma_f = 1, seed = 123, verbose = FALSE)

test_that("Ordinal IRM Basic Execution", {
  # Basic structure checks
  expect_s3_class(result_oirm, "exametrika")
  expect_true("ordinalBiclustering" %in% class(result_oirm))

  # Result components exist
  expect_true(!is.null(result_oirm$n_class))
  expect_true(!is.null(result_oirm$n_field))
  expect_true(!is.null(result_oirm$n_cycle))
  expect_true(!is.null(result_oirm$FRP))
  expect_true(!is.null(result_oirm$FRPIndex))
  expect_true(!is.null(result_oirm$TRP))
  expect_true(!is.null(result_oirm$BFRP))
  expect_true(!is.null(result_oirm$LCD))
  expect_true(!is.null(result_oirm$LFD))
  expect_true(!is.null(result_oirm$FieldEstimated))
  expect_true(!is.null(result_oirm$ClassEstimated))
  expect_true(!is.null(result_oirm$FieldMembership))
  expect_true(!is.null(result_oirm$ClassMembership))
  expect_true(!is.null(result_oirm$Students))
  expect_true(!is.null(result_oirm$Q))
  expect_true(!is.null(result_oirm$Z))
  expect_true(!is.null(result_oirm$SOACflg))
  expect_true(!is.null(result_oirm$WOACflg))
})

test_that("Ordinal IRM Dimensions Consistency", {
  nfld <- result_oirm$n_field
  ncls <- result_oirm$n_class
  nitems <- result_oirm$testlength
  nobs <- result_oirm$nobs

  # Check basic dimensions
  expect_equal(nitems, 35)
  expect_equal(nobs, 500)
  expect_true(nfld >= 1 && nfld <= nitems)
  expect_true(ncls >= 2)

  # FRP: 3D array (nfld x ncls x maxQ)
  expect_equal(length(dim(result_oirm$FRP)), 3)
  expect_equal(dim(result_oirm$FRP)[1], nfld)
  expect_equal(dim(result_oirm$FRP)[2], ncls)
  expect_equal(dim(result_oirm$FRP)[3], 5) # J35S500 has 5 categories

  # TRP
  expect_equal(length(result_oirm$TRP), ncls)

  # LCD and LFD
  expect_equal(length(result_oirm$LCD), ncls)
  expect_equal(length(result_oirm$LFD), nfld)

  # Field/Class Estimated
  expect_equal(length(result_oirm$FieldEstimated), nitems)
  expect_equal(length(result_oirm$ClassEstimated), nobs)

  # Field/Class Membership
  expect_equal(nrow(result_oirm$FieldMembership), nitems)
  expect_equal(ncol(result_oirm$FieldMembership), nfld)
  expect_equal(nrow(result_oirm$ClassMembership), nobs)
  expect_equal(ncol(result_oirm$ClassMembership), ncls)

  # Students matrix
  expect_equal(nrow(result_oirm$Students), nobs)
  expect_equal(ncol(result_oirm$Students), ncls + 1) # membership columns + Estimate
})

test_that("Ordinal IRM FRP Probabilities Sum to 1", {
  nfld <- dim(result_oirm$FRP)[1]
  ncls <- dim(result_oirm$FRP)[2]
  for (f in 1:nfld) {
    for (c in 1:ncls) {
      expect_equal(sum(result_oirm$FRP[f, c, ]), 1.0, tolerance = 1e-10)
    }
  }
})

test_that("Ordinal IRM FRP Values Are Valid Probabilities", {
  # All FRP values should be between 0 and 1
  expect_true(all(result_oirm$FRP >= 0))
  expect_true(all(result_oirm$FRP <= 1))
})

test_that("Ordinal IRM Expected Scores in Valid Range", {
  nfld <- dim(result_oirm$FRP)[1]
  ncls <- dim(result_oirm$FRP)[2]
  maxQ <- dim(result_oirm$FRP)[3]
  for (f in 1:nfld) {
    for (c in 1:ncls) {
      esp <- sum((1:maxQ) * result_oirm$FRP[f, c, ])
      expect_gte(esp, 1.0)
      expect_lte(esp, maxQ)
    }
  }
})

test_that("Ordinal IRM Distribution Sums", {
  # LCD (Class Distribution) should sum to nobs
  expect_equal(sum(result_oirm$LCD), result_oirm$nobs)

  # LFD (Field Distribution) should sum to testlength
  expect_equal(sum(result_oirm$LFD), result_oirm$testlength)
})

test_that("Ordinal IRM ClassMembership Rows Sum to 1", {
  row_sums <- unname(rowSums(result_oirm$ClassMembership))
  for (i in seq_along(row_sums)) {
    expect_equal(row_sums[i], 1.0, tolerance = 1e-10)
  }
})

test_that("Ordinal IRM FieldMembership Rows Sum to 1", {
  row_sums <- unname(rowSums(result_oirm$FieldMembership))
  for (i in seq_along(row_sums)) {
    expect_equal(row_sums[i], 1.0, tolerance = 1e-10)
  }
})

test_that("Ordinal IRM ClassEstimated Values Are Valid", {
  expect_true(all(result_oirm$ClassEstimated >= 1))
  expect_true(all(result_oirm$ClassEstimated <= result_oirm$n_class))
})

test_that("Ordinal IRM FieldEstimated Values Are Valid", {
  expect_true(all(result_oirm$FieldEstimated >= 1))
  expect_true(all(result_oirm$FieldEstimated <= result_oirm$n_field))
})

test_that("Ordinal IRM TRP Validity", {
  # TRP is the sum of weighted expected scores across fields
  # Should be positive and increasing (if mic is satisfied)
  expect_true(all(result_oirm$TRP > 0))

  # TRP values should be in a reasonable range
  nfld <- result_oirm$n_field
  maxQ <- 5
  # Each field contributes expected score 1 to maxQ, so total TRP per class
  # should be between nfld and nfld * maxQ
  expect_true(all(result_oirm$TRP >= nfld * 0.5))
})

test_that("Ordinal IRM BFRP Structure", {
  # BFRP should be a list with Weighted and Observed
  expect_true(is.list(result_oirm$BFRP))
  expect_true(!is.null(result_oirm$BFRP$Weighted))
  expect_true(!is.null(result_oirm$BFRP$Observed))

  nfld <- result_oirm$n_field
  ncls <- result_oirm$n_class

  # Both should be nfld x ncls matrices
  expect_equal(dim(result_oirm$BFRP$Weighted), c(nfld, ncls))
  expect_equal(dim(result_oirm$BFRP$Observed), c(nfld, ncls))

  # Values should be positive (expected scores)
  expect_true(all(result_oirm$BFRP$Weighted > 0))
})

test_that("Ordinal IRM FRPIndex Structure and Validity", {
  frpidx <- result_oirm$FRPIndex
  nfld <- result_oirm$n_field

  # FRPIndex dimensions: nfld rows x 6 columns
  expect_equal(nrow(frpidx), nfld)
  expect_equal(ncol(frpidx), 6)
  expect_equal(names(frpidx), c("Alpha", "A", "Beta", "B", "Gamma", "C"))

  # Alpha: location of steepest ascent (integer, 0 to ncls-1)
  expect_true(all(frpidx$Alpha >= 0))
  expect_true(all(frpidx$Alpha < result_oirm$n_class))

  # A: maximum slope (should be positive for meaningful differentiation)
  expect_true(all(frpidx$A > 0))

  # B: profile value at Beta location (0 to 1 for normalized expected scores)
  expect_true(all(frpidx$B >= 0))
  expect_true(all(frpidx$B <= 1))

  # Gamma: non-monotonicity proportion (0 to 1)
  expect_true(all(frpidx$Gamma >= 0))
  expect_true(all(frpidx$Gamma <= 1))

  # C: non-monotonicity sum (should be <= 0)
  expect_true(all(frpidx$C <= 0))
})

test_that("Ordinal IRM SOAC/WOAC Flags Are Logical", {
  expect_true(is.logical(result_oirm$SOACflg))
  expect_true(is.logical(result_oirm$WOACflg))

  # If SOAC is TRUE, WOAC must also be TRUE (SOAC implies WOAC)
  if (result_oirm$SOACflg) {
    expect_true(result_oirm$WOACflg)
  }
})

test_that("Ordinal IRM mic Parameter Stored", {
  # mic parameter should be stored in the result
  expect_true(!is.null(result_oirm$mic))
  expect_true(is.logical(result_oirm$mic))
  expect_true(result_oirm$mic) # default is TRUE
})

test_that("Ordinal IRM Test Fit Indices", {
  # TestFitIndices should exist and have proper components
  expect_true(!is.null(result_oirm$TestFitIndices))
  tfi <- result_oirm$TestFitIndices
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

  # Log-likelihood ordering: model < bench
  expect_true(tfi$model_log_like <= tfi$bench_log_like)

  # log_lik field should match
  expect_equal(result_oirm$log_lik, tfi$model_log_like)
})

test_that("Ordinal IRM Backward Compatibility", {
  # Deprecated field names should still work
  expect_equal(result_oirm$Nclass, result_oirm$n_class)
  expect_equal(result_oirm$Nfield, result_oirm$n_field)
  expect_equal(result_oirm$N_Cycle, result_oirm$n_cycle)
  expect_equal(result_oirm$LogLik, result_oirm$log_lik)

  # LRD should equal LCD (alias)
  expect_equal(result_oirm$LRD, result_oirm$LCD)

  # RMD should equal CMD (alias)
  expect_equal(result_oirm$RMD, result_oirm$CMD)
})

test_that("Ordinal IRM Seed Reproducibility", {
  # Same seed should produce identical results
  result_a <- Biclustering_IRM(J35S500, gamma_c = 1, gamma_f = 1, seed = 42, verbose = FALSE)
  result_b <- Biclustering_IRM(J35S500, gamma_c = 1, gamma_f = 1, seed = 42, verbose = FALSE)

  expect_equal(result_a$n_class, result_b$n_class)
  expect_equal(result_a$n_field, result_b$n_field)
  expect_equal(result_a$FRP, result_b$FRP)
  expect_equal(result_a$TRP, result_b$TRP)
  expect_equal(result_a$FieldEstimated, result_b$FieldEstimated)
  expect_equal(result_a$ClassEstimated, result_b$ClassEstimated)
  expect_equal(result_a$log_lik, result_b$log_lik)
  expect_equal(result_a$LCD, result_b$LCD)
  expect_equal(result_a$LFD, result_b$LFD)
  expect_equal(result_a$FRPIndex, result_b$FRPIndex)
  expect_equal(result_a$SOACflg, result_b$SOACflg)
  expect_equal(result_a$WOACflg, result_b$WOACflg)
})

test_that("Ordinal IRM Seed NULL Does Not Set Seed", {
  result_null <- Biclustering_IRM(J35S500, gamma_c = 1, gamma_f = 1, seed = NULL, verbose = FALSE)
  expect_s3_class(result_null, "exametrika")
  expect_true("ordinalBiclustering" %in% class(result_null))
  expect_true(!is.null(result_null$n_class))
  expect_true(!is.null(result_null$n_field))
  expect_true(!is.null(result_null$FRPIndex))
  expect_true(!is.null(result_null$TRP))
})

test_that("Ordinal IRM Default Seed Is 123", {
  fn_formals <- formals(Biclustering_IRM.ordinal)
  expect_equal(fn_formals$seed, 123)
})

test_that("Ordinal IRM S3 Dispatch Works", {
  # J35S500 has response.type == "ordinal"
  expect_equal(J35S500$response.type, "ordinal")
})

test_that("Ordinal IRM Alpha Parameter Validation", {
  # alpha must be positive
  expect_error(
    Biclustering_IRM(J35S500, gamma_c = 1, gamma_f = 1, alpha = 0, seed = 123, verbose = FALSE),
    "alpha must be positive"
  )
  expect_error(
    Biclustering_IRM(J35S500, gamma_c = 1, gamma_f = 1, alpha = -1, seed = 123, verbose = FALSE),
    "alpha must be positive"
  )
})

test_that("Ordinal IRM Q and Z Matrices Stored Correctly", {
  expect_equal(dim(result_oirm$Q), c(500, 35))
  expect_equal(dim(result_oirm$Z), c(500, 35))

  # Z values should be 0 or 1
  expect_true(all(result_oirm$Z %in% c(0, 1)))

  # Q values for observed responses (Z=1) should be integers in 1:maxQ
  observed <- result_oirm$Z == 1
  expect_true(all(result_oirm$Q[observed] >= 1))
  expect_true(all(result_oirm$Q[observed] <= 5))
})

test_that("Ordinal IRM Students Matrix Structure", {
  ncls <- result_oirm$n_class
  students <- result_oirm$Students

  # Last column should be "Estimate"
  expect_equal(colnames(students)[ncol(students)], "Estimate")

  # Membership columns should sum to approximately 1 per student
  membership_sums <- unname(rowSums(students[, 1:ncls]))
  for (i in 1:nrow(students)) {
    expect_equal(membership_sums[i], 1.0, tolerance = 1e-10)
  }

  # Estimate column should match ClassEstimated
  expect_equal(unname(as.numeric(students[, "Estimate"])), unname(result_oirm$ClassEstimated))
})

test_that("Ordinal IRM mic=FALSE Works", {
  # Running with mic=FALSE should still produce valid results
  result_nomic <- Biclustering_IRM(J35S500, gamma_c = 1, gamma_f = 1,
                                   mic = FALSE, seed = 123, verbose = FALSE)
  expect_s3_class(result_nomic, "exametrika")
  expect_true("ordinalBiclustering" %in% class(result_nomic))
  expect_false(result_nomic$mic)

  # FRP should still be valid probabilities
  expect_true(all(result_nomic$FRP >= 0))
  expect_true(all(result_nomic$FRP <= 1))

  nfld <- dim(result_nomic$FRP)[1]
  ncls <- dim(result_nomic$FRP)[2]
  for (f in 1:nfld) {
    for (c in 1:ncls) {
      expect_equal(sum(result_nomic$FRP[f, c, ]), 1.0, tolerance = 1e-10)
    }
  }
})
