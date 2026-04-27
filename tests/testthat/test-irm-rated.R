library(exametrika)

### Rated IRM (Biclustering_IRM.rated) Tests
### Data: J21S300 (rated, 21 items, 300 students, 4 categories)

### Setup - run model once and share across tests
result_rirm <- Biclustering_IRM(J21S300, gamma_c = 1, gamma_f = 1, seed = 123, verbose = FALSE)

test_that("Rated IRM Basic Execution", {
  # Basic structure checks
  expect_s3_class(result_rirm, "exametrika")
  expect_true("ratedBiclustering" %in% class(result_rirm))

  # Result components exist
  expect_true(!is.null(result_rirm$n_class))
  expect_true(!is.null(result_rirm$n_field))
  expect_true(!is.null(result_rirm$n_cycle))
  expect_true(!is.null(result_rirm$FRP))
  expect_true(!is.null(result_rirm$FieldFRP))
  expect_true(!is.null(result_rirm$quasiFRP))
  expect_true(!is.null(result_rirm$FRPIndex))
  expect_true(!is.null(result_rirm$TRP))
  expect_true(!is.null(result_rirm$LCD))
  expect_true(!is.null(result_rirm$LFD))
  expect_true(!is.null(result_rirm$FieldEstimated))
  expect_true(!is.null(result_rirm$ClassEstimated))
  expect_true(!is.null(result_rirm$FieldMembership))
  expect_true(!is.null(result_rirm$ClassMembership))
  expect_true(!is.null(result_rirm$Students))
  expect_true(!is.null(result_rirm$FieldAnalysis))
  expect_true(!is.null(result_rirm$Q))
  expect_true(!is.null(result_rirm$U))
  expect_true(!is.null(result_rirm$Z))
  expect_true(!is.null(result_rirm$SOACflg))
  expect_true(!is.null(result_rirm$WOACflg))
})

test_that("Rated IRM Dimensions Consistency", {
  nfld <- result_rirm$n_field
  ncls <- result_rirm$n_class
  nitems <- result_rirm$testlength
  nobs <- result_rirm$nobs

  # Check basic dimensions
  expect_equal(nitems, 21)
  expect_equal(nobs, 300)
  expect_true(nfld >= 1 && nfld <= nitems)
  expect_true(ncls >= 2)

  # FRP: 3D array (nfld x ncls x maxQ)
  expect_equal(length(dim(result_rirm$FRP)), 3)
  expect_equal(dim(result_rirm$FRP)[1], nfld)
  expect_equal(dim(result_rirm$FRP)[2], ncls)
  expect_equal(dim(result_rirm$FRP)[3], 4) # J21S300 has 4 categories

  # FieldFRP: nfld x ncls
  expect_equal(dim(result_rirm$FieldFRP), c(nfld, ncls))

  # quasiFRP: nitems x ncls
  expect_equal(dim(result_rirm$quasiFRP), c(nitems, ncls))

  # TRP
  expect_equal(length(result_rirm$TRP), ncls)

  # LCD and LFD
  expect_equal(length(result_rirm$LCD), ncls)
  expect_equal(length(result_rirm$LFD), nfld)

  # Field/Class Estimated
  expect_equal(length(result_rirm$FieldEstimated), nitems)
  expect_equal(length(result_rirm$ClassEstimated), nobs)

  # Field/Class Membership
  expect_equal(nrow(result_rirm$FieldMembership), nitems)
  expect_equal(ncol(result_rirm$FieldMembership), nfld)
  expect_equal(nrow(result_rirm$ClassMembership), nobs)
  expect_equal(ncol(result_rirm$ClassMembership), ncls)

  # Students matrix: membership + Estimate + Rank-Up Odds + Rank-Down Odds
  expect_equal(nrow(result_rirm$Students), nobs)
  expect_equal(ncol(result_rirm$Students), ncls + 3)
})

test_that("Rated IRM FRP Probabilities Sum to 1", {
  nfld <- dim(result_rirm$FRP)[1]
  ncls <- dim(result_rirm$FRP)[2]
  for (f in 1:nfld) {
    for (c in 1:ncls) {
      expect_equal(sum(result_rirm$FRP[f, c, ]), 1.0, tolerance = 1e-10)
    }
  }
})

test_that("Rated IRM FRP Values Are Valid Probabilities", {
  expect_true(all(result_rirm$FRP >= 0))
  expect_true(all(result_rirm$FRP <= 1))
})

test_that("Rated IRM FieldFRP Values Are Valid", {
  expect_true(all(result_rirm$FieldFRP >= 0, na.rm = TRUE))
  expect_true(all(result_rirm$FieldFRP <= 1, na.rm = TRUE))
})

test_that("Rated IRM quasiFRP Values Are Valid", {
  expect_true(all(result_rirm$quasiFRP >= 0))
  expect_true(all(result_rirm$quasiFRP <= 1))
})

test_that("Rated IRM Distribution Sums", {
  expect_equal(sum(result_rirm$LCD), result_rirm$nobs)
  expect_equal(sum(result_rirm$LFD), result_rirm$testlength)
})

test_that("Rated IRM ClassMembership Rows Sum to 1", {
  row_sums <- unname(rowSums(result_rirm$ClassMembership))
  for (i in seq_along(row_sums)) {
    expect_equal(row_sums[i], 1.0, tolerance = 1e-10)
  }
})

test_that("Rated IRM FieldMembership Rows Sum to 1", {
  row_sums <- unname(rowSums(result_rirm$FieldMembership))
  for (i in seq_along(row_sums)) {
    expect_equal(row_sums[i], 1.0, tolerance = 1e-10)
  }
})

test_that("Rated IRM ClassEstimated Values Are Valid", {
  expect_true(all(result_rirm$ClassEstimated >= 1))
  expect_true(all(result_rirm$ClassEstimated <= result_rirm$n_class))
})

test_that("Rated IRM FieldEstimated Values Are Valid", {
  expect_true(all(result_rirm$FieldEstimated >= 1))
  expect_true(all(result_rirm$FieldEstimated <= result_rirm$n_field))
})

test_that("Rated IRM Two-Layer Fit Indices", {
  # Layer 1: Binary fit indices (default)
  tfi <- result_rirm$TestFitIndices
  expect_s3_class(tfi, "exametrika")
  expect_true("ModelFit" %in% class(tfi))
  expect_true(!is.null(tfi$model_log_like))
  expect_true(!is.null(tfi$AIC))
  expect_true(!is.null(tfi$BIC))
  expect_true(!is.null(tfi$CFI))
  expect_true(!is.null(tfi$RMSEA))
  expect_true(tfi$model_log_like < 0)

  # Binary layer: CFI and RMSEA should NOT be NA
  expect_false(is.na(tfi$CFI))
  expect_false(is.na(tfi$RMSEA))

  # Layer 2: Nominal fit indices
  tfi_nom <- result_rirm$TestFitIndices_nominal
  expect_s3_class(tfi_nom, "exametrika")
  expect_true("ModelFit" %in% class(tfi_nom))
  expect_true(!is.null(tfi_nom$AIC))
  expect_true(!is.null(tfi_nom$BIC))
  expect_true(tfi_nom$model_log_like < 0)

  # Nominal layer: CFI and RMSEA should be NA
  expect_true(is.na(tfi_nom$CFI))
  expect_true(is.na(tfi_nom$RMSEA))
  expect_true(is.na(tfi_nom$bench_log_like))

  # log_lik fields should match
  expect_equal(result_rirm$log_lik, tfi$model_log_like)
  expect_equal(result_rirm$log_lik_nominal, tfi_nom$model_log_like)
})

test_that("Rated IRM FRPIndex Structure and Validity", {
  frpidx <- result_rirm$FRPIndex
  nfld <- result_rirm$n_field

  expect_true(nrow(frpidx) >= 1)
  expect_equal(ncol(frpidx), 6)
  expect_equal(names(frpidx), c("Alpha", "A", "Beta", "B", "Gamma", "C"))

  expect_true(all(frpidx$Alpha >= 0))
  expect_true(all(frpidx$A > 0))
  expect_true(all(frpidx$B >= 0))
  expect_true(all(frpidx$B <= 1))
  expect_true(all(frpidx$Gamma >= 0))
  expect_true(all(frpidx$Gamma <= 1))
})

test_that("Rated IRM SOAC/WOAC Flags Are Logical", {
  expect_true(is.logical(result_rirm$SOACflg))
  expect_true(is.logical(result_rirm$WOACflg))

  if (result_rirm$SOACflg) {
    expect_true(result_rirm$WOACflg)
  }
})

test_that("Rated IRM Backward Compatibility", {
  expect_equal(result_rirm$Nclass, result_rirm$n_class)
  expect_equal(result_rirm$Nfield, result_rirm$n_field)
  expect_equal(result_rirm$N_Cycle, result_rirm$n_cycle)
  expect_equal(result_rirm$LogLik, result_rirm$log_lik)

  expect_equal(result_rirm$LRD, result_rirm$LCD)
  expect_equal(result_rirm$RMD, result_rirm$CMD)
})

test_that("Rated IRM Seed Reproducibility", {
  result_a <- Biclustering_IRM(J21S300, gamma_c = 1, gamma_f = 1, seed = 42, verbose = FALSE)
  result_b <- Biclustering_IRM(J21S300, gamma_c = 1, gamma_f = 1, seed = 42, verbose = FALSE)

  expect_equal(result_a$n_class, result_b$n_class)
  expect_equal(result_a$n_field, result_b$n_field)
  expect_equal(result_a$FRP, result_b$FRP)
  expect_equal(result_a$FieldFRP, result_b$FieldFRP)
  expect_equal(result_a$TRP, result_b$TRP)
  expect_equal(result_a$FieldEstimated, result_b$FieldEstimated)
  expect_equal(result_a$ClassEstimated, result_b$ClassEstimated)
  expect_equal(result_a$log_lik, result_b$log_lik)
  expect_equal(result_a$log_lik_nominal, result_b$log_lik_nominal)
  expect_equal(result_a$LCD, result_b$LCD)
  expect_equal(result_a$LFD, result_b$LFD)
  expect_equal(result_a$SOACflg, result_b$SOACflg)
  expect_equal(result_a$WOACflg, result_b$WOACflg)
})

test_that("Rated IRM Seed NULL Does Not Set Seed", {
  result_null <- Biclustering_IRM(J21S300, gamma_c = 1, gamma_f = 1, seed = NULL, verbose = FALSE)
  expect_s3_class(result_null, "exametrika")
  expect_true("ratedBiclustering" %in% class(result_null))
  expect_true(!is.null(result_null$n_class))
  expect_true(!is.null(result_null$n_field))
})

test_that("Rated IRM Default Seed Is 123", {
  fn_formals <- formals(Biclustering_IRM.rated)
  expect_equal(fn_formals$seed, 123)
})

test_that("Rated IRM S3 Dispatch Works", {
  expect_equal(J21S300$response.type, "rated")
})

test_that("Rated IRM Q U Z Matrices Stored Correctly", {
  expect_equal(dim(result_rirm$Q), c(300, 21))
  expect_equal(dim(result_rirm$U), c(300, 21))
  expect_equal(dim(result_rirm$Z), c(300, 21))

  # Z values should be 0 or 1
  expect_true(all(result_rirm$Z %in% c(0, 1)))

  # Q values for observed responses should be in 1:4
  observed <- result_rirm$Z == 1
  expect_true(all(result_rirm$Q[observed] >= 1))
  expect_true(all(result_rirm$Q[observed] <= 4))

  # U values should be 0 or 1 (binary correct/incorrect)
  expect_true(all(result_rirm$U %in% c(0, 1)))
})

test_that("Rated IRM Students Matrix Structure", {
  ncls <- result_rirm$n_class
  students <- result_rirm$Students

  # Should have: membership columns + Estimate + Rank-Up Odds + Rank-Down Odds
  expect_equal(colnames(students)[ncls + 1], "Estimate")
  expect_equal(colnames(students)[ncls + 2], "Rank-Up Odds")
  expect_equal(colnames(students)[ncls + 3], "Rank-Down Odds")

  # Membership columns should sum to approximately 1 per student
  membership_sums <- unname(rowSums(students[, 1:ncls]))
  for (i in 1:nrow(students)) {
    expect_equal(membership_sums[i], 1.0, tolerance = 1e-10)
  }

  # Estimate column should match ClassEstimated
  expect_equal(unname(as.numeric(students[, "Estimate"])), unname(result_rirm$ClassEstimated))
})

test_that("Rated IRM FieldAnalysis Structure", {
  fa <- result_rirm$FieldAnalysis
  nfld <- result_rirm$n_field

  expect_equal(nrow(fa), result_rirm$testlength)
  expect_equal(ncol(fa), 2 + nfld) # CRR + LFE + Field1..FieldN

  # CRR should be between 0 and 1
  expect_true(all(fa[, "CRR"] >= 0))
  expect_true(all(fa[, "CRR"] <= 1))

  # LFE should be valid field indices
  expect_true(all(fa[, "LFE"] >= 1))
  expect_true(all(fa[, "LFE"] <= nfld))
})

test_that("Rated IRM print method runs without error", {
  expect_no_error(capture.output(print(result_rirm)))
})
