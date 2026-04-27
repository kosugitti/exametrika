library(exametrika)

### DistractorAnalysis Tests
### Data: J21S300 (rated, 21 items, 300 students, 4 categories)

### Setup - run models once and share across tests
result_lra <- LRA(J21S300, nrank = 5, mic = TRUE, verbose = FALSE)
da_lra <- DistractorAnalysis(result_lra)

result_bic <- Biclustering(J21S300,
  ncls = 5, nfld = 3, method = "R",
  maxiter = 300, verbose = FALSE
)
da_bic <- DistractorAnalysis(result_bic)

# === LRA.rated tests =====================================================

test_that("DistractorAnalysis.LRArated Basic Structure", {
  expect_s3_class(da_lra, "DistractorAnalysis")
  expect_true("exametrika" %in% class(da_lra))

  expect_true(!is.null(da_lra$freq_table))
  expect_true(!is.null(da_lra$prop_table))
  expect_true(!is.null(da_lra$chisq_table))
  expect_true(!is.null(da_lra$pvalue_table))
  expect_true(!is.null(da_lra$cramersv_table))
  expect_true(!is.null(da_lra$CA))
  expect_true(!is.null(da_lra$n_rank))
  expect_true(!is.null(da_lra$maxQ))
  expect_true(!is.null(da_lra$nitems))
  expect_true(!is.null(da_lra$ItemLabel))
  expect_true(!is.null(da_lra$msg))

  # LRA should NOT have field info
  expect_null(da_lra$n_field)
  expect_null(da_lra$FieldEstimated)
  expect_null(da_lra$field_items)
})

test_that("DistractorAnalysis.LRArated Dimensions", {
  expect_equal(da_lra$nitems, 21)
  expect_equal(da_lra$n_rank, 5)
  expect_equal(da_lra$maxQ, 4)

  expect_equal(length(da_lra$freq_table), 21)
  expect_equal(length(da_lra$prop_table), 21)
  expect_equal(dim(da_lra$chisq_table), c(21, 5))
  expect_equal(dim(da_lra$pvalue_table), c(21, 5))
  expect_equal(dim(da_lra$cramersv_table), c(21, 5))
  expect_equal(length(da_lra$CA), 21)
  expect_equal(length(da_lra$ItemLabel), 21)
})

test_that("DistractorAnalysis.LRArated Frequency Table Structure", {
  for (j in 1:da_lra$nitems) {
    mat <- da_lra$freq_table[[j]]
    expect_equal(dim(mat), c(5, 4))
    expect_true(all(mat >= 0))
    expect_true(all(mat == round(mat))) # integers
  }
})

test_that("DistractorAnalysis.LRArated Proportion Table Sums to 1", {
  for (j in 1:da_lra$nitems) {
    row_sums <- unname(rowSums(da_lra$prop_table[[j]]))
    for (r in seq_along(row_sums)) {
      if (sum(da_lra$freq_table[[j]][r, ]) > 0) {
        expect_equal(row_sums[r], 1.0, tolerance = 1e-10)
      }
    }
  }
})

test_that("DistractorAnalysis.LRArated Proportions in Valid Range", {
  for (j in 1:da_lra$nitems) {
    expect_true(all(da_lra$prop_table[[j]] >= 0))
    expect_true(all(da_lra$prop_table[[j]] <= 1))
  }
})

test_that("DistractorAnalysis.LRArated Chi-square Values Are Non-negative", {
  expect_true(all(da_lra$chisq_table >= 0, na.rm = TRUE))
})

test_that("DistractorAnalysis.LRArated P-values in Valid Range", {
  expect_true(all(da_lra$pvalue_table >= 0, na.rm = TRUE))
  expect_true(all(da_lra$pvalue_table <= 1, na.rm = TRUE))
})

test_that("DistractorAnalysis.LRArated Cramer's V in Valid Range", {
  expect_true(all(da_lra$cramersv_table >= 0, na.rm = TRUE))
  expect_true(all(da_lra$cramersv_table <= 1, na.rm = TRUE))
})

test_that("DistractorAnalysis.LRArated CA Values Are Valid", {
  expect_true(all(da_lra$CA >= 1))
  expect_true(all(da_lra$CA <= da_lra$maxQ))
})

# === ratedBiclustering tests =============================================

test_that("DistractorAnalysis.ratedBiclustering Basic Structure", {
  expect_s3_class(da_bic, "DistractorAnalysis")
  expect_true("exametrika" %in% class(da_bic))

  # Should have field info
  expect_true(!is.null(da_bic$n_field))
  expect_true(!is.null(da_bic$FieldEstimated))
  expect_true(!is.null(da_bic$field_items))
})

test_that("DistractorAnalysis.ratedBiclustering Dimensions", {
  expect_equal(da_bic$nitems, 21)
  expect_equal(da_bic$maxQ, 4)

  expect_equal(length(da_bic$freq_table), 21)
  expect_equal(length(da_bic$prop_table), 21)
  expect_equal(dim(da_bic$chisq_table), c(21, da_bic$n_rank))
  expect_equal(dim(da_bic$pvalue_table), c(21, da_bic$n_rank))
  expect_equal(dim(da_bic$cramersv_table), c(21, da_bic$n_rank))
})

test_that("DistractorAnalysis.ratedBiclustering Field Info", {
  nfld <- da_bic$n_field
  expect_true(nfld >= 1)
  expect_equal(length(da_bic$field_items), nfld)
  expect_equal(length(da_bic$FieldEstimated), 21)

  # All items covered by fields
  all_items <- sort(unname(unlist(da_bic$field_items)))
  expect_equal(all_items, 1:21)

  # FieldEstimated values match field_items
  for (f in 1:nfld) {
    expect_true(all(da_bic$FieldEstimated[da_bic$field_items[[f]]] == f))
  }
})

test_that("DistractorAnalysis.ratedBiclustering Frequency Table Structure", {
  ncls <- da_bic$n_rank
  maxQ <- da_bic$maxQ
  for (j in 1:da_bic$nitems) {
    mat <- da_bic$freq_table[[j]]
    expect_equal(dim(mat), c(ncls, maxQ))
    expect_true(all(mat >= 0))
    expect_true(all(mat == round(mat)))
  }
})

test_that("DistractorAnalysis.ratedBiclustering Proportion Table Sums to 1", {
  for (j in 1:da_bic$nitems) {
    row_sums <- unname(rowSums(da_bic$prop_table[[j]]))
    for (r in seq_along(row_sums)) {
      if (sum(da_bic$freq_table[[j]][r, ]) > 0) {
        expect_equal(row_sums[r], 1.0, tolerance = 1e-10)
      }
    }
  }
})

test_that("DistractorAnalysis.ratedBiclustering Statistics in Valid Range", {
  expect_true(all(da_bic$chisq_table >= 0, na.rm = TRUE))
  expect_true(all(da_bic$pvalue_table >= 0, na.rm = TRUE))
  expect_true(all(da_bic$pvalue_table <= 1, na.rm = TRUE))
  expect_true(all(da_bic$cramersv_table >= 0, na.rm = TRUE))
  expect_true(all(da_bic$cramersv_table <= 1, na.rm = TRUE))
})

# === Print filtering tests ===============================================

test_that("DistractorAnalysis print with items filter works", {
  expect_output(print(da_lra, items = 1:3), "Item01")
  expect_output(print(da_lra, items = 1:3), "Item03")
})

test_that("DistractorAnalysis print with ranks filter works", {
  expect_output(print(da_lra, ranks = c(1, 5)), "Rank1")
  expect_output(print(da_lra, ranks = c(1, 5)), "Rank5")
})

test_that("DistractorAnalysis print with both filters works", {
  expect_output(print(da_lra, items = 10, ranks = c(1, 5)), "Item10")
})

test_that("DistractorAnalysis print shows field grouping for Biclustering", {
  expect_output(print(da_bic), "Field Grouping")
  expect_output(print(da_bic), "Field 1")
})

test_that("DistractorAnalysis print does not show field grouping for LRA", {
  output <- capture.output(print(da_lra, items = 1))
  expect_false(any(grepl("Field Grouping", output)))
})

# === Plot tests ==========================================================

test_that("DistractorAnalysis plot runs without error", {
  expect_no_error(plot(da_lra, items = 1))
  expect_no_error(plot(da_bic, items = 1))
})

test_that("DistractorAnalysis plot with ranks filter works", {
  expect_no_error(plot(da_lra, items = 1, ranks = c(1, 5)))
  expect_no_error(plot(da_bic, items = 1:4, ranks = c(1, 3, 5), nc = 2, nr = 2))
})

# === Biclustering_IRM.rated test =========================================

test_that("DistractorAnalysis works with Biclustering_IRM.rated", {
  result_irm <- Biclustering_IRM(J21S300,
    gamma_c = 1, gamma_f = 1,
    seed = 123, verbose = FALSE
  )
  da_irm <- DistractorAnalysis(result_irm)

  expect_s3_class(da_irm, "DistractorAnalysis")
  expect_true(!is.null(da_irm$n_field))
  expect_true(!is.null(da_irm$field_items))
  expect_equal(da_irm$nitems, 21)
  expect_equal(da_irm$maxQ, 4)
  expect_equal(length(da_irm$CA), 21)
})

# === LRA.rated with different nrank ======================================

test_that("DistractorAnalysis adapts to nrank=4 LRA results", {
  result_lra4 <- LRA(J21S300, nrank = 4, mic = TRUE, verbose = FALSE)
  da4 <- DistractorAnalysis(result_lra4)
  expect_equal(da4$n_rank, 4)
  expect_equal(dim(da4$chisq_table), c(21, 4))
  expect_equal(dim(da4$pvalue_table), c(21, 4))
  expect_equal(dim(da4$cramersv_table), c(21, 4))
  for (j in 1:da4$nitems) {
    expect_equal(dim(da4$freq_table[[j]]), c(4, 4))
  }
})
