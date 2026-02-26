library(exametrika)

### GridSearch Tests

### Setup - run Biclustering GridSearch once and share across tests
tmp_gs <- dataFormat(J35S515)
result_gs_bic <- GridSearch(tmp_gs,
  max_ncls = 3, max_nfld = 3,
  fun = "Biclustering", index = "BIC", verbose = FALSE
)

test_that("GridSearch Basic Execution (Biclustering)", {
  # Basic structure checks
  expect_s3_class(result_gs_bic, "exametrika")
  expect_true("GridSearch" %in% class(result_gs_bic))

  # Result components exist
  expect_true(!is.null(result_gs_bic$index_matrix))
  expect_true(!is.null(result_gs_bic$optimal_ncls))
  expect_true(!is.null(result_gs_bic$optimal_nfld))
  expect_true(!is.null(result_gs_bic$optimal_result))

  # index_matrix should be a matrix
  expect_true(is.matrix(result_gs_bic$index_matrix))

  # Optimal values should be within search range
  expect_true(result_gs_bic$optimal_ncls >= 2 && result_gs_bic$optimal_ncls <= 3)
  expect_true(result_gs_bic$optimal_nfld >= 2 && result_gs_bic$optimal_nfld <= 3)
})

test_that("GridSearch Optimal Result Has Fit Indices", {
  # Optimal result should have TestFitIndices
  opt <- result_gs_bic$optimal_result
  expect_true(!is.null(opt$TestFitIndices))
  expect_true(!is.null(opt$TestFitIndices$BIC))

  # BIC should be finite
  expect_true(is.finite(opt$TestFitIndices$BIC))
})

test_that("GridSearch LCA Method", {
  tmp <- dataFormat(J15S500)
  result <- GridSearch(tmp,
    max_ncls = 4,
    fun = "LCA", index = "BIC", verbose = FALSE
  )

  # Basic structure
  expect_s3_class(result, "exametrika")
  expect_true("GridSearch" %in% class(result))

  # LCA returns index_vec instead of index_matrix (no nfld dimension)
  expect_true(!is.null(result$index_vec))
  expect_true(!is.null(result$optimal_ncls))
  expect_true(!is.null(result$optimal_result))

  # Optimal ncls should be within range
  expect_true(result$optimal_ncls >= 2 && result$optimal_ncls <= 4)
})
