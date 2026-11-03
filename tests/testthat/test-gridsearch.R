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

test_that("GridSearch records index and fun, and plot() draws without error", {
  expect_equal(result_gs_bic$index, "BIC")
  expect_equal(result_gs_bic$fun, "Biclustering")
  expect_true(is.matrix(result_gs_bic$empty_matrix))
  expect_equal(dim(result_gs_bic$empty_matrix), dim(result_gs_bic$index_matrix))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(result_gs_bic))
  expect_no_error(plot(result_gs_bic, style = "persp"))
  expect_no_error(plot(result_gs_bic, type = "Fit"))
  expect_error(plot(result_gs_bic, type = "IRP"))
})

test_that("GridSearch skips settings that left a class empty (skip_empty)", {
  # One response pattern dominates (as in a symptom checklist where most
  # respondents answer no to everything): with the classes sharing a uniform
  # prior, LCA with three or more classes spends several classes on that
  # pattern, so some classes end up with no member.
  set.seed(1)
  U <- rbind(
    matrix(0L, nrow = 500, ncol = 6),
    matrix(rbinom(80 * 6, 1, 0.3), nrow = 80, ncol = 6),
    matrix(rbinom(20 * 6, 1, 0.7), nrow = 20, ncol = 6)
  )
  colnames(U) <- paste0("Item", 1:6)
  dat <- dataFormat(U)
  expect_true(all(LCA(dat, ncls = 2)$LCD > 0))
  expect_true(any(LCA(dat, ncls = 3)$LCD == 0))

  res <- suppressMessages(
    GridSearch(dat, max_ncls = 5, fun = "LCA", index = "BIC", verbose = FALSE)
  )
  expect_true(is.logical(res$empty_vec))
  expect_equal(names(res$empty_vec), names(res$index_vec))
  expect_true(length(res$empty_settings) > 0)
  # every flagged setting really has an empty class, and the optimum is not one
  for (setting in res$empty_settings) {
    fit <- LCA(dat, ncls = setting$ncls)
    expect_true(any(fit$LCD == 0))
  }
  expect_false(res$empty_vec[paste0("ncls", res$optimal_ncls)])
  expect_true(all(res$optimal_result$LCD > 0))
  # the fit index is still recorded for the flagged settings
  expect_true(all(is.finite(res$index_vec[res$empty_vec])))

  # without the guard the plain minimum wins, which here is a flagged setting
  res_all <- suppressMessages(
    GridSearch(dat,
      max_ncls = 5, fun = "LCA", index = "BIC", verbose = FALSE,
      skip_empty = FALSE
    )
  )
  expect_equal(unname(res_all$optimal_ncls), unname(which.min(res_all$index_vec)) + 1)
  expect_true(res_all$empty_vec[paste0("ncls", res_all$optimal_ncls)])

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(res))
  expect_output(print(res), "left a class, rank or field empty")
})

test_that("GridSearch accepts index = 'BIC_elbo' and the fits carry it", {
  tmp <- dataFormat(J15S500)
  res <- suppressMessages(GridSearch(tmp, max_ncls = 3, fun = "LCA", index = "BIC_elbo"))
  expect_equal(res$index, "BIC_elbo")
  f <- LCA(tmp, ncls = 2)
  expect_equal(unname(res$index_vec["ncls2"]), f$BIC_elbo)
  expect_equal(f$BIC_elbo, -2 * f$elbo + 2 * f$testlength * log(f$nobs))
  fe <- LCA(tmp, ncls = 2, mixing = "estimate")
  expect_equal(fe$BIC_elbo, -2 * fe$elbo + (2 * fe$testlength + 1) * log(fe$nobs))
  gb <- suppressMessages(GridSearch(J35S515, max_ncls = 3, max_nfld = 3, index = "BIC_elbo"))
  b <- Biclustering(J35S515, ncls = 2, nfld = 2)
  expect_equal(unname(gb$index_matrix["ncls 2", "nfld 2"]), b$BIC_elbo)
  expect_equal(b$BIC_elbo, -2 * b$elbo + 4 * log(b$nobs * b$testlength))
})
