# Tier 1 smoke checks for polytomous biclustering, the part that runs on CRAN.
# test-polytomous-biclustering.R holds the full-size checks and is skipped there.
# Small slices of the bundled data keep every block well under half a second;
# the assertions are structural, not comparisons against reference numbers.

smoke_ord <- head_rows_dat(J35S500, 120)
smoke_nom <- head_rows_dat(J20S600, 120)
smoke_rat <- head_rows_dat(J21S300, 120)

frp_sums_to_one <- function(fit) {
  return(all(abs(apply(fit$FRP, c(1, 2), sum) - 1) < 1e-8))
}

test_that("ordinal Biclustering runs from the default start", {
  fit <- Biclustering(smoke_ord, ncls = 3, nfld = 3, method = "B", verbose = FALSE)
  expect_s3_class(fit, "ordinalBiclustering")
  expect_equal(dim(fit$FRP)[1:2], c(3, 3))
  expect_true(frp_sums_to_one(fit))
  expect_equal(fit$init, "auto")
  expect_true(fit$init_selected %in% names(fit$init_elbo))
  expect_equal(fit$elbo, max(fit$init_elbo, na.rm = TRUE))
})

test_that("isotonic ordinal Ranklustering satisfies SOAC on the model TRP", {
  fit <- Biclustering(smoke_ord, ncls = 3, nfld = 3, method = "R", nstart = 0, verbose = FALSE)
  expect_equal(fit$estimation, "isotonic")
  expect_true(fit$SOACflg)
  expect_true(fit$WOACflg)
  expect_length(fit$TRP_empirical, 3)
})

test_that("nominal and rated Biclustering run", {
  nom <- suppressMessages(Biclustering(smoke_nom, ncls = 3, nfld = 2, verbose = FALSE))
  expect_s3_class(nom, "nominalBiclustering")
  expect_true(frp_sums_to_one(nom))
  rat <- suppressMessages(Biclustering(smoke_rat, ncls = 3, nfld = 2, verbose = FALSE))
  expect_s3_class(rat, "ratedBiclustering")
  expect_true(all(is.finite(rat$TestFitIndices$model_log_like)))
})

test_that("an empty category keeps its slot in ordinal Biclustering", {
  d <- smoke_ord
  d$Q[d$Q[, 1] == 1, 1] <- 2
  fit <- suppressWarnings(Biclustering(d, ncls = 2, nfld = 2, method = "B", nstart = 0, verbose = FALSE))
  expect_equal(dim(fit$FRP)[3], max(smoke_ord$Q))
})
