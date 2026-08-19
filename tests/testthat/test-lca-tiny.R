# LCA cross-validated against Mathematica on a tiny generated dataset
# ------------------------------------------------------------------
# The reference fixtures in this file come from the same Mathematica
# implementation as the Chapter05LCA_* ones, run on a 120x8 frame instead of
# J15S500 (500x15). The point is to keep the numeric cross-validation while
# paying a fraction of the time: CRAN's Windows build ships the reference BLAS
# and its 10-minute checktime limit is what 1.13.0 was rejected on.
#
# The data is generated with a fixed seed by
# develop/mtmk15forVer13/make_tiny_data.R and stored as a fixture, so R and
# Mathematica read the same bytes. It carries no missing values on purpose:
# at this size the likelihood surface is shallow enough that the two
# implementations settle on different local optima once responses go missing
# (measured 2026-08-19: log-likelihoods 76 apart with 5% missing, and agreeing
# to 1e-13 without). The benchmark and null log-likelihoods agree either way,
# so the missing-data handling itself is covered by the full-size fixtures.

tiny <- read.csv(
  test_path("fixtures", "tiny_data", "tinyLCA.csv"),
  check.names = FALSE
)
tiny_test <- load_ref("TinyLCA_Test.csv")
tiny_class <- load_ref("TinyLCA_Class.csv")
tiny_item <- load_ref("TinyLCA_Item.csv")

tiny_fit <- LCA(dataFormat(tiny, na = -99), ncls = 3, verbose = FALSE)

test_that("tiny LCA reproduces Mathematica's test-level fit indices", {
  # rows 14:29 of the Test sheet, in the order TestFitIndices reports them
  expect <- as.numeric(tiny_test[14:29, 2])
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  expect_equal(as.numeric(tiny_fit$TestFitIndices), expect, tolerance = 1e-4)
})

test_that("tiny LCA reproduces Mathematica's log-likelihood to machine precision", {
  # the headline number: a tolerance loose enough to hide a real disagreement
  # would defeat the purpose of cross-validating at all
  expect_equal(
    tiny_fit$TestFitIndices$model_log_like,
    as.numeric(tiny_test[tiny_test[[1]] == "Log-Likelihood(Analysis Model)", 2]),
    tolerance = 1e-10
  )
  expect_equal(tiny_fit$n_cycle, as.integer(as.numeric(
    tiny_test[tiny_test[[1]] == "N of EM Cycles", 2]
  )))
})

test_that("tiny LCA reproduces Mathematica's class reference matrix", {
  # IRP columns of the Item sheet: one column per class, one row per item
  irp <- grep("^IRP ", names(tiny_item), value = TRUE)
  expect_matrix_equal(tiny_fit$IRP, tiny_item[, irp])
})

test_that("tiny LCA reproduces Mathematica's class distributions", {
  expect_numeric_equal(tiny_fit$TRP, tiny_class[1, -1])
  expect_numeric_equal(tiny_fit$LCD, tiny_class[2, -1])
  expect_numeric_equal(tiny_fit$CMD, tiny_class[3, -1])
})
