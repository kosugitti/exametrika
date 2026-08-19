# LCA cross-validated against Mathematica, and the missing-data path with it
# --------------------------------------------------------------------------
# One of five small fixtures that carry the numeric cross-validation against
# Shojima's Mathematica implementation; see develop/tiny_fixtures_refactor.md
# for why there are five rather than one.
#
# tinyLCA is 150x12 and gets its own dataset because LCA is the one chapter
# where the agreement is exact, and only if no cell of the class reference
# matrix saturates. A saturated cell does not hold an estimate: it holds
# exp(-testlength), the constant added inside the logarithm, and the two
# implementations then differ in the fourth decimal of the log-likelihood.
# LCA needs neither a field structure nor an order, so the items are split into
# three groups and class k is good at group k. Every cell lands in [0.03, 0.97],
# nothing saturates, and it converges in 7 cycles against the 140 the first
# attempt took.

tiny_lca <- dataFormat(read.csv(
  test_path("fixtures", "tiny_data", "tinyLCA.csv"),
  check.names = FALSE
))
lca_fit <- LCA(tiny_lca, ncls = 3, verbose = FALSE)
lca_test <- load_ref("TinyLCA_Test.csv")
lca_item <- load_ref("TinyLCA_Item.csv")
lca_class <- load_ref("TinyLCA_Class.csv")
lca_key <- function(k) as.numeric(lca_test[lca_test[[1]] == k, 2])

test_that("tiny LCA matches Mathematica's log-likelihood and cycle count", {
  # asserted tightly on purpose: the measured difference is 0
  expect_equal(lca_fit$TestFitIndices$model_log_like,
    lca_key("Log-Likelihood(Analysis Model)"),
    tolerance = 1e-12
  )
  expect_equal(lca_fit$n_cycle, as.integer(lca_key("N of EM Cycles")))
})

test_that("tiny LCA matches Mathematica's class reference matrix", {
  expect_matrix_equal(
    lca_fit$IRP, lca_item[, grep("^IRP ", names(lca_item))],
    tolerance = 1e-12
  )
})

test_that("tiny LCA matches Mathematica's class distributions", {
  expect_numeric_equal(lca_fit$TRP, lca_class[1, -1], tolerance = 1e-12)
  expect_numeric_equal(lca_fit$LCD, lca_class[2, -1], tolerance = 1e-12)
  expect_numeric_equal(lca_fit$CMD, lca_class[3, -1], tolerance = 1e-10)
})

test_that("the class reference matrix stays away from the boundary", {
  # the property the fixture was built for. If a future edit lets a cell
  # saturate, the assertions above stop being exact and this says why.
  irp <- as.matrix(lca_fit$IRP)
  expect_gt(min(irp), 1e-3)
  expect_lt(max(irp), 1 - 1e-3)
})

# --- the missing-data path -------------------------------------------------
# tinyMissing is tinyLCA with 5% of the responses removed. With responses
# missing the two implementations settle on different local optima, so the
# fitted numbers cannot be compared. What they still agree on exactly are the
# quantities computed from the observed data under the same Z matrix -- the
# per-item respondent counts and correct response rates, and the benchmark and
# null log-likelihoods. That is the missing-data handling itself.

tiny_missing <- dataFormat(
  read.csv(test_path("fixtures", "tiny_data", "tinyMissing.csv"), check.names = FALSE),
  na = -99
)
miss_test <- load_ref("TinyMissing_Test.csv")
miss_item <- load_ref("TinyMissing_Item.csv")
miss_key <- function(k) as.numeric(miss_test[miss_test[[1]] == k, 2])

test_that("tiny LCA counts respondents and correct responses around the gaps", {
  expect_equal(
    unname(colSums(tiny_missing$Z)),
    as.numeric(miss_item[["Number of Respondents"]])
  )
  expect_equal(as.numeric(crr(tiny_missing)),
    as.numeric(miss_item[["Correct Response Rate"]]),
    tolerance = 1e-12
  )
})

test_that("tiny LCA matches the benchmark and null models with missing data", {
  fit <- LCA(tiny_missing, ncls = 3, verbose = FALSE)
  expect_equal(fit$TestFitIndices$bench_log_like,
    miss_key("Log-Likelihood(Benchmark Model)"),
    tolerance = 1e-10
  )
  expect_equal(fit$TestFitIndices$null_log_like,
    miss_key("Log-Likelihood(Null Model)"),
    tolerance = 1e-10
  )
  expect_equal(as.numeric(fit$TestFitIndices$null_df), miss_key("DF(Null Model)"))
})
