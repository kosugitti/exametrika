# LRA (GTM) cross-validated against Mathematica on a tiny generated dataset
# ------------------------------------------------------------------------
# Fourth chapter of the tiny-fixture work; see develop/tiny_fixtures_plan.md.
#
# The data has a monotone rank structure planted in it -- which is what GTM
# assumes -- but only 120 respondents, so sampling noise leaves the empirical
# profiles non-monotone in places. That matters: with a clean monotone sample
# the filter has nothing to smooth, mic = TRUE and mic = FALSE return the same
# numbers, and a test that compares against both is only pretending to
# distinguish them. Here the two reference sets differ by 0.10 in the item
# reference profiles.
#
# Both agree with Mathematica to machine precision: log-likelihood 1.1e-13,
# profiles 5.6e-16, and the same EM cycle counts. LRA is like LCA and unlike
# IRT -- the two implementations run the same EM to the same fixed point.

tiny_lra <- read.csv(
  test_path("fixtures", "tiny_data", "tinyLRA.csv"),
  check.names = FALSE
)
tiny_lra_dat <- dataFormat(tiny_lra, na = -99)

for (mic_on in c(FALSE, TRUE)) {
  local({
    mic <- mic_on
    tag <- sprintf("GTMmic%d", as.integer(mic))
    fit <- LRA(tiny_lra_dat,
      nrank = 4, method = "GTM", mic = mic,
      maxiter = 1000, verbose = FALSE
    )
    ref_test <- load_ref(sprintf("TinyLRA_%s_Test.csv", tag))
    ref_item <- load_ref(sprintf("TinyLRA_%s_Item.csv", tag))
    ref_rank <- load_ref(sprintf("TinyLRA_%s_Rank.csv", tag))
    key <- function(k) as.numeric(ref_test[ref_test[[1]] == k, 2])

    test_that(sprintf("tiny LRA %s reproduces Mathematica's log-likelihood and cycles", tag), {
      expect_equal(fit$TestFitIndices$model_log_like,
        key("Log-Likelihood(Analysis Model)"),
        tolerance = 1e-10
      )
      expect_equal(fit$n_cycle, as.integer(key("N of EM Cycles")))
      expect_true(fit$converge)
    })

    test_that(sprintf("tiny LRA %s reproduces Mathematica's item reference profiles", tag), {
      expect_matrix_equal(
        fit$IRP, ref_item[, grep("^IRP ", names(ref_item))],
        tolerance = 1e-10
      )
    })

    test_that(sprintf("tiny LRA %s reproduces Mathematica's rank distributions", tag), {
      expect_numeric_equal(fit$TRP, ref_rank[1, -1], tolerance = 1e-10)
      expect_numeric_equal(fit$LRD, ref_rank[2, -1], tolerance = 1e-10)
      expect_numeric_equal(fit$RMD, ref_rank[3, -1], tolerance = 1e-10)
    })
  })
}

test_that("the smoothing filter changes the answer on this data", {
  # otherwise the two blocks above would be asserting the same thing twice
  a <- LRA(tiny_lra_dat, nrank = 4, method = "GTM", mic = FALSE, maxiter = 1000, verbose = FALSE)
  b <- LRA(tiny_lra_dat, nrank = 4, method = "GTM", mic = TRUE, maxiter = 1000, verbose = FALSE)
  expect_gt(max(abs(as.matrix(a$IRP) - as.matrix(b$IRP))), 1e-3)
})

test_that("LRA(method = 'GTM') passes maxiter through to the EM loop", {
  # regression: the GTM branch dropped maxiter and always stopped at emclus()'s
  # default of 100, whatever the caller asked for and whatever the help said.
  # Found 2026-08-19 because this dataset needs 113 cycles; J15S500 converged
  # inside 100 and hid it.
  short <- suppressWarnings(
    LRA(tiny_lra_dat, nrank = 4, method = "GTM", mic = FALSE, maxiter = 20, verbose = FALSE)
  )
  expect_equal(short$n_cycle, 20L)
  expect_false(short$converge)
})
