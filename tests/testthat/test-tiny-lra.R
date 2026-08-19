# LRA (GTM) and the monotonicity filter, cross-validated against Mathematica
# --------------------------------------------------------------------------
# tinyRough (120x10) gets its own dataset for the opposite reason to the others.
# `mic` is not a property of the data -- it is an argument, and the test fits
# the same frame twice, once with it and once without. But if the sample is
# already monotone across ranks the filter has nothing to smooth, both fits
# return the same numbers, and comparing against both proves nothing. That is
# what a first attempt did. So the true structure here is monotone while the
# sample is small enough that noise leaves the empirical profiles bumpy; the
# generator picks the draw with the largest violation.
#
# Both fits agree with Mathematica to machine precision -- 5.6e-16 and 6.1e-16
# in the item reference profiles, with the same cycle counts. LRA is like LCA
# and unlike IRT: the two implementations run the same EM to the same fixed
# point.

tiny_rough <- dataFormat(read.csv(
  test_path("fixtures", "tiny_data", "tinyRough.csv"),
  check.names = FALSE
))

for (mic_on in c(FALSE, TRUE)) {
  local({
    mic <- mic_on
    tag <- sprintf("mic%d", as.integer(mic))
    label <- sprintf("mic = %s", mic)
    fit <- LRA(tiny_rough,
      nrank = 4, method = "GTM", mic = mic,
      maxiter = 1000, verbose = FALSE
    )
    ref_test <- load_ref(sprintf("TinyLRA%s_Test.csv", tag))
    ref_item <- load_ref(sprintf("TinyLRA%s_Item.csv", tag))
    ref_rank <- load_ref(sprintf("TinyLRA%s_Rank.csv", tag))
    key <- function(k) as.numeric(ref_test[ref_test[[1]] == k, 2])

    test_that(sprintf("tiny LRA %s matches the log-likelihood and cycles", label), {
      expect_equal(fit$TestFitIndices$model_log_like,
        key("Log-Likelihood(Analysis Model)"),
        tolerance = 1e-10
      )
      expect_equal(fit$n_cycle, as.integer(key("N of EM Cycles")))
      expect_true(fit$converge)
    })

    test_that(sprintf("tiny LRA %s matches the item reference profiles", label), {
      expect_matrix_equal(fit$IRP, ref_item[, grep("^IRP ", names(ref_item))],
        tolerance = 1e-10
      )
    })

    test_that(sprintf("tiny LRA %s matches the rank distributions", label), {
      expect_numeric_equal(fit$TRP, ref_rank[1, -1], tolerance = 1e-10)
      expect_numeric_equal(fit$LRD, ref_rank[2, -1], tolerance = 1e-10)
      expect_numeric_equal(fit$RMD, ref_rank[3, -1], tolerance = 1e-10)
    })
  })
}

test_that("the smoothing filter changes the answer on this data", {
  # without this the two blocks above would assert the same thing twice
  a <- LRA(tiny_rough, nrank = 4, method = "GTM", mic = FALSE, maxiter = 1000, verbose = FALSE)
  b <- LRA(tiny_rough, nrank = 4, method = "GTM", mic = TRUE, maxiter = 1000, verbose = FALSE)
  expect_gt(max(abs(as.matrix(a$IRP) - as.matrix(b$IRP))), 1e-3)
})

test_that("LRA(method = 'GTM') passes maxiter through to the EM loop", {
  # regression: the GTM branch dropped maxiter and always stopped at the EM
  # loop's own default of 100, whatever the caller asked for and whatever the
  # help said. This dataset needs 113 cycles unsmoothed, which is how it showed.
  short <- suppressWarnings(
    LRA(tiny_rough, nrank = 4, method = "GTM", mic = FALSE, maxiter = 20, verbose = FALSE)
  )
  expect_equal(short$n_cycle, 20L)
  expect_false(short$converge)
})
