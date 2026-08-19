# Biclustering / Ranklustering cross-validated against Mathematica
# ----------------------------------------------------------------
# Fifth chapter of the tiny-fixture work; see develop/tiny_fixtures_plan.md.
# This one does not reach machine precision, and the reason is worth stating
# because it is a property of the chapter rather than of the dataset.
#
# Both implementations add a small constant inside the logarithms to keep them
# finite. Mathematica uses exp(-testlength); this package uses
# .Machine$double.eps, which does not shrink as the test gets longer. On the
# 35-item J35S515 the two are the same order (6.3e-16 against 2.2e-16) and the
# log-likelihoods agree to 3.4e-06. On twelve items exp(-12) is 6.1e-06 -- ten
# orders larger -- and the model log-likelihood comes out 2.1e-02 apart. Adding
# items would close the gap and defeat the purpose of a tiny fixture.
#
# So the assertions are chosen by what the constant can reach. It enters the
# analysis-model likelihood; it does not enter the benchmark and null models,
# which match exactly, nor does it move the field reference profiles by more
# than 4.6e-06. The EM cycle counts agree exactly, which is the sharper check
# on the iteration itself: a change in the E- or M-step would move them.
#
# The reference values come from GTM. Ranklustering's default estimation became
# "isotonic" in 2.0.0, so the comparison passes estimation = "GTM" explicitly --
# comparing the isotonic fit against a GTM reference is what a first attempt did,
# and it disagreed by 6.3 in log-likelihood.

tiny_bicl <- read.csv(
  test_path("fixtures", "tiny_data", "tinyBicl.csv"),
  check.names = FALSE
)
# no na = argument: this frame has no missing values, and passing one changes
# how dataFormat reads it
tiny_bicl_dat <- dataFormat(tiny_bicl)

bicl_specs <- list(
  list(name = "Biclustering", sheet = "Bicluster", args = list(method = "B")),
  list(
    name = "Ranklustering", sheet = "Rankluster",
    args = list(method = "R", estimation = "GTM")
  )
)

for (spec_i in bicl_specs) {
  local({
    spec <- spec_i
    fit <- suppressMessages(do.call(Biclustering, c(
      list(tiny_bicl_dat, ncls = 3, nfld = 3, mic = TRUE, verbose = FALSE),
      spec$args
    )))
    ref_test <- load_ref(sprintf("Tiny%s_Test.csv", spec$name))
    ref_block <- load_ref(sprintf("Tiny%s_%s.csv", spec$name, spec$sheet))
    key <- function(k) as.numeric(ref_test[ref_test[[1]] == k, 2])

    test_that(sprintf("tiny %s takes the same number of EM cycles", spec$name), {
      expect_equal(fit$n_cycle, as.integer(key("N of EM Cycles")))
      expect_true(fit$converge)
    })

    test_that(sprintf("tiny %s reproduces the benchmark and null models exactly", spec$name), {
      # these do not pass through the log-constant
      expect_equal(fit$TestFitIndices$bench_log_like,
        key("Log-Likelihood(Benchmark Model)"),
        tolerance = 1e-12
      )
      expect_equal(fit$TestFitIndices$null_log_like,
        key("Log-Likelihood(Null Model)"),
        tolerance = 1e-12
      )
    })

    test_that(sprintf("tiny %s reproduces the field reference profiles", spec$name), {
      expect_matrix_equal(fit$FRP, ref_block[1:3, 2:4], tolerance = 1e-4)
    })

    test_that(sprintf("tiny %s reproduces the analysis log-likelihood to the constant", spec$name), {
      expect_equal(fit$TestFitIndices$model_log_like,
        key("Log-Likelihood(Analysis Model)"),
        tolerance = 1e-4
      )
    })
  })
}

test_that("Biclustering(maxiter) reaches the EM loop", {
  # regression: the binary branch set maxemt <- 1000 outright, so the argument
  # was accepted and discarded. The polytomous branches always wrote
  # maxemt <- maxiter. Found 2026-08-19.
  short <- suppressWarnings(suppressMessages(
    Biclustering(tiny_bicl_dat,
      ncls = 3, nfld = 3, method = "B", mic = TRUE,
      maxiter = 3, verbose = FALSE
    )
  ))
  expect_lt(short$n_cycle, 10L)
  expect_false(short$converge)
})
