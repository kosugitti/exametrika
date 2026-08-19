# Biclustering / Ranklustering cross-validated against Mathematica
# ----------------------------------------------------------------
# Shares tinyCommon (240x15) with the IRT tests. Both arms agree with
# Mathematica to about 1e-07 in the field reference profiles and take exactly
# the same number of EM cycles.
#
# The cycle count is the sharper of the two assertions. A change in the E- or
# M-step moves it; a change in floating-point details does not.
#
# The reference values come from GTM. Ranklustering's default estimation became
# "isotonic" in 2.0.0, so the comparison passes estimation = "GTM" explicitly.
# Comparing an isotonic fit against a GTM reference is what a first attempt did,
# and it disagreed by 6.3 in log-likelihood.

tiny_common_bicl <- dataFormat(read.csv(
  test_path("fixtures", "tiny_data", "tinyCommon.csv"), check.names = FALSE
))

bicl_arms <- list(
  list(name = "Bicl", label = "Biclustering", sheet = "Bicluster",
       args = list(method = "B")),
  list(name = "Rankl", label = "Ranklustering", sheet = "Rankluster",
       args = list(method = "R", estimation = "GTM"))
)

for (arm_i in bicl_arms) {
  local({
    arm <- arm_i
    fit <- suppressMessages(do.call(Biclustering, c(
      list(tiny_common_bicl, ncls = 3, nfld = 3, mic = TRUE, verbose = FALSE),
      arm$args
    )))
    ref_test <- load_ref(sprintf("Tiny%s_Test.csv", arm$name))
    ref_block <- load_ref(sprintf("Tiny%s_%s.csv", arm$name, arm$sheet))
    key <- function(k) as.numeric(ref_test[ref_test[[1]] == k, 2])

    test_that(sprintf("tiny %s takes the same EM cycles as Mathematica", arm$label), {
      expect_equal(fit$n_cycle, as.integer(key("N of EM Cycles")))
      expect_true(fit$converge)
    })

    test_that(sprintf("tiny %s matches the field reference profiles", arm$label), {
      expect_matrix_equal(fit$FRP, ref_block[1:3, 2:4], tolerance = 1e-6)
    })

    test_that(sprintf("tiny %s matches the benchmark and null models", arm$label), {
      expect_equal(fit$TestFitIndices$bench_log_like,
        key("Log-Likelihood(Benchmark Model)"), tolerance = 1e-10)
      expect_equal(fit$TestFitIndices$null_log_like,
        key("Log-Likelihood(Null Model)"), tolerance = 1e-10)
    })
  })
}

test_that("the two arms give different answers on this data", {
  # otherwise the blocks above would assert the same thing twice
  b <- suppressMessages(Biclustering(tiny_common_bicl, ncls = 3, nfld = 3,
    method = "B", mic = TRUE, verbose = FALSE))
  r <- suppressMessages(Biclustering(tiny_common_bicl, ncls = 3, nfld = 3,
    method = "R", estimation = "GTM", mic = TRUE, verbose = FALSE))
  expect_gt(max(abs(as.matrix(b$FRP) - as.matrix(r$FRP))), 1e-3)
})

test_that("Biclustering(maxiter) reaches the EM loop", {
  # regression: the binary branch set its cap to 1000 outright, so the argument
  # was accepted and discarded. The polytomous branches always passed it on.
  short <- suppressWarnings(suppressMessages(
    Biclustering(tiny_common_bicl,
      ncls = 3, nfld = 3, method = "B", mic = TRUE,
      maxiter = 3, verbose = FALSE
    )
  ))
  expect_lt(short$n_cycle, 10L)
  expect_false(short$converge)
})
