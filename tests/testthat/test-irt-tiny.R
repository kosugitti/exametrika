# IRT cross-validated against Mathematica on a tiny generated dataset
# ------------------------------------------------------------------
# Companion to test-lca-tiny.R; see develop/tiny_fixtures_plan.md. Same idea --
# run the same small frame through both implementations and keep the numeric
# comparison -- but the tolerances here are looser, and for a reason worth
# recording.
#
# LCA agrees to 1e-13 because both sides run the same EM to the same fixed
# point. IRT does not: the two optimisers stop on different criteria, so the
# estimates differ in the fourth decimal no matter how much data they are given.
# Measured 2026-08-19 on 2PL: 200x10 disagrees by 1.2e-3 in log-likelihood and
# 4.0e-5 in the item parameters, and J15S500 (500x15) by 2.5e-3 and 1.6e-4 --
# the small frame is the *closer* of the two. The existing full-size tests use
# tolerance = 1e-3 for the same reason, so these match them rather than
# inventing a stricter bar the implementations cannot meet.

tiny_irt <- read.csv(
  test_path("fixtures", "tiny_data", "tinyIRT.csv"),
  check.names = FALSE
)
tiny_irt_dat <- dataFormat(tiny_irt, na = -99)

ref_cols <- list(
  `2` = c("Slope", "Location"),
  `3` = c("Slope", "Location", "Lower Asymptote"),
  `4` = c("Slope", "Location", "Lower Asymptote", "Upper Asymptote")
)
fit_cols <- list(
  `2` = c("slope", "location"),
  `3` = c("slope", "location", "lowerAsym"),
  `4` = c("slope", "location", "lowerAsym", "upperAsym")
)

for (m in 2:4) {
  local({
    model <- m
    tag <- sprintf("%dPL", model)
    fit <- IRT(model = model, tiny_irt_dat, verbose = FALSE)
    ref_test <- load_ref(sprintf("TinyIRT%dpl_Test.csv", model))
    ref_item <- load_ref(sprintf("TinyIRT%dpl_Item.csv", model))

    test_that(sprintf("tiny IRT %s reproduces Mathematica's item parameters", tag), {
      expect_matrix_equal(
        fit$params[, fit_cols[[as.character(model)]]],
        ref_item[, ref_cols[[as.character(model)]]],
        tolerance = 1e-3
      )
    })

    test_that(sprintf("tiny IRT %s reproduces Mathematica's log-likelihood", tag), {
      expect_equal(
        fit$TestFitIndices$model_log_like,
        as.numeric(ref_test[ref_test[[1]] == "Log-Likelihood(Analysis Model)", 2]),
        tolerance = 1e-3
      )
    })

    test_that(sprintf("tiny IRT %s reproduces Mathematica's test-level fit indices", tag), {
      # The Test sheet lists these by name; look them up rather than by row
      # offset, which differs between chapters (LCA carries an extra
      # "N of Classes" row, so its indices are shifted by one).
      key <- c(
        model_log_like = "Log-Likelihood(Analysis Model)",
        bench_log_like = "Log-Likelihood(Benchmark Model)",
        null_log_like = "Log-Likelihood(Null Model)",
        model_Chi_sq = "Chi-square(Analysis Model)",
        null_Chi_sq = "Chi-square(Null Model)",
        model_df = "DF(Analysis Model)",
        null_df = "DF(Null Model)",
        NFI = "NFI", RFI = "RFI", IFI = "IFI", TLI = "TLI", CFI = "CFI",
        RMSEA = "RMSEA", AIC = "AIC", CAIC = "CAIC", BIC = "BIC"
      )
      expect <- vapply(key, function(k) {
        as.numeric(ref_test[ref_test[[1]] == k, 2])
      }, numeric(1))
      got <- unlist(fit$TestFitIndices)[names(key)]
      expect_equal(unname(got), unname(expect), tolerance = 1e-3)
    })
  })
}
