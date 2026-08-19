# IRT cross-validated against Mathematica on tinyCommon
# -----------------------------------------------------
# tinyCommon (240x15) carries a class-by-field structure and is shared with the
# biclustering tests; see develop/tiny_fixtures_refactor.md.
#
# The tolerances here are looser than LCA's, and not because the fixture is
# small. LCA agrees exactly because both implementations run the same EM to the
# same fixed point. IRT does not: the two optimisers stop on different criteria,
# and no amount of data closes that. Measured on 2PL, the item parameters differ
# by 1.4e-04 here and by 1.6e-04 on the full-size J15S500 -- the small frame is
# the closer of the two. The full-size tests already assert at 1e-3 for the same
# reason, so these match rather than inventing a bar the implementations cannot
# meet.

tiny_common <- dataFormat(read.csv(
  test_path("fixtures", "tiny_data", "tinyCommon.csv"),
  check.names = FALSE
))

irt_cols <- list(
  `2` = list(ref = c("Slope", "Location"), fit = c("slope", "location")),
  `3` = list(
    ref = c("Slope", "Location", "Lower Asymptote"),
    fit = c("slope", "location", "lowerAsym")
  ),
  `4` = list(
    ref = c("Slope", "Location", "Lower Asymptote", "Upper Asymptote"),
    fit = c("slope", "location", "lowerAsym", "upperAsym")
  )
)

for (model_n in 2:4) {
  local({
    m <- model_n
    tag <- sprintf("%dPL", m)
    fit <- suppressWarnings(IRT(model = m, tiny_common, verbose = FALSE))
    ref_test <- load_ref(sprintf("TinyIRT%dpl_Test.csv", m))
    ref_item <- load_ref(sprintf("TinyIRT%dpl_Item.csv", m))

    test_that(sprintf("tiny IRT %s matches Mathematica's item parameters", tag), {
      expect_matrix_equal(
        fit$params[, irt_cols[[as.character(m)]]$fit],
        ref_item[, irt_cols[[as.character(m)]]$ref],
        tolerance = 1e-3
      )
    })

    test_that(sprintf("tiny IRT %s matches Mathematica's fit indices", tag), {
      # looked up by name: the row offsets differ between chapters, and reusing
      # LCA's positional recipe here once compared the wrong rows silently
      key <- c(
        model_log_like = "Log-Likelihood(Analysis Model)",
        bench_log_like = "Log-Likelihood(Benchmark Model)",
        null_log_like = "Log-Likelihood(Null Model)",
        model_Chi_sq = "Chi-square(Analysis Model)",
        null_Chi_sq = "Chi-square(Null Model)",
        model_df = "DF(Analysis Model)", null_df = "DF(Null Model)",
        NFI = "NFI", RFI = "RFI", IFI = "IFI", TLI = "TLI", CFI = "CFI",
        RMSEA = "RMSEA", AIC = "AIC", CAIC = "CAIC", BIC = "BIC"
      )
      expected <- vapply(key, function(k) {
        as.numeric(ref_test[ref_test[[1]] == k, 2])
      }, numeric(1))
      got <- unlist(fit$TestFitIndices)[names(key)]
      expect_equal(unname(got), unname(expected), tolerance = 1e-3)
    })
  })
}
