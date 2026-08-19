# CTT cross-validated against Mathematica on a tiny generated dataset
# ------------------------------------------------------------------
# Third chapter of the tiny-fixture work; see develop/tiny_fixtures_plan.md.
#
# The tolerances differ by quantity here, and the split is informative: what is
# computed in closed form agrees to machine precision, and what goes through a
# numerical search does not. Measured 2026-08-19 on 150x10:
#
#   Phi coefficient              2.8e-15   closed form
#   Mutual information           1.4e-10   closed form, more arithmetic
#   Tetrachoric correlation      2.5e-05   bivariate normal integral, searched
#   Alpha / Omega                5.1e-06   Omega minimises a loss (FindMinimum)
#   Alpha if item deleted        5.9e-06   likewise
#
# So the assertions below are not held to one number. Asserting 1e-4 everywhere
# would pass, but it would stop the closed-form quantities from catching
# anything, and those are exactly the ones that should be exact.
#
# The Ch03CTT.wl driver was written for this work and verified first on the
# full-size data: it reproduces all eleven existing Chapter03CTT_* fixtures
# byte for byte, so a difference here would be the implementation, not the
# pipeline.

tiny_ctt <- read.csv(
  test_path("fixtures", "tiny_data", "tinyCTT.csv"),
  check.names = FALSE
)
tiny_ctt_dat <- dataFormat(tiny_ctt, na = -99, id = 1)

ctt_test <- load_ref("TinyCTT_Test.csv")
ctt_item <- load_ref("TinyCTT_Item.csv")

test_that("tiny CTT reproduces Mathematica's closed-form matrices exactly", {
  expect_matrix_equal(
    PhiCoefficient(tiny_ctt_dat),
    load_ref("TinyCTT_Phi_Coefficient.csv")[, -1],
    tolerance = 1e-12
  )
  expect_matrix_equal(
    MutualInformation(tiny_ctt_dat),
    load_ref("TinyCTT_Mutual_Information.csv")[, -1],
    tolerance = 1e-8
  )
})

test_that("tiny CTT reproduces Mathematica's tetrachoric correlations", {
  # a bivariate normal integral solved numerically on both sides
  expect_matrix_equal(
    TetrachoricCorrelationMatrix(tiny_ctt_dat),
    load_ref("TinyCTT_Tetrachoric_Correlation.csv")[, -1],
    tolerance = 1e-4
  )
})

test_that("tiny CTT reproduces Mathematica's reliability coefficients", {
  rel <- CTT(tiny_ctt_dat)
  keys <- c(
    "Alpha(Covariance)", "Alpha(Phi)", "Alpha(Tetrachoric)",
    "Omega(Covariance)", "Omega(Phi)", "Omega(Tetrachoric)"
  )
  expect_equal(as.character(rel$Reliability[[1]]), keys)
  expect_numeric_equal(
    rel$Reliability[[2]],
    ctt_test[match(keys, ctt_test[[1]]), 2],
    tolerance = 1e-4
  )
})

test_that("tiny CTT reproduces Mathematica's alpha-if-item-deleted", {
  rel <- CTT(tiny_ctt_dat)
  expect_matrix_equal(
    rel$ReliabilityExcludingItem[, -1],
    ctt_item[, grep("Alpha If Item Deleted", names(ctt_item))],
    tolerance = 1e-4
  )
})
