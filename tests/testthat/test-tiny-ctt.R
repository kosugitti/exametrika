# CTT cross-validated against Mathematica on tinyCTT
# ---------------------------------------------------
# tinyCTT (250x8) gets its own dataset because tetrachoric correlation needs
# every item pair's 2x2 table to be populated. A pair with an empty cell sends
# the estimate to the boundary and the two implementations then disagree wildly
# -- measured 0.503 against 0.907 on an earlier shared fixture. The datasets
# that converge fastest are exactly the ones that produce empty cells, so this
# chapter cannot share theirs. Here the smallest cell holds 31 observations and
# the correlations stay in 0.2..0.6.
#
# The tolerances differ by quantity, and the split is the point: what is
# computed in closed form agrees to machine precision and what goes through a
# numerical search does not. Measured -- Phi 4.9e-15, mutual information
# 1.4e-10, tetrachoric 2.4e-05, alpha and omega 6.8e-06. Holding all of them at
# 1e-4 would pass while quietly stopping the exact ones from catching anything.
#
# The Ch03CTT.wl driver was written for this work and verified on the full-size
# data first: it reproduces all eleven existing Chapter03CTT_* fixtures byte for
# byte, so a disagreement here is the implementation, not the pipeline.

tiny_ctt <- dataFormat(read.csv(
  test_path("fixtures", "tiny_data", "tinyCTT.csv"),
  check.names = FALSE
))
ctt_test <- load_ref("TinyCTT_Test.csv")
ctt_item <- load_ref("TinyCTT_Item.csv")

test_that("tiny CTT matches Mathematica's closed-form matrices exactly", {
  expect_matrix_equal(
    PhiCoefficient(tiny_ctt),
    load_ref("TinyCTT_Phi_Coefficient.csv")[, -1],
    tolerance = 1e-12
  )
  expect_matrix_equal(
    MutualInformation(tiny_ctt),
    load_ref("TinyCTT_Mutual_Information.csv")[, -1],
    tolerance = 1e-8
  )
})

test_that("tiny CTT matches Mathematica's tetrachoric correlations", {
  # a bivariate normal integral solved numerically on both sides
  expect_matrix_equal(
    TetrachoricCorrelationMatrix(tiny_ctt),
    load_ref("TinyCTT_Tetrachoric_Correlation.csv")[, -1],
    tolerance = 1e-4
  )
})

test_that("no item pair leaves a cell of its 2x2 table empty", {
  # the property the fixture was built for: without it the tetrachoric
  # assertion above stops meaning anything
  U <- tiny_ctt$U
  smallest <- Inf
  for (a in seq_len(ncol(U) - 1)) {
    for (b in seq(a + 1, ncol(U))) {
      tab <- table(factor(U[, a], 0:1), factor(U[, b], 0:1))
      smallest <- min(smallest, min(tab))
    }
  }
  expect_gt(smallest, 10)
})

test_that("tiny CTT matches Mathematica's reliability coefficients", {
  rel <- CTT(tiny_ctt)
  keys <- c(
    "Alpha(Covariance)", "Alpha(Phi)", "Alpha(Tetrachoric)",
    "Omega(Covariance)", "Omega(Phi)", "Omega(Tetrachoric)"
  )
  expect_equal(as.character(rel$Reliability[[1]]), keys)
  expect_numeric_equal(
    rel$Reliability[[2]], ctt_test[match(keys, ctt_test[[1]]), 2],
    tolerance = 1e-4
  )
})

test_that("tiny CTT matches Mathematica's alpha-if-item-deleted", {
  rel <- CTT(tiny_ctt)
  expect_matrix_equal(
    rel$ReliabilityExcludingItem[, -1],
    ctt_item[, grep("Alpha If Item Deleted", names(ctt_item))],
    tolerance = 1e-4
  )
})
