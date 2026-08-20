library(exametrika)

### GOALS - Mathematica reference data
testReport <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_ScoreReport.csv"),
  check.names = FALSE
)
ItemReport <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_ItemReport.csv"),
  check.names = FALSE
)
catQuant <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_ItemCategoryQuantileRepo.csv"),
  check.names = FALSE
)
catIRPreport <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_CatIRPreport.csv"),
  check.names = FALSE
)
RankScoreMat <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_RankScoremat.csv"),
  check.names = FALSE
)
ScoreMemb <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_ScoreMemb.csv"),
  check.names = FALSE
)
RankQuantMat <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_RankQuantMat.csv"),
  check.names = FALSE
)
MembQuantMat <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_MembQuantMat.csv"),
  check.names = FALSE
)
ItemFit1 <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_ItemFit1.csv"),
  check.names = FALSE
)
ItemFit2 <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_ItemFit2.csv"),
  check.names = FALSE
)
TestFit <- read.csv(
  test_path("fixtures", "mathematica_reference", "13NNT_TestFit.csv"),
  check.names = FALSE
)


# The 13NNT references are full-data only (the data is embedded in the
# Mathematica notebook), so this file is CI-only: lazy accessor +
# skip_on_cran() in every block. CRAN-side rated-LRA coverage lives in
# test-distractor.R.
.nnt <- NULL
nnt_model <- function() {
  if (is.null(.nnt)) .nnt <<- LRA(J35S5000, nrank = 10, mic = TRUE, verbose = F)
  return(.nnt)
}

test_that("Test Info", {
  skip_on_cran()
  expect <- testReport[, 2] |>
    unlist() |>
    as.numeric()
  actual <- nnt_model()$ScoreReport |>
    as.matrix() |>
    unlist() |>
    as.numeric()
  expect_equal(actual[-6], expect, tolerance = 1e-4)
})

test_that("Item Info", {
  skip_on_cran()
  # Exclude CORR column (col 5) which contains Mathematica formula strings
  expect <- ItemReport[, c(2:4, 6)] |> as.matrix()
  storage.mode(expect) <- "double"
  actual <- nnt_model()$ItemReport |>
    unclass() |>
    as.data.frame()
  actual <- actual[, names(actual) != "ItemLabel", drop = FALSE]
  actual <- as.matrix(actual)
  # Remove ItemSD (col 4) and ItemCORR (col 5); keep Obs, ObsRatio, ItemMean, ItemCORR_R
  actual <- actual[, -c(4, 5)]
  rownames(actual) <- NULL
  colnames(actual) <- colnames(expect) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("Category Quantile", {
  skip_on_cran()
  expect <- catQuant[, 2:12] |> as.matrix()
  actual <- nnt_model()$CatQuant |>
    unclass() |>
    as.data.frame()
  actual <- actual[, !names(actual) %in% c("Item", "Category"), drop = FALSE]
  actual <- as.matrix(actual)
  rownames(actual) <- NULL
  colnames(actual) <- colnames(expect) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("Category IRP Report", {
  skip_on_cran()
  expect <- catIRPreport[, -1] |> as.matrix()
  actual <- nnt_model()$ICRP[, -c(1, 2)] |> as.matrix()
  rownames(actual) <- NULL
  colnames(actual) <- colnames(expect) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("Rank Score Matrix", {
  skip_on_cran()
  # CSV from col_names=FALSE sheet; convert to numeric matrix
  expect <- apply(as.matrix(RankScoreMat), 2, as.numeric)
  actual <- nnt_model()$ScoreRank |> as.matrix()
  # actual[,1:10] <- actual[,10:1]
  actual[1:36, ] <- actual[36:1, ]
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("Score Membership", {
  skip_on_cran()
  # CSV from col_names=FALSE sheet; convert to numeric matrix
  expect <- apply(as.matrix(ScoreMemb), 2, as.numeric)
  actual <- nnt_model()$ScoreMembership |> as.matrix()
  actual[1:36, ] <- round(actual[36:1, ], 8)
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("Item Fit", {
  skip_on_cran()
  expect <- ItemFit2[, -1] |> as.matrix()
  actual <- nnt_model()$ItemFitIndices |>
    unclass() |>
    as.data.frame() |>
    as.matrix()
  actual <- actual[, -c(1, 2, 3, 5, 7)]
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-3)
})

test_that("Test Fit", {
  skip_on_cran()
  expect <- TestFit[, 3] |>
    unlist() |>
    as.vector()
  actual <- nnt_model()$TestFitIndices |>
    unlist() |>
    as.vector()
  expect <- expect[c(1, 2, 4:10)]
  actual <- actual[c(4, 6, 8:14)]
  expect_equal(actual, expect, tolerance = 1e-3)
})
