# Extracted from test-lra-ordinal.R:129

# prequel ----------------------------------------------------------------------
library(exametrika)
testReport <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_TestReport.csv"),
  check.names = FALSE
)
itemReport <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_ItemReport.csv"),
  check.names = FALSE
)
catQuant <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_CatQuantReport.csv"),
  check.names = FALSE
)
cumRatio <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_CumRatio.csv"),
  check.names = FALSE
)
testRefProf <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_TestRefProf.csv"),
  check.names = FALSE
)
TRP_ref <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_TRP.csv"),
  check.names = FALSE
)
RankProf <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_RankProf.csv"),
  check.names = FALSE
)
ScoreRank <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_ScoreRank.csv"),
  check.names = FALSE
)
TesFit1 <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_TestFit1.csv"),
  check.names = FALSE
)
TesFit2 <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_TestFit2.csv"),
  check.names = FALSE
)
TesFit3 <- read.csv(
  test_path("fixtures", "mathematica_reference", "12GNT_TestFit3.csv"),
  check.names = FALSE
)
result <- LRA(J15S3810, mic = TRUE, nrank = 3, method = "GTM")

# test -------------------------------------------------------------------------
expect <- ScoreRank[, -1] |> as.data.frame()
actual <- result$ScoreMembership |>
    as.matrix() |>
    as.data.frame()
actual[1:28, ] <- actual[28:1, ]
rownames(expect) <- rownames(actual) <- NULL
colnames(expect) <- colnames(actual) <- NULL
expect_equal(actual, expect, tolerance = 1e-4)
