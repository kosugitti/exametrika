# Extracted from test-lra-ordinal.R:153

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
expect <- TesFit2[, -c(1, 4)] |> as.data.frame()
actual <- result$ItemFitIndices |>
  unclass() |>
  as.data.frame()
actual <- actual[, -c(1, 2, 3, 5, 7)]
rownames(expect) <- rownames(actual) <- NULL
colnames(expect) <- colnames(actual) <- NULL
names(expect) <- names(actual) <- NULL
expect <- as.matrix(expect)
actual <- as.matrix(actual)
cols_to_test <- c(1:7, 9)
expect_equal(actual[, cols_to_test], expect[, cols_to_test], tolerance = 1e-4)
