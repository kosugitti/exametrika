library(exametrika)

### GOALS - Mathematica reference data
Test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter10LDR_Test.csv"),
  check.names = FALSE
)
CCRR <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter10LDR_CCRR.csv"),
  check.names = FALSE
)
MR <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter10LDR_Marginal_Rankluster.csv"),
  check.names = FALSE
)
Item <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter10LDR_Item.csv"),
  check.names = FALSE
)
Student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter10LDR_Student.csv"),
  check.names = FALSE
)

### Target
fieldFile <- test_path("fixtures", "auxiliary_data", "FixFieldLDB.csv")
ncls <- 5
edgeFile <- test_path("fixtures", "auxiliary_data", "EdgesLDB.csv")
FieldData <- read.csv(fieldFile)
conf <- FieldData[, 2]
adj_file <- edgeFile
tgt <- LDB(U = J35S515, ncls = 5, conf = conf, adj_file = edgeFile)


# test1 Test ------------------------------------------------------

test_that("Test Info", {
  expect <- Test[15:30, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- tgt$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

# CCRR ------------------------------------------------------------

test_that("Conditional Correct Response Rate", {
  expect <- CCRR[-c(11, 22, 33, 44, 55), 2:14] |>
    unlist() |>
    na.omit() |>
    as.vector() |>
    as.numeric()
  result <- tgt$CCRR_table[, 3:15] |>
    unlist() |>
    na.omit() |>
    as.vector() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


# Marginal Rankluster ---------------------------------------------

test_that("Marginal Rankluster", {
  expect <- MR[1:10, 2:6] |>
    unlist() |>
    as.vector()
  result <- tgt$FRP |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MR[1:10, 8:13] |>
    unlist() |>
    as.vector()
  result <- tgt$FRPIndex |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MR[11, 2:6] |>
    unlist() |>
    as.vector()
  result <- tgt$TRP |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MR[12, 2:6] |>
    unlist() |>
    as.vector()
  result <- tgt$LRD |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MR[13, 2:6] |>
    unlist() |>
    as.vector()
  result <- tgt$RMD |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})


# Item ------------------------------------------------------------

test_that("Items", {
  expect <- Item$`Latent Field` |>
    unlist() |>
    as.vector()
  result <- tgt$FieldEstimated |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})


# Students ------------------------------------------------------------

test_that("Students", {
  expect <- Student[, 6:13] |>
    unlist() |>
    as.vector()
  result <- tgt$Students |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})
