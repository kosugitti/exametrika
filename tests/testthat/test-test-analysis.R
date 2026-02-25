# Prepare Section ---------------------------------------------------------
library(exametrika)

Ch03Tests <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter03CTT_Test.csv"),
  check.names = FALSE
)

SimpleStatistics <- Ch03Tests[1:23, 1:2]
names(SimpleStatistics) <- c("name", "value")
SimpleStatistics$value <- as.numeric(SimpleStatistics$value)

Ch03Tests2 <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter03CTT_Test.csv"),
  check.names = FALSE
)

Dimensionality_ref <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter03CTT_Dimensionality.csv"),
  check.names = FALSE
)

CTTexpect <- Ch03Tests[24:29, ]

Ch03Items <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter03CTT_Item.csv"),
  check.names = FALSE
)
Ch03Items <- Ch03Items[, sapply(Ch03Items, is.numeric), drop = FALSE]

## read raw data (J20S400 package data is pre-formatted as nominal)
U <- read.csv(
  test_path("fixtures", "sample_data", "J20S400.csv"),
  check.names = FALSE
)
dat <- dataFormat(U, na = -99, id = 1)

# Test Section ------------------------------------------------------------

test_that("Simple Test Statistics", {
  result <- TestStatistics(dat)
  expect <- Ch03Tests2[1:23, 2] |>
    unlist() |>
    as.vector()
  result <- unclass(result) |>
    unlist() |>
    as.vector()
  expect_equal(object = result, expected = expect)
})

test_that("Dimensionality Analysis", {
  result <- Dimensionality(dat) |>
    unclass() |>
    unlist() |>
    matrix(ncol = 4)
  expect <- Dimensionality_ref |>
    as.matrix() |>
    unname()
  expect_equal(object = result[, 2], expected = expect[, 2], tolerance = 1e-4)
  expect_equal(object = result[, 3] / 100, expected = expect[, 3], tolerance = 1e-4)
  expect_equal(object = result[, 4] / 100, expected = expect[, 4], tolerance = 1e-4)
})


test_that("Reliability", {
  result <- CTT(dat)
  result <- result$Reliability[, 2] |> as.matrix()
  expect <- CTTexpect[, 2] |>
    as.matrix() |>
    unname()
  expect_equal(object = result, expected = expect, tolerance = 1e-4)
})

test_that("Item Del Reliability", {
  result <- CTT(dat)
  result <- result$ReliabilityExcludingItem[, -1] |> unname()
  expect <- Ch03Items[, 8:10] |> unname()
  expect_equal(object = result, expected = expect, tolerance = 1e-4)
})
