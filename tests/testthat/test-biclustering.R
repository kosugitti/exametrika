library(exametrika)

### GOALS - Mathematica reference data (Biclustering)
test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Biclustering_Test.csv"),
  check.names = FALSE
)
Bicluster <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Biclustering_Bicluster.csv"),
  check.names = FALSE
)
items <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Biclustering_Item.csv"),
  check.names = FALSE
)
student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Biclustering_Student.csv"),
  check.names = FALSE
)


### Setup (Biclustering)
tmp <- dataFormat(J35S515)
Bic <- Biclustering(tmp, ncls = 6, nfld = 5, method = "B", mic = TRUE)

### Tests (Biclustering)
test_that("Biclustering Test Fit", {
  expect <- test[15:30, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- Bic$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("Biclustering Class Info", {
  ## FRP
  expect <- Bicluster[1:5, 2:7] |>
    unlist() |>
    unname() |>
    as.vector()
  result <- Bic$FRP |>
    unname() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  ## TRP
  expect <- Bicluster[6, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$TRP
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- Bicluster[7, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$LRD
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- Bicluster[8, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$ClassMembership |>
    colSums() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("Biclustering Item Info", {
  ## IRP
  expect <- items[, 6:10] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$FieldMembership |>
    unlist() |>
    unname() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Estimated
  expect <- items[, 11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$FieldEstimated |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("Biclustering Students", {
  ## Membership
  expect <- student[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$Students[, 1:6] |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


# Ranklustering ---------------------------------------------------

test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Ranklustering_Test.csv"),
  check.names = FALSE
)
Rankluster <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Ranklustering_Rankluster.csv"),
  check.names = FALSE
)
items <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Ranklustering_Item.csv"),
  check.names = FALSE
)
student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Ranklustering_Student.csv"),
  check.names = FALSE
)


### Setup (Ranklustering)
tmp <- dataFormat(J35S515)
Bic <- Biclustering(tmp, ncls = 6, nfld = 5, method = "R", mic = TRUE)

### Tests (Ranklustering)
test_that("Ranklustering Test Fit", {
  expect <- test[15:30, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- Bic$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("Ranklustering Class Info", {
  ## FRP
  expect <- Rankluster[1:5, 2:7] |>
    unlist() |>
    unname() |>
    as.vector()
  result <- Bic$FRP |>
    unname() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  ## TRP
  expect <- Rankluster[6, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$TRP
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- Rankluster[7, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$LRD
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- Rankluster[8, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$ClassMembership |>
    colSums() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Index
  expect <- Rankluster[1:5, 9:14] |>
    unlist() |>
    unname() |>
    as.vector()
  result <- Bic$FRPIndex |>
    unlist() |>
    unname() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("Ranklustering Item Info", {
  ## IRP
  expect <- items[, 6:10] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$FieldMembership |>
    unlist() |>
    unname() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Estimated
  expect <- items[, 11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$FieldEstimated |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("Ranklustering Students", {
  ## Membership
  expect <- student[, 6:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$Students[, 1:9] |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})
