library(exametrika)

### GOALS - Mathematica reference data
test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter05LCA_Test.csv"),
  check.names = FALSE
)
class <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter05LCA_Class.csv"),
  check.names = FALSE
)
items <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter05LCA_Item.csv"),
  check.names = FALSE
)
student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter05LCA_Student.csv"),
  check.names = FALSE
)


### Setup
tmp <- dataFormat(J15S500, na = -99)
model <- LCA(tmp, ncls = 5)

### Tests
test_that("LCA Test Info", {
  expect <- test[14:29, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- model$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LCA Class Info", {
  ## TRP
  expect <- class[1, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$TRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- class[2, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$LCD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- class[3, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$CMD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("LCA Item Info", {
  ## IRP
  expect <- items[, 6:10] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## FitIndex
  expect <- items[, c(15, 11, 12, 16, 13, 17, 14, 18:26)] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$ItemFitIndices |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LCA Students", {
  ## Membership
  expect <- student[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})
