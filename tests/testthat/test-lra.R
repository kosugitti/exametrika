library(exametrika)

### GOALS - Mathematica reference data
test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter06LRA_GTMmic0_Test.csv"),
  check.names = FALSE
)
class <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter06LRA_GTMmic0_Rank.csv"),
  check.names = FALSE
)
items <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter06LRA_GTMmic0_Item.csv"),
  check.names = FALSE
)
student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter06LRA_GTMmic0_Student.csv"),
  check.names = FALSE
)


### Setup
tmp <- dataFormat(J15S500, na = -99)
model <- LRA(tmp, nrank = 6)

### Tests
test_that("LRA Test Fit", {
  expect <- test[15:30, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- model$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Class Info", {
  ## TRP
  expect <- class[1, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$TRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- class[2, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$LRD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- class[3, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$RMD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Item Info", {
  ## IRP
  expect <- items[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## IRP index
  expect <- items[, 12:17] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRPIndex |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## FitIndex
  expect <- items[, c(22, 18, 19, 23, 20, 24, 21, 25:33)] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$ItemFitIndices |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Student Info", {
  ## Membership probabilities
  expect <- student[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 1:6] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Latent Rank Estimate
  expect <- student[, 12] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 7] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Rank-Up/Down Odds
  expect <- student[, 13:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 8:9] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})


test_that("LRA Students", {
  ## Membership
  expect <- student[, 6:12] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 1:7] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## RUO/RDO
  expect <- student[, 13:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 8:9] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})
