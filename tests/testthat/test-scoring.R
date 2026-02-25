### Read Output of Mathematica
library(exametrika)

Ch03CTT <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter03CTT_Student.csv"),
  check.names = FALSE
)
Ch03CTT <- Ch03CTT[, sapply(Ch03CTT, is.numeric), drop = FALSE]

## read raw data (J20S400 package data is pre-formatted as nominal)
dat <- read.csv(
  test_path("fixtures", "sample_data", "J20S400.csv"),
  check.names = FALSE
)

test_that("NRS", {
  result <- nrs(dat, na = -99) |> as.vector()
  expect <- Ch03CTT$`Number-Right Score` |> as.vector()
  expect_equal(result, expect)
})

test_that("passage", {
  result <- passage(dat, na = -99) |> as.vector()
  expect <- Ch03CTT$`Passage Rate` |> as.vector()
  expect_equal(result, expect)
})

test_that("sscore", {
  result <- sscore(dat, na = -99) |> as.vector()
  expect <- Ch03CTT$`Standardized Score` |> as.vector()
  expect_equal(result, expect)
})

test_that("percentile", {
  result <- percentile(dat, na = -99) |>
    as.vector() |>
    ceiling()
  expect <- Ch03CTT$`Percentile Rank` |> as.vector()
  expect_equal(result, expect)
})

test_that("stanine", {
  result <- stanine(dat, na = -99)
  test <- result$stanineScore |> as.numeric()
  expect <- Ch03CTT$Stanine |> as.vector()
  expect_equal(test, expect)
})
