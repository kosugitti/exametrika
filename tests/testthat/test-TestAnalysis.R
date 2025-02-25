# Prepare Section ---------------------------------------------------------
library(tidyverse, quietly = TRUE)
library(readxl)

Ch03Tests <- read_excel("../../develop/Chapter03CTT.xlsx", sheet = "Test")

SimpleStatistics <- Ch03Tests[1:23, 1:2] %>%
  rename(name = 1, value = 2) %>%
  mutate(value = as.numeric(value))

Ch03Tests2 <- read_excel("../../develop/Chapter03CTT.xlsx")

Dimensionality <- read_excel("../../develop/Chapter03CTT.xlsx", sheet = "Dimensionality")

CTTexpect <- Ch03Tests[24:29, ]

Ch03Items <- read_excel("../../develop/Chapter03CTT.xlsx",
  sheet = "Item",
) %>%
  select_if(is.numeric) %>%
  as.data.frame()

## read same data
U <- read_csv("../../develop/sampleData/J20S400.csv")
dat <- dataFormat(U, na = -99, id = 1)

# Test Section ------------------------------------------------------------

test_that("Simple Test Statistics", {
  result <- TestStatistics(dat)
  expect <- Ch03Tests2[1:23, 2] %>%
    unlist() %>%
    as.vector()
  result <- unclass(result) %>%
    unlist() %>%
    as.vector()
  expect_equal(object = result, expected = expect)
})

test_that("Dimenosnality Analysis", {
  result <- Dimensionality(dat) %>%
    unclass() %>%
    unlist() %>%
    matrix(ncol = 4)
  expect <- Dimensionality %>%
    as.matrix() %>%
    unname()
  expect_equal(object = result[, 2], expected = expect[, 2], tolerance = 1e-4)
  expect_equal(object = result[, 3] / 100, expected = expect[, 3], tolerance = 1e-4)
  expect_equal(object = result[, 4] / 100, expected = expect[, 4], tolerance = 1e-4)
})


test_that("Reliability", {
  result <- CTT(dat)
  result <- result$Reliability[, 2] %>% as.matrix()
  expect <- CTTexpect[, 2] %>%
    as.matrix() %>%
    unname()
  expect_equal(object = result, expected = expect, tolerance = 1e-4)
})

test_that("Item Del Reliability", {
  result <- CTT(dat)
  result <- result$ReliabilityExcludingItem[, -1] %>% unname()
  expect <- Ch03Items[, 8:10] %>% unname()
  expect_equal(object = result, expected = expect, tolerance = 1e-4)
})
