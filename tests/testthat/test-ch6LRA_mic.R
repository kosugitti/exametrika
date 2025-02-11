library(tidyverse)
### GOALS
library(readxl)
library(exametrika)
test <- read_excel("../../develop/Chapter06LRA(GTMmic1).xlsx", sheet = "Test")
class <- read_excel("../../develop/Chapter06LRA(GTMmic1).xlsx", sheet = "Rank")
items <- read_excel("../../develop/Chapter06LRA(GTMmic1).xlsx", sheet = "Item")
student <- read_excel("../../develop/Chapter06LRA(GTMmic1).xlsx", sheet = "Student")


### Target

model <- LRA(J15S500, nrank = 6, mic = TRUE)

### test
test_that("LCA Test Info", {
  expect <- test[15:30, 2] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- model$TestFitIndices %>% as.numeric()
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
  expect_equal(result, expect, tolerance = 1e-3)
  ## CMD
  expect <- class[3, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$RMD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Item Info1 IRP", {
  ## IRP
  expect <- items[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Item Info2 IRPindex", {
  ## IRP index
  expect <- items[, 12:17] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRPIndex |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Item Info3 FitIndex", {
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
  ##
  expect <- items[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRP |>
    unlist() |>
    as.numeric()
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


test_that("LCA Students", {
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
