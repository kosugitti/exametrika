library(tidyverse)
### GOALS
library(readxl)
library(exametrika)
test <- read_excel("../../develop/Chapter05LCA.xlsx", sheet = "Test")
class <- read_excel("../../develop/Chapter05LCA.xlsx", sheet = "Class")
items <- read_excel("../../develop/Chapter05LCA.xlsx", sheet = "Item")
student <- read_excel("../../develop/Chapter05LCA.xlsx", sheet = "Student")


### Target
dat <- read_csv("../../develop/sampleData/J15S500.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- dataFormat(dat, na = -99)
U <- ifelse(is.na(tmp$U), 0, tmp$U) * tmp$Z

model <- LCA(tmp, ncls = 5)
plot(model, type = "IRP")
plot(model, type = "TRP")
plot(model, type = "LCD")
plot(model, type = "CMP", students = 1:15, nr = 5, nc = 3)

### test
test_that("LCA Test Info", {
  expect <- test[14:29, 2] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- model$TestFitIndices %>% as.numeric()
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
