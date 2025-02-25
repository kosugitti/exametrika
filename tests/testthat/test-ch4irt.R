library(tidyverse)
library(exametrika)

### GOALS
library(readxl)

### model2
pl2Test <- read_excel("../../develop/Chapter04IRT2pl.xlsx", sheet = "Test")
pl2Item <- read_excel("../../develop/Chapter04IRT2pl.xlsx", sheet = "Item")
pl2Student <- read_excel("../../develop/Chapter04IRT2pl.xlsx", sheet = "Student")
pl2Q3mat <- read_excel("../../develop/Chapter04IRT2pl.xlsx", sheet = "Q3")

dat <- read_csv("../../develop/sampleData/J15S500.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- dataFormat(dat, na = -99)

result2 <- IRT(model = 2, tmp)

test_that("2PL model Test Info", {
  expect <- pl2Test[13:28, 2] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16)]
  result <- result2$TestFitIndices %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("2PL model Item Params Info", {
  expect <- pl2Item[, 7:6] %>%
    unlist() %>%
    as.numeric()
  result <- result2$params %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl2Item$`PSD(Slope)` %>%
    unlist() %>%
    as.numeric()
  result <- result2$itemPSD[, 1] %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl2Item$`PSD(Loc)` %>%
    unlist() %>%
    as.numeric()
  result <- result2$itemPSD[, 2] %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})

test_that("2PL model Item FitIndices", {
  expect <- pl2Item$`Log-Likelihood(Benchmark Model)` %>%
    unlist() %>%
    as.numeric()
  result <- result2$ItemFitIndices$bench_log_like %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl2Item$`Log-Likelihood(Null Model)` %>%
    unlist() %>%
    as.numeric()
  result <- result2$ItemFitIndices$null_log_like %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl2Item$`Log-Likelihood(Analysis Model)` %>%
    unlist() %>%
    as.numeric()
  result <- result2$ItemFitIndices$model_log_like %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl2Item[, 18:26] %>%
    unlist() %>%
    as.numeric()
  result <- result2$ItemFitIndices %>%
    unclass() %>%
    as.data.frame()
  result <- result[, 8:16] %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})


test_that("2PL model Students", {
  expect <- pl2Student$`EAP Score` %>%
    unlist() %>%
    as.numeric()
  result <- result2$ability$EAP %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl2Student$`Posterior SD` %>%
    unlist() %>%
    as.numeric()
  result <- result2$ability$PSD %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})

test_that("2PL Q3mat", {
  expect <- pl2Q3mat[, -1] %>%
    unlist() %>%
    as.numeric()
  result <- result2$Q3mat %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-2)
})


# model3 ------------------------------------------------------------------
pl3Test <- read_excel("../../develop/Chapter04IRT3pl.xlsx", sheet = "Test")
pl3Item <- read_excel("../../develop/Chapter04IRT3pl.xlsx", sheet = "Item")
pl3Student <- read_excel("../../develop/Chapter04IRT3pl.xlsx", sheet = "Student")


dat <- read_csv("../../develop/sampleData/J15S500.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- dataFormat(dat, na = -99)

result3 <- IRT(model = 3, tmp)

test_that("3PL model Test Info", {
  expect <- pl3Test[13:28, 2] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16)]
  result <- result3$TestFitIndices %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("3PL model Item Params Info", {
  expect <- pl3Item[, 7:9] %>%
    unlist() %>%
    as.numeric()
  result <- result3$params %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl3Item[, 10:12] %>%
    unlist() %>%
    as.numeric()
  result <- result3$itemPSD %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})

test_that("3PL model Item FitIndices", {
  expect <- pl3Item$`Log-Likelihood(Benchmark Model)` %>%
    unlist() %>%
    as.numeric()
  result <- result3$ItemFitIndices$bench_log_like %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl3Item$`Log-Likelihood(Null Model)` %>%
    unlist() %>%
    as.numeric()
  result <- result3$ItemFitIndices$null_log_like %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl3Item$`Log-Likelihood(Analysis Model)` %>%
    unlist() %>%
    as.numeric()
  result <- result3$ItemFitIndices$model_log_like %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl3Item[, 20:28] %>%
    unlist() %>%
    as.numeric()
  result <- result3$ItemFitIndices %>%
    unclass() %>%
    as.data.frame()
  result <- result[, 8:16] %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})


test_that("3PL model Students", {
  expect <- pl3Student$`EAP Score` %>%
    unlist() %>%
    as.numeric()
  result <- result3$ability$EAP %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl3Student$`Posterior SD` %>%
    unlist() %>%
    as.numeric()
  result <- result3$ability$PSD %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})



# model4 ------------------------------------------------------------------
pl4Test <- read_excel("../../develop/Chapter04IRT4pl.xlsx", sheet = "Test")
pl4Item <- read_excel("../../develop/Chapter04IRT4pl.xlsx", sheet = "Item")
pl4Student <- read_excel("../../develop/Chapter04IRT4pl.xlsx", sheet = "Student")


dat <- read_csv("../../develop/sampleData/J15S500.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- dataFormat(dat, na = -99)
result4 <- IRT(model = 4, tmp)

test_that("4PL model Test Info", {
  expect <- pl4Test[13:28, 2] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16)]
  result <- result4$TestFitIndices %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("4PL model Item Params", {
  expect <- pl4Item[, 7:10] %>%
    unlist() %>%
    as.numeric()
  result <- result4$params %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})

test_that("4PL model Item PSD", {
  expect <- pl4Item[, 11:14] %>%
    unlist() %>%
    as.numeric()
  result <- result4$itemPSD %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})

test_that("4PL log-lik Bench", {
  expect <- pl4Item$`Log-Likelihood(Benchmark Model)` %>%
    unlist() %>%
    as.numeric()
  result <- result4$ItemFitIndices$bench_log_like %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})

test_that("4PL log-lik Null", {
  expect <- pl4Item$`Log-Likelihood(Null Model)` %>%
    unlist() %>%
    as.numeric()
  result <- result4$ItemFitIndices$null_log_like %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})
test_that("4PL log-lik Analysis", {
  expect <- pl4Item$`Log-Likelihood(Analysis Model)` %>%
    unlist() %>%
    as.numeric()
  result <- result4$ItemFitIndices$model_log_like %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})

test_that("4PL Standardized FIdx", {
  expect <- pl4Item[, 22:30] %>%
    unlist() %>%
    as.numeric()
  result <- result4$ItemFitIndices %>%
    unclass() %>%
    as.data.frame()
  result <- result[, 8:16] %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})


test_that("4PL model Students", {
  expect <- pl4Student$`EAP Score` %>%
    unlist() %>%
    as.numeric()
  result <- result4$ability$EAP %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)

  expect <- pl4Student$`Posterior SD` %>%
    unlist() %>%
    as.numeric()
  result <- result4$ability$PSD %>%
    unlist() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})
