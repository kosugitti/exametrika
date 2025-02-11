library(tidyverse)
### GOALS
library(readxl)
library(exametrika)
testReport <- read_excel("../../develop/12GNT.xlsx", sheet = "TestReport", col_names = FALSE)
itemReport <- read_excel("../../develop/12GNT.xlsx", sheet = "ItemReport")
catQuant <- read_excel("../../develop/12GNT.xlsx", sheet = "CatQuantReport")
cumRatio <- read_excel("../../develop/12GNT.xlsx", sheet = "CumRatio")
testRefProf <- read_excel("../../develop/12GNT.xlsx", sheet = "TestRefProf")
TRP <- read_excel("../../develop/12GNT.xlsx", sheet = "TRP")
RankProf <- read_excel("../../develop/12GNT.xlsx", sheet = "RankProf")
ScoreRank <- read_excel("../../develop/12GNT.xlsx", sheet = "ScoreRank")
TesFit1 <- read_excel("../../develop/12GNT.xlsx", sheet = "TestFit1")
TesFit2 <- read_excel("../../develop/12GNT.xlsx", sheet = "TestFit2")
TesFit3 <- read_excel("../../develop/12GNT.xlsx", sheet = "TestFit3")


result <- LRA(J15S3810, mic = TRUE, nrank = 3)

test_that("Test Info", {
  expect <- testReport[, 2] %>%
    unlist() %>%
    as.numeric()
  actual <- result$ScoreReport %>%
    as.matrix() %>%
    unlist() %>%
    as.numeric()
  expect_equal(actual, expect, tolerance = 1e-4)
})

test_that("Item Info", {
  expect <- itemReport[, -1] %>% as.matrix()
  actual <- result$ItemReport %>%
    unclass() %>%
    as.data.frame() %>%
    select(-ItemLabel) %>%
    as.matrix()
  rownames(actual) <- NULL
  colnames(actual) <- colnames(expect) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("CatQuant Ref Mat", {
  expect <- catQuant[, 2:5] %>% as.matrix()
  actual <- result$CatQuant[, 3:6] %>% as.matrix()
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("IC Boundary", {
  expect <- cumRatio[, 2:4] %>% as.matrix()
  actual <- result$ICBR[, 3:5] %>% as.matrix()
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-4)
})

test_that("IC Reference Profile", {
  expect <- testRefProf[, 2:4] %>% as.matrix()
  actual <- result$ICRP[, 3:5] %>% as.matrix()
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("TRP", {
  TRP <- TRP[, 2:4] %>%
    as.matrix() %>%
    unname()
  expect1 <- TRP[1, ] %>% as.vector()
  expect2 <- TRP[2, ] %>% as.vector()
  actual1 <- result$TRP %>% as.vector()
  actual2 <- result$LRD %>% as.vector()
  expect_equal(actual1, expect1, tolerance = 1e-6)
  expect_equal(actual2, expect2, tolerance = 1e-6)
})

test_that("rank Prof", {
  expect <- RankProf[, -1] %>% as.matrix()
  actual <- result$Students[, c(1, 2, 3, 5, 4)] %>% as.matrix()
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-4)
})

test_that("scoreRank", {
  expect <- ScoreRank[, -1] %>% as.data.frame()
  actual <- result$ScoreMembership %>%
    as.matrix() %>%
    as.data.frame()
  actual[1:28, ] <- actual[28:1, ]
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-4)
})

test_that("Item Fit", {
  expect <- TesFit2[, -c(1, 4)] %>% as.data.frame()
  actual <- result$ItemFitIndices %>% as.data.frame()
  actual <- actual[, -c(2, 4)]
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  names(expect) <- names(actual) <- NULL
  expect <- as.matrix(expect)
  actual <- as.matrix(expect)
  expect_equal(actual, expect, tolerance = 1e-4)
})

test_that("Item Test", {
  expect <- TesFit3[-3, 3] %>% as.matrix()
  actual <- result$TestFitIndices %>% as.matrix()
  actual <- actual[-c(1, 2, 4, 6), ] %>% as.matrix()
  expect <- as.matrix(expect)
  actual <- as.matrix(expect)
  expect_equal(actual, expect, tolerance = 1e-4)
})
