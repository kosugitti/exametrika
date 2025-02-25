library(tidyverse)
### GOALS
library(readxl)
library(exametrika)
testReport <- read_excel("../../develop/13NNT.xlsx", sheet = "ScoreReport", col_names = FALSE)
ItemReport <- read_excel("../../develop/13NNT.xlsx", sheet = "ItemReport")
catQuant <- read_excel("../../develop/13NNT.xlsx", sheet = "ItemCategoryQuantileRepo")
catIRPreport <- read_excel("../../develop/13NNT.xlsx", sheet = "CatIRPreport", col_types = c("guess", rep("numeric", 10)))
RankScoreMat <- read_excel("../../develop/13NNT.xlsx", sheet = "RankScoremat", col_names = FALSE)
ScoreMemb <- read_excel("../../develop/13NNT.xlsx", sheet = "ScoreMemb", col_names = FALSE)
RankQuantMat <- read_excel("../../develop/13NNT.xlsx", sheet = "RankQuantMat", col_names = FALSE)
MembQuantMat <- read_excel("../../develop/13NNT.xlsx", sheet = "MembQuantMat", col_names = FALSE)
ItemFit1 <- read_excel("../../develop/13NNT.xlsx", sheet = "ItemFit1")
ItemFit2 <- read_excel("../../develop/13NNT.xlsx", sheet = "ItemFit2")
TestFit <- read_excel("../../develop/13NNT.xlsx", sheet = "TestFit")


result <- LRA(J35S5000, nrank = 10, mic = TRUE, verbose = F)

test_that("Test Info", {
  expect <- testReport[, 2] %>%
    unlist() %>%
    as.numeric()
  actual <- result$ScoreReport %>%
    as.matrix() %>%
    unlist() %>%
    as.numeric()
  expect_equal(actual[-6], expect, tolerance = 1e-4)
})

test_that("Item Info", {
  expect <- ItemReport[, 2:6] %>% as.matrix()
  actual <- result$ItemReport %>%
    unclass() %>%
    as.data.frame() %>%
    select(-ItemLabel) %>%
    as.matrix()
  actual <- actual[, -4]
  rownames(actual) <- NULL
  colnames(actual) <- colnames(expect) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("Cat Quant", {
  expect <- catQuant[, 2:12] %>% as.matrix()
  actual <- result$CatQuant %>%
    unclass() %>%
    as.data.frame() %>%
    select(-c(Item, Category)) %>%
    as.matrix()
  rownames(actual) <- NULL
  colnames(actual) <- colnames(expect) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("catIRPreport", {
  expect <- catIRPreport[, -1] %>% as.matrix()
  actual <- result$ICRP[, -c(1, 2)] %>% as.matrix()
  rownames(actual) <- NULL
  colnames(actual) <- colnames(expect) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("Rank score mat", {
  expect <- RankScoreMat %>% as.matrix()
  actual <- result$ScoreRank %>% as.matrix()
  # actual[,1:10] <- actual[,10:1]
  actual[1:36, ] <- actual[36:1, ]
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("Score memb", {
  expect <- ScoreMemb %>% as.matrix()
  actual <- result$ScoreMembership %>% as.matrix()
  actual[1:36, ] <- actual[36:1, ] %>% round(8)
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-6)
})

test_that("item fit", {
  expect <- ItemFit2[, -1] %>% as.matrix()
  actual <- result$ItemFitIndices %>%
    as.data.frame() %>%
    as.matrix()
  actual <- actual[, -c(2, 4)]
  rownames(expect) <- rownames(actual) <- NULL
  colnames(expect) <- colnames(actual) <- NULL
  expect_equal(actual, expect, tolerance = 1e-3)
})

test_that("test fit", {
  expect <- TestFit[, 3] %>%
    unlist() %>%
    as.vector()
  actual <- result$TestFitIndices %>%
    unlist() %>%
    as.vector()
  expect <- expect[c(1, 2, 4:10)]
  actual <- actual[c(5, 7, 9:15)]
  expect_equal(actual, expect, tolerance = 1e-3)
})
