library(tidyverse)
### GOALS
library(readxl)
library(exametrika)
ScoreReport <- read_excel("develop/13NNT.xlsx", sheet = "ScoreReport")
ItemReport <- read_excel("develop/13NNT.xlsx", sheet = "ItemReport")
ItemCategory <- read_excel("develop/13NNT.xlsx", sheet = "ItemCategoryQuantileRepo")
catIRPreport <- read_excel("develop/13NNT.xlsx", sheet = "CatIRPreport")
RankScoreMat <- read_excel("develop/13NNT.xlsx", sheet = "RankScoremat")
ScoreMemb <- read_excel("develop/13NNT.xlsx", sheet = "ScoreMemb", col_names = FALSE)
RankQuantMat <- read_excel("develop/13NNT.xlsx", sheet = "RankQuantMat", col_names = FALSE)
MembQuantMat <- read_excel("develop/13NNT.xlsx", sheet = "MembQuantMat", col_names = FALSE)
ItemFit1 <- read_excel("develop/13NNT.xlsx", sheet = "ItemFit1")
ItemFit2 <- read_excel("develop/13NNT.xlsx", sheet = "ItemFit2")
TestFit <- read_excel("develop/13NNT.xlsx", sheet = "TestFit")



test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
