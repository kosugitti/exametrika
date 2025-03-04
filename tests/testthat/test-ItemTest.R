### Read Output of Mathematica
suppressPackageStartupMessages(library(tidyverse))
library(readxl)

# シート名のベクターを作成
sheets <- c(
  "Item", "Joint Sample Size", "Joint Correct Response Rate",
  "Conditional Correct Response Ra", "Item Lift", "Mutual Information",
  "Phi Coefficient", "Tetrachoric Correlation"
)

# 各シートからデータを読み込む
data_list <- lapply(sheets, function(sheet) {
  suppressMessages(read_excel("../../develop/Chapter03CTT.xlsx", sheet = sheet)) %>%
    select_if(is.numeric) %>%
    as.matrix()
})

# リストに名前をつける
names(data_list) <- c("Ch03Items", "Ch03JSS", "Ch03JCRR", "Ch03CCRR", "Ch03IL", "Ch03MI", "Ch03Phi", "Ch03Tet")

## read same data
dat <- suppressMessages(read_csv("../../develop/sampleData/J20S400.csv"))

# Test Section ------------------------------------------------------------

test_that("crr", {
  result <- crr(dat, na = -99) %>% as.vector()
  expect <- data_list$Ch03Items[, 2] %>% as.vector()
  expect_equal(result, expect)
})


test_that("Item Odds", {
  result <- ItemOdds(dat, na = -99) %>% as.vector()
  expect <- data_list$Ch03Items[, 3] %>% as.vector()
  expect_equal(result, expect)
})

test_that("Item Threshold", {
  result <- ItemThreshold(dat, na = -99) %>% as.vector()
  expect <- data_list$Ch03Items[, 4] %>% as.vector()
  expect_equal(result, expect)
})

test_that("Item Entropy", {
  result <- ItemEntropy(dat, na = -99) %>% as.vector()
  expect <- data_list$Ch03Items[, 5] %>% as.vector()
  expect_equal(result, expect)
})

# Between Items Section ---------------------------------------------------

test_that("Joint sample size", {
  result <- JointSampleSize(dat, na = -99) %>%
    unclass() %>%
    unname()
  expect <- data_list$Ch03JSS %>% unname()
  expect_equal(result, expect)
})

test_that("Joint Correct Response Rate", {
  tmp <- dataFormat(dat, na = -99)
  result <- JCRR(tmp) %>%
    unclass() %>%
    unname()
  expect <- data_list$Ch03JCRR %>% unname()
  expect_equal(result, expect)
})


test_that("Conditional Correct Repsonse Rate", {
  tmp <- dataFormat(dat, na = -99)
  result <- CCRR(tmp) %>%
    unclass() %>%
    unname()
  expect <- data_list$Ch03CCRR %>% unname()
  expect_equal(result, expect)
})


test_that("Item Lift", {
  result <- ItemLift(dat, na = -99) %>%
    unclass() %>%
    unname()
  expect <- data_list$Ch03IL %>% unname()
  expect_equal(result, expect)
})

test_that("Mutual Informaiton", {
  result <- MutualInformation(dat, na = -99) %>%
    unclass() %>%
    unname()
  expect <- data_list$Ch03MI %>% unname()
  expect_equal(result, expect)
})

test_that("Phi Coefficient", {
  result <- PhiCoefficient(dat, na = -99) %>%
    unclass() %>%
    unname()
  expect <- data_list$Ch03Phi %>% unname()
  expect_equal(result, expect)
})

test_that("Tetrachoric Correlation Matrix", {
  result <- TetrachoricCorrelationMatrix(dat, na = -99) %>%
    unclass() %>%
    unname()
  expect <- data_list$Ch03Tet %>% unname()
  expect_equal(expected = expect, object = result, tolerance = 1e-4)
})

test_that("Item Total Correlation", {
  result <- ItemTotalCorr(dat, na = -99)
  expect <- data_list$Ch03Items[, 6]
  expect_equal(expected = expect, object = as.vector(result))
})


test_that("Item Total Biserial Correlation", {
  tmp <- dataFormat(dat, na = -99)
  result <- ITBiserial(tmp) %>% as.vector()
  expect <- data_list$Ch03Items[, 7] %>% as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})
