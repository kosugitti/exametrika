library(tidyverse)
### GOALS
library(readxl)
library(exametrika)
Test <- read_excel("../../develop/Chapter11BINET.xlsx", sheet = "Test")
CCRR <- read_excel("../../develop/Chapter11BINET.xlsx", sheet = "CCRR")
LDPSR <- read_excel("../../develop/Chapter11BINET.xlsx", sheet = "LDPSR")
MB <- read_excel("../../develop/Chapter11BINET.xlsx", sheet = "Marginal Bicluster")
Item <- read_excel("../../develop/Chapter11BINET.xlsx", sheet = "Item")
Student <- read_excel("../../develop/Chapter11BINET.xlsx", sheet = "Student")

### Target
fieldFile <- "../../develop/mtmk15forVer13/FixFieldBINET.csv"
edgeFile <- "../../develop/mtmk15forVer13/EdgesBINET.csv"
FieldData <- read.csv(fieldFile)
conf <- FieldData[, 2]
tgt <- BINET(
  U = J35S515,
  ncls = 13, nfld = 12,
  conf = conf, adj_file = edgeFile
)

# test1 Test ------------------------------------------------------

test_that("Test Info", {
  expect <- Test[16:31, 2] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- tgt$MG_FitIndices %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- Test[16:31, 3] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- tgt$SM_FitIndices %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


# CCRR ------------------------------------------------------

test_that("CCRR", {
  for (i in 1:12) {
    st <- (i - 1) * 14 + 1
    ed <- st + 12
    expect <- CCRR[st:ed, 2:5] %>%
      unlist() %>%
      unname() %>%
      na.omit() %>%
      as.numeric()
    result <- tgt$PSRP[[i]] %>%
      unlist() %>%
      as.numeric() %>%
      na.omit() %>%
      as.vector()
    expect_equal(result, expect, tolerance = 1e-4)
  }
})

# LDPSR ------------------------------------------------------
test_that("LDPSR", {
  expect <- LDPSR[, 1] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  result <- tgt$LDPSR[, 1]
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- LDPSR[, 2:4] %>%
    unlist() %>%
    as.vector()
  result <- tgt$LDPSR[, 2:4] %>%
    unlist() %>%
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- LDPSR[, 6:10] %>%
    unlist() %>%
    unname() %>%
    na.omit() %>%
    as.numeric()
  result <- tgt$LDPSR[, 6:10] %>%
    unlist() %>%
    unname() %>%
    na.omit() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- LDPSR[, 11:15] %>%
    unlist() %>%
    unname() %>%
    na.omit() %>%
    as.numeric()
  result <- tgt$LDPSR[, 11:15] %>%
    unlist() %>%
    unname() %>%
    na.omit() %>%
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

# LDPSR ------------------------------------------------------
test_that("Marginal Bicluster", {
  expect <- MB[1:12, 2:14] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  result <- tgt$FRP %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MB[1:12, 15] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  result <- tgt$LFD %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MB[13, 2:14] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  result <- tgt$TRP %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MB[14, 2:14] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  result <- tgt$LCD %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MB[15, 2:14] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  result <- tgt$CMD %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

# Students ------------------------------------------------------------

test_that("Students", {
  expect <- Student[, 6:18] %>%
    unlist() %>%
    as.vector()
  result <- tgt$Students[, 1:13] %>%
    unlist() %>%
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- Student[, 19] %>%
    unlist() %>%
    as.vector()
  result <- tgt$Students[, 14] %>%
    unlist() %>%
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- Student[, 20:24] %>%
    unlist() %>%
    as.vector()
  result <- tgt$NextStage %>%
    unlist() %>%
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})
