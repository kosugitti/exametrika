library(exametrika)

### GOALS - Mathematica reference data
Test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter09LDLRA_Test.csv"),
  check.names = FALSE
)
Rank <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter09LDLRA_Rank.csv"),
  check.names = FALSE
)
Item <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter09LDLRA_Item.csv"),
  check.names = FALSE
)
CCRR <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter09LDLRA_CCRR.csv"),
  check.names = FALSE
)
Student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter09LDLRA_Student.csv"),
  check.names = FALSE
)

### Target
tgt <- LDLRA(J12S5000,
  ncls = 5,
  adj_file = test_path("fixtures", "auxiliary_data", "DAG_file.csv")
)

# Test ------------------------------------------------------------

test_that("Test Info", {
  expect <- Test[14:29, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- tgt$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


# Rank ------------------------------------------------------------


test_that("Rank Info", {
  expect <- Rank[1, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$TRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- Rank[2, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$LRD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- Rank[3, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$RMD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


# Item ------------------------------------------------------------
test_that("Item Info", {
  R1 <- Item[, 6:9]
  R2 <- Item[, 10:13]
  R3 <- Item[, 14:17]
  R4 <- Item[, 18:21]
  R5 <- Item[, 22:25]
  colnames(R1) <- colnames(R2) <- colnames(R3) <- colnames(R4) <- colnames(R5)
  expect <- rbind(R1, R2, R3, R4, R5) |>
    unlist() |>
    as.vector()
  result <- tgt$Estimation_table[, 3:6] |>
    unlist() |>
    as.vector()
  colnames(expect) <- colnames(result)
  expect_equal(result, expect, tolerance = 1e-4)

  expect <- Item[, 26:30] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$IRP |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- Item[, 31:36] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$IRPIndex |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})

# CCRR ------------------------------------------------------------

test_that("Conditional Correct Response Rate", {
  expect <- CCRR[, 1] |>
    unlist() |>
    as.vector()
  result <- tgt$CCRR_table$`Child Item` |> unclass()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- CCRR[, 2] |>
    unlist() |>
    as.vector()
  result <- tgt$CCRR_table$Rank |> unclass()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- CCRR[, 3] |>
    unlist() |>
    as.vector()
  result <- tgt$CCRR_table$`N of Parents` |> unclass()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- CCRR[, 4] |>
    unlist() |>
    as.vector()
  result <- tgt$CCRR_table$`Parent Items` |> unclass()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- CCRR[, 5] |>
    unlist() |>
    as.vector()
  result <- tgt$CCRR_table$PIRP |> unclass()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- CCRR[, 6] |>
    unlist() |>
    as.vector()
  result <- tgt$CCRR_table$`Conditional CRR` |> unclass()
  expect_equal(result, expect, tolerance = 1e-4)
})


# Student ---------------------------------------------------------

test_that("Students", {
  expect <- Student[, 6:13] |>
    unlist() |>
    as.vector()
  result <- tgt$Students |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})
