library(exametrika)

### GOALS - Mathematica reference data
Test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter08BNM_Test.csv"),
  check.names = FALSE
)
Item <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter08BNM_Item.csv"),
  check.names = FALSE
)
CCRR <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter08BNM_CCRR.csv"),
  check.names = FALSE
)
Student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter08BNM_Student.csv"),
  check.names = FALSE
)

### Setup
tmp <- dataFormat(J5S10)
library(igraph)
DAG <-
  matrix(
    c(
      "Item01", "Item02",
      "Item02", "Item03",
      "Item02", "Item04",
      "Item03", "Item05",
      "Item04", "Item05"
    ),
    ncol = 2, byrow = TRUE
  )
## graph object
g <- igraph::graph_from_data_frame(DAG)
## Adj mmatrix
adj_mat <- as.matrix(igraph::as_adjacency_matrix(g))
tgt <- BNM(tmp, adj_matrix = adj_mat)


# test1 Test ------------------------------------------------------

test_that("Test Info", {
  expect <- Test[12:27, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- tgt$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


# Item ------------------------------------------------------------

test_that("Item Info", {
  expect <- Item[1:5, 6:9] |>
    unlist() |>
    unname()
  expect <- expect[-10] |>
    as.numeric() |>
    na.omit() |>
    as.vector()
  result <- tgt$param |>
    as.numeric() |>
    na.omit() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})


# CCRR ------------------------------------------------------------

test_that("Conditional Correct Response Rate", {
  expect <- CCRR[, 1] |>
    unlist() |>
    unname()
  result <- tgt$CCRR[, 1]
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- CCRR[, 2] |>
    unlist() |>
    unname()
  result <- tgt$CCRR[, 2]
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- CCRR[, 3] |>
    unlist() |>
    unname()
  result <- tgt$CCRR[, 3]
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- CCRR[, 4] |>
    unlist() |>
    unname()
  result <- tgt$CCRR[, 4]
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- CCRR[, 5] |>
    unlist() |>
    unname()
  expect <- expect[-9] |>
    as.numeric()
  result <- tgt$CCRR[, 5]
  result <- result[-9] |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})
