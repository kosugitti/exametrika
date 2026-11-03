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

test_that("the network layout gives every item its own position", {
  # Nine items, the shape of the checklist DAG that exposed the overlap
  adj <- matrix(0, 9, 9, dimnames = list(paste0("I", 1:9), paste0("I", 1:9)))
  adj["I1", "I3"] <- adj["I3", "I5"] <- adj["I1", "I4"] <- adj["I4", "I5"] <- 1
  adj["I2", "I6"] <- adj["I6", "I7"] <- adj["I5", "I8"] <- adj["I7", "I9"] <- 1
  lay <- exametrika:::dag_layout_base(adj)
  expect_equal(nrow(lay), 9)
  expect_equal(anyDuplicated(paste(round(lay$x, 8), round(lay$y, 8))), 0)
  # parents sit above their children
  edges <- which(adj == 1, arr.ind = TRUE)
  expect_true(all(lay$y[edges[, 1]] > lay$y[edges[, 2]]))
  # sources on the top layer
  expect_equal(unname(lay$layer[c(1, 2)]), c(0, 0))
  # an edgeless graph is one layer with distinct positions
  lay0 <- exametrika:::dag_layout_base(adj * 0)
  expect_equal(anyDuplicated(paste(round(lay0$x, 8), round(lay0$y, 8))), 0)
  # a cycle does not hang
  cyc <- adj * 0
  cyc["I1", "I2"] <- cyc["I2", "I3"] <- cyc["I3", "I1"] <- 1
  expect_equal(nrow(exametrika:::dag_layout_base(cyc)), 9)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(exametrika:::plot_dag(adj, main = "test"))
  expect_no_error(exametrika:::plot_dag(adj,
    edge_labels = data.frame(From = "I1", To = "I3", Field = "Field01")
  ))
})
