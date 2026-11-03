library(exametrika)

# Adjacency-matrix graph utilities (R/00_graph_base.R) against igraph, which
# the package still imports for its igraph-object inputs and outputs.

random_adj <- function(n, p, seed) {
  set.seed(seed)
  m <- matrix(rbinom(n * n, 1, p), n, n)
  diag(m) <- 0
  dimnames(m) <- list(paste0("V", 1:n), paste0("V", 1:n))
  m
}

test_that("is_dag_base() and is_connected_base() agree with igraph", {
  for (seed in 1:40) {
    n <- sample(2:9, 1)
    m <- random_adj(n, runif(1, 0.05, 0.5), seed)
    g <- igraph::graph_from_adjacency_matrix(m)
    expect_equal(exametrika:::is_dag_base(m), igraph::is_dag(g), info = paste("seed", seed))
    expect_equal(
      exametrika:::is_connected_base(m),
      igraph::is_connected(g, mode = "weak"),
      info = paste("seed", seed)
    )
  }
  # upper-triangular matrices (the GA search space) are always acyclic
  m <- random_adj(8, 0.5, 99)
  m[lower.tri(m)] <- 0
  expect_true(exametrika:::is_dag_base(m))
  # a self-loop is a cycle
  m2 <- m
  m2[2, 2] <- 1
  expect_false(exametrika:::is_dag_base(m2))
  expect_true(exametrika:::is_connected_base(matrix(0, 1, 1)))
  expect_false(exametrika:::is_connected_base(matrix(0, 2, 2)))
})

test_that("adj_from_edges() matches the igraph route and rejects unknown labels", {
  labels <- paste0("Item", sprintf("%02d", 1:6))
  edges <- data.frame(From = c("Item01", "Item02", "Item05"), To = c("Item03", "Item03", "Item06"))
  adj <- exametrika:::adj_from_edges(edges, labels)
  via_igraph <- exametrika:::fill_adj(igraph::graph_from_data_frame(edges), labels)
  expect_equal(adj, via_igraph)
  expect_equal(dimnames(adj), list(labels, labels))
  expect_equal(sum(adj), 3)
  expect_error(
    exametrika:::adj_from_edges(data.frame(From = "Item01", To = "Item99"), labels),
    "do not align"
  )
  empty <- exametrika:::adj_from_edges(edges[0, ], labels)
  expect_equal(sum(empty), 0)
})

test_that("edges_from_adj() reproduces igraph's edge order", {
  m <- random_adj(7, 0.4, 7)
  mine <- exametrika:::edges_from_adj(m)
  theirs <- igraph::as_data_frame(igraph::graph_from_adjacency_matrix(m))
  expect_equal(mine$from, theirs$from)
  expect_equal(mine$to, theirs$to)
})

test_that("fill_adj() accepts a named matrix and aligns it to the labels", {
  labels <- paste0("I", 1:5)
  small <- matrix(0, 2, 2, dimnames = list(c("I4", "I2"), c("I4", "I2")))
  small["I4", "I2"] <- 1
  adj <- exametrika:::fill_adj(small, labels)
  expect_equal(dim(adj), c(5, 5))
  expect_equal(adj["I4", "I2"], 1)
  expect_equal(sum(adj), 1)
  expect_error(exametrika:::fill_adj(matrix(0, 2, 2), labels), "row and column names")
})

test_that("BNM() gives the same result from a matrix, a file and an igraph object", {
  dat <- dataFormat(J5S10)
  labs <- dat$ItemLabel
  adj <- matrix(0, 5, 5, dimnames = list(labs, labs))
  adj[1, 2] <- adj[2, 3] <- adj[1, 4] <- 1
  f <- tempfile(fileext = ".csv")
  write.csv(exametrika:::edges_from_adj(adj), f, row.names = FALSE, quote = FALSE)
  b1 <- suppressMessages(BNM(dat, adj_matrix = adj))
  b2 <- suppressMessages(BNM(dat, adj_file = f))
  b3 <- suppressMessages(BNM(dat, g = igraph::graph_from_adjacency_matrix(adj)))
  expect_equal(b1$adj, b2$adj)
  expect_equal(b1$adj, b3$adj)
  expect_equal(b1$TestFitIndices, b2$TestFitIndices)
  expect_equal(b1$TestFitIndices, b3$TestFitIndices)
  expect_s3_class(b1$g, "igraph")
})
