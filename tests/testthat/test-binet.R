library(exametrika)

### GOALS - Mathematica reference data
Test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter11BINET_Test.csv"),
  check.names = FALSE
)
CCRR <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter11BINET_CCRR.csv"),
  check.names = FALSE
)
LDPSR <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter11BINET_LDPSR.csv"),
  check.names = FALSE
)
MB <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter11BINET_Marginal_Bicluster.csv"),
  check.names = FALSE
)
Item <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter11BINET_Item.csv"),
  check.names = FALSE
)
Student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter11BINET_Student.csv"),
  check.names = FALSE
)

### Target
fieldFile <- test_path("fixtures", "auxiliary_data", "FixFieldBINET.csv")
edgeFile <- test_path("fixtures", "auxiliary_data", "EdgesBINET.csv")
FieldData <- read.csv(fieldFile)
conf <- FieldData[, 2]
tgt <- BINET(
  U = J35S515,
  ncls = 13, nfld = 12,
  conf = conf, adj_file = edgeFile
)

# test1 Test ------------------------------------------------------

test_that("Test Info", {
  expect <- Test[16:31, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- tgt$MG_FitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- Test[16:31, 3] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- tgt$SM_FitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


# CCRR ------------------------------------------------------

test_that("Conditional Correct Response Rate", {
  for (i in 1:12) {
    st <- (i - 1) * 14 + 1
    ed <- st + 12
    expect <- CCRR[st:ed, 2:5] |>
      unlist() |>
      unname() |>
      na.omit() |>
      as.numeric()
    result <- tgt$PSRP[[i]] |>
      unlist() |>
      as.numeric() |>
      na.omit() |>
      as.vector()
    expect_equal(result, expect, tolerance = 1e-4)
  }
})

# LDPSR ------------------------------------------------------
test_that("LDPSR", {
  expect <- LDPSR[, 1] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$LDPSR[, 1]
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- LDPSR[, 2:4] |>
    unlist() |>
    as.vector()
  result <- tgt$LDPSR[, 2:4] |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- LDPSR[, 6:10] |>
    unlist() |>
    unname() |>
    na.omit() |>
    as.numeric()
  result <- tgt$LDPSR[, 6:10] |>
    unlist() |>
    unname() |>
    na.omit() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- LDPSR[, 11:15] |>
    unlist() |>
    unname() |>
    na.omit() |>
    as.numeric()
  result <- tgt$LDPSR[, 11:15] |>
    unlist() |>
    unname() |>
    na.omit() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

# LDPSR ------------------------------------------------------
test_that("Marginal Bicluster", {
  expect <- MB[1:12, 2:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$FRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MB[1:12, 15] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$LFD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MB[13, 2:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$TRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MB[14, 2:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$LCD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- MB[15, 2:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- tgt$CMD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

# Students ------------------------------------------------------------

test_that("Students", {
  expect <- Student[, 6:18] |>
    unlist() |>
    as.vector()
  result <- tgt$Students[, 1:13] |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- Student[, 19] |>
    unlist() |>
    as.vector()
  result <- tgt$Students[, 14] |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  expect <- Student[, 20:24] |>
    unlist() |>
    as.vector()
  result <- tgt$NextStage |>
    unlist() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})

# adj_list input path -------------------------------------------------

test_that("BINET works with adj_list input", {
  # Build adj_list from the same edge data used in adj_file
  edges_data <- data.frame(
    From = c(1, 2, 3, 4, 5, 7, 2, 4, 6, 8, 10, 6, 6, 11, 8, 9, 12),
    To = c(2, 4, 5, 5, 6, 11, 3, 7, 9, 12, 12, 10, 8, 12, 12, 11, 13),
    Field = c(1, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 7, 8, 8, 9, 9, 12)
  )
  ncls <- 13
  nfld <- 12
  cls_labels <- sprintf("Class%02d", 1:ncls)

  adj_list_input <- list()
  for (i in 1:nfld) {
    adj_R <- edges_data[edges_data$Field == i, 1:2, drop = FALSE]
    adj <- matrix(0, ncol = ncls, nrow = ncls)
    colnames(adj) <- rownames(adj) <- cls_labels
    if (nrow(adj_R) > 0) {
      for (r in seq_len(nrow(adj_R))) {
        adj[adj_R$From[r], adj_R$To[r]] <- 1
      }
    }
    adj_list_input[[i]] <- adj
  }

  tgt_list <- BINET(
    U = J35S515,
    ncls = ncls, nfld = nfld,
    conf = conf, adj_list = adj_list_input
  )

  # Results should match adj_file version
  expect_equal(tgt_list$log_lik, tgt$log_lik, tolerance = 1e-8)
  expect_equal(tgt_list$FRP, tgt$FRP, tolerance = 1e-8)
  expect_equal(tgt_list$TRP, tgt$TRP, tolerance = 1e-8)
  expect_equal(tgt_list$LCD, tgt$LCD, tolerance = 1e-8)
  expect_equal(tgt_list$CMD, tgt$CMD, tolerance = 1e-8)
  expect_equal(tgt_list$all_adj, tgt$all_adj)
  expect_equal(tgt_list$Students, tgt$Students, tolerance = 1e-8)

  # all_g should have Field edge attribute
  e_df <- igraph::as_data_frame(tgt_list$all_g)
  expect_true("Field" %in% colnames(e_df))
  expect_equal(nrow(e_df), nrow(igraph::as_data_frame(tgt$all_g)))
})

# g_list input path ---------------------------------------------------

test_that("BINET works with g_list input", {
  # Build g_list from the same adj_list used above
  edges_data <- data.frame(
    From = c(1, 2, 3, 4, 5, 7, 2, 4, 6, 8, 10, 6, 6, 11, 8, 9, 12),
    To = c(2, 4, 5, 5, 6, 11, 3, 7, 9, 12, 12, 10, 8, 12, 12, 11, 13),
    Field = c(1, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 7, 8, 8, 9, 9, 12)
  )
  ncls <- 13
  nfld <- 12
  cls_labels <- sprintf("Class%02d", 1:ncls)

  g_list_input <- list()
  for (i in 1:nfld) {
    adj_R <- edges_data[edges_data$Field == i, 1:2, drop = FALSE]
    adj <- matrix(0, ncol = ncls, nrow = ncls)
    colnames(adj) <- rownames(adj) <- cls_labels
    if (nrow(adj_R) > 0) {
      for (r in seq_len(nrow(adj_R))) {
        adj[adj_R$From[r], adj_R$To[r]] <- 1
      }
    }
    g_list_input[[i]] <- igraph::graph_from_adjacency_matrix(adj, mode = "directed")
  }

  tgt_glist <- BINET(
    U = J35S515,
    ncls = ncls, nfld = nfld,
    conf = conf, g_list = g_list_input
  )

  # Results should match adj_file version
  expect_equal(tgt_glist$log_lik, tgt$log_lik, tolerance = 1e-8)
  expect_equal(tgt_glist$FRP, tgt$FRP, tolerance = 1e-8)
  expect_equal(tgt_glist$TRP, tgt$TRP, tolerance = 1e-8)
  expect_equal(tgt_glist$all_adj, tgt$all_adj)

  # all_g should have Field edge attribute
  e_df <- igraph::as_data_frame(tgt_glist$all_g)
  expect_true("Field" %in% colnames(e_df))
})
