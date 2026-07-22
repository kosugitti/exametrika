#' @title Data Frame DAG to adj
#' @description inner function of return full length adj from data.frame
#' @param g igraph object
#' @param ItemLabel Labels for all items. Information should held by the
#' exametrika class data.
#' @importFrom igraph as_adjacency_matrix
#' @return adjacency matrix
#' @noRd

fill_adj <- function(g, ItemLabel) {
  testlength <- length(ItemLabel)
  tmp_adj <- as.matrix(igraph::as_adjacency_matrix(g))
  adj <- matrix(0, ncol = testlength, nrow = testlength)
  colnames(adj) <- rownames(adj) <- ItemLabel
  tmp_col_names <- colnames(tmp_adj)
  common_cols <- intersect(colnames(adj), tmp_col_names)
  adj[tmp_col_names, common_cols] <- tmp_adj[, common_cols, drop = FALSE]
  adj <- adj[sort(rownames(adj)), sort(colnames(adj))]
  return(adj)
}

#' @title Parent-response pattern counts for BNM
#' @description
#' Shared counting kernel for `BNM()` and the GA/PBIL fitness path. For each
#' item, the responses of its parent items are encoded as a big-endian binary
#' pattern index (first parent in adjacency row order = most significant bit,
#' offset by 1), and the (smoothed-free) correct/incorrect counts are
#' aggregated per (item, pattern). The parent set does not depend on the
#' subject, so the encoding is one matrix product per item instead of one
#' scalar computation per (subject, item). Missing responses are coded -1 in
#' `U`, so the indicator must be `U == 1` (a plain `U %*% w` would subtract
#' the weight instead). The correct-count aggregation deliberately reproduces
#' the historical `colSums(tmp$U * PIRP_array)` semantics by summing raw `U`
#' values (including -1 for missing) within each pattern.
#' @param tmp exametrika-formatted data
#' @param adj adjacency matrix, rows/cols aligned with the columns of `tmp$U`
#' @return list with `n1`/`n0` (item x pattern count matrices),
#' `item_pattern_max`, and `npa` (parents per item)
#' @noRd
BNM_pirp_counts <- function(tmp, adj) {
  testlength <- ncol(adj)
  nobs <- NROW(tmp$U)
  npa <- colSums(adj)
  item_pattern_max <- 2^max(npa)

  u_is1 <- tmp$U == 1
  incorrect <- tmp$Z * (1 - tmp$U)
  n_PIRP_1 <- matrix(0, nrow = testlength, ncol = item_pattern_max)
  n_PIRP_0 <- matrix(0, nrow = testlength, ncol = item_pattern_max)
  for (j in 1:testlength) {
    pa <- which(adj[, j] == 1)
    if (length(pa) == 0) {
      pos <- rep.int(1L, nobs)
    } else {
      weights <- 2^((length(pa) - 1):0)
      pos <- as.vector(u_is1[, pa, drop = FALSE] %*% weights) + 1
    }
    agg <- rowsum(cbind(tmp$U[, j], incorrect[, j]), pos)
    idx <- as.integer(rownames(agg))
    n_PIRP_1[j, idx] <- agg[, 1]
    n_PIRP_0[j, idx] <- agg[, 2]
  }
  return(list(
    n1 = n_PIRP_1, n0 = n_PIRP_0,
    item_pattern_max = item_pattern_max, npa = npa
  ))
}

#' @title Benchmark-model statistics for BIC-only fitness evaluation
#' @description
#' Precomputes the benchmark-model log-likelihood and parameter count exactly
#' as `TestFit()` does. These depend only on the data, not on the candidate
#' graph, so the GA/PBIL loops compute them once and reuse them for every
#' candidate instead of re-running the full `TestFit()`.
#' @param U response matrix (`tmp$U`)
#' @param Z missing indicator matrix (`tmp$Z`)
#' @return list with `ell_B` and `bench_nparm`
#' @noRd
BNM_bench_stats <- function(U, Z) {
  nitem <- NCOL(U)
  nobs <- NROW(U)
  const <- exp(-nitem)
  total <- rowSums(Z * U)
  totalList <- sort(unique(total))
  totalDist <- as.vector(table(total))
  ntotal <- length(totalList)
  MsG <- matrix(0, ncol = nobs, nrow = ntotal)
  for (i in 1:nobs) {
    MsG[which(total[i] == totalList), i] <- 1
  }
  U1gj <- MsG %*% (Z * U)
  U0gj <- replicate(nitem, totalDist, simplify = "matrix") - U1gj
  PjG <- U1gj / totalDist
  ell_B <- sum(colSums(U1gj * log(PjG + const) + U0gj * log(1 - PjG + const)))
  return(list(ell_B = ell_B, bench_nparm = ntotal * nitem))
}

#' @title BIC-only fitness kernel for BNM structure search
#' @description
#' Computes the BIC of a candidate adjacency matrix without any of `BNM()`'s
#' output construction (CCRR table, igraph objects, DAG checks — GA/PBIL
#' candidates live on the upper triangle, so they are always simple DAGs).
#' Reproduces `BNM()`'s numbers exactly: the adjacency matrix is sorted by
#' label first (as `fill_adj()` does), and the BIC expression matches
#' `calcFitIndices()` term by term with the cached benchmark statistics.
#' @param tmp exametrika-formatted data
#' @param adj candidate adjacency matrix with item labels as dimnames
#' @param bench cached result of `BNM_bench_stats()`
#' @param beta1 Beta prior parameter 1
#' @param beta2 Beta prior parameter 2
#' @return BIC value (identical to `BNM(...)$TestFitIndices$BIC`)
#' @noRd
BNM_fit_BIC <- function(tmp, adj, bench, beta1 = 1, beta2 = 1) {
  adj <- adj[sort(rownames(adj)), sort(colnames(adj))]
  testlength <- ncol(adj)
  nobs <- NROW(tmp$U)

  counts <- BNM_pirp_counts(tmp, adj)
  denom0 <- sign(counts$n1 + counts$n0)
  param <- beta_posterior_mode(counts$n1, counts$n0 + counts$n1, beta1, beta2)

  const <- pmin(exp(-testlength), 1e-10)
  bounded <- pmax(pmin((param * denom0 + const), 1 - const), const)
  model_loglike <- sum(counts$n1 * log(bounded) + counts$n0 * log(1 - bounded), na.rm = TRUE)
  model_nparam <- sum(denom0)

  chi_A <- 2 * (bench$ell_B - model_loglike)
  df_A <- bench$bench_nparm - model_nparam
  return(chi_A - df_A * log(nobs))
}

#' @title Posterior-mode correct-response rate under a Beta(beta1, beta2) prior
#' @description
#' Shared formula used by BNM/LD_param_est(LDLRA)/BINET for the conditional
#' correct-response rate: the posterior mode of a Binomial proportion under a
#' Beta(beta1, beta2) prior, `(count + beta1 - 1) / (total + beta1 + beta2 - 2)`,
#' where `beta1` is the prior pseudo-count on the "success"/correct side (the
#' side that appears in `count`) and `beta2` is the pseudo-count on the
#' "failure"/incorrect side. `count`/`total` may be scalars, vectors, or
#' arrays of matching shape.
#' @param count Number of correct/matching responses
#' @param total Total number of (non-missing) responses
#' @param beta1 Beta distribution parameter 1 (prior pseudo-count for `count`)
#' @param beta2 Beta distribution parameter 2 (prior pseudo-count for the
#' complement of `count`)
#' @return Posterior-mode rate, same shape as `count`/`total`. When
#' `total == 0` and `beta1 + beta2 == 2` the cell is undefined (0/0) and
#' `NaN` is returned; every caller must handle that case explicitly
#' (`BNM()` masks such cells via its `denom0` indicator and reports them as
#' "NaN(0/0)", `LDLRA()` and `BINET()` stop with an informative error).
#' @noRd
beta_posterior_mode <- function(count, total, beta1, beta2) {
  (count + beta1 - 1) / (total + beta1 + beta2 - 2)
}

#' @title Bayesian Network Model
#' @description
#' performs Bayesian Network Model with specified graph structure
#' @details
#' This function performs a Bayesian network analysis on the relationships
#' between items. This corresponds to Chapter 8 of the text. It uses the igraph
#' package for graph visualization and checking the adjacency matrix.
#' You need to provide either a graph object or a CSV file where the graph
#'  structure is specified.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param g Specify a graph object suitable for the igraph class.
#' @param adj_file specify CSV file where the graph structure is specified.
#' @param adj_matrix specify adjacency matrix.
#' @param beta1 Beta distribution parameter 1 (for correct responses). Default is 1.
#' @param beta2 Beta distribution parameter 2 (for incorrect responses). Default is 1.
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph as_adjacency_matrix
#' @importFrom utils read.csv
#' @importFrom igraph V
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{crr}{correct response ratio}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#'  \item{adj}{Adjacency matrix}\
#'  \item{param}{Learned Parameters}
#'  \item{CCRR_table}{Correct Response Rate tables}
#' }
#' @examples
#' \donttest{
#' # Create a Directed Acyclic Graph (DAG) structure for item relationships
#' # Each row represents a directed edge from one item to another
#' DAG <-
#'   matrix(
#'     c(
#'       "Item01", "Item02", # Item01 influences Item02
#'       "Item02", "Item03", # Item02 influences Item03
#'       "Item02", "Item04", # Item02 influences Item04
#'       "Item03", "Item05", # Item03 influences Item05
#'       "Item04", "Item05" # Item04 influences Item05
#'     ),
#'     ncol = 2, byrow = TRUE
#'   )
#'
#' # Convert the DAG matrix to an igraph object for network analysis
#' g <- igraph::graph_from_data_frame(DAG)
#' g
#'
#' # Create adjacency matrix from the graph
#' # Shows direct connections between items (1 for connection, 0 for no connection)
#' adj_mat <- as.matrix(igraph::as_adjacency_matrix(g))
#' print(adj_mat)
#'
#' # Fit Bayesian Network Model using the specified adjacency matrix
#' # Analyzes probabilistic relationships between items based on the graph structure
#' result.BNM <- BNM(J5S10, adj_matrix = adj_mat)
#' result.BNM
#' }
#'
#' @export

BNM <- function(U, na = NULL, Z = NULL, w = NULL,
                g = NULL, adj_file = NULL, adj_matrix = NULL,
                beta1 = 1, beta2 = 1) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (tmp$response.type != "binary") {
    response_type_error(tmp$response.type, "BNM")
  }


  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)

  # graph check
  if (is.null(g) && is.null(adj_file) && is.null(adj_matrix)) {
    stop("Specify the graph in either matrix form, CSV file, or as a graph object.")
  }
  if (!is.null(g)) {
    value <- class(g)[1]
    if (value != "igraph") {
      stop("The provided graph is not compatible with the igraph class.")
    }
  }
  if (!is.null(adj_matrix)) {
    g <- igraph::graph_from_adjacency_matrix(adj_matrix)
  }
  if (!is.null(adj_file)) {
    DAG <- read.csv(adj_file, header = TRUE)
    g <- igraph::graph_from_data_frame(DAG)
  }

  graph_label <- V(g)$name
  if (!any(graph_label %in% tmp$ItemLabel)) {
    stop("Some labels in the graph do not align with the item labels.")
  }

  # get Adj matrix
  adj <- fill_adj(g, tmp$ItemLabel)

  ### Adj mat check
  adjU <- adj + t(adj)
  simpleFLG <- ifelse(max(adjU) <= 1, 1, 0)
  testlength <- ncol(adj)
  # `adj^i` is R's elementwise power, not matrix power, so it never actually
  # tests for cycles/paths of length i. Use igraph's own DAG/connectivity
  # checks on the same adjacency matrix instead.
  adj_graph <- igraph::graph_from_adjacency_matrix(adj)
  acyclicFLG <- ifelse(igraph::is_dag(adj_graph), 1, 0)
  connectedFLG <- ifelse(igraph::is_connected(adj_graph, mode = "weak"), 1, 0)
  dag <- simpleFLG * acyclicFLG
  cdag <- dag * connectedFLG

  # Initialize
  # Parent-response pattern counts: the per-(subject, item) binary encoding
  # and the one-hot PIRP array are replaced by one matrix product and one
  # rowsum() per item inside BNM_pirp_counts() — same numbers, no S x J loop.
  counts <- BNM_pirp_counts(tmp, adj)
  npa <- counts$npa
  item_pattern_max <- counts$item_pattern_max
  n_PIRP_1 <- counts$n1
  n_PIRP_0 <- counts$n0

  denom0 <- sign(n_PIRP_1 + n_PIRP_0)

  param <- beta_posterior_mode(n_PIRP_1, n_PIRP_0 + n_PIRP_1, beta1, beta2)
  rownames(param) <- tmp$ItemLabel
  colnames(param) <- paste("PIRP", 1:ncol(param))

  # Model Fit
  const <- pmin(exp(-testlength), 1e-10)
  bounded <- pmax(pmin((param * denom0 + const), 1 - const), const)
  model_loglike <- sum(n_PIRP_1 * log(bounded) + n_PIRP_0 * log(1 - bounded), na.rm = T)
  model_nparam <- sum(denom0)
  FitIndices <- TestFit(tmp$U, tmp$Z, model_loglike, model_nparam)

  ## for output
  item_ptn <- 2^colSums(adj)
  for (j in 1:testlength) {
    for (i in 1:item_pattern_max) {
      if (i > item_ptn[j]) {
        param[j, i] <- NA
      }
    }
  }

  CCRR_table <- as.data.frame(matrix(NA, nrow = sum(item_ptn), ncol = 5))
  colnames(CCRR_table) <- c("Child Item", "N of Parents", "Parent Items", "PIRP", "Conditional CRR")
  CCRR_table[, 1] <- rep(tmp$ItemLabel, item_ptn)
  CCRR_table[, 2] <- rep(colSums(adj), item_ptn)
  parent_items <- vapply(1:testlength, function(j) {
    pa <- which(adj[, j] == 1)
    if (length(pa) == 0) {
      return("No Parents")
    }
    return(paste(colnames(tmp$U)[pa], collapse = ", "))
  }, character(1))
  CCRR_table[, 3] <- rep(parent_items, item_ptn)

  CCRR_table[, 4] <- unlist(sapply(colSums(adj), BitRespPtn))

  vec <- numeric(sum(item_ptn))
  cnt <- 0
  for (j in 1:testlength) {
    for (i in 1:item_ptn[j]) {
      cnt <- cnt + 1
      vec[cnt] <- sprintf("%.7f", as.numeric(param[j, i]))
      if (is.nan(param[j, i])) {
        vec[cnt] <- "NaN(0/0)"
      }
    }
  }
  CCRR_table[, 5] <- vec

  ret <- structure(list(
    U = U,
    testlength = testlength,
    nobs = nobs,
    crr = crr(tmp),
    ItemLabel = tmp$ItemLabel,
    adj = adj,
    g = g,
    acyclicFLG = acyclicFLG,
    connectedFLG = connectedFLG,
    param = param,
    TestFitIndices = FitIndices,
    log_lik = model_loglike,
    CCRR_table = CCRR_table
  ), class = c("exametrika", "BNM"))
  return(ret)
}

#' @title Binary pattern maker
#' @param n decimal numbers
#' @return binary patterns
#' @details
#' if n <- 1, return 0,1
#' if n <- 2, return 00,01,10,11
#' and so on.
#'

BitRespPtn <- function(n) {
  if (n == 0) {
    ptn <- "No Pattern"
  } else {
    ptn <- sapply(0:(2^n - 1), function(x) {
      binary_str <- as.integer(intToBits(x)[1:n])
      paste0(rev(binary_str), collapse = "")
    })
  }
  return(ptn)
}
