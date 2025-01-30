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
  for (col_name in colnames(adj)) {
    if (col_name %in% tmp_col_names) {
      for (tmp_col in tmp_col_names) {
        adj[tmp_col, col_name] <- tmp_adj[tmp_col, col_name]
      }
    }
  }
  adj <- adj[sort(rownames(adj)), sort(colnames(adj))]
  return(adj)
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

BNM <- function(U, Z = NULL, w = NULL, na = NULL,
                g = NULL, adj_file = NULL, adj_matrix = NULL) {
  # data format
  if (class(U)[1] != "exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
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
  acyclicFLG <- 0
  connectedFLG <- 0
  for (i in 1:(testlength - 1)) {
    acyclicFLG <- acyclicFLG + sum(diag(adj^i))
    connectedFLG <- connectedFLG + min(sum(adjU^i))
  }
  acyclicFLG <- ifelse(acyclicFLG == 0, 1, 0)
  connectedFLG <- ifelse(connectedFLG > 0, 1, 0)
  dag <- simpleFLG * acyclicFLG
  cdag <- dag * connectedFLG

  # Initialize
  beta0 <- beta1 <- 1
  npa <- colSums(adj)

  pir <- lapply(1:length(npa), function(i) {
    if (npa[i] > 0) {
      mat <- as.matrix(tmp$U[, adj[, i] == 1])
      colnames(mat) <- colnames(tmp$U)[adj[, i] == 1]
      return(mat)
    } else {
      mat <- as.matrix(rep(0, nobs))
      colnames(mat) <- "No Parents"
      return(mat)
    }
  })

  PIRP_mat <- matrix(nrow = nobs, ncol = testlength)
  for (s in 1:nobs) {
    for (j in 1:testlength) {
      # For each element of the matrix, calculate the decimal value from
      # the binary representation represented by the row of the pir list
      # at the corresponding index rev() reverses the binary vector,
      # which() identifies the indices of 1s,
      # and sum(2^(indices - 1)) converts binary to decimal
      PIRP_mat[s, j] <- sum(2^(which(rev(pir[[j]][s, ]) == 1) - 1)) + 1
    }
  }

  item_pattern_max <- 2^max(npa)
  PIRP_array <- array(0, dim = c(nobs, testlength, item_pattern_max))
  for (s in 1:nobs) {
    for (j in 1:testlength) {
      PIRP_array[s, j, PIRP_mat[s, j]] <- 1
    }
  }

  n_PIRP_1 <- matrix(nrow = testlength, ncol = item_pattern_max)
  n_PIRP_0 <- matrix(nrow = testlength, ncol = item_pattern_max)
  for (i in 1:item_pattern_max) {
    n_PIRP_1[, i] <- colSums(tmp$U * PIRP_array[, , i])
    n_PIRP_0[, i] <- colSums(tmp$Z * (1 - tmp$U) * PIRP_array[, , i])
  }

  deno <- n_PIRP_0 + n_PIRP_1 + beta0 + beta1 - 2
  denom0 <- sign(n_PIRP_1 + n_PIRP_0)

  param <- (n_PIRP_1 + beta1 - 1) / deno
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
  parent_items <- lapply(pir, function(mat) {
    paste(colnames(mat), collapse = ", ")
  })
  CCRR_table[, 3] <- rep(unlist(parent_items), item_ptn)

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
    crr = crr(U),
    ItemLabel = tmp$ItemLabel,
    adj = adj,
    g = g,
    acyclicFLG = acyclicFLG,
    connectedFLG = connectedFLG,
    param = param,
    TestFitIndices = FitIndices,
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
