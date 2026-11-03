# Graph utilities on adjacency matrices, without igraph.
# Rows are parents, columns are children; a nonzero entry is an edge.
# These replace igraph calls in the network models (BNM, LDLRA, LDB, BINET)
# for structure checks and for the conversion between edge lists and
# adjacency matrices (2.3.0, step 1 of dropping the igraph dependency).

#' @title Layers of a directed graph by Kahn's algorithm
#' @description
#' Removes every current source at once and records the pass in which each
#' node was removed, which is the length of the longest path from a source.
#' Nodes left over belong to a cycle; they are released one at a time (the
#' node with the fewest open parents first) so that the layering still
#' covers every node.
#' @param adj Square adjacency matrix
#' @return List with `layer` (integer vector, 0-based) and `acyclic`
#' @noRd
kahn_layers <- function(adj) {
  adj <- (as.matrix(adj) != 0) * 1
  n <- nrow(adj)
  if (n == 0) {
    return(list(layer = integer(0), acyclic = TRUE))
  }
  diag(adj) <- 0
  layer <- rep(NA_integer_, n)
  indeg <- colSums(adj)
  remaining <- rep(TRUE, n)
  acyclic <- TRUE
  cur <- 0L
  while (any(remaining)) {
    src <- which(remaining & indeg <= 0)
    if (length(src) == 0) {
      acyclic <- FALSE
      cand <- which(remaining)
      src <- cand[which.min(indeg[cand])]
    }
    layer[src] <- cur
    remaining[src] <- FALSE
    indeg <- indeg - colSums(adj[src, , drop = FALSE])
    cur <- cur + 1L
  }
  return(list(layer = layer, acyclic = acyclic))
}

#' @title Is the directed graph acyclic?
#' @param adj Square adjacency matrix
#' @return Logical
#' @noRd
is_dag_base <- function(adj) {
  adj <- as.matrix(adj)
  if (any(diag(adj) != 0)) {
    return(FALSE)
  }
  return(kahn_layers(adj)$acyclic)
}

#' @title Is the graph weakly connected?
#' @description Breadth-first search on the undirected version.
#' @param adj Square adjacency matrix
#' @return Logical (`TRUE` for an empty or single-node graph)
#' @noRd
is_connected_base <- function(adj) {
  adj <- (as.matrix(adj) != 0)
  n <- nrow(adj)
  if (n <= 1) {
    return(TRUE)
  }
  und <- adj | t(adj)
  seen <- rep(FALSE, n)
  seen[1] <- TRUE
  frontier <- 1L
  while (length(frontier) > 0) {
    nxt <- which(colSums(und[frontier, , drop = FALSE]) > 0 & !seen)
    seen[nxt] <- TRUE
    frontier <- nxt
  }
  return(all(seen))
}

#' @title Adjacency matrix from an edge list
#' @description
#' Builds the full labels x labels matrix from a data frame whose first two
#' columns are the parent and child labels. An endpoint that is not among
#' `labels` is an error, as it was when the edge list went through igraph.
#' @param edges Data frame (or matrix) with the parent in column 1 and the
#'   child in column 2
#' @param labels Node labels, in the order the matrix should have
#' @return Square 0/1 matrix with `labels` as dimnames
#' @noRd
adj_from_edges <- function(edges, labels) {
  adj <- matrix(0, nrow = length(labels), ncol = length(labels))
  dimnames(adj) <- list(labels, labels)
  if (NROW(edges) == 0) {
    return(adj)
  }
  from <- as.character(edges[[1]])
  to <- as.character(edges[[2]])
  if (!all(c(from, to) %in% labels)) {
    stop("Some labels in the graph do not align with the item labels.")
  }
  adj[cbind(match(from, labels), match(to, labels))] <- 1
  return(adj)
}

#' @title Edge list from an adjacency matrix
#' @description Rows are scanned parent by parent, as igraph did when it
#' built a graph from the matrix, so files written from the result keep
#' their former order.
#' @param adj Square adjacency matrix with dimnames
#' @return Data frame with columns `from` and `to`
#' @noRd
edges_from_adj <- function(adj) {
  adj <- as.matrix(adj)
  labels <- rownames(adj)
  if (is.null(labels)) labels <- as.character(seq_len(nrow(adj)))
  e <- which(t(adj) != 0, arr.ind = TRUE) # t(): row-major order
  return(data.frame(
    from = labels[e[, 2]], to = labels[e[, 1]],
    stringsAsFactors = FALSE
  ))
}
