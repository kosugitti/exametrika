#' @title Utility function for searching DAG
#' @description
#' Function to limit the number of parent nodes
#' @details
#' When generating an adjacency matrix using GA, the number of edges coming
#' from a single node should be limited to 2 or 3. This is because if
#' there are too many edges, it becomes difficult to interpret in practical
#' applications. This function works to adjust the sampling of the randomly
#' generated adjacency matrix so that the column sum of the upper triangular
#' elements fits within the set limit.
#' @param vec gene Vector corresponding to the upper triangular of the adjacency matrix
#' @param testlength test length. In this context it means a number of nodes.
#' @param maxParents Upper limit of number of nodes.

maxParents_penalty <- function(vec, testlength, maxParents) {
  adj_check <- matrix(0, ncol = testlength, nrow = testlength)
  adj_check[upper.tri(adj_check)] <- vec
  for (i in 2:testlength) {
    vec <- adj_check[, i]
    if (sum(vec) > maxParents) {
      ones_index <- which(vec == 1)
      selection <- sample(ones_index, maxParents)
      vec[-selection] <- 0
      adj_check[, i] <- vec
    }
  }
  vec <- adj_check[upper.tri(adj_check)]
  return(vec)
}
