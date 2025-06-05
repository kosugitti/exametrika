#' @title Initialize field membership profile probabilities
#' @description
#' Calculates initial probabilities for the field membership profile matrix
#' in biclustering analysis. This function generates probability distributions
#' based on class and field effects to initialize the profile matrix.
#' @param Q Number of categories
#' @param class_effect Class effect parameter (numeric value influencing class-based probabilities)
#' @param field_effect Field effect parameter (numeric value influencing field-based probabilities)
#' @return A normalized probability vector of length Q for initializing the field membership profile matrix
#' @noRd
init_field_membership_probs <- function(Q, class_effect, field_effect) {
  q_vals <- 1:Q / Q
  high_influence <- q_vals * class_effect
  low_influence <- q_vals[order(q_vals, decreasing = TRUE)] * field_effect
  probs <- high_influence + low_influence
  return(probs / sum(probs))
}

#' @title Create filter matrix for clustering
#' @description
#' Generates a filter matrix used in clustering algorithms. The matrix is constructed
#' with diagonal elements based on the number of classes, and off-diagonal elements
#' that create a tridiagonal structure. The first and last columns are normalized.
#' @param ncls Number of classes (integer value determining the matrix dimensions)
#' @return A square filter matrix of size ncls x ncls with normalized first and last columns
#' @noRd
create_filter_matrix <- function(ncls) {
  f0 <- ifelse(ncls < 5, 1.05 - 0.05 * ncls,
    ifelse(ncls < 10, 1.00 - 0.04 * ncls,
      0.80 - 0.02 * ncls
    )
  )
  f1 <- diag(0, ncls)
  f1[row(f1) == col(f1) - 1] <- (1 - f0) / 2
  Fil <- diag(rep(f0, ncls)) + t(f1) + f1
  Fil[, 1] <- Fil[, 1] / sum(Fil[, 1])
  Fil[, ncls] <- Fil[, ncls] / sum(Fil[, ncls])

  return(Fil)
}
