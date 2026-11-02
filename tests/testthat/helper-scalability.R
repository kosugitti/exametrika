# Data sets shared by test-scalability.R and develop/scalability_reference_mokken.R.
# Keep the two in sync: the mokken reference values were computed on exactly these.
make_scalability_cases <- function() {
  # J5S1000, complete cases, the three items with four categories
  d <- dataFormat(J5S1000)
  keep <- rowSums(d$Z) == ncol(d$Z)
  j5 <- unname(d$Q[keep, c(1, 3, 5)])

  # Six ordinal items on one latent trait, the last one reverse-keyed
  set.seed(20261102)
  n <- 200
  th <- stats::rnorm(n)
  ord <- vapply(1:6, function(j) {
    findInterval(th + stats::rnorm(n), c(-1, 0, 1)) + 1L
  }, integer(n))
  ord[, 6] <- 5L - ord[, 6]

  # The same items dichotomised
  bin <- (ord > 2) * 1L

  return(list(j5s1000 = j5, ordinal_reversed = ord, binary = bin))
}

# Data sets for ManifestMonotonicity(), shared with
# develop/monotonicity_reference_mokken.R in the same way.
make_monotonicity_cases <- function() {
  set.seed(20261103)
  n <- 400
  th <- stats::rnorm(n)
  mono <- vapply(1:6, function(j) {
    findInterval(th + stats::rnorm(n), c(-1, 0, 1)) + 1L
  }, integer(n))
  # Item 1 single-peaked in the trait
  peaked <- mono
  peaked[, 1] <- findInterval(-abs(th) * 1.5 + stats::rnorm(n, 0, 0.7), c(-2, -1, 0)) + 1L
  # Item 6 reverse-keyed
  reversed <- mono
  reversed[, 6] <- 5L - reversed[, 6]
  return(list(
    monotone = list(X = mono, minsize = NULL),
    peaked = list(X = peaked, minsize = NULL),
    peaked_small_groups = list(X = peaked, minsize = 40),
    reversed = list(X = reversed, minsize = NULL),
    binary = list(X = (peaked > 2) * 1L, minsize = NULL)
  ))
}
