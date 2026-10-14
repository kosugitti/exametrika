# Helper functions for exametrika test comparisons
# This file is automatically sourced by testthat before test execution.

#' Load a Mathematica reference CSV fixture
#'
#' @param filename CSV filename (without path)
#' @return data.frame with check.names = FALSE
load_ref <- function(filename) {
  read.csv(
    test_path("fixtures", "mathematica_reference", filename),
    check.names = FALSE
  )
}

#' Load an auxiliary data CSV fixture
#'
#' @param filename CSV filename (without path)
#' @return data.frame with check.names = FALSE
load_aux <- function(filename) {
  read.csv(
    test_path("fixtures", "auxiliary_data", filename),
    check.names = FALSE
  )
}

#' Compare matrices with cleaned row/col names
#'
#' Strips rownames and colnames from both matrices before comparison.
#' @param actual Actual matrix or data.frame
#' @param expected Expected matrix or data.frame
#' @param tolerance Numeric tolerance (default 1e-4)
expect_matrix_equal <- function(actual, expected, tolerance = 1e-4) {
  actual <- as.matrix(actual)
  expected <- as.matrix(expected)
  rownames(actual) <- rownames(expected) <- NULL
  colnames(actual) <- colnames(expected) <- NULL
  expect_equal(actual, expected, tolerance = tolerance)
}

#' Compare numeric vectors extracted from data structures
#'
#' Unlist and convert both values to numeric vectors before comparison.
#' @param actual Actual value (vector, list, or data.frame)
#' @param expected Expected value (vector, list, or data.frame)
#' @param tolerance Numeric tolerance (default 1e-4)
expect_numeric_equal <- function(actual, expected, tolerance = 1e-4) {
  actual <- actual |>
    unlist() |>
    unname() |>
    as.numeric()
  expected <- expected |>
    unlist() |>
    unname() |>
    as.numeric()
  expect_equal(actual, expected, tolerance = tolerance)
}

#' Take the first n rows of formatted data
#'
#' Structural claims (class, dimensions, sums to one, reproducibility) do not
#' depend on the number of rows, so the blocks that run on CRAN use this to
#' stay small. The metadata (response.type, categories, CA) is kept.
head_rows_dat <- function(x, n) {
  x$ID <- x$ID[seq_len(n)]
  for (f in c("Q", "U", "Z")) {
    if (!is.null(x[[f]])) x[[f]] <- x[[f]][seq_len(n), , drop = FALSE]
  }
  return(x)
}
