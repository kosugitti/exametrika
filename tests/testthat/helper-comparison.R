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

#' 整形済みデータの先頭 n 行を取る
#'
#' 構造的な主張(クラス・次元・和が1・再現性)は行数に依存しないので，
#' CRAN で走るブロックはこれで小さくする。メタデータ(response.type,
#' categories, CA)は保たれる。
head_rows_dat <- function(x, n) {
  x$ID <- x$ID[seq_len(n)]
  for (f in c("Q", "U", "Z")) {
    if (!is.null(x[[f]])) x[[f]] <- x[[f]][seq_len(n), , drop = FALSE]
  }
  return(x)
}
