# Tests for MonotonicityCheck(): iteration of the reference set and
# subscale-wise checking.

make_check_data <- function() {
  set.seed(20261104)
  n <- 400
  th1 <- stats::rnorm(n)
  th2 <- stats::rnorm(n)
  item <- function(th) findInterval(th + stats::rnorm(n), c(-1, 0, 1)) + 1L
  f1 <- vapply(1:6, function(j) item(th1), integer(n))
  f1[, 5] <- 5L - f1[, 5] # reverse-keyed
  f1[, 6] <- findInterval(-abs(th1) * 1.5 + stats::rnorm(n, 0, 0.7), c(-2, -1, 0)) + 1L # single-peaked
  f2 <- vapply(1:4, function(j) item(th2), integer(n))
  X <- cbind(f1, f2)
  colnames(X) <- c(paste0("a", 1:6), paste0("b", 1:4))
  return(X)
}

test_that("planted reverse-keyed and single-peaked items are flagged", {
  X <- make_check_data()
  fld <- rep(c("a", "b"), c(6, 4))
  mc <- suppressMessages(MonotonicityCheck(X, fields = fld))
  it <- mc$items
  flag <- stats::setNames(it$flag, it$Item)
  expect_match(flag["a5"], "reverse?", fixed = TRUE)
  expect_match(flag["a6"], "non-monotone", fixed = TRUE)
  expect_true(all(flag[c("a1", "a2", "a3", "a4", "b1", "b2", "b3", "b4")] == ""))
  expect_equal(mc$fields$Field, c("a", "b"))
  expect_gt(mc$fields$H_clean[1], mc$fields$H_all[1])
})

test_that("pooling two dimensions hides less than checking by field", {
  X <- make_check_data()
  pooled <- suppressMessages(MonotonicityCheck(X))
  expect_equal(nrow(pooled$fields), 1)
  expect_lt(pooled$fields$H_all, 0.3)
})

test_that("fields must match the items", {
  X <- make_check_data()
  expect_error(suppressMessages(MonotonicityCheck(X, fields = 1:3)), "one entry per item")
  expect_warning(
    suppressMessages(MonotonicityCheck(X, fields = rep(c("a", "b", "c"), c(6, 3, 1)))),
    "fewer than three items"
  )
})
