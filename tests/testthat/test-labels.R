# Respondent IDs and item labels must survive into the output of every model.
#
# Before 2.1.0 they survived only where the matrix a field was derived from
# happened to carry dimnames, so which fields were labelled differed by model:
# ordinal biclustering lost them entirely, and `ClassEstimated` was unnamed
# everywhere. The tests below pin the contract rather than the accident.

# 12 items with jittered difficulty: 8 items leave the score distribution
# lumpy enough for stanine to warn about merged boundaries.
make_binary <- function(n = 150, j = 12, seed = 1) {
  set.seed(seed)
  theta <- rnorm(n)
  beta <- seq(-1.2, 1.2, length.out = j) + rnorm(j, sd = 0.15)
  p <- outer(theta, beta, function(a, b) 1 / (1 + exp(-(a - b))))
  u <- matrix(rbinom(n * j, 1, p), nrow = n)
  colnames(u) <- paste0("Q_", sprintf("%02d", seq_len(j)))
  dataFormat(data.frame(ID = paste0("S_", sprintf("%03d", seq_len(n))), u))
}

make_ordinal <- function(n = 150, j = 12, ncat = 4, seed = 2) {
  set.seed(seed)
  theta <- rnorm(n)
  q <- matrix(0L, nrow = n, ncol = j)
  for (jj in seq_len(j)) {
    cuts <- sort(rnorm(ncat - 1, mean = jj / j - 0.5, sd = 0.6))
    q[, jj] <- as.integer(cut(theta + rnorm(n, sd = 0.7),
      breaks = c(-Inf, cuts, Inf), labels = FALSE
    ))
  }
  colnames(q) <- paste0("Q_", sprintf("%02d", seq_len(j)))
  dataFormat(data.frame(ID = paste0("S_", sprintf("%03d", seq_len(n))), q))
}

expect_student_labels <- function(fit, dat, fields) {
  for (nm in fields) {
    v <- fit[[nm]]
    if (is.null(v)) next
    labs <- if (is.matrix(v) || is.data.frame(v)) rownames(v) else names(v)
    expect_identical(labs, dat$ID, info = paste("student labels missing on", nm))
  }
}

expect_item_labels <- function(fit, dat, fields) {
  for (nm in fields) {
    v <- fit[[nm]]
    if (is.null(v)) next
    labs <- if (is.matrix(v) || is.data.frame(v)) rownames(v) else names(v)
    expect_identical(labs, dat$ItemLabel, info = paste("item labels missing on", nm))
  }
}

test_that("dataFormat puts IDs on the rows and item labels on the columns", {
  d <- make_binary()
  expect_identical(rownames(d$U), d$ID)
  expect_identical(colnames(d$U), d$ItemLabel)
  expect_identical(rownames(d$Z), d$ID)
})

test_that("binary biclustering labels both axes", {
  d <- make_binary()
  fit <- Biclustering(d, ncls = 3, nfld = 3, method = "B", verbose = FALSE)
  expect_student_labels(fit, d, c("ClassEstimated", "ClassMembership", "Students"))
  expect_item_labels(fit, d, c("FieldEstimated", "FieldMembership"))
  expect_identical(colnames(fit$FieldMembership), paste0("Field", 1:3))
  expect_identical(colnames(fit$ClassMembership), paste0("Class", 1:3))
})

test_that("ordinal biclustering labels both axes", {
  # This is the case that lost every label before 2.1.0.
  d <- make_ordinal()
  fit <- Biclustering(d, ncls = 3, nfld = 3, method = "R", verbose = FALSE)
  expect_student_labels(fit, d, c("ClassEstimated", "ClassMembership", "Students"))
  expect_item_labels(fit, d, c("FieldEstimated", "FieldMembership"))
})

test_that("ranklustering uses Rank, not Class, for the membership columns", {
  d <- make_binary()
  fit <- Biclustering(d, ncls = 3, nfld = 3, method = "R", verbose = FALSE)
  expect_identical(colnames(fit$ClassMembership), paste0("Rank", 1:3))
})

test_that("LCA and LRA label the respondent axis", {
  d <- make_binary()
  expect_student_labels(LCA(d, ncls = 3), d, c("Students", "StudentClass"))
  expect_student_labels(LRA(d, nrank = 3), d, c("Students", "StudentRank"))
})

test_that("labels are not attached to a field of the wrong length", {
  # FieldAnalysis is sorted by CRR and field, so positional labelling would be
  # wrong; it must keep the labels it inherits, in its own order.
  d <- make_binary()
  fit <- Biclustering(d, ncls = 3, nfld = 3, method = "B", verbose = FALSE)
  expect_true(all(rownames(fit$FieldAnalysis) %in% d$ItemLabel))
  expect_false(identical(rownames(fit$FieldAnalysis), d$ItemLabel))
})
