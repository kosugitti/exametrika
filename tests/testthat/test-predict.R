# predict(): one E-step with the fitted parameters held fixed.
# On the data the model was fitted to it must reproduce the stored membership,
# which holds only because every model now returns the posterior under its
# final parameters (2.2.0).

memb_of <- function(fit, C) {
  M <- if (!is.null(fit$ClassMembership)) fit$ClassMembership else fit$Students[, seq_len(C)]
  return(unname(as.matrix(M)))
}

expect_reproduces <- function(fit, data) {
  p <- predict(fit, data)
  C <- NCOL(p$Membership)
  expect_equal(unname(p$Membership), memb_of(fit, C), tolerance = 1e-10)
  expect_equal(unname(p$Estimate), max.col(memb_of(fit, C), ties.method = "first"))
  expect_true(all(is.finite(p$log_lik)))
  return(invisible(p))
}

test_that("predict reproduces the fitted membership (binary)", {
  expect_reproduces(LCA(J5S10, ncls = 2, verbose = FALSE), J5S10)
  expect_reproduces(suppressMessages(LRA(J15S500, nrank = 3, verbose = FALSE)), J15S500)
  expect_reproduces(Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE), J15S500)
})

test_that("predict reproduces the fitted membership (polytomous)", {
  skip_on_cran()
  expect_reproduces(suppressMessages(LCA(J5S1000, ncls = 3, verbose = FALSE)), J5S1000)
  expect_reproduces(LRA(J5S1000, nrank = 3, verbose = FALSE), J5S1000)
  expect_reproduces(suppressMessages(LCA(J21S300, ncls = 3, verbose = FALSE)), J21S300)
  expect_reproduces(suppressMessages(LRA(J21S300, nrank = 3, verbose = FALSE)), J21S300)
  expect_reproduces(Biclustering(J5S1000, ncls = 3, nfld = 2, method = "R", nstart = 0, verbose = FALSE), J5S1000)
  expect_reproduces(suppressMessages(Biclustering(J20S600, ncls = 3, nfld = 2, verbose = FALSE)), J20S600)
  expect_reproduces(suppressMessages(Biclustering(J21S300, ncls = 3, nfld = 2, verbose = FALSE)), J21S300)
})

test_that("predict takes a plain matrix, aligns items by name and ignores extra columns", {
  fit <- LCA(J5S10, ncls = 2, verbose = FALSE)
  X <- J5S10$U
  X[J5S10$Z == 0] <- NA
  p1 <- predict(fit, J5S10)
  p2 <- predict(fit, cbind(extra = 1, X[, rev(colnames(X))]))
  expect_equal(unname(p2$Membership), unname(p1$Membership))
  expect_error(predict(fit, X[, -1]), "lacks the item")
})

test_that("predict leaves missing responses out of the likelihood", {
  fit <- LCA(J5S10, ncls = 2, verbose = FALSE)
  X <- J5S10$U[1:3, ]
  X[1, ] <- NA
  p <- predict(fit, X)
  expect_equal(unname(p$Membership[1, ]), c(0.5, 0.5))
  expect_equal(unname(p$log_lik[1]), 0)
  X2 <- J5S10$U[1:3, ]
  X2[2, 1] <- -99
  p2 <- predict(fit, X2, na = -99)
  X3 <- J5S10$U[1:3, ]
  X3[2, 1] <- NA
  expect_equal(p2$Membership, predict(fit, X3)$Membership)
})

test_that("predict scores a category the fit never saw as missing, with a warning", {
  set.seed(1)
  Q <- matrix(sample(1:3, 40 * 4, replace = TRUE), ncol = 4)
  colnames(Q) <- paste0("V", 1:4)
  fit <- suppressMessages(LCA(dataFormat(Q, response.type = "ordinal"), ncls = 2, verbose = FALSE))
  new <- Q[1:2, ]
  new[1, 1] <- 5
  expect_warning(p <- predict(fit, new), "V1: 5")
  ref <- Q[1:2, ]
  ref[1, 1] <- NA
  expect_equal(p$Membership, predict(fit, ref)$Membership)
})

test_that("predict keeps the per-item category map of item-level fits", {
  # Item V1 never uses category 2, so the fit renumbers its codes 1, 3 to 1, 2;
  # new data must be read through the same map, not by position
  set.seed(2)
  Q <- matrix(sample(1:3, 60 * 4, replace = TRUE), ncol = 4)
  Q[Q[, 1] == 2, 1] <- 3
  colnames(Q) <- paste0("V", 1:4)
  fit <- suppressWarnings(suppressMessages(LCA(dataFormat(Q, response.type = "ordinal"),
    ncls = 2, verbose = FALSE
  )))
  expect_equal(fit$CategoryMap[[1]], c(1, 3))
  expect_reproduces(fit, Q)
})

test_that("predict refuses unsupported models", {
  fit <- IRT(J5S10, model = 2, verbose = FALSE)
  expect_error(predict(fit, J5S10), "not available")
})
