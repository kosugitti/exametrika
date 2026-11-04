# equate(): new items and respondents on the scale of a fit. The parameters
# of the fit are fixed, so when the "new" items are the fit's own items the
# result must reproduce the fit.

as_na_matrix <- function(d, binary = TRUE) {
  X <- if (binary) d$U else d$Q
  X[d$Z == 0] <- NA
  colnames(X) <- d$ItemLabel
  return(X)
}

test_that("equate reproduces a binary biclustering fit on its own items", {
  fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
  X <- as_na_matrix(J15S500)
  colnames(X)[13:15] <- c("new1", "new2", "new3")
  eq <- equate(fit, X)
  expect_s3_class(eq, "exametrikaEquate")
  expect_true(eq$converge)
  expect_equal(eq$AnchorItems, J15S500$ItemLabel[1:12])
  expect_equal(unname(eq$FieldEstimated), unname(fit$FieldEstimated[13:15]))
  expect_equal(unname(eq$Membership), unname(fit$ClassMembership), tolerance = 1e-5)
  expect_equal(unname(eq$Estimate), unname(fit$ClassEstimated))
  expect_length(eq$NewFieldLogBF, 3)
  expect_true(all(is.finite(eq$NewFieldLogBF)))
  expect_length(eq$AnchorLogBF, 12)
  expect_true(all(is.finite(eq$AnchorLogBF)))
  expect_equal(eq$NewItems$ItemLabel, c("new1", "new2", "new3"))
  expect_equal(eq$total_log_lik, sum(eq$log_lik))
  expect_output(print(eq), "New items")
  expect_output(print(eq), "anchor items of this data")
})

test_that("a reversed item stands out in the new-field diagnostic", {
  fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
  X <- as_na_matrix(J15S500)
  X2 <- cbind(X[, 1:12], copy1 = X[, 1], rev1 = 1 - X[, 1])
  eq <- equate(fit, X2)
  expect_gt(eq$NewFieldLogBF[["rev1"]], max(eq$AnchorLogBF))
  expect_lt(eq$NewFieldLogBF[["copy1"]], eq$NewFieldLogBF[["rev1"]])
})

test_that("equate accepts a subset of the fitted items as anchors and exametrikaData", {
  fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
  X <- as_na_matrix(J15S500)
  colnames(X)[13:15] <- c("new1", "new2", "new3")
  eq <- equate(fit, X[, c(1:6, 13:15)])
  expect_length(eq$AnchorItems, 6)
  expect_length(eq$AnchorLogBF, 6)
  expect_equal(NROW(eq$Membership), 500)
  eq2 <- equate(fit, suppressMessages(dataFormat(X[, c(1:6, 13:15)])))
  expect_equal(eq2$Membership, eq$Membership)
  expect_equal(eq2$NewFieldLogBF, eq$NewFieldLogBF)
})

test_that("missing responses in a new item are left out", {
  fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
  X <- as_na_matrix(J15S500)
  colnames(X)[13:15] <- c("new1", "new2", "new3")
  X[1:50, "new1"] <- NA
  eq <- equate(fit, X)
  expect_true(all(is.finite(eq$log_lik)))
  X[, "new2"] <- NA
  eq2 <- equate(fit, X)
  expect_equal(unname(eq2$FieldMembership["new2", ]), rep(0.5, 2))
  expect_error(equate(fit, cbind(X[, 1:12], bad = 2)), "coded 0/1")
})

test_that("a reference built with as_biclustering equates and predicts like the fit", {
  fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
  ref <- as_biclustering(fit$FRP, fit$FieldEstimated)
  expect_s3_class(ref, "exametrika")
  expect_equal(class(ref)[2], "BiclusteringReference")
  expect_output(print(ref), "Biclustering reference: 2 fields, 3 classes, 15 items \\(binary\\)")
  X <- as_na_matrix(J15S500)
  colnames(X)[13:15] <- c("new1", "new2", "new3")
  eq_fit <- equate(fit, X)
  eq_ref <- equate(ref, X)
  expect_equal(eq_ref$Membership, eq_fit$Membership, tolerance = 1e-10)
  expect_equal(eq_ref$NewFieldLogBF, eq_fit$NewFieldLogBF, tolerance = 1e-10)
  p <- predict(ref, J15S500)
  expect_equal(unname(p$Membership), unname(fit$ClassMembership), tolerance = 1e-10)
})

test_that("as_biclustering checks its inputs", {
  FRP <- matrix(c(0.2, 0.8, 0.3, 0.9), 2)
  expect_error(as_biclustering(FRP, c(1, 2, 1)), "named")
  expect_error(as_biclustering(FRP, c(a = 1, b = 3)), "between 1 and 2")
  expect_error(as_biclustering(FRP * 2, c(a = 1, b = 2)), "probabilities")
  bad <- array(0.5, dim = c(2, 2, 3))
  expect_error(as_biclustering(bad, c(a = 1, b = 2)), "sum to one")
  ref <- as_biclustering(FRP, factor(c(a = "2", b = "1")), msg = "Rank")
  expect_equal(unname(ref$FieldEstimated), c(2, 1))
  expect_equal(colnames(ref$FRP), c("Rank1", "Rank2"))
})

test_that("equate estimates the reference profiles of new items in LCA and LRA", {
  X <- as_na_matrix(J15S500)
  colnames(X)[13:15] <- c("new1", "new2", "new3")
  lca <- LCA(J15S500, ncls = 3, verbose = FALSE)
  eq <- equate(lca, X)
  expect_equal(dim(eq$IRP), c(3, 3))
  expect_equal(unname(eq$IRP), unname(as.matrix(lca$IRP[13:15, ])), tolerance = 1e-2)
  expect_output(print(eq), "Reference profiles of the new items")
  lra <- suppressMessages(LRA(J15S500, nrank = 3, verbose = FALSE))
  eq_iso <- equate(lra, X)
  expect_true(all(apply(eq_iso$IRP, 1, function(r) all(diff(r) >= -1e-12))))
  expect_equal(unname(eq_iso$IRP), unname(as.matrix(lra$IRP[13:15, ])), tolerance = 1e-2)
  lra_gtm <- suppressMessages(LRA(J15S500, nrank = 3, method = "GTM", verbose = FALSE))
  eq_gtm <- equate(lra_gtm, X)
  expect_equal(unname(eq_gtm$IRP), unname(as.matrix(lra_gtm$IRP[13:15, ])), tolerance = 1e-3)
})

test_that("equate refuses what it cannot do", {
  fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
  X <- as_na_matrix(J15S500)
  expect_error(equate(fit, X), "no new item")
  Y <- X
  colnames(Y) <- paste0("other", seq_len(NCOL(Y)))
  expect_error(equate(fit, Y), "shares no item")
  expect_error(equate(fit, unname(X)), "item names")
  expect_error(equate(IRT(J15S500, verbose = FALSE), X), "not available")
  som <- suppressMessages(LRA(J15S500, nrank = 3, method = "SOM", maxiter = 20, verbose = FALSE))
  expect_error(equate(som, X), "SOM")
})

test_that("equate handles the polytomous biclustering models", {
  skip_on_cran()
  X <- as_na_matrix(J5S1000, binary = FALSE)
  colnames(X)[5] <- "newA"
  fit <- Biclustering(J5S1000, ncls = 3, nfld = 2, method = "R", nstart = 0, verbose = FALSE)
  eq <- equate(fit, X)
  expect_equal(unname(eq$FieldEstimated), unname(fit$FieldEstimated[5]))
  expect_true(is.finite(eq$NewFieldLogBF))
  expect_true(all(is.finite(eq$AnchorLogBF)))
  ref <- as_biclustering(fit$FRP, fit$FieldEstimated,
    category_shift = fit$CategoryShift, ordered = TRUE, msg = "Rank"
  )
  expect_output(print(ref), "4 categories")
  eq_ref <- equate(ref, X)
  expect_equal(eq_ref$NewFieldLogBF, eq$NewFieldLogBF, tolerance = 1e-10)
  expect_equal(eq_ref$Membership, eq$Membership, tolerance = 1e-10)
  p <- predict(ref, J5S1000)
  expect_equal(unname(p$Membership), unname(fit$ClassMembership), tolerance = 1e-10)
  # A category the fit never saw is treated as missing, with a warning
  X_bad <- X
  X_bad[1, "newA"] <- 99
  expect_warning(equate(fit, X_bad), "never used")
  XN <- as_na_matrix(J21S300, binary = FALSE)
  colnames(XN)[20:21] <- c("nA", "nB")
  fit_nom <- suppressMessages(Biclustering(J21S300, ncls = 3, nfld = 2, verbose = FALSE))
  eq_nom <- equate(fit_nom, XN)
  expect_equal(unname(eq_nom$FieldEstimated), unname(fit_nom$FieldEstimated[20:21]))
  XG <- as_na_matrix(J20S600, binary = FALSE)
  colnames(XG)[19:20] <- c("gA", "gB")
  fit_gtm <- suppressMessages(Biclustering(J20S600,
    ncls = 3, nfld = 2, method = "R",
    estimation = "GTM", nstart = 0, verbose = FALSE
  ))
  eq_gtm <- equate(fit_gtm, XG)
  expect_equal(unname(eq_gtm$FieldEstimated), unname(fit_gtm$FieldEstimated[19:20]))
})

test_that("equate estimates category profiles of new items in nominal LCA and ordinal LRA", {
  skip_on_cran()
  X <- as_na_matrix(J5S1000, binary = FALSE)
  colnames(X)[5] <- "newA"
  lca <- suppressMessages(LCA(J5S1000, ncls = 3, verbose = FALSE))
  eq <- equate(lca, X)
  expect_equal(eq$ICRP$ItemLabel, rep("newA", 4))
  expect_equal(eq$ICRP$CategoryLabel, 1:4)
  expect_equal(unname(colSums(as.matrix(eq$ICRP[, 3:5]))), rep(1, 3))
  fit_rows <- lca$ICRP[lca$ICRP$ItemLabel == J5S1000$ItemLabel[5], 3:5]
  expect_equal(unname(as.matrix(eq$ICRP[, 3:5])), unname(as.matrix(fit_rows)), tolerance = 0.05)
  expect_output(print(eq), "Category reference profiles")
  lra <- LRA(J5S1000, nrank = 3, verbose = FALSE)
  eq_iso <- equate(lra, X)
  P <- as.matrix(eq_iso$ICRP[, paste0("rank", 1:3)])
  expect_equal(unname(colSums(P)), rep(1, 3))
  # Order restriction: the upper-cumulative probabilities do not fall with the rank
  upper <- apply(P, 2, function(p) rev(cumsum(rev(p)))[-1])
  expect_true(all(apply(upper, 1, function(r) all(diff(r) >= -1e-6))))
  lra_gtm <- LRA(J5S1000, nrank = 3, method = "GTM", verbose = FALSE)
  eq_gtm <- equate(lra_gtm, X)
  fit_rows <- lra_gtm$ICRP[lra_gtm$ICRP$ItemLabel == J5S1000$ItemLabel[5], paste0("rank", 1:3)]
  expect_equal(unname(as.matrix(eq_gtm$ICRP[, 3:5])), unname(as.matrix(fit_rows)), tolerance = 0.05)
  X_empty <- X
  X_empty[, "newA"] <- NA
  expect_error(equate(lca, X_empty), "without any response")
})
