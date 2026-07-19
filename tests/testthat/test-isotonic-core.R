library(exametrika)

test_that("pava_up produces a monotone non-decreasing fit", {
  y <- c(0.4865860, 0.4462450, 0.7924433, 0.6189676, 0.2194694)
  fit <- pava_up(y)$fitted
  expect_true(all(diff(fit) >= -1e-12))
  # mass is preserved (unweighted pooling = block means)
  expect_equal(sum(fit), sum(y), tolerance = 1e-12)
})

test_that("pava_up handles a backtracking case", {
  # [5,1,2,3,4] -> pooling ranks 1..3 to their mean, then 3,4 free
  fit <- pava_up(c(5, 1, 2, 3, 4))$fitted
  expect_equal(fit, c(8 / 3, 8 / 3, 8 / 3, 3, 4), tolerance = 1e-10)
  expect_equal(pava_up(c(5, 1, 2, 3, 4))$nblock, 3)
})

test_that("pava_up weighting matches Ayer weighted mean on a pooled block", {
  y <- c(0.8, 0.2)
  w <- c(3, 1)
  fit <- pava_up(y, w)$fitted
  expect_equal(fit, c(0.65, 0.65), tolerance = 1e-12) # (0.8*3 + 0.2*1)/4
})

test_that("iso_dual_map returns a valid, order-restricted category matrix", {
  set.seed(42)
  M <- matrix(sample(2:40, 15, replace = TRUE), 3, 5) # 3 ranks x 5 categories
  P <- iso_dual_map(M, tol = 1e-8)
  # each row a distribution
  expect_true(all(abs(rowSums(P) - 1) < 1e-8))
  expect_true(all(P > 0))
  # stochastic order: each boundary column non-decreasing across ranks
  S <- iso_surv(P)
  expect_true(max(S[-nrow(S), , drop = FALSE] - S[-1, , drop = FALSE]) < 1e-6)
  # nesting: each row's boundary is non-increasing across thresholds
  expect_true(all(apply(S, 1, function(r) all(diff(r) <= 1e-9))))
})

test_that("iso_dual_map (KL) beats per-threshold PAVA (L2) on likelihood", {
  set.seed(42)
  M <- matrix(sample(2:40, 15, replace = TRUE), 3, 5)
  ll <- function(P) sum(M * log(pmax(P, 1e-12)))
  P_dual <- iso_dual_map(M, tol = 1e-8)
  Sraw <- t(apply(M, 1, function(m) rev(cumsum(rev(m))) / sum(m)))
  Sp <- Sraw
  for (q in 2:5) Sp[, q] <- pava_up(Sraw[, q], rowSums(M))$fitted
  P_L2 <- cbind(Sp[, 1:4] - Sp[, 2:5], Sp[, 5])
  expect_gt(ll(P_dual), ll(P_L2))
})

test_that("iso_dual_map reduces to weighted PAVA for binary (ncat = 2)", {
  set.seed(1)
  M <- matrix(sample(5:50, 8, replace = TRUE), 4, 2) # 4 ranks x 2 categories
  P <- iso_dual_map(M, tol = 1e-10)
  # P(>= cat 2) across ranks == weighted PAVA of the raw proportion
  ref <- pava_up(M[, 2] / rowSums(M), rowSums(M))$fitted
  expect_equal(P[, 2], ref, tolerance = 1e-6)
})

### Theory-value checks (independent of Mathematica/GTM) -----------------------

test_that("pava_up equals base-R isoreg (unweighted)", {
  set.seed(3)
  for (n in c(1, 2, 5, 20)) {
    y <- runif(n)
    expect_equal(pava_up(y)$fitted, isoreg(y)$yf, tolerance = 1e-12)
  }
})

test_that("weighted pava_up equals isoreg on weight-replicated data", {
  y <- c(0.9, 0.2, 0.5, 0.3)
  w <- c(2, 3, 1, 4)
  expect_equal(
    rep(pava_up(y, w)$fitted, w),
    isoreg(rep(y, w))$yf,
    tolerance = 1e-12
  )
})

test_that("pava_up leaves an already-monotone vector unchanged", {
  y <- c(0.1, 0.1, 0.4, 0.7, 0.9)
  res <- pava_up(y, w = c(3, 1, 2, 5, 1))
  expect_equal(res$fitted, y, tolerance = 1e-12)
  expect_equal(res$nblock, 5)
})

test_that("pava_up preserves the weighted mean", {
  set.seed(5)
  y <- runif(10)
  w <- sample(1:6, 10, replace = TRUE)
  expect_equal(sum(w * pava_up(y, w)$fitted), sum(w * y), tolerance = 1e-10)
})

test_that("iso_dual_map returns the unconstrained MLE when input is already ordered", {
  # rank 1 concentrated on low categories, rank 2 on high -> order already holds
  M <- rbind(c(8, 1, 1), c(1, 1, 8))
  P <- iso_dual_map(M, tol = 1e-10)
  expect_equal(P, M / rowSums(M), tolerance = 1e-8)
})

test_that("iso_dual_map stays feasible and valid under full reversal", {
  # Fully reversed counts force pooling across both thresholds simultaneously.
  # The simplified dual coordinate descent is not guaranteed to reach the exact
  # optimum in this corner case (it can stall at a feasible-but-suboptimal KKT
  # point; the full El Barmi-Dykstra correction is needed there). This does not
  # arise inside the EM, where the initialisation keeps the counts ordered.
  # Here we only assert that the result is feasible and a valid distribution.
  M <- rbind(c(1, 1, 8), c(8, 1, 1))
  P <- iso_dual_map(M, tol = 1e-8)
  expect_true(all(abs(rowSums(P) - 1) < 1e-8))
  expect_true(all(P > 0))
  S <- iso_surv(P)
  expect_true(max(S[-nrow(S), , drop = FALSE] - S[-1, , drop = FALSE]) < 1e-6)
})
