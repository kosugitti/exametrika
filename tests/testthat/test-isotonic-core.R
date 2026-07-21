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
  S <- iso_upper_cum(P)
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

test_that("iso_dual_map pools to the combined MLE under full reversal", {
  # Fully reversed counts violate the order at every threshold, so the
  # constrained MLE ties all boundaries -> both ranks collapse to the single
  # combined-count distribution colSums(M)/sum(M). This is the closed-form
  # optimum (and the case that exposed a stopping-criterion bug: convergence
  # must be judged on the likelihood, not the residual violation).
  M <- rbind(c(1, 1, 8), c(8, 1, 1))
  P <- iso_dual_map(M)
  pooled <- colSums(M) / sum(M) # c(0.45, 0.1, 0.45)
  expect_equal(P[1, ], pooled, tolerance = 1e-5)
  expect_equal(P[2, ], pooled, tolerance = 1e-5)
})

test_that("the C++ iso_dual_map reproduces the R reference exactly", {
  # iso_dual_map() dispatches to src/isotonic_core.cpp; iso_dual_map_ref() is
  # the pure-R original. The C++ port follows the same arithmetic operation for
  # operation and only skips rebuilding rows that a given multiplier cannot
  # touch, so the two must agree to the bit, not merely to a tolerance.
  cases <- list(
    rbind(c(1, 1, 8), c(8, 1, 1)), # full reversal, all boundaries pooled
    rbind(c(5, 25, 20), c(20, 5, 25)), # one boundary violated, one slack
    rbind(c(8, 1, 1), c(1, 1, 8)), # already ordered -> theta stays 0
    rbind(c(3, 4, 5), c(4, 4, 4), c(5, 4, 3)), # 3 ranks
    rbind(c(2, 2, 2, 2), c(1, 3, 3, 1), c(4, 1, 1, 4)) # 4 categories
  )
  for (M in cases) {
    expect_identical(
      iso_dual_map(M, maxiter = 200, tol = 1e-8),
      iso_dual_map_ref(M, maxiter = 200, tol = 1e-8)
    )
  }
})

test_that("the C++ solution is a valid order-restricted solution on a larger table", {
  # Size where the R reference is too slow to run in the test suite (~270s),
  # so check the properties directly instead of against the reference.
  set.seed(20260721)
  M <- matrix(stats::runif(120, 1, 100), nrow = 20, ncol = 6)
  P <- iso_dual_map(M, maxiter = 200, tol = 1e-7)
  expect_equal(rowSums(P), rep(1, 20), tolerance = 1e-8)
  expect_true(all(P > 0))
  S <- iso_upper_cum(P)
  # Ranks must be non-decreasing at every boundary. The dual is stopped on the
  # log-likelihood (a GEM step inside EM), so allow the small residual slack
  # that partial optimisation leaves behind.
  expect_lt(max(S[-nrow(S), , drop = FALSE] - S[-1, , drop = FALSE]), 1e-3)
})
