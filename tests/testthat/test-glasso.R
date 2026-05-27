# Tests for Graphical Lasso

### Setup (full J15S3810 for numerical stability)
tmp <- J15S3810
S_test <- PolychoricCorrelationMatrix(tmp)


test_that("soft_thresholding works correctly", {
  expect_equal(soft_thresholding(0.5, 0.3), 0.2)
  expect_equal(soft_thresholding(-0.5, 0.3), -0.2)
  expect_equal(soft_thresholding(0.2, 0.3), 0)
  expect_equal(soft_thresholding(c(0.5, -0.5, 0.2), 0.3), c(0.2, -0.2, 0))
})


test_that("glasso_one at lambda = 0 matches solve(S)", {
  res <- glasso_one(S_test, lambda = 0, eps = 1e-8, max_iter = 200)
  expect_true(res$converged)
  expect_equal(res$Theta, unname(solve(S_test)), tolerance = 1e-4)
})


test_that("glasso_one at large lambda yields diagonal Theta", {
  big_lambda <- max(abs(S_test - diag(diag(S_test)))) * 2
  res <- glasso_one(S_test, lambda = big_lambda)
  off_diag <- res$Theta[upper.tri(res$Theta)]
  expect_true(all(abs(off_diag) < 1e-6))
})


test_that("glasso_one returns expected structure", {
  res <- glasso_one(S_test, lambda = 0.1)
  expect_named(res, c("Theta", "W", "Beta", "niter", "converged"))
  expect_true(is.matrix(res$Theta))
  expect_true(is.logical(res$converged))
})


test_that("compute_EBIC_glasso returns scalar", {
  Theta_hat <- glasso_one(S_test, lambda = 0.1)$Theta
  ebic <- compute_EBIC_glasso(S_test, Theta_hat, n = 200, p = 15, gamma = 0.5)
  expect_length(ebic, 1)
  expect_true(is.numeric(ebic))
})


test_that("Glasso returns expected structure", {
  res <- Glasso(tmp, n_lambda = 10)
  expect_s3_class(res, "exametrika")
  expect_named(res, c("theta", "W", "lambda_opt", "gamma", "ebic_opt", "n_edge", "path"))
  expect_true(is.matrix(res$theta))
  expect_equal(dim(res$theta), c(15, 15))
})


test_that("Glasso theta is symmetric", {
  res <- Glasso(tmp, n_lambda = 10)
  expect_lt(max(abs(res$theta - t(res$theta))), 1e-8)
})


test_that("Glasso path has the expected length", {
  res <- Glasso(tmp, n_lambda = 10)
  expect_equal(nrow(res$path), 10)
  expect_named(res$path, c("lambda", "ebic", "n_edge"))
})


test_that("Glasso rejects non-ordinal data", {
  binary_data <- J5S10
  expect_error(Glasso(binary_data))
})


# --- Divergence handling (v1.14.0) ------------------------------
# When the BCD inner loop produces NaN/Inf (typical for N << p with very
# small lambda), glasso_one() should return converged = FALSE rather than
# crashing on `if (diff < eps)`, and Glasso() should warn + break the
# lambda search while returning the best solution found so far.

test_that("glasso_one returns converged = FALSE when BCD diverges", {
  # Inject NaN into S directly to force BCD to produce NaN values.
  # Previously this crashed at `if (diff < eps)` with
  # "missing value where TRUE/FALSE needed".
  S_bad <- S_test
  S_bad[1, 2] <- NaN
  S_bad[2, 1] <- NaN
  res <- glasso_one(S_bad, lambda = 0.1, max_iter = 20)
  expect_named(res, c("Theta", "W", "Beta", "niter", "converged"))
  expect_false(res$converged)
})


test_that("Glasso early-break wiring: warn + best solution + NA path", {
  # End-to-end check that the divergence machinery on the Glasso() side
  # is wired up: when `glasso_one()` reports `converged = FALSE` mid-grid,
  # Glasso() must (a) emit a warning, (b) break the lambda loop, and
  # (c) preserve NA in `path$ebic` for the skipped tail.
  #
  # We exercise this through the public API by monkey-patching
  # `glasso_one` to flip `converged = FALSE` on the third lambda.
  ns <- asNamespace("exametrika")
  orig <- ns$glasso_one
  call_count <- 0L
  patched <- function(...) {
    call_count <<- call_count + 1L
    out <- orig(...)
    if (call_count == 3L) out$converged <- FALSE
    out
  }
  unlockBinding("glasso_one", ns)
  on.exit({
    assign("glasso_one", orig, envir = ns)
    lockBinding("glasso_one", ns)
  })
  assign("glasso_one", patched, envir = ns)

  expect_warning(
    res <- Glasso(J15S3810, n_lambda = 10),
    "diverged"
  )
  # A valid best solution must still be returned.
  expect_false(is.null(res$theta))
  expect_true(is.finite(res$ebic_opt))
  expect_true(is.finite(res$lambda_opt))
  # The path tail past the breakpoint must be NA.
  expect_true(any(is.na(res$path$ebic)))
})
