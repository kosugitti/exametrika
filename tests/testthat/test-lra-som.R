# Property tests for the SOM method.
#   SOM runs its annealing schedule to completion and has no convergence test.
#   Its numbers depend on the RNG engine, so they are not compared against the
#   Mathematica reference.

test_that("SOM runs the annealing schedule without a convergence warning", {
  expect_silent(
    res <- LRA(J15S500, nrank = 3, method = "SOM", maxiter = 10, seed = 123)
  )
  expect_true(res$converge)
  expect_equal(res$n_cycle, 10)
})

test_that("SOM is reproducible for a given seed and varies across seeds", {
  a <- LRA(J15S500, nrank = 3, method = "SOM", maxiter = 10, seed = 123)
  b <- LRA(J15S500, nrank = 3, method = "SOM", maxiter = 10, seed = 123)
  d <- LRA(J15S500, nrank = 3, method = "SOM", maxiter = 10, seed = 456)
  expect_identical(a$IRP, b$IRP)
  expect_false(identical(a$IRP, d$IRP))
})

test_that("SOM leaves the caller's RNG state untouched", {
  set.seed(42)
  before <- runif(1)
  set.seed(42)
  invisible(LRA(J15S500, nrank = 3, method = "SOM", maxiter = 10, seed = 1))
  after <- runif(1)
  expect_identical(before, after)
})

test_that("SOM with mic = TRUE returns monotonically increasing IRPs", {
  res <- LRA(J15S500, nrank = 3, method = "SOM", maxiter = 10, seed = 123, mic = TRUE)
  expect_true(all(apply(res$IRP, 1, function(r) all(diff(r) >= -1e-12))))
})

# A minimal, independently written SOM used as a reference. It follows the
# original Mathematica routine: reseed every epoch, present students one at a
# time, and apply the monotonicity sort after each student rather than at the
# end of the epoch. Ties are resolved in favour of the larger rank.
som_reference <- function(U, ncls, maxiter, seed, mic = FALSE) {
  testlength <- ncol(U)
  samplesize <- nrow(U)
  const <- exp(-testlength)

  alpha <- ((maxiter - 1:maxiter) * 1 + (1:maxiter - 1) * 0.01) / (maxiter - 1)
  sigma <- ((maxiter - 1:maxiter) * 1 + (1:maxiter - 1) * 0.12) / (maxiter - 1)
  kappa <- ((maxiter - 1:maxiter) * 0.01 + (1:maxiter - 1) * 0.0001) / (maxiter - 1)
  r <- seq(-ncls + 1, ncls - 1)
  hhhmat <- outer(seq_len(maxiter), seq_along(r), function(t, k) {
    alpha[t] * ncls / samplesize * exp(-(r[k])^2 / (2 * ncls^2 * sigma[t]^2))
  })

  ref <- matrix(rep(1:ncls / (ncls + 1), each = testlength), nrow = testlength)
  prior <- rep(1 / ncls, ncls)

  for (epoch in seq_len(maxiter)) {
    set.seed(seed + epoch)
    ord <- order(runif(samplesize, 1, 100))
    for (s in ord) {
      ll <- as.vector(U[s, ] %*% log(ref + const) +
        (1 - U[s, ]) %*% log(1 - ref + const)) + log(prior)
      winner <- max(which(ll == max(ll)))
      hhh <- matrix(
        rep(hhhmat[epoch, (ncls + 1 - winner):(2 * ncls - winner)], each = testlength),
        nrow = testlength
      )
      ref <- ref + hhh * (U[s, ] - ref)
      if (mic) {
        ref <- t(apply(ref, 1, sort))
      }
      prior <- prior + kappa[epoch] / ncls
      prior[winner] <- prior[winner] - kappa[epoch]
      prior <- pmin(pmax(prior, const), 1)
    }
  }
  return(t(ref))
}

test_that("SOM matches an independent reference implementation", {
  set.seed(17)
  U <- matrix(rbinom(8 * 5, 1, 0.5), nrow = 8, ncol = 5)
  Z <- matrix(1, nrow(U), ncol(U))

  for (mic in c(FALSE, TRUE)) {
    fit <- somclus(U, Z, ncls = 3, mic = mic, maxiter = 4, seed = 99)
    ref <- som_reference(U, ncls = 3, maxiter = 4, seed = 99, mic = mic)
    expect_equal(fit$classRefMat, ref, tolerance = 1e-12)
  }
})
