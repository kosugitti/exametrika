# Exact ICL (R/00_ExactICL.R) -------------------------------------------------

test_that("binary blocks reduce to the beta-binomial marginal", {
  set.seed(3)
  U <- matrix(rbinom(60, 1, 0.6), 20, 3)
  Z <- matrix(1, 20, 3)
  cls <- matrix(0, 20, 2)
  cls[cbind(1:20, rep(1:2, 10))] <- 1
  ZU <- exametrika:::binary_indicator_array(U, Z)
  r <- exametrika:::icl_exact(ZU, cls, NULL,
    alpha = 1, ncat = rep(2, 3), log_prior_cls = rep(-log(2), 2)
  )
  hand <- 0
  for (j in 1:3) {
    for (c in 1:2) {
      n1 <- sum(U[cls[, c] == 1, j])
      n <- sum(cls[, c])
      hand <- hand + lbeta(1 + n1, 1 + n - n1) - lbeta(1, 1)
    }
  }
  expect_equal(r$log_p_blocks, hand)
  expect_equal(r$log_p_assign, -20 * log(2))
  expect_equal(r$ICL_exact, -2 * (hand - 20 * log(2)))
  expect_equal(r$ICL_exact_se, 0)
})

test_that("one class and one field give a single Dirichlet-multinomial", {
  set.seed(4)
  Q <- matrix(sample(1:3, 80, replace = TRUE), 20, 4)
  ZU <- array(0, dim = c(20, 4, 3))
  for (q in 1:3) ZU[, , q] <- (Q == q) * 1
  r <- exametrika:::icl_exact(ZU, matrix(1, 20, 1), matrix(1, 4, 1),
    alpha = 1, ncat = 3, log_prior_cls = 0
  )
  n_q <- tabulate(Q, 3)
  hand <- lgamma(3) - 3 * lgamma(1) + sum(lgamma(1 + n_q)) - lgamma(3 + 80)
  expect_equal(r$log_p_blocks, hand)
  expect_equal(r$log_p_assign, 0) # -J log 1
})

test_that("the hook-length prior equals 1/C! for binary data and the particle estimate otherwise", {
  for (C in 2:8) {
    expect_equal(exametrika:::log_order_prior_uniform(C, 2), -lfactorial(C))
  }
  # Q = 4, C = 5: the hook product is 217,728,000 and (3!)^5 = 7776
  expect_equal(exametrika:::log_order_prior_uniform(5, 4), log(7776 / 217728000))
  set.seed(1)
  mc <- exametrika:::log_order_chain_mc(matrix(1, 5, 4), 20000)
  expect_lt(abs(mc$log_p - exametrika:::log_order_prior_uniform(5, 4)), 4 * mc$se)
  # Binary: the Rao-Blackwellised chain reproduces 1/C! as well
  set.seed(1)
  mb <- exametrika:::log_order_chain_mc(matrix(1, 8, 2), 20000)
  expect_lt(abs(mb$log_p + lfactorial(8)), 4 * mb$se + 0.02)
  # Non-flat prior goes through the particle method and reports an error
  set.seed(1)
  mc2 <- exametrika:::log_order_prior(3, c(2, 2), 5000)
  expect_true(is.finite(mc2$log_p) && mc2$se > 0)
})

test_that("the posterior order probability is 1 for clearly ordered counts and tiny for reversed ones", {
  cnt <- rbind(c(80, 15, 5), c(30, 40, 30), c(5, 15, 80))
  set.seed(1)
  po <- exametrika:::log_order_posterior(cnt, rep(1, 3), 2000)
  expect_equal(po$log_p, 0)
  expect_false(po$capped)
  set.seed(1)
  rev <- exametrika:::log_order_posterior(cnt[3:1, ], rep(1, 3), 2000)
  expect_lt(rev$log_p, -10)
  # The chain estimate resolves probabilities far below 1/M: ten ranks whose
  # adjacent pairs are near ties give about (1/2)^9 per boundary
  set.seed(2)
  tie <- matrix(50, 10, 2)
  po2 <- exametrika:::log_order_posterior(tie, c(1, 1), 4000)
  expect_lt(abs(po2$log_p - (-lfactorial(10))), 4 * po2$se + 0.5)
})

test_that("an isotonic fit on ordered data differs from the unordered one by the prior probability of the order", {
  # Strongly ordered binary data: three ranks, every item monotone
  set.seed(7)
  N <- 300
  rank <- rep(1:3, each = N / 3)
  p <- cbind(c(0.1, 0.5, 0.9), c(0.2, 0.5, 0.8), c(0.1, 0.4, 0.9), c(0.3, 0.6, 0.9))
  U <- sapply(1:4, function(j) rbinom(N, 1, p[rank, j]))
  colnames(U) <- paste0("I", 1:4)
  Z <- matrix(1, N, 4)
  memb <- matrix(0, N, 3)
  memb[cbind(1:N, rank)] <- 1
  ZU <- exametrika:::binary_indicator_array(U, Z)
  un <- exametrika:::icl_exact(ZU, memb, NULL, alpha = 1, ncat = rep(2, 4), log_prior_cls = rep(-log(3), 3))
  or <- exametrika:::icl_exact(ZU, memb, NULL,
    alpha = 1, ncat = rep(2, 4),
    log_prior_cls = rep(-log(3), 3), ordered = TRUE, M = 2000
  )
  # P(R | data) is about 1 for every item, so the isotonic fit is favoured by
  # -2 * 4 * log P(R) with P(R) = 1/3! per item (minus the Monte Carlo slack)
  expect_gt(or$log_p_order_post, -0.05)
  expect_equal(or$ICL_exact - un$ICL_exact, -2 * 4 * lfactorial(3) - 2 * or$log_p_order_post)
  expect_lt(or$ICL_exact_se, 0.1)
})

test_that("ICL_exact is invariant to renaming nominal categories", {
  set.seed(8)
  Q <- matrix(sample(1:3, 300, replace = TRUE), 50, 6)
  colnames(Q) <- paste0("I", 1:6)
  a <- Biclustering(dataFormat(Q, response.type = "nominal"), ncls = 2, nfld = 2, method = "B", verbose = FALSE)
  Q2 <- Q
  Q2[Q == 1] <- 3
  Q2[Q == 3] <- 1
  b <- Biclustering(dataFormat(Q2, response.type = "nominal"), ncls = 2, nfld = 2, method = "B", verbose = FALSE)
  expect_s3_class(a, "nominalBiclustering")
  expect_equal(a$ICL_exact, b$ICL_exact, tolerance = 1e-6)
})

test_that("every model returns ICL_exact, NA for GTM and SOM", {
  lca <- LCA(J15S500, ncls = 3)
  expect_true(is.finite(lca$ICL_exact))
  expect_equal(lca$ICL_exact_se, 0)
  lra <- LRA(J15S500, nrank = 3)
  expect_true(is.finite(lra$ICL_exact))
  expect_gte(lra$ICL_exact_se, 0)
  expect_true(is.na(LRA(J15S500, nrank = 3, method = "GTM")$ICL_exact))
  expect_true(is.na(LRA(J15S500, nrank = 3, method = "SOM")$ICL_exact))
  bi <- Biclustering(J15S500, ncls = 3, nfld = 3, method = "B")
  expect_true(is.finite(bi$ICL_exact))
  rk <- Biclustering(J15S500, ncls = 3, nfld = 3, method = "R")
  expect_true(is.finite(rk$ICL_exact))
  expect_true(is.na(Biclustering(J15S500, ncls = 3, nfld = 3, method = "R", estimation = "GTM")$ICL_exact))
  # Estimated class weights enter the assignment term
  est <- LCA(J15S500, ncls = 3, mixing = "estimate")
  expect_true(is.finite(est$ICL_exact))
  expect_false(isTRUE(all.equal(est$ICL_exact, lca$ICL_exact)))
  # Printing shows the row
  expect_output(print(lca), "ICL_exact")
})

test_that("ICL_exact is reproducible and leaves the global stream alone", {
  set.seed(123)
  before <- runif(1)
  set.seed(123)
  a <- LRA(J15S500, nrank = 3)
  after <- runif(1)
  expect_equal(before, after)
  b <- LRA(J15S500, nrank = 3)
  expect_equal(a$ICL_exact, b$ICL_exact)
})

test_that("ordinal models return ICL_exact", {
  skip_on_cran()
  lra <- LRA(J15S3810, nrank = 3)
  expect_true(is.finite(lra$ICL_exact))
  expect_true(is.na(LRA(J15S3810, nrank = 3, method = "GTM")$ICL_exact))
  bi <- Biclustering(J15S3810, ncls = 3, nfld = 3, method = "B")
  expect_true(is.finite(bi$ICL_exact))
  expect_equal(bi$ICL_exact_se, 0)
  rk <- Biclustering(J15S3810, ncls = 3, nfld = 3, method = "R")
  expect_true(is.finite(rk$ICL_exact))
})

test_that("GridSearch accepts index = 'ICL_exact'", {
  skip_on_cran()
  gs <- GridSearch(J15S500, fun = "LRA", max_ncls = 4, index = "ICL_exact", verbose = FALSE)
  expect_true(gs$optimal_ncls %in% 2:4)
})

test_that("the BIC-type ICL uses the model's mixing weights and log n", {
  set.seed(5)
  L <- matrix(rnorm(60), 20, 3)
  post <- exp(L) / rowSums(exp(L))
  vb <- exametrika:::vb_fit_single(L, post, nparam = 6)
  cls01 <- exametrika:::hard_membership(post)
  expect_equal(vb$ICL, -2 * (sum(cls01 * L) - 20 * log(3)) + 6 * log(20))
  expect_equal(vb$BIC_elbo, -2 * vb$elbo + 6 * log(20))
  w <- c(0.5, 0.3, 0.2)
  vb2 <- exametrika:::vb_fit_single(L, post, nparam = 6, log_prior = log(w), n_weight_params = 2)
  expect_equal(vb2$ICL, -2 * (sum(cls01 * L) + sum(colSums(cls01) * log(w))) + 8 * log(20))
})

# A fully reversed item: rank 1 answers the top category, rank 3 the bottom
# one, so the posterior order probability is about exp(-321)
reversed_item <- function() {
  N <- 300
  ZU <- array(0, dim = c(N, 1, 3))
  rank <- rep(1:3, each = 100)
  ZU[cbind(seq_len(N), 1, c(3, 2, 1)[rank])] <- 1
  clsmemb <- matrix(0, N, 3)
  clsmemb[cbind(seq_len(N), rank)] <- 1
  return(list(ZU = ZU, clsmemb = clsmemb))
}

test_that("importance sampling matches the lattice value of the order probability", {
  # Reference values: P(S_1 <= ... <= S_C) of independent Dirichlet ranks by
  # numerical integration on a 2000 x 2000 lattice (Q = 3), stable to 1e-5
  # between lattice sizes 1000 and 2000
  a_mild <- rbind(c(30, 40, 30), c(35, 35, 30), c(25, 40, 35), c(20, 40, 40))
  a_small <- rbind(c(3, 5, 2), c(6, 2, 2), c(2, 3, 5))
  r1 <- exametrika:::with_private_seed(1, exametrika:::log_order_posterior(a_mild - 1, 1, 10000))
  r2 <- exametrika:::with_private_seed(1, exametrika:::log_order_posterior(a_small - 1, 1, 10000))
  expect_lt(abs(r1$log_p - (-3.010895)), 0.02)
  expect_lt(abs(r2$log_p - (-3.064148)), 0.02)
  expect_lt(r1$se, 0.05)
  expect_false(r1$capped)
})

test_that("a fully reversed item is no longer capped", {
  # The particle method used before 2.3.0 accepted no draw here and returned
  # a bound near log(1 / M) per step. Importance sampling reaches the region;
  # the lattice value is -321.0 (lattices of 1000, 2000, 4000: -320.88,
  # -320.95, -320.97). The log of the estimate is biased downwards in so
  # extreme a case, by about 1.5 even with 10^6 draws per batch.
  d <- reversed_item()
  res <- expect_silent(exametrika:::icl_exact(d$ZU, d$clsmemb,
    alpha = 1, ncat = 3, log_prior_cls = rep(-log(3), 3), ordered = TRUE
  ))
  expect_true(is.finite(res$ICL_exact_se))
  expect_lt(abs(res$log_p_order_post - (-321.0)), 3)
})

test_that("a bound names the rows whose counts run against the order", {
  # When every importance-sampling batch comes back empty the particle method
  # takes over, and its capped value is a bound
  d <- reversed_item()
  local_mocked_bindings(
    log_order_is = function(a, M, batches = 20) {
      return(list(log_p = -Inf, se = NA_real_, capped = FALSE, n_zero = batches))
    }
  )
  expect_warning(
    res <- exametrika:::icl_exact(d$ZU, d$clsmemb,
      alpha = 1, ncat = 3, log_prior_cls = rep(-log(3), 3), ordered = TRUE, M = 2000
    ),
    "item 1 \\(log10 P <= -[0-9.]+\\): the block counts of the hard assignment run against the order"
  )
  expect_true(is.na(res$ICL_exact_se))
  expect_true(is.finite(res$ICL_exact))
})

test_that("empty batches are reported without making a bound", {
  d <- reversed_item()
  local_mocked_bindings(
    log_order_is = function(a, M, batches = 20) {
      return(list(log_p = -300, se = 0.1, capped = FALSE, n_zero = 3L))
    }
  )
  expect_warning(
    res <- exametrika:::icl_exact(d$ZU, d$clsmemb,
      alpha = 1, ncat = 3, log_prior_cls = rep(-log(3), 3), ordered = TRUE
    ),
    "fewer importance-sampling batches than requested in item 1 \\(3 of 20\\)"
  )
  expect_equal(res$log_p_order_post, -300)
  expect_true(is.finite(res$ICL_exact_se))
})
