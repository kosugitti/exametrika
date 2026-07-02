library(exametrika)

# ================================================================
# IRM Gibbs sampler: C++ vs R reference implementation
# ================================================================
#
# The Rcpp implementation `irm_gibbs_core_cpp()` is a direct translation
# of the R reference `irm_gibbs_core(..., use_cpp = FALSE)`. RNG calls
# go through R-level sample.int() and rmultinom() so the unif_rand()
# consumption order matches base R.
#
# Bit-identical agreement holds for short runs (the cases tested in this
# file: <= 10 iterations on the bundled J20S600 / J35S500 datasets). On
# real polytomous data with many iterations, sub-LSB floating-point
# ordering differences inside `lmvbeta()` accumulate through the CRP
# likelihood and can flip a single `rmultinom()` outcome, after which
# the two chains follow different sample paths. The marginal posterior
# distributions remain statistically indistinguishable between the two
# paths (verified empirically; see NEWS.md v1.13.0).
#
# TODO: add a long-iteration parity test on real-data-like configurations
# to characterise where (and how reliably) the two paths diverge.

setup_state <- function(dat) {
  nitems <- NCOL(dat$Q)
  nobs <- NROW(dat$Q)
  maxQ <- max(dat$categories)
  Uq <- array(0, dim = c(nobs, nitems, maxQ))
  for (s in seq_len(nobs)) {
    for (j in seq_len(nitems)) {
      Uq[s, j, dat$Q[s, j]] <- 1
    }
  }
  mode_cat <- apply(dat$Q, 1, function(x) {
    tab <- table(x)
    as.integer(names(which.max(tab)))
  })
  ncls0 <- length(unique(mode_cat))
  cls01 <- matrix(0, nobs, ncls0)
  for (i in seq_len(nobs)) cls01[i, mode_cat[i]] <- 1
  fld01 <- diag(nitems)
  list(
    Uq = Uq, Z = dat$Z, cls01 = cls01, fld01 = fld01,
    maxQ = maxQ, nobs = nobs, nitems = nitems
  )
}

run_R <- function(s, max_iter, seed = 42) {
  set.seed(seed)
  exametrika:::irm_gibbs_core(
    Uq = s$Uq, Z = s$Z, cls01 = s$cls01, fld01 = s$fld01,
    gamma_c = 1, gamma_f = 1, alpha_vec = rep(1, s$maxQ),
    max_iter = max_iter, stable_limit = 9999, verbose = FALSE,
    use_cpp = FALSE
  )
}

run_C <- function(s, max_iter, seed = 42) {
  set.seed(seed)
  exametrika:::irm_gibbs_core(
    Uq = s$Uq, Z = s$Z, cls01 = s$cls01, fld01 = s$fld01,
    gamma_c = 1, gamma_f = 1, alpha_vec = rep(1, s$maxQ),
    max_iter = max_iter, stable_limit = 9999, verbose = FALSE,
    use_cpp = TRUE
  )
}

# nominal data
state_nom <- setup_state(dataFormat(J20S600))

test_that("nominal IRM Gibbs: C++ matches R after 1 iteration", {
  rR <- run_R(state_nom, max_iter = 1)
  rC <- run_C(state_nom, max_iter = 1)
  expect_equal(rC$ncls, rR$ncls)
  expect_equal(rC$nfld, rR$nfld)
  expect_equal(rC$cls, rR$cls)
  expect_equal(rC$fld, rR$fld)
  expect_equal(rC$Nc, rR$Nc)
  expect_equal(rC$Nf, rR$Nf)
  expect_equal(unname(as.matrix(rC$cls01)), unname(as.matrix(rR$cls01)))
  expect_equal(unname(as.matrix(rC$fld01)), unname(as.matrix(rR$fld01)))
  expect_equal(as.numeric(rC$U_fcq), as.numeric(rR$U_fcq))
})

test_that("nominal IRM Gibbs: C++ matches R after 10 iterations", {
  rR <- run_R(state_nom, max_iter = 10)
  rC <- run_C(state_nom, max_iter = 10)
  expect_equal(rC$ncls, rR$ncls)
  expect_equal(rC$nfld, rR$nfld)
  expect_equal(rC$cls, rR$cls)
  expect_equal(rC$fld, rR$fld)
  expect_equal(as.numeric(rC$U_fcq), as.numeric(rR$U_fcq))
})

test_that("nominal IRM Gibbs: C++ matches R with different seed", {
  rR <- run_R(state_nom, max_iter = 5, seed = 777)
  rC <- run_C(state_nom, max_iter = 5, seed = 777)
  expect_equal(rC$cls, rR$cls)
  expect_equal(rC$fld, rR$fld)
})

# ordinal data
state_ord <- setup_state(dataFormat(J35S500))

test_that("ordinal IRM Gibbs: C++ matches R after 1 iteration", {
  rR <- run_R(state_ord, max_iter = 1)
  rC <- run_C(state_ord, max_iter = 1)
  expect_equal(rC$ncls, rR$ncls)
  expect_equal(rC$nfld, rR$nfld)
  expect_equal(rC$cls, rR$cls)
  expect_equal(rC$fld, rR$fld)
  expect_equal(as.numeric(rC$U_fcq), as.numeric(rR$U_fcq))
})

test_that("ordinal IRM Gibbs: C++ matches R after 5 iterations", {
  rR <- run_R(state_ord, max_iter = 5)
  rC <- run_C(state_ord, max_iter = 5)
  expect_equal(rC$cls, rR$cls)
  expect_equal(rC$fld, rR$fld)
})

test_that("Biclustering_IRM nominal end-to-end (C++ default) is reproducible", {
  set.seed(42)
  r1 <- Biclustering_IRM(J20S600,
    gamma_c = 1, gamma_f = 1,
    maxiter = 30, verbose = FALSE
  )
  set.seed(42)
  r2 <- Biclustering_IRM(J20S600,
    gamma_c = 1, gamma_f = 1,
    maxiter = 30, verbose = FALSE
  )
  expect_equal(r1$n_class, r2$n_class)
  expect_equal(r1$n_field, r2$n_field)
  expect_equal(as.numeric(r1$ClassMembership), as.numeric(r2$ClassMembership))
  expect_equal(as.numeric(r1$FieldMembership), as.numeric(r2$FieldMembership))
})
