library(exametrika)

# ================================================================
# Binary IRM Gibbs sampler: C++ vs R reference implementation
# ================================================================
#
# `irm_binary_gibbs_cpp()` is a line-by-line translation of the R
# reference `irm_binary_gibbs_r()`. Every random draw is delegated to
# R (sample.int / rmultinom), so with the same seed the two must agree
# bit for bit: same class and field assignments, same number of
# iterations.

binary_initial_state <- function(U) {
  tmp <- dataFormat(U)
  tmp$U <- tmp$U * tmp$Z
  nobs <- NROW(tmp$U)
  nitems <- NCOL(tmp$U)
  pattern <- sort(unique(nrs(tmp)))
  ncls <- length(pattern)
  cls01 <- matrix(0, nrow = nobs, ncol = ncls)
  for (i in seq_len(nobs)) cls01[i, which(pattern == nrs(tmp)[i])] <- 1
  rownames(cls01) <- tmp$ID
  cls <- cls01 %*% seq_len(ncls)
  fld <- order(order(crr(tmp), decreasing = TRUE))
  fld01 <- matrix(0, nrow = nitems, ncol = nitems)
  for (i in seq_len(nitems)) fld01[i, fld[i]] <- 1
  rownames(fld01) <- tmp$ItemLabel
  Ccf <- t(cls01) %*% tmp$U %*% fld01
  Fcf <- t(cls01) %*% (tmp$Z * (1 - tmp$U)) %*% fld01
  list(
    tmp = tmp, cls01 = cls01, fld01 = fld01, cls = cls, fld = fld,
    Nc = colSums(cls01), Nf = colSums(fld01), Ccf = Ccf, Fcf = Fcf, Ncf = Ccf + Fcf
  )
}

run_binary_pair <- function(U, seed, maxiter) {
  s <- binary_initial_state(U)
  const <- 1e-100
  set.seed(seed)
  r <- exametrika:::irm_binary_gibbs_r(
    s$tmp, s$cls01, s$fld01, s$cls, s$fld, s$Nc, s$Nf, s$Ccf, s$Fcf, s$Ncf,
    gamma_c = 1, gamma_f = 1, gamp = 1, const = const,
    maxiter = maxiter, stable_limit = 9999, verbose = FALSE
  )
  draw_one <- function(ptab) {
    ptab <- round(ptab + const, digits = -log10(const))
    return(which(rmultinom(1, 1, ptab) == 1))
  }
  set.seed(seed)
  cc <- exametrika:::irm_binary_gibbs_cpp(
    s$tmp$U, s$tmp$Z, s$cls01, s$fld01, 1, 1, 1, const,
    maxiter, 9999, FALSE, draw_one, sample.int
  )
  list(r = r, cpp = cc)
}

expect_same_gibbs <- function(pair) {
  r <- pair$r
  cc <- pair$cpp
  expect_equal(cc$ncls, r$ncls)
  expect_equal(cc$nfld, r$nfld)
  expect_equal(cc$iter, r$iter)
  expect_identical(
    as.vector(unname(cc$cls01 %*% seq_len(cc$ncls))),
    as.vector(unname(r$cls01 %*% seq_len(r$ncls)))
  )
  expect_identical(
    as.vector(unname(cc$fld01 %*% seq_len(cc$nfld))),
    as.vector(unname(r$fld01 %*% seq_len(r$nfld)))
  )
}

test_that("binary IRM Gibbs: C++ matches the R reference on J15S500", {
  expect_same_gibbs(run_binary_pair(J15S500, seed = 2, maxiter = 10))
})

test_that("binary IRM Gibbs: C++ matches the R reference with missing data", {
  skip_on_cran()
  set.seed(20260527)
  U <- matrix(rbinom(300 * 20, 1, 0.55), 300, 20)
  U[sample.int(300 * 20, size = 120)] <- -1L
  dat <- dataFormat(data = U, na = -1)
  expect_same_gibbs(run_binary_pair(dat, seed = 5, maxiter = 10))
})

test_that("binary IRM Gibbs: C++ matches the R reference on a long run", {
  skip_on_cran()
  expect_same_gibbs(run_binary_pair(J35S515, seed = 123, maxiter = 30))
})

test_that("binary Biclustering_IRM() gives the same answer as the R-only version", {
  skip_on_cran()
  # reference values recorded with exametrika 2.3.0 before the C++ port
  fit <- Biclustering_IRM(J15S500, gamma_c = 1, gamma_f = 1, seed = 123, verbose = FALSE)
  expect_equal(fit$n_class, 9)
  expect_equal(fit$n_field, 5)
  expect_equal(fit$log_lik, -3384.534315, tolerance = 1e-8)
})
