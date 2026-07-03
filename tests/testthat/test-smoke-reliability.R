library(exametrika)

# Smoke tests for the reliability / item-discrimination helpers on the
# packaged J15S500 binary data: results stay inside their valid domain and
# reproduce the current reference values (loose tolerances, since these are
# smoke tests rather than the exact Mathematica-reference checks elsewhere).

test_that("AlphaCoefficient() on J15S500 is in (0,1) and stable", {
  a <- AlphaCoefficient(J15S500)
  expect_named(a, c("AlphaCov", "AlphaPhi", "AlphaTetrachoric"))
  vals <- unlist(a)
  expect_true(all(vals > 0 & vals < 1))
  # Coefficient alpha <= its tetrachoric-based counterpart here.
  expect_lt(a$AlphaCov, a$AlphaTetrachoric)
  expect_equal(a$AlphaCov, 0.6252, tolerance = 1e-3)
  expect_equal(a$AlphaPhi, 0.6304, tolerance = 1e-3)
  expect_equal(a$AlphaTetrachoric, 0.7714, tolerance = 1e-3)
})

test_that("OmegaCoefficient() on J15S500 is in (0,1) and stable", {
  o <- OmegaCoefficient(J15S500)
  expect_named(o, c("OmegaCov", "OmegaPhi", "OmegaTetrachoric"))
  vals <- unlist(o)
  expect_true(all(vals > 0 & vals < 1))
  # Omega (BFGS global optimum) is deterministic despite its runif() start.
  expect_equal(o$OmegaCov, 0.6319, tolerance = 1e-3)
  expect_equal(o$OmegaPhi, 0.6368, tolerance = 1e-3)
  expect_equal(o$OmegaTetrachoric, 0.7788, tolerance = 1e-3)
})

test_that("BiserialCorrelation() recovers a strong synthetic association", {
  set.seed(3)
  t <- rnorm(500)
  i <- as.numeric(t + rnorm(500, 0, 0.5) > 0)
  rho <- BiserialCorrelation(i, t)
  expect_true(rho >= -1 && rho <= 1)
  expect_equal(rho, 0.9179, tolerance = 1e-2)
  # Argument order does not matter (binary/continuous auto-detected).
  expect_equal(BiserialCorrelation(t, i), rho, tolerance = 1e-6)
})

test_that("BiserialCorrelation() item-total values match ITBiserial()", {
  tmp <- dataFormat(J15S500)
  total <- scale(rowSums(tmp$U * tmp$Z))[, 1] # standard-normal continuous score
  bs <- vapply(
    seq_len(ncol(tmp$U)),
    function(j) BiserialCorrelation(tmp$U[, j], total),
    numeric(1)
  )
  expect_true(all(bs >= -1 & bs <= 1))
  expect_equal(bs, unname(ITBiserial(J15S500)), tolerance = 1e-3)
})

test_that("BiserialCorrelation() rejects non binary-continuous input", {
  # Two continuous variables: neither is binary -> error.
  expect_error(BiserialCorrelation(rnorm(50), rnorm(50)), "binary")
})
