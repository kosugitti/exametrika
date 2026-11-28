## Resubmission to fix the noLD check (2.3.1)

This patch release fixes the test failure reported by Prof. Ripley on
2026-11-28 for the 'noLD' additional check (deadline 2026-12-19).

* Three tests in `test-irm-binary-cpp.R` require the C++ binary IRM Gibbs
  sampler to agree bit for bit with the R reference implementation. The C++
  code accumulates sums in long double to reproduce R's `sum()`; on an R
  built with long double disabled `sum()` works in double, the rounding
  differs and the two chains diverge. These tests are now skipped when
  `.Machine$sizeof.longdouble` is 0. Estimation is unchanged.
* No other changes.

## Test environments

* local macOS (aarch64-apple-darwin), R 4.6.2: 0 errors, 0 warnings, 0 notes
* R-hub v2 (GitHub Actions): nold, Linux, macOS and Windows on R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

ggExametrika (the only reverse dependency on CRAN) is not affected: no
exported code changed.

The previous release (2.3.0) was accepted on 2026-11-25; this submission is
less than a month later because it answers the CRAN request above.
