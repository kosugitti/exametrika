## Test environments
* local macOS (aarch64-apple-darwin25.0.0): R 4.5.2

## R CMD check results

### Local
0 errors | 0 warnings | 1 note

The single NOTE ("unable to verify current time") is a transient network
issue during the check and not related to the package itself.

### Downstream dependencies

There are currently no downstream dependencies for this package on CRAN.

## Version 1.9.0

This is a minor release with bug fixes, new features for polytomous Biclustering
analysis, and a complete test suite modernization. No breaking changes.

### Bug Fixes

* **CAIC (Consistent AIC) formula correction**: Fixed the CAIC penalty term from
  `log(n + 1)` to `log(n) + 1` per Bozdogan (1987, Psychometrika, 52(3), p.358,
  Proposition 2, Eq.44). The original Mathematica implementation had this error
  (`Log[nobs + 1]`), and the R port inherited it. The corrected formula now matches
  the published definition: `CAIC(k) = -2 log L + k * (log(n) + 1)`. This affects
  all models that compute fit indices (IRT, LCA, LRA, Biclustering, IRM, BNM,
  LDLRA, LDB, BINET, GRM).
* **GridSearch `index` parameter fixes**: Added alias support for common fit index
  names (e.g., "loglik" -> "model_log_like"), early validation of invalid indices,
  and corrected log-likelihood optimization direction (maximize, not minimize).
* **LRA.ordinal / LRA.rated category computation fix**: Fixed `apply()` returning
  matrix instead of list when all items have the same number of categories.
* **GRM ItemFitIndices fix**: Same class of `apply()/table()` bug as above.
* **LRA.ordinal mixed category count validation**: Added informative error for
  datasets with non-uniform category counts.
* **BINET `g_list`/`adj_list` input path fix**: Fixed undefined variable error
  and incorrect length validation.
* **LCA/LRA FRP plot type removal**: Properly rejected unsupported FRP plot type.
* **LCA `msg` field assignment**: Fixed `<-` to `=` inside `structure()` call.
* **RMP/CMP single student plot error**: Fixed dimension drop error.
* **LRA.ordinal/LRA.rated TestFitIndices**: Fixed null/saturated log-likelihood
  label swap and unified to standard 16-field ModelFit structure.

### New Features

* **New sample datasets**: J35S500 (ordinal, 500x35) and J20S600 (nominal, 600x20).
* **New plot types for polytomous Biclustering**: FRP, FCRP, FCBR, ScoreField, RRV.
* **FRPIndex**: Field Reference Profile indices for ordinal Biclustering.
* **Return value structure unification**: Extended snake_case field names to all
  functions (LDLRA, LDB, BINET, Biclustering.nominal/ordinal, IRM). Unified
  TestFitIndices to standard 16-field ModelFit structure across all models.

### Test Suite Modernization

* **Complete migration from Excel to CSV fixtures**: Removed all 14 legacy test
  files that depended on `tidyverse` and `readxl`. Replaced with 23 modern test
  files using base R `read.csv()`. Zero external package dependencies beyond
  `testthat`.
* **85 Mathematica reference CSV files** for cross-validation across all models.
* **680 tests**, all passing.
