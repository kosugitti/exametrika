## Test environments
* local macOS (aarch64-apple-darwin25.0.0): R 4.5.2

## R CMD check results

### Local
0 errors | 0 warnings | 2 notes

Notes:
1. CRAN incoming feasibility: URL check on README.md link
2. HTML Tidy version check (local environment only)

### Downstream dependencies

There are currently no downstream dependencies for this package on CRAN.

## Version 1.11.0

This release adds Rated IRM (Infinite Relational Model for multiple-choice data),
Distractor Analysis, and a new compact sample dataset. It also fixes bugs in
`Biclustering.nominal()` and the rated Biclustering models.
No breaking changes.

### New Features

* **`Biclustering_IRM.rated()`**: IRM-based biclustering for rated (multiple-choice
  with correct answers) data. Internally calls `Biclustering_IRM.nominal()` for
  estimation, then post-processes with class sorting by correct response rate and
  two-layer fit indices (binary with CFI/RMSEA, nominal with AIC/BIC/CAIC only).
  Completes the IRM support for all four data types (binary, ordinal, nominal, rated).

* **`DistractorAnalysis()`**: S3 generic for analyzing response category distributions
  by rank/class. Computes observed frequencies, proportions, chi-square tests against
  chance level, and Cramer's V effect sizes. Works with `LRA.rated` and rated
  Biclustering models. Supports `items` and `ranks` filtering in `print()` and `plot()`.

* **`J21S300`**: New rated sample dataset (21 items, 300 students, 4 categories)
  for testing rated models. Smaller and faster than J35S5000.

### Bug Fixes

* **Fixed `maxiter` parameter ignored in `Biclustering.nominal()`**: The `maxiter`
  parameter was accepted but internally overridden by a hardcoded value of 100.
  Now correctly uses the user-specified `maxiter`.

* **Fixed spurious "No ID column detected" message in rated Biclustering models**:
  The `crr()` function was called with a raw matrix instead of an exametrika object,
  triggering an unnecessary `dataFormat()` call.
