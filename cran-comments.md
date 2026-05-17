## Test environments

* local macOS (aarch64-apple-darwin25.0.0): R 4.6.0
* R-hub v2: linux (R-release), macos-arm64 (R-release), windows (R-release)
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

### Downstream dependencies

There are currently no downstream dependencies for this package on CRAN.

## Version 1.13.1 (resubmission of 1.13.0)

This is a resubmission of 1.13.0. The 1.13.0 submission was rejected by
the CRAN auto-check service for a single NOTE on
r-devel-windows-x86_64:

    Overall checktime 11 min > 10 min

The package itself passed cleanly on both Windows and Debian
(Status: OK / OK). To bring the Windows checktime comfortably under the
10-minute limit, this 1.13.1 release marks the slowest test blocks in
`test-grm.R` and `test-irm.R` with `skip_on_cran()`. The same tests
continue to run locally and on R-hub / win-devel via the `NOT_CRAN`
environment variable that `testthat` sets, so coverage is unchanged
outside CRAN.

No user-visible package changes between 1.13.0 and 1.13.1; the package
contents below are otherwise identical to 1.13.0.

## Version 1.13.0

This release introduces Graphical Lasso (`Glasso`) and Chatterjee's xi
correlation (`chatterjee_xi`, `xi_stable`, `chatterjee_matrix`) as a
preparatory step toward polytomous Bayesian network structure learning.
Because the previous CRAN release was 1.11.0, this submission also rolls
up the development changes from 1.12.0–1.12.2 that were never published
to CRAN: a C++ IRM Gibbs sampler (about 4x speedup, bit-identical to the
R reference under the same `set.seed()`), class-side confirmatory
clustering (`conf_class`), bug fixes in confirmatory Biclustering and
`GRM()`, EM stability fixes for empty-field/class corner cases, and a
relaxation in `dataFormat(response.type = "rated")` so that an
unobserved correct-answer category produces a warning instead of an
error. No breaking changes; all deprecated names from previous releases
continue to work unchanged.

### New Features (1.13.0)

* **Graphical Lasso (`Glasso`)**: sparse precision matrix estimation
  from ordinal item response data using polychoric correlations and
  block coordinate descent (Friedman, Hastie, Tibshirani 2008) with
  EBIC-based lambda selection (Foygel and Drton 2010) and warm-starting
  across the lambda grid. Returns the selected precision matrix,
  selected lambda, EBIC, edge count, and the full search path.

* **Chatterjee's xi correlation**: `chatterjee_xi()` (single-shot),
  `xi_stable()` (B-replication average for tie-stability),
  `chatterjee_matrix()` (asymmetric pairwise xi matrix with
  pairwise-complete handling of missing values). The asymmetry of
  xi(j, k) versus xi(k, j) enables direction detection in
  graphical-model construction.

* **`print.exametrika` Glasso branch**: summarizes the estimated model
  (optimal lambda, EBIC, edge count, precision matrix).

### Rolled-up Features from 1.12.x

* **C++ IRM Gibbs sampler**: `irm_gibbs_core_cpp()` (internal, in
  `src/irm_gibbs_core.cpp`) replaces the R reference loop for production
  runs. RNG calls are routed through R-level `sample.int()` and
  `rmultinom()` via `Rcpp::Function`, so output is bit-identical to the
  R reference under the same `set.seed()`. About 4x speedup on the
  bundled test datasets (J20S600 nominal: 82 to 20 ms/iter; J35S500
  ordinal: 76 to 19 ms/iter). The R reference is preserved behind
  `irm_gibbs_core(use_cpp = FALSE)` for cross-checking. No new package
  dependencies (Rcpp was already in `LinkingTo:`).

* **Class-side confirmatory Biclustering**: New `conf_class` argument on
  `Biclustering()` (binary, ordinal, nominal, and rated via dispatch).
  Fixes class memberships during EM the way `conf` fixes field
  memberships. For Ranklustering (`method = "R"`), the
  neighbour-smoothing step is skipped when `conf_class` is in effect.
  The new argument is additive and defaults to `NULL`, preserving the
  previous behavior for all existing callers.

### Rolled-up Bug Fixes from 1.12.x

* **Confirmatory Biclustering** (ordinal/nominal): the `conf` argument
  was effectively ignored after the first iteration in
  `Biclustering.ordinal()`, and rejected every well-formed input due to
  a `NCOL(U)` length-check bug in `Biclustering.nominal()` /
  `Biclustering.ordinal()` (which made confirmatory nominal Biclustering
  unreachable since v1.10.0). Both are fixed; matrix-form `conf`
  assignment is also fixed across all three implementations.

* **`Biclustering.ordinal()` now honors `maxiter`** (the inner EM cap
  was hardcoded to 100, mirroring the `Biclustering.nominal()` bug
  fixed in 1.11.0).

* **`GRM()` fit indices**: the benchmark log-domain epsilon
  `exp(-nitems * 100)` underflowed to 0 for moderate item counts and
  propagated `NaN` through every fit index; degrees of freedom were
  also misformed, clamping `CFI`, `TLI`, `IFI`, and `RMSEA` to zero
  whenever `chi^2 < df`. Both are fixed and the convention now matches
  `Biclustering.ordinal()`. `GRM()` also now accepts integer-coded
  ordinal responses with arbitrary coding (not just 1..K).

* **`GridSearch()` tolerates per-cell fit errors** instead of aborting
  the entire grid. Errors at grid corners (e.g. empty-cluster edge
  cases at extreme `ncls`/`nfld`) are now recorded in `failed_settings`
  like non-convergence.

* **Biclustering EM stability**: both `Biclustering.nominal()` and
  `Biclustering.ordinal()` no longer abort with
  `"missing value where TRUE/FALSE needed"` when extreme grid
  configurations leave fields or classes empty during EM. The two
  log-likelihood checks inside the EM loop now treat a non-finite
  `test_log_lik` as a non-converged exit. This also resolves the same
  crash in `Biclustering.rated()`, which calls `Biclustering.nominal()`
  internally.

* **`dataFormat(response.type = "rated")`**: an unobserved
  correct-answer category (e.g., a hard item where no respondent chose
  the CA, or small samples where the CA category did not appear by
  chance) now produces a warning instead of an error. The affected
  items are encoded as all-incorrect (`U[, j] == 0`), matching the
  correct downstream behavior. This applies to both the matrix-input
  path (`dataFormat`) and the long-format path (`longdataFormat`).
  Mistyped CAs still surface as warnings.

### Performance (additional, beyond the C++ Gibbs core)

* **Vectorized EM hot path** in `Biclustering.ordinal()` and
  `.nominal()`: replaced several `apply()` calls with vectorized base-R
  primitives (`rowSums`/`colSums` with `dims` on 3D arrays,
  `pmin.int`/`pmax.int`, `max.col`). Output is bit-identical to 1.11.0
  on every tested configuration. About 1.2 to 1.3 times faster per
  fit, compounding across `GridSearch()` grids.

* **Vectorized `Uq` one-hot encoding**: the per-fit `nobs * nitems`
  R-level loop was replaced with a single C-level matrix-index
  assignment. About 1.2 to 5.3 times faster on small to mid datasets.

### Output structure changes

* **`Biclustering.ordinal()` now returns `SmoothedMembership`**: the
  smoothed (Ranklustering-filtered) class membership matrix is now part
  of the return value, mirroring `Biclustering.binary()`. This fills a
  long-standing gap and lets callers verify that the smoothing step is
  correctly skipped when `conf_class` is in effect.

### Compatibility

No breaking changes. All deprecated names from previous releases
continue to work unchanged.
