## Test environments

* local macOS (aarch64-apple-darwin25.0.0): R 4.5.3
* R-hub v2: linux (R-release), macos-arm64 (R-release), windows (R-release)
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

### Downstream dependencies

There are currently no downstream dependencies for this package on CRAN.

## Version 1.12.0

This release introduces a C++ implementation of the IRM Gibbs sampler shared by
`Biclustering_IRM.{nominal,ordinal,rated}` (about 4x faster, bit-identical to the
R reference under the same `set.seed()`), adds class-side confirmatory clustering
to `Biclustering()`, and fixes several pre-existing bugs in the confirmatory
field-clustering branches and in `GRM()`. No breaking changes.

### New Features

* **C++ IRM Gibbs sampler**: `irm_gibbs_core_cpp()` (internal, in
  `src/irm_gibbs_core.cpp`) replaces the R reference loop for production runs.
  RNG calls are routed through R-level `sample.int()` and `rmultinom()` via
  `Rcpp::Function`, so output is bit-identical to the R reference under the
  same `set.seed()`. Roughly 4x speedup on the bundled test datasets
  (J20S600 nominal: 82 to 20 ms/iter; J35S500 ordinal: 76 to 19 ms/iter).
  The R reference is preserved behind `irm_gibbs_core(use_cpp = FALSE)` for
  cross-checking. No new package dependencies (Rcpp was already in
  `LinkingTo:`).

* **Class-side confirmatory Biclustering**: New `conf_class` argument on
  `Biclustering()` (binary, ordinal, nominal, and rated via dispatch). Fixes
  class memberships during EM the way `conf` fixes field memberships. For
  Ranklustering (`method = "R"`), the neighbour-smoothing step is skipped
  when `conf_class` is in effect. The new argument is additive and defaults
  to `NULL`, preserving the previous behavior for all existing callers.

### Bug Fixes

* **Confirmatory ordinal Biclustering**: `Biclustering.ordinal()` was
  overwriting `fldmemb` with the E-step estimate every iteration, so the
  `conf` argument only affected the initial value. The implementation now
  re-applies the supplied membership immediately after the field-side
  E-step, matching `Biclustering.binary()`.

* **Confirmatory nominal Biclustering**: `Biclustering.nominal()` validated
  `length(conf)` against `NCOL(U)`, but `U` is an `exametrika` list object
  so `NCOL(U)` always returned 1. The check rejected every well-formed
  `conf` vector, making confirmatory nominal Biclustering unreachable
  since v1.10.0. Replaced with `NCOL(U$Q)`. The same fix was applied to
  `Biclustering.ordinal()`. `Biclustering.binary()` was unaffected at
  runtime (because `U` is rebound to `tmp$U * tmp$Z` before the check)
  but was changed to `NCOL(tmp$U)` for consistency.

* **Matrix-form `conf` in confirmatory Biclustering**: When `conf` was
  supplied as a 0/1 membership matrix instead of a vector, all three
  implementations validated the matrix but never assigned it to
  `conf_mat`, so the subsequent `nfld <- NCOL(conf_mat)` failed with
  "object 'conf_mat' not found". Fixed by adding the missing assignment.

* **`Biclustering.ordinal()` now honors `maxiter`**: The inner EM cap was
  hardcoded to 100 and ignored the user-supplied `maxiter`, mirroring the
  `Biclustering.nominal()` bug fixed in 1.11.0.

* **`GRM()` fit indices no longer return `NaN`**: The benchmark log-domain
  epsilon `exp(-nitems * 100)` underflowed to 0 for moderate item counts,
  which propagated `NaN` through every fit index. The benchmark and null
  loops now skip zero-count categories explicitly.

* **`GRM()` degrees of freedom were computed incorrectly**: Both model and
  null df were misformed, clamping `CFI`, `TLI`, `IFI`, and `RMSEA` to
  zero whenever `chi^2 < df`. The convention now matches
  `Biclustering.ordinal()`.

* **`GRM()` now accepts any integer-coded ordinal responses, not just
  1..K**: Category counts were derived from `apply(dat, 2, max)`. Data
  coded from 0 or with gaps undercounted `ncat[j]` and indexed out of
  range. Each item's responses are now remapped to contiguous 1..K codes
  on entry.

* **`GridSearch()` now tolerates per-cell fit errors**: Errors at grid
  corners (e.g. empty-cluster edge cases at extreme `ncls`/`nfld`) used
  to abort the entire grid. The call is now wrapped in `tryCatch` and
  errors are recorded in `failed_settings` like non-convergence.

### Performance (additional, beyond the C++ Gibbs core)

* **Vectorized EM hot path in `Biclustering.ordinal()` and `.nominal()`**:
  Replaced several `apply()` calls with vectorized base-R primitives
  (`rowSums`/`colSums` with `dims` on 3D arrays, `pmin.int`/`pmax.int`,
  `max.col`). Output is bit-identical to 1.11.0 on every tested
  configuration. About 1.2 to 1.3 times faster per fit, compounding
  across `GridSearch()` grids.

* **Vectorized `Uq` one-hot encoding**: The per-fit `nobs * nitems`
  R-level loop was replaced with a single C-level matrix-index assignment.
  About 1.2 to 5.3 times faster on small to mid datasets.

### Output structure changes

* **`Biclustering.ordinal()` now returns `SmoothedMembership`**: The
  smoothed (Ranklustering-filtered) class membership matrix is now part
  of the return value, mirroring `Biclustering.binary()`. This fills a
  long-standing gap and lets callers verify that the smoothing step is
  correctly skipped when `conf_class` is in effect.

### Compatibility

No breaking changes. All deprecated names from previous releases continue
to work unchanged.
