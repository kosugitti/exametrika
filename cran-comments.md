## Test environments

* local macOS (aarch64-apple-darwin25.0.0): R 4.6.0
* R-hub v2: linux (R-release), macos-arm64 (R-release), windows (R-release)
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

### Downstream dependencies

There are currently no downstream dependencies for this package on CRAN.

## This is a minor release (1.14.0)

This release responds to an R Journal review request and fixes several
bugs. There are no breaking changes; all deprecated names from previous
releases continue to work unchanged.

### Improvements

* **`plot.exametrika()` now forwards graphical parameters supplied via
  `...`** to every plot type. The `...` argument documented on the
  `plot.exametrika()` help page was previously captured but never passed
  to the internal dispatch functions, so standard graphical parameters
  (`pch`, `las`, `cex`, `col`, `lty`, `lwd`, and overrides of `xlab`,
  `ylab`, `main`) were silently ignored. They are now forwarded
  consistently to the underlying base R plotting calls, including
  manually drawn axes, for all plot types. This is achieved without
  adding any dependency (base graphics only).

### Bug fixes

* **`Glasso()` no longer aborts on numerical divergence.** When a lambda
  in the search path drives the block coordinate descent to a
  non-finite solution, `Glasso()` now warns and falls back to the best
  finite solution instead of erroring out.
* **Binary IRM crash on real data with missing values.**
  `Biclustering_IRM()` on a binary data object containing missing cells
  no longer crashes in the Gibbs sampler; the missing-data mask is now
  applied correctly.
* **IRT Test Information Function plot title typo** (`"Informaiton"` ->
  `"Information"`), the Biclustering array-plot panel title typo
  (`"Clusterd Data"` -> `"Clustered Data"`), and four user-facing
  message typos were corrected. No behaviour change.

The CRAN check time on r-devel-windows-x86_64 remains within the
10-minute limit: the heaviest real-data fit tests in `test-grm.R` and
`test-irm.R` are wrapped in `skip_on_cran()` (introduced in 1.13.1) and
continue to run locally and on R-hub / win-devel via `NOT_CRAN`, so
coverage is unchanged outside CRAN.
