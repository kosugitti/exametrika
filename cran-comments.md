## Test environments
* local macOS (aarch64-apple-darwin25.0.0): R 4.5.2
* R-hub (linux, macos-arm64, windows)
* win-builder (devel)

## R CMD check results

### Local
0 errors | 0 warnings | 1 note

The single NOTE is:
> checking for future file timestamps ... NOTE
> unable to verify current time

This is a network connectivity issue during the check, not a package problem.

### Downstream dependencies

There are currently no downstream dependencies for this package.

## Version 1.8.1

This is a patch release with bug fixes only. No breaking changes.

### dataFormat Function
* Fixed factor ID column detection: `dataFormat()` now correctly identifies factor-type ID columns before converting factors to numeric. Previously, factor ID columns with many levels (>=20) triggered a "Too many categories" error instead of being recognized as IDs.
* Removed unused internal helper function (`is_response_data()`).

### GridSearch Function
* Fixed ordinal data support: GridSearch now correctly handles ordinal data by using `obj$Q` instead of `obj$U` for test length calculation.
* Resolved nfld=1 parameter issue that caused crashes with ordinal datasets.

### Biclustering.ordinal Function
* Enhanced numerical stability to prevent division by zero and NaN errors.
* Fixed convergence failures in specific parameter combinations (e.g., ncls=4 with nfld=5).
* Improved robustness for edge cases where field membership probabilities approach zero.
