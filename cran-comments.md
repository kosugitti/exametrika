## Test environments
* local macOS (aarch64-apple-darwin25.0.0): R 4.5.2

## R CMD check results

### Local
0 errors | 0 warnings | 0 notes

### Downstream dependencies

There are currently no downstream dependencies for this package.

## Version 1.9.0

This is a minor release adding new features for polytomous (ordinal/nominal) Biclustering analysis. No breaking changes.

### New Sample Datasets
* **J35S500**: Simulated ordinal dataset (500 students, 35 items, 5 categories, 5 classes × 5 fields).
* **J20S600**: Simulated nominal dataset (600 students, 20 items, 4 categories, 5 classes × 4 fields).

### New Plot Types for Polytomous Biclustering
* **FRP**: Expected score line plot per field with `stat` parameter ("mean", "median", "mode").
* **FCRP**: Category probability plot per field with `style` parameter ("line", "bar").
* **FCBR**: Boundary probability plot per field (ordinal only).
* **ScoreField**: Heatmap of expected scores across fields and classes/ranks.
* **RRV**: Transposed reference vector with fields on x-axis.

### FRPIndex for Ordinal Biclustering
* Computes profile shape indices (Alpha, A, Beta, B, Gamma, C) from normalized expected scores.

### Other Improvements
* Array plots now display missing values in a distinct color.
* Ordinal Biclustering `print()` now displays FRPIndex.
* Internal refactoring of `plot.exametrika()` into model-family files for maintainability.
