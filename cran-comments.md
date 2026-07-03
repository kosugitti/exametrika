## Test environments

* local macOS (aarch64-apple-darwin25.0.0): R 4.6.1
* R-hub v2: linux (R-release), macos-arm64 (R-release), windows (R-release)
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

### Downstream dependencies

ggExametrika (CRAN) depends on exametrika. It was checked against this
release: all its tests pass unchanged (the API changes below only alter
defaults of top-level estimation functions, which ggExametrika does not
call).

## This is a minor release (1.15.0)

This release fixes a batch of bugs found in a full-codebase audit
(missing-data handling, category-code handling for polytomous data, graph
validity checks) and unifies gratuitously inconsistent argument names and
defaults across the model functions. All deprecated names from previous
releases continue to work unchanged; one argument rename in this release
(`Biclustering_IRM()`'s `max_iter` -> `maxiter`) keeps the old name working
with a deprecation warning.

### Improvements

* `dataFormat()` and `longdataFormat()` now accept column names (character
  strings) in addition to column numbers for their ID/response/weight
  column arguments.

### API consistency changes

* The `verbose` argument now defaults to `FALSE` in every function that
  has one (previously a near-even split between `TRUE` and `FALSE`
  defaults across models).
* `Biclustering_IRM()`: `max_iter` renamed to `maxiter` (matching the rest
  of the package); the old name still works with a deprecation warning.
  Its ordinal method's `mic`/`EM_limit` defaults now match the sibling
  methods.
* Missing-data arguments unified to the order `(na, Z, w)` across the
  network models; only positional calls are affected.

### Bug fixes (selection)

* Missing responses were silently counted as incorrect in auxiliary
  outputs of `IRT()`, `Biclustering()`, `BNM()`, `LDLRA()`, `LDB()`, and
  `BINET()`, and crashed `BINET()` entirely.
* 0-based category codes were silently dropped by the polytomous
  Biclustering/IRM functions and by `DistractorAnalysis()`.
* `BNM()`/`BINET()` graph acyclicity checks used element-wise `adj^i`
  instead of matrix powers and were ineffective; replaced with
  `igraph::is_dag()` (igraph is an existing dependency).
* `print()` on a `GridSearch()` result always crashed.
* `dataFormat()` missed zero-variance items whenever they also had
  missing responses, which crashed `IRT()` downstream.
* Raw matrix/data.frame input crashed `CTT()`, `BNM()`, `LDLRA()`,
  `LDB()`, and `BINET()`.

Regression tests pinning all of the above were added
(`tests/testthat/test-regression-1150.R`).

The CRAN check time on r-devel-windows-x86_64 remains within the
10-minute limit: the heaviest real-data fit tests in `test-grm.R` and
`test-irm.R` are wrapped in `skip_on_cran()` (introduced in 1.13.1) and
continue to run locally and on R-hub / win-devel via `NOT_CRAN`, so
coverage is unchanged outside CRAN.
