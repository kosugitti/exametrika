## Test environments

* local macOS (aarch64-apple-darwin), R 4.6.2: 0 errors, 0 warnings, 0 notes
* win-builder R-devel (2026-11-17 r90926 ucrt): Status OK (0 errors,
  0 warnings, 0 notes), check time 457 s
* R-hub v2 (GitHub Actions, commit 6c18e60): Linux, macOS and Windows on
  R-devel: Status OK (0 errors, 0 warnings, 0 notes)

## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a minor release (2.3.0)

* New model-selection tools: `BIC_elbo` and `ICL_exact` for all latent
  class, latent rank and biclustering fits, `plot()` for `GridSearch()`
  results, and `GridSearch()` no longer selects a setting with an empty class.
* `mixing = "estimate"` lets `LCA()`, `LRA()` and `Biclustering()` estimate
  the class weights (the default keeps them equal, as before).
* New functions `equate()`, `MonotonicityCheck()`, `Scalability()` and
  `ManifestMonotonicity()`; rank-up odds and `RankUpShift` for ordinal
  Ranklustering.
* Fixes that change estimates or fit indices, hence a minor rather than a
  patch release: `M2()` with missing responses, fit indices of the ordinal
  and rated LRA now use the number of respondents, the binary IRM no longer
  double-counts a respondent who opens a new class, and class/rank/field
  sizes no longer double-count ties (see NEWS.md).
* igraph is no longer used in the computations of the network models; the
  igraph objects in the output are kept for now and will be removed in a
  later major release.

## Reverse dependencies

ggExametrika (the only reverse dependency on CRAN) passes its checks and
tests against this version.

It has been one month since the previous release (2.2.0, published
2026-10-25).
