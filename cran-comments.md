## Test environments

* local macOS (aarch64-apple-darwin25.0.0), R 4.6.1: 0 errors, 0 warnings,
  1 NOTE (HTML manual validation, an artifact of the local HTML Tidy version)
* win-builder R-devel (2026-09-21 r90579 ucrt): OK, check time 322 s
* R-hub v2 (R-devel): linux and windows OK. The macos-arm64 job failed while
  building the dependency mvtnorm from source (symbol not found:
  '__FortranAModReal8'), before exametrika itself was built; this appears to
  be a toolchain issue on the runner.

## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a minor release (2.1.0)

* Fixes to LRA(method = "SOM"), which had drifted from the reference
  implementation: the presentation order was frozen across epochs when `seed`
  was given, and the monotonicity sort under `mic = TRUE` was applied per epoch
  instead of per respondent. Estimates from method = "SOM" change accordingly,
  hence the minor version bump. The SOM inner loop now runs in C++.
* Ordinal Biclustering now rolls back both parameter arrays together when an
  EM sweep is rejected.
* graphics functions used by the raster Array plot are now imported in
  NAMESPACE.

It has been more than one month since the previous release (2.0.0, accepted
2026-08-20).
