## Test environments
* local macOS install: R 4.4.2
* GitHub Actions (ubuntu-latest): R-devel
* GitHub Actions (windows-latest): R-devel
* GitHub Actions (macOS-13): R-devel
* win-builder (devel)

## R CMD check results

### Local, GitHub Actions
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

### win-builder


### Downstream dependencies
There are currently no downstream dependencies for this package.

### Version 1.5.1

This is a major update since version 1.2.0, adding several new features:

* Added support for polytomous and ordinal response data
* Implemented new models: LRA for ordinal data, GRM
* Added visualization capabilities for various analyses
* Improved numerical stability and standardized terminology
* Added field analysis for Biclustering and Class/Rank Reference Vector plots
* Added functionality for polychoric/polyserial correlation calculation
* Bug fixes and performance improvements

All tests pass with 0 errors, 0 warnings, and 0 notes across all test environments.
