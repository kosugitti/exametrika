## Test environments
* local macOS install: R 4.5.1
* GitHub Actions (ubuntu-latest): R-devel
* GitHub Actions (windows-latest): R-devel
* GitHub Actions (macOS-13): R-devel
* win-builder (devel)

## R CMD check results

### Local, GitHub Actions
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

### win-builder
Installation time in seconds: 22
Check time in seconds: 110
Status: OK

### Downstream dependencies

There are currently no downstream dependencies for this package.

## Version 1.6.3

This is a patch level update with significant performance improvements:

* Major performance enhancement for GRM (Graded Response Model) with C++ implementation
* High-performance polychoric correlation computation using C++
* Enhanced GridSearch() function with improved convergence handling
* Improved numerical stability and visualization enhancements

All tests pass with 0 errors, 0 warnings, and 0 notes across all test environments.


