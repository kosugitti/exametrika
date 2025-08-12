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
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
Status: OK
Installation time: 9 seconds
Check time: 84 seconds

### Downstream dependencies

There are currently no downstream dependencies for this package.
Version 1.6.0
This is a minor update since version 1.5.2, adding new features and bug fixes:

### New Features:

Added Biclustering.nominal() function for nominal data analysis
Added Biclustering.ordinal() function for ordinal data analysis
New GridSearch() function for grid search optimization of model parameters

### Bug Fixes:

Fixed output typos in class/rank terminology
Added duplicate ID validation to dataFormat() function
Fixed ID column detection in dataFormat() function
Fixed stanine division error when data splitting fails

All tests pass with 0 errors, 0 warnings, and 0 notes across all test environments.


