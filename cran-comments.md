## Test environments
* local macOS install: R 4.5.2
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

## Version 1.8.0

This is a minor version update focusing on naming convention improvements and output formatting:

### Naming Convention Improvements
* Standardized return value field names to snake_case for consistency:
  - `n_class`, `n_field`, `n_rank`, `n_cycle`, `log_lik` (new recommended names)
  - Old names (`Nclass`, `Nfield`, `Nrank`, `N_Cycle`, `LogLik`) still work via deprecation path
* All old field names remain functional for backward compatibility
* Will be removed in future major version (2.0.0)

### Output Formatting Improvements
* Progress messages now display properly in R Markdown documents
* GridSearch() added `verbose` parameter (default: TRUE)
* Improved iteration display format in Biclustering_IRM()
* Fixed verbose behavior in LRA.ordinal() and LRA.rated() (default changed to FALSE)
* All progress messages use proper line breaks for knitr compatibility

### Bug Fixes
* Fixed Array plot color mapping for binary data
* Fixed Ranklustering Array plot sorting order

This release maintains full backward compatibility following semantic versioning (MINOR version bump).
