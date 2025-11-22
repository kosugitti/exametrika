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

## Version 1.7.0

This is a minor version update with API improvements following semantic versioning:

* Renamed IRM() to Biclustering_IRM() for consistency with structure learning naming conventions (follows the model_method pattern like BNM_GA(), LDLRA_PBIL())
* Added .Deprecated() warnings to all renamed functions from version 1.6.5:
  - StrLearningGA_BNM() → BNM_GA()
  - StrLearningPBIL_BNM() → BNM_PBIL()
  - StrLearningPBIL_LDLRA() → LDLRA_PBIL()
  - IRM() → Biclustering_IRM()
* All old function names still work for backward compatibility but display deprecation warnings
* Updated all documentation and examples to use new function names
* Internal class names remain unchanged for stability

This is a minor version bump (1.6.5 → 1.7.0) because it introduces new exported functions while maintaining full backward compatibility. All changes are non-breaking - existing code will continue to work with deprecation warnings guiding users to the new API.

All tests pass with 0 errors, 0 warnings, and 0 notes across all test environments.
