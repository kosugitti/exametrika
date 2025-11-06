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

## Version 1.6.5

This is a patch level update with critical bugfixes and parameter customization enhancements:

* Critical bugfix for LCA() response type validation - fixed incorrect variable reference in response type checking
* Added beta1 and beta2 parameters to all Beta distribution-based functions to allow customization of prior density parameters in Bayesian parameter estimation (LRA.binary, LCA, Biclustering.binary, BNM, LDB, LDLRA, and internal LD_param_est function)
* Added alpha parameter to polytomous models (Biclustering.ordinal, Biclustering.nominal) for customization of Dirichlet prior concentration parameters
* Simplified function names for structure learning functions: StrLearningGA_BNM → BNM_GA, StrLearningPBIL_BNM → BNM_PBIL, StrLearningPBIL_LDLRA → LDLRA_PBIL
* All default parameter values preserve backward compatibility with previous versions

All tests pass with 0 errors, 0 warnings, and 0 notes across all test environments.


