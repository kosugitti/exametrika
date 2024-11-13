## Test environments
* local macOS install: R 4.4.2
* GitHub Actions (ubuntu-latest): R-devel
* GitHub Actions (windows-latest): R-devel
* GitHub Actions (macOS-13): R-devel
* GitHub Actions (macOS-latest, ARM64): R-devel
* win-builder (devel)

## R CMD check results

### Local, GitHub Actions
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

### win-builder
0 errors ✔ | 0 warnings ✔ | 1 note ✔

* checking CRAN incoming feasibility ... NOTE
  Possibly misspelled words in DESCRIPTION:
    These are correct spellings of technical terms:
    - Biclustering: A technical term for a clustering method
    - IRT: Abbreviation for Item Response Theory
    - exametrika: The package name

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Version 1.1.0
This is a major update from version 1.0.0 that adds support for polytomous data.

## RESUBMISSION

This is a resubmission of version 1.1.0. In this version, I have addressed the following issues raised by the CRAN team:

1. **Removed redundant "Tools for/A collection of tools" from the title and description.**
2. **Added more details about the package functionality and implemented methods in the Description field.**
3. **Removed unnecessary spaces in the Description field.**
4. **Added references for the methods implemented in the package in the Description field, using the recommended format.**
5. **Added \value documentation to the relevant .Rd files, explaining the structure and meaning of the outputs.**
6. **Replaced \dontrun{} with \donttest{} in the examples, as the examples are executable.**
7. **Ensured that the package does not change the user's options, par, or working directory without properly restoring the previous state using on.exit().**

With these changes, I believe the package is now ready for CRAN resubmission. Please let me know if you have any other feedback or concerns.

