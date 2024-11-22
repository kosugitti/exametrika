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
This is a resubmission of version 1.1.0. I have:

* Removed external file URIs from README.md as requested by the CRAN team.
