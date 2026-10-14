## Test environments

* local macOS (aarch64-apple-darwin25.0.0), R 4.6.1: 0 errors, 0 warnings,
  1 NOTE (HTML manual validation, an artifact of the local HTML Tidy version)
* win-builder R-devel (2026-10-13 r90668 ucrt): Status OK (0 errors,
  0 warnings, 0 notes), check time 360 s
* GitHub Actions (commit 79f33d9): macOS, Windows and Ubuntu on R release,
  Ubuntu on R-devel and R oldrel-1: Status OK (0 errors, 0 warnings, 0 notes)

## R CMD check results

0 errors | 0 warnings | 0 notes (win-builder R-devel, GitHub Actions)

Locally the only note is "Skipping checking HTML validation: 'tidy' doesn't
look like recent enough HTML Tidy", an artifact of the local HTML Tidy version.

## This is a minor release (2.2.0)

* New `predict()` method: classifies new respondents with a fitted latent
  class, latent rank or biclustering model, holding the estimated parameters
  fixed.
* All latent class, latent rank and biclustering fits now report the evidence
  lower bound, the respondent-side marginal log-likelihood, the posterior
  entropy and ICL. For nominal and ordinal biclustering `log_lik` is now the
  evidence lower bound.
* Ordinal biclustering gains data-based starting values and a few screened
  random starts (`init`, `nstart`, `seed`); the session's random number stream
  is left untouched.
* Fixes that change estimates, hence the minor version bump and not a patch:
  category codes are no longer renumbered per item in the biclustering family;
  memberships are now the posterior under the returned parameters; the
  biclustering EM rolls back field memberships together with the reference
  profiles; SOAC/WOAC for ordinal biclustering are judged on the model; and
  about fifty further estimation fixes from a pre-release audit (see NEWS.md).
  The reference values used in the tests were regenerated from the corrected
  reference implementation.

## Reverse dependencies

ggExametrika (the only reverse dependency on CRAN) passes its tests against
this version.

It has been more than one month since the previous release (2.1.0, accepted
2026-09-25).
