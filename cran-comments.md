## Test environments

* local macOS (aarch64-apple-darwin25.0.0), R 4.6.1: 0 errors, 0 warnings, 0 notes
* GitHub Actions: ubuntu (R-devel, R-release, R-oldrel), macOS (R-release),
  Windows (R-release), all with the full test suite (`NOT_CRAN=true`,
  5,291 tests, 0 skipped): OK
* R-hub v2: linux, macos-arm64, windows (R-devel): OK
* win-builder R-devel (2026-08-20): OK, check time 460 s
  (limit 600 s; tests 113 s)

## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a major release (2.0.0)

The version was raised to 2.0.0 because a bug fix in the E-step changes the
numbers every EM-based model returns: the same code on the same data now gives
different estimates. Since a major bump was happening anyway, the deprecated
names scheduled for removal are removed in the same release, so that the break
happens once rather than twice.

### The breaking change, and why it was necessary

Several E-steps normalised their posterior memberships by subtracting the row
*minimum* before exponentiating, instead of the row maximum. That pushes every
exponent positive and then clips at `exp(700)` to stop the overflow, so all
values above the clip collapse onto one number: two fields differing by a factor
of `exp(400)` came out equally likely.

How far a row spreads depends on what is summed over. A class posterior sums over
items and stays small, so the clip is rarely reached. A *field* posterior sums
over respondents, so it grows with the sample -- the visible symptom was
therefore backwards, with more data giving a worse answer. Measured on ordinal
data with 24 items in 3 true fields: fields were recovered exactly up to 500
respondents, degraded at 700, and by 1000 two of the three fields had merged and
one came out empty (adjusted Rand index 1.00 falling to 0.55).

Affected: `Biclustering()` and `Ranklustering()` on ordinal and nominal data,
`LCA()` on nominal and rated data, and `LDB()`. Binary biclustering was already
correct. All of them now share one `row_softmax()` helper.

Users who need to reproduce numbers published with earlier versions of the
package should pin the earlier version; the previous results were affected by
the bug described above.

### Removals

The functions and field names deprecated in earlier releases (`IRM()`,
`StrLearningGA_BNM()`, and the `Nclass` / `Nfield` / `N_Cycle` / `LogLik` field
aliases, among others) are removed, together with their backward-compatible
aliases. The function renames have carried a deprecation warning since 1.7.0 and
the field-name aliases since 1.8.0.

### Title change

The `Title:` field changes from "Test Theory Analysis and Biclustering" to
"Test Data Engineering". The package implements Shojima (2022,
ISBN:978-9811699856), whose subject is test data engineering, and the previous
title singled out one method (biclustering) among CTT, IRT, LCA, LRA, BNM and
the local-dependence models. The breadth of methods, biclustering included,
remains described in `Description:`.

### New in this release

* `M2()`: the limited-information goodness-of-fit statistic, for `LRA()` and
  `Biclustering()`, so that models with different response-pattern spaces can be
  compared on one scale. `add_M2()` attaches it to an existing fit. `M2()`
  reports its memory requirement and declines sizes the machine cannot hold
  rather than exhausting it.
* `LCA.nominal()` and `LCA.rated()`.
* Margin-based fit indices, kept separate from the response-pattern ones.
* Speedups in the order-restricted (isotonic) solver and in the BNM/LDLRA
  structure search. Results are unchanged.

## Downstream dependencies

`ggExametrika` (CRAN) has exametrika in `Suggests`. It was checked against this
release with the removals in place.

Its shipped code is unaffected: every use of a removed field name sits inside a
helper that tries the current name first (`.first_non_null(data$n_class,
data$Nclass, ...)`), so the removal only makes a fallback argument `NULL`. Three
of its *test* assertions read the removed names directly; those have been updated
in its development version, and its suite then passes in full against this
release. No user-facing behaviour of `ggExametrika` changes, so the two do not
need to be submitted together.

`shinyExametrika` (not on CRAN) passes its suite unchanged against this release,
for the same reason: it reads fields through a `safe_field()` helper that
prefers the current name.

## Check time

Version 1.13.0 was rejected at "Overall checktime 11 min > 10 min" on Windows.
The test suite is now two-tiered: CRAN runs small synthetic fixtures
(cross-validated against the reference Mathematica implementation) plus a smoke
test per user-facing function, while the full-size reference comparisons and
reproducibility runs carry `skip_on_cran()` and run on every push in CI
(GitHub Actions, three platforms, `NOT_CRAN=true`) and locally. win-builder
measured the whole check at 460 s with tests at 113 s.
