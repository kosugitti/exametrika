## Test environments

* local macOS (aarch64-apple-darwin25.0.0): R 4.6.1
* R-hub v2: linux (R-release), macos-arm64 (R-release), windows (R-release)
* win-builder: R-devel

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
release:
its full test suite passes unchanged (0 failures). Its uses of the removed field
names all go through a helper that tries the current name first and falls back to
the legacy one, so the removals are inert there. No source change is required in
`ggExametrika` for it to keep working, and no simultaneous submission is needed.

## Check time

The heaviest real-data fit tests in `test-grm.R` and `test-irm.R` remain wrapped
in `skip_on_cran()` (introduced in 1.13.1) and continue to run locally and on
R-hub / win-devel via `NOT_CRAN`, so coverage outside CRAN is unchanged. The test
suite runs in about 135s wall / 54s elapsed locally (5,323 passing, 21 skipped).
