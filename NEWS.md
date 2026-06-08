# exametrika 1.14.0

## Improvements

- **`plot.exametrika()` now forwards graphical parameters supplied via `...`
  to every plot type (R Journal review request).** Previously the `...`
  argument documented on the `plot.exametrika()` help page was captured but
  never passed on: the internal dispatch functions (`plot_irt_model()`,
  `plot_grm_model()`, `plot_common_profiles()`, the polytomous Biclustering
  plotters, the array plot, and the network plotters) did not accept or
  propagate it, so standard graphical parameters were silently ignored. For
  example, `plot(result.LRA, type = "IRP", items = 1:4, nc = 2, nr = 2,
  las = 2, pch = 16)` changed neither the axis-label orientation (`las`)
  nor the plotting symbol (`pch`).

  User-supplied parameters are now forwarded consistently to the underlying
  base R plotting calls (`plot`, `barplot`, `image`, `lines`, `curve`) for
  **all** plot types, **including manually drawn axes** so that `las`,
  `cex.axis`, and similar parameters take effect on every axis. Standard
  parameters such as `pch`, `las`, `cex`, `col`, `lty`, and `lwd` work as
  expected, and user values take precedence over the package defaults, so
  `xlab`, `ylab`, `main`, etc. can be overridden. This is achieved without
  adding any dependency: the package remains lightweight and base-graphics
  only.

  Internally this is implemented via small helpers (`merge_plot_dots()`,
  `call_plot()`, `draw_curve()`) that merge the package defaults with the
  user's `...` (user wins) and dispatch through `do.call()`; the IRT/GRM
  curves are drawn by grid evaluation through `draw_curve()` instead of
  `curve()` so that the non-standard evaluation of `curve()`'s first
  argument does not block parameter forwarding.

## Bug fixes

- **IRT Test Information Function plot title typo.** The `type = "TIF"`
  plot for IRT models had `main = "Test Informaiton Function"`; it now reads
  `"Test Information Function"` (`R/00_plot_irt.R`).

- **User-facing message typos and missing-word fixes.** Four cosmetic but
  user-visible string fixes following a family-wide audit (exametrika,
  ggExametrika, shinyExametrika) prompted by the `Clusterd` -> `Clustered`
  rename. No behaviour change; messages now read correctly.
  - `R/04C_ParameterEstimation.R`: IRT slope warning had a missing space
    after the period (`"... exceeds 10.Please exercise caution ..."`
    -> `"... exceeds 10. Please exercise caution ..."`).
  - `R/02_TestItemFunctions.R`: `CCRR.nominal()` info message was missing
    the verb "Using" (`"... binary data only. Conditional Selection Rate
    for your polytomous data instead."` -> `"... binary data only. Using
    Conditional Selection Rate for your polytomous data instead."`), now
    matching the parallel messages in `CCRR.ordinal`/`CCRR.rated`/`CCRR.binary`.
  - `R/09_LDLRA.R` and `R/10_LDB.R`: max-parents warning had a comma
    where a period belongs (`"... is N, Please check."` -> `"... is N.
    Please check."`).

- **`plot.exametrika()` Biclustering panel: typo "Clusterd" -> "Clustered".**
  Fixed a long-standing misspelling in the base-R Biclustering array plot
  (`R/00_plot_biclustering.R`): the panel title `"Clusterd Data"` is now
  `"Clustered Data"`, the inline comment `## Clusterd Plot` is corrected to
  `## Clustered Plot`, and the internal variable `clusterd_data` is renamed
  to `clustered_data`. No user-facing API change. The downstream
  `ggExametrika::plotArray_gg()` had inherited and propagated the typo into
  its argument names (`Clusterd`, `Clusterd_lines`, `Clusterd_lines_color`);
  that is being corrected in ggExametrika 1.1.1 as a coordinated release.

- **Binary IRM: crash on real data with missing values.** `Biclustering_IRM`
  on a binary `exametrika` data object containing missing cells
  (`tmp$Z[s, j] == 0`) crashed with *"確率ベクトル中にNAがあります"*
  (NA in probability vector) inside `rmultinom()`, after `log()` produced
  `NaN` in the Gibbs sampler. The cause was that `tmp$U` retains the
  `-1` missingness marker from `dataFormat()` and the IRM aggregated it
  directly (`tmp$U %*% fld01`), producing negative counts in `CcfPlus`
  whose log was `NaN`. The pre-existing line `U <- tmp$U * tmp$Z` was a
  dead variable. `R/07_IRM.R` now assigns the masked matrix back as
  `tmp$U <- tmp$U * tmp$Z`, so all downstream `tmp$U` references see
  zero in missing cells. Reproduced on HCI (651×20, 1.5% missing) and
  SAT12 (600×32 binarised, 0.4% missing).

- **Graphical Lasso: graceful handling of numerical divergence.** When the
  inner block coordinate descent (BCD) of `Glasso()` produced non-finite
  values — typically with high-dimensional, near-singular polychoric
  correlation matrices (`N <= p` regime) and very small lambdas — the
  previous implementation crashed in R's `if (diff < eps)` test with
  *"missing value where TRUE/FALSE needed"*, discarding all of the
  intermediate work along the lambda grid.

  - `glasso_one()` now detects non-finite values in the BCD update of
    `beta`, in the inner convergence statistic `diff`, and in the working
    covariance `W`, and returns `converged = FALSE` instead of crashing.
  - `Glasso()` now reacts to `converged = FALSE` (or a non-finite EBIC)
    by emitting a `warning()` that names the offending lambda and the
    best lambda found so far, and **breaks the lambda search**, returning
    the best solution encountered up to that point. If divergence happens
    on the very first lambda the call still errors out, since no solution
    exists to return.
  - The `path` element of the result preserves `NA` for any lambda values
    skipped by the early break, making the breakpoint visible to the user.

  Observed concretely on a 98-item / 143-respondent ordinal dataset
  (polychoric condition number ~1.4e5): the old `Glasso()` crashed at
  lambda = 0.0096 after ~25 minutes of work; the new implementation
  returns the same `lambda_opt = 0.1003` solution with a clear warning
  and a `path` whose two trailing entries are `NA`.

# exametrika 1.13.1

Resubmission of the 1.13.0 release. The 1.13.0 submission was rejected
by the CRAN auto-check service for a single `Overall checktime` NOTE on
r-devel-windows-x86_64 (11 min vs. the 10 min limit). The package itself
passed cleanly on both Windows and Debian (Status: OK / OK).

## Test suite (no user-visible changes)

- The two heaviest test files now skip their slowest blocks on CRAN to
  keep `R CMD check` comfortably under the 10-minute Windows limit. The
  same tests continue to run locally and on R-hub / win-devel via the
  `NOT_CRAN` environment variable that testthat sets.
  - `test-grm.R`: the `J15S3810` regression (a 15-item / 3,810-respondent
    fit, ~60s) and the `nitems >= 8` underflow regression (~8s) are
    skipped on CRAN. Default-coverage GRM tests on small data continue
    to run on CRAN.
  - `test-irm.R`: the `J35S515` shared-fixture Gibbs run (~23s) and the
    two reproducibility tests that re-run Gibbs (~22s combined) are
    skipped on CRAN. The `Default seed is 123` structural check still
    runs on CRAN.

# exametrika 1.13.0

This release also incorporates the development changes from 1.12.0–1.12.2
(never published to CRAN). Their entries are retained below for traceability.

## New features

- **Graphical Lasso (`Glasso`)**: New function for sparse precision matrix
  estimation from ordinal item response data. The polychoric correlation
  matrix is computed internally and the optimal regularization parameter
  is selected by Extended Bayesian Information Criterion (EBIC; Foygel
  and Drton 2010). Implements block coordinate descent (Friedman, Hastie,
  Tibshirani 2008; Algorithm 17.2 of Hastie, Tibshirani, Friedman 2009)
  with cyclical coordinate descent for the inner lasso step. Warm-starting
  across the lambda grid accelerates the search.

  Internal helpers `glasso_one()` (single-lambda solver) and
  `compute_EBIC_glasso()` (EBIC computation) are available but not
  exported.

  Returns a list with `theta` (selected precision matrix), `lambda_opt`
  (selected lambda), `ebic_opt`, `n_edge`, and `path` (data frame of
  lambda, ebic, n_edge over the search grid).

- **`print.exametrika` Glasso method**: Added a Glasso branch to the
  shared `print.exametrika()` dispatcher to summarize the estimated
  model (optimal lambda, EBIC value, edge count, precision matrix).

- **Chatterjee's xi correlation (`chatterjee_xi`, `xi_stable`,
  `chatterjee_matrix`)**: New family of functions implementing
  Chatterjee's (2021) rank-based correlation coefficient.
  `chatterjee_xi()` computes the single-shot value with random
  tie-breaking. `xi_stable()` averages B replications (default
  B = 1000) to stabilize against tie-induced variability and returns
  a list with the mean, standard deviation, standard error, and B.
  `chatterjee_matrix()` produces the p x p asymmetric pairwise xi
  matrix from ordinal data with pairwise-complete handling of
  missing values; the asymmetry between xi(j, k) and xi(k, j)
  enables direction detection in graphical-model construction.

## Documentation and CRAN check cleanup

- **Roxygen Markdown bracket escapes**: Wrapped bracketed expressions
  inside roxygen blocks of `Glasso()` and `chatterjee_matrix()` in
  backticks (e.g., `` `[0, 1]` ``, `` `[j, k]` ``, `` `Q[i, j]` ``) so
  that roxygen2 no longer rewrites them as `\link{}` cross-references.
  Resolves R CMD check WARNINGs about missing link targets.
- **`Glasso()` `...` documented**: Added `@param ...` to `Glasso()` so
  that R CMD check's "Undocumented arguments" WARNING is cleared.
- **`compute_EBIC_glasso()` partial-match fix**: Replaced
  `determinant(Theta, log = TRUE)` with the full argument name
  `logarithm = TRUE`, removing the partial-argument-match NOTE.

# exametrika 1.12.2

## Bug fixes

- **`dataFormat(response.type = "rated")` no longer errors when a CA
  category is unobserved**: previously, if no respondent chose item
  `j`'s correct-answer category (e.g., everyone got it wrong on a hard
  item, or the sample was small enough that the CA category did not
  appear by chance), `dataFormat()` aborted with
  `"CA for item j is not a valid response category"`. This rejected
  legitimate test data where an item is uniformly difficult or where
  the sample size is small. The check has been relaxed to a
  `warning()`; the affected items are encoded as all-incorrect (`U[, j]
  == 0`), which is the correct downstream behavior. This applies to
  both the matrix-input path (`dataFormat`) and the long-format path
  (`longdataFormat`). Mistyped CAs (e.g., `CA[j] = 8` when categories
  are 1–7) still surface as warnings, so the diagnostic value is
  preserved.

# exametrika 1.12.1

## Bug fixes

- **Biclustering EM stability for empty fields/classes**: Both
  `Biclustering.nominal()` and `Biclustering.ordinal()` no longer abort
  with `"missing value where TRUE/FALSE needed"` when extreme grid
  configurations (e.g., very small `ncls` combined with very large
  `nfld`) leave fields or classes empty during EM. The two
  log-likelihood checks inside the EM loop — the relative convergence
  test and the monotonicity guard — now treat a non-finite
  `test_log_lik` as a non-converged exit instead of throwing. Affected
  cells are returned with `converge = FALSE` so that `GridSearch()`
  skips them automatically when comparing criteria. This also resolves
  the same crash in `Biclustering.rated()`, which calls
  `Biclustering.nominal()` internally.

# exametrika 1.12.0

## New features

- **Class-side Confirmatory Biclustering**: `Biclustering()` now accepts
  a `conf_class` argument that fixes class memberships during EM. Like
  `conf` (which fixes field memberships), `conf_class` accepts either a
  vector of class labels (one per respondent) or a 0/1 membership matrix
  (respondents x classes). When supplied, the class-side E-step is
  overridden every iteration. For Ranklustering (`method = "R"`), the
  neighbour-smoothing step is skipped (`smoothed_memb <- clsmemb`) since
  smoothing pre-fixed labels would defeat the purpose of fixing them.
  Available for `binary`, `ordinal`, `nominal`, and `rated` data; can be
  combined with `conf` to fix both fields and classes simultaneously.
  Note: `rated` re-orders classes by correct rate after estimation, so
  the output class labels may not match the input labels (the
  individual-to-class mapping is preserved up to relabeling).

  Number of model parameters (`nparam`) is intentionally not adjusted
  when `conf` or `conf_class` is in effect: only PiFR / BCRM cell
  probabilities count as parameters in this implementation, and
  membership matrices are latent posteriors, not parameters.

## Bug fixes

- **Confirmatory ordinal Biclustering: field membership now stays fixed
  during EM**: `Biclustering.ordinal()` was overwriting `fldmemb` with the
  E-step estimate every iteration, so the `conf`/`conf_mat` argument only
  affected the initial value. Aligned the implementation with
  `Biclustering.binary()` by re-applying `fldmemb <- conf_mat` immediately
  after the field-side E-step. With this fix `result$FieldEstimated`
  matches the user-supplied assignment exactly.
- **Confirmatory nominal Biclustering: conf argument is now actually
  honored**: `Biclustering.nominal()` validated `length(conf)` against
  `NCOL(U)`, but `U` is an `exametrika` list object so `NCOL(U)` always
  returned 1. The check rejected every well-formed `conf` vector with
  "conf vector size does NOT match with data.", making confirmatory
  nominal Biclustering unreachable since v1.10.0. Replaced with
  `NCOL(U$Q)` (and the matrix-form `NROW(conf)` check, plus the
  `conf_mat` allocation, in the same way).
- **Confirmatory ordinal Biclustering: conf length check actually
  validates input**: same `NCOL(U)` issue as nominal, but in
  `Biclustering.ordinal()`. Fixed by switching to `NCOL(U$Q)`.
- **Confirmatory binary Biclustering: conf size checks now reference the
  formatted response matrix explicitly**: `Biclustering.binary()` happened
  to work because `U` is rebound to `tmp$U * tmp$Z` before the conf block,
  so `NCOL(U)` returned the item count. Switched to `NCOL(tmp$U)` for
  consistency with the ordinal/nominal fixes and to make the intent
  obvious to readers.
- **Confirmatory Biclustering: matrix-form `conf` is no longer silently
  dropped**: when `conf` was supplied as a membership matrix
  (items x fields) instead of a vector, all three implementations
  (`binary`, `ordinal`, `nominal`) validated the matrix but never copied
  it into `conf_mat`, so the subsequent `nfld <- NCOL(conf_mat)` failed
  with `object 'conf_mat' not found`. Added `conf_mat <- as.matrix(conf)`
  in the matrix branch.

- **`Biclustering.ordinal()` now honors the `maxiter` argument**: The
  inner EM iteration cap was hardcoded to 100 (`maxemt <- 100`) and
  ignored the user-supplied `maxiter`. This mirrors the bug that was
  fixed for `Biclustering.nominal()` in 1.11.0. Callers that pass a
  larger `maxiter` (e.g. `2000` in Monte Carlo studies) now actually
  use that ceiling.

- **`GRM()` fit indices no longer return `NaN` for moderate or larger
  item counts**: The benchmark model used
  `const <- exp(-nitems * 100)` as a log-domain epsilon inside
  `sum(cat_counts * log(cat_probs + const))`. For about 8 or more items
  this expression underflows to exactly 0 in IEEE-754 double precision,
  so `log(0) = -Inf` propagates through `0 * -Inf = NaN` and poisons
  every downstream index (model_Chi_sq, NFI, CFI, RMSEA, AIC, CAIC,
  BIC). The benchmark and null loops now skip zero-count categories
  explicitly, removing the need for an additive epsilon.

- **`GRM()` degrees of freedom were computed incorrectly**: The model
  df was set to `n_pattern * (ncat - 1) + 1` and the null df to
  `n_pattern * (ncat - 1)`, which is neither `(# bench params) - (#
  model params)` nor `(# bench params) - (# null params)`. The inflated
  df then clamped `CFI`, `TLI`, `IFI`, and `RMSEA` to zero whenever
  `chi^2 < df` (and the previous `df_A > df_B` inequality was the wrong
  direction to begin with). The convention now matches
  `Biclustering.ordinal()`:
  - `bench_nparam_j = n_pattern * (ncat_j - 1)`
  - `null_nparam_j  = ncat_j - 1`
  - `model_nparam_j = ncat_j`  (one slope plus `ncat_j - 1` thresholds)
  - `df_A_j = bench_nparam_j - model_nparam_j`
  - `df_B_j = bench_nparam_j - null_nparam_j`

- **`GRM()` now accepts any integer-coded ordinal responses, not just
  1..K**: Category counts were derived from `apply(dat, 2, max)`, which
  assumes responses are already 1-indexed. Data coded from 0 (e.g.
  0..3) or with gaps (e.g. 1, 2, 4) undercounted `ncat[j]` by one or
  more and then indexed `grm_prob()[resp]` / `v[resp]` out of range,
  producing either an outright `invalid subscript type 'list'` error
  (on `J15S3810`-style data) or silently truncated threshold columns.
  Each item's responses are now remapped to contiguous 1..K codes via
  `match()` against the sorted unique values on entry, with `ncat[j]`
  derived from the mapped levels; both the R model-fit blocks and the
  C++ log-likelihood receive 1-based input regardless of the user's
  coding.

- **`GridSearch()` now tolerates per-cell fit errors**: Previously, a
  single `Biclustering()` (or `LCA()` / `LRA()`) call that raised an
  error at a grid corner (for example, empty-cluster edge cases at
  large `ncls`/`nfld` with small `nobs`/`nitems`) would propagate out
  of `GridSearch()` and abort the entire grid. The call to the
  underlying analysis function is now wrapped in `tryCatch`, and
  errors are handled the same way as non-convergence: the cell is
  marked `NA` in the index matrix and the `(ncls, nfld)` pair is
  recorded in `failed_settings`. `GridSearch()` still raises only
  when *all* grid cells fail, preserving the existing "all-failed"
  error.

## Performance

- **C++ implementation of the IRM Gibbs sampler core**: The collapsed
  Gibbs sampler shared by `Biclustering_IRM.nominal()`,
  `Biclustering_IRM.ordinal()`, and `Biclustering_IRM.rated()` is now
  implemented in C++ via Rcpp (`src/irm_gibbs_core.cpp`). The R
  reference implementation in `R/00_IRM_Gibbs_CORE.R` is preserved
  behind `irm_gibbs_core(..., use_cpp = FALSE)` for cross-checking.

  - **Numerical reproducibility (revised 2026-05-07)**: RNG calls are
    routed through R-level `sample.int()` and `rmultinom()` (via
    `Rcpp::Function`) so the `unif_rand()` consumption order matches
    base R exactly. The C-level entry points (`Rcpp::sample`,
    `R::rmultinom`) consume RNG differently from base R and were
    explicitly avoided.

    The C++ and R reference paths are bit-identical for the first
    Gibbs iterations on the bundled test datasets and on real
    polytomous data (verified through iteration 2 on J143S32 ordinal),
    but **diverge from iteration 3 onward** on real data due to
    sub-LSB floating-point ordering differences accumulating through
    the `lmvbeta()` calls in the CRP likelihood. Once the divergence
    flips a single `rmultinom()` outcome, the two chains follow
    different sample paths.

    Empirically the two paths still **target the same posterior**:
    on a 50-seed comparison (M = 66 ordinal items, N = 143), the
    marginal distributions of `n_field` and `n_class` are
    statistically indistinguishable between paths (Wilcoxon
    p = 0.56 / 0.81). Use either path for inference; do not rely
    on a single seed reproducing the same `(n_field, n_class)`
    across the two paths.

    The parity tests in `tests/testthat/test-irm-gibbs-cpp.R`
    cover only short runs (<= 10 iterations) and therefore did not
    catch this. A longer-iteration test on real-data-like
    configurations is on the roadmap.
  - **Wall-clock**: roughly 4x speedup on the inner Gibbs loop on the
    bundled test datasets (J20S600 nominal: 82 to 20 ms/iter;
    J35S500 ordinal: 76 to 19 ms/iter). The end-to-end
    `Biclustering_IRM()` call also benefits proportionally on larger
    iteration counts (e.g. simulation runs).
  - **No new dependencies**: Rcpp is already in `LinkingTo:`; the
    implementation uses only `Rcpp::Function` and `<vector>`/`<cmath>`.

- **Vectorized EM hot path in `Biclustering.ordinal()`**: The per-iteration
  cost of the ordinal EM loop has been reduced by rewriting five hot spots
  in base R without introducing new package dependencies.
    - Replaced `apply(Ufcq_prior, c(1, 2), function(x) rev(cumsum(rev(x))))`
      with an in-place reverse cumulative sum loop over the category axis.
      The previous form dispatched `nfld * ncls` R-level calls per EM
      iteration; the new form performs `maxQ - 1` vectorized array
      additions.
    - Replaced `apply(X, 1, min)` and `apply(X, 1, max)` with
      `do.call(pmin.int, as.data.frame(X))` /
      `do.call(pmax.int, as.data.frame(X))`. These compute the row-wise
      reduction in a small number of C-level calls rather than one
      R-level call per row. `apply(X, 1, which.max)` was similarly
      replaced by `max.col(X, ties.method = "first")`.
    - Precomputed `log(BBRM[,,q] - BBRM[,,q+1] + const)` once per EM
      iteration instead of independently in the class-side and field-side
      E-steps for every `q`.
    - Precomputed `Z * Uq[,,q]` once per fit (call it `ZU`) via
      column-major recycling (`Uq * as.vector(Z)`), replacing about eight
      separate elementwise products per EM iteration and reusing the
      same array in the post-EM fit-index blocks. The `replicate(maxQ, Z)`
      allocation that produced `Zrep` has been removed as a byproduct.
    - Replaced `apply(X, c(1, 2), sum)` and `apply(X, c(2, 3), sum)` on
      3-D arrays with `rowSums(X, dims = 2)` / `colSums(X, dims = 1)`,
      and `apply(M, 2, sum)` on matrices with `colSums(M)`.

  Output is bit-identical to 1.11.0 on every tested configuration
  (`max(abs(old - new)) == 0` for `BCRM`, `BBRM`, `ClassMembership`,
  `FieldMembership`, `TestFitIndices`, and the full EM trajectory).
  Single-fit wall-clock on typical sizes (ncls, nfld up to 20 by 15 on
  J35S500) improves by roughly 1.2 to 1.3 times; the per-EM-iteration
  speedup compounds across `GridSearch()` grids.

- **Vectorized 3-D apply reductions in `Biclustering.nominal()`**: The
  same `rowSums/colSums(X, dims = ...)` substitutions were applied to
  the nominal M-step and null-model blocks, and `Zrep` was eliminated
  in favor of the `ZU` precomputation. Output is bit-identical to
  1.11.0.

- **Vectorized `Uq` one-hot encoding in `Biclustering.ordinal()` and
  `Biclustering.nominal()`**: The construction of the one-hot response
  array `Uq[i, j, tmp$Q[i,j]] = 1` previously used a `nobs * nitems`
  nested R loop. It now writes all ones in one C-level matrix-index
  assignment, restricted to non-missing cells (`tmp$Z == 1`). Single-fit
  wall-clock on the validation matrix improves by 1.2-5.3x over 1.11.0
  (up to 5x on small cases where the loop overhead dominated). Output
  is bit-identical to 1.11.0 at every downstream location (the missing
  entries of the old `Uq` were never read because every consumer
  applies the `tmp$Z` mask).

## Output structure

- **`Biclustering.ordinal()` now returns `SmoothedMembership`**: The
  smoothed (Ranklustering-filtered) class membership matrix is now part
  of the return value, mirroring `Biclustering.binary()`. This fills a
  long-standing gap (only the binary form previously exposed it) and
  makes Ranklustering with `conf_class` introspectable: callers can
  verify that the smoothing step is correctly skipped when class labels
  are pre-fixed (`SmoothedMembership` equals `ClassMembership` in that
  case).

## Tests

- Added a `print()` smoke test for `Biclustering_IRM.rated()` results
  (`test-irm-rated.R`).
- Added an `nrank = 4` `LRA.rated` regression test for
  `DistractorAnalysis()` to cover varying rank counts
  (`test-distractor.R`).

## Notes

- No changes to package `Imports` or `Depends`. The new C++ Gibbs
  sampler uses Rcpp, which was already declared in `LinkingTo:`.
- No new exported functions. The new `conf_class` argument on
  `Biclustering()` is additive and defaults to `NULL`, preserving the
  previous behavior for all existing callers.

# exametrika 1.11.0

## New Features

- **`Biclustering_IRM.rated()`: Rated IRM (Infinite Relational Model for multiple-choice data)**: Performs IRM-based biclustering for rated data (items with correct answers and multiple response categories). Internally calls `Biclustering_IRM.nominal()` for estimation via Dirichlet-Multinomial collapsed Gibbs sampler, then post-processes the results: classes are sorted by correct response rate (ranklustering), and two layers of fit indices are reported — binary (item correct/incorrect, with full benchmark model including CFI/RMSEA) and nominal (category-level, AIC/BIC/CAIC only). Returns `TestFitIndices` (binary, default) and `TestFitIndices_nominal` (nominal) in the output.

- **`DistractorAnalysis()`: Distractor analysis for rated models**: S3 generic function for analyzing response category distributions by rank/class. Computes observed frequency tables, proportion tables, chi-square tests against chance level, and Cramer's V effect sizes for each item-by-rank cell. Works with `LRA.rated` and `Biclustering.rated` / `Biclustering_IRM.rated` results. For Biclustering models, output is grouped by field. Supports `items` and `ranks` filtering in both `print()` and `plot()` methods.

## New Data

- **`J21S300`**: New rated sample dataset (21 items, 300 students, 4 response categories) for testing rated Biclustering and IRM models. Smaller and faster than J35S5000 (35 items, 5000 students).

## Bug Fixes

- **Fixed `maxiter` parameter ignored in `Biclustering.nominal()`**: The `maxiter` parameter was accepted but internally overridden by a hardcoded `maxemt <- 100`. Now correctly uses `maxiter` as the iteration limit.

- **Fixed spurious "No ID column detected" message in `Biclustering.rated()` and `Biclustering_IRM.rated()`**: The `crr()` function was called with a raw matrix (`tmp$Z * tmp$U`) instead of an exametrika object, triggering an unnecessary `dataFormat()` call. Now creates a temporary binary-typed exametrika object to avoid the message.

# exametrika 1.10.2

## New Features

- **`Biclustering.rated()`: Rated (multiple-choice) Biclustering**: Performs biclustering for rated data (items with correct answers and multiple response categories). Internally calls `Biclustering.nominal()` for estimation, then post-processes the results: classes are sorted by correct response rate (ranklustering), and two layers of fit indices are reported — binary (item correct/incorrect, with full benchmark model) and nominal (category-level, AIC/BIC/CAIC only). The binary layer uses item-level correct response rates per class without field constraints (`nparam = nitems * ncls`). Supports both Biclustering (`method = "B"`) and Ranklustering (`method = "R"`) modes. Returns `TestFitIndices` (binary) and `TestFitIndices_nominal` (nominal) in the output. Added 16 tests for rated Biclustering using J35S5000.

## Improvements

- **Renamed `ItemFRP` to `quasiFRP` in `Biclustering.rated()` output**: The item-level correct response rate matrix (J x C) is computed without field constraints because the field assignments come from nominal analysis and have different meaning in the binary context. The name `quasiFRP` (quasi Field Reference Profile) clarifies this distinction from the standard field-constrained FRP.

## Bug Fixes

- **Fixed `subscript out of bounds` in nominal/ordinal Biclustering with large `nfld`**: The initial field assignment `ceiling(1:nitems / (nitems / nfld))` could exceed `nfld` due to floating-point rounding. Added `pmin(..., nfld)` clamping, consistent with the binary Biclustering fix already in place.
- **Fixed redundant `dataFormat()` call in `Biclustering.rated()`**: The function was unnecessarily re-calling `dataFormat()` on already-formatted data, causing repeated "No ID column detected" messages during `GridSearch()`. Now reuses the existing exametrika object directly.
- **Fixed Array plot for polytomous Biclustering (rated/nominal/ordinal)**: Array plots for polytomous Biclustering models were rendered in black and white because `x$U` (binary correctness matrix) was used instead of `x$Q` (polytomous responses). Now correctly uses `x$Q` for polytomous models.
- **Fixed FCRP plot `invalid graphics state` error**: `par(mfrow=...)` and `layout()` conflicted when plotting FCRP/FCBR. Now skips `par(mfrow=...)` for plot types that use `layout()` internally.
- **Improved `id` parameter validation in `dataFormat()`**: When a vector (e.g., `id = tmp$ID`) was passed instead of a column number, the error message was cryptic (`the condition has length > 1`). Now validates that `id` is a single integer and provides a clear error message.
- **Fixed nominal Biclustering/IRM fit indices**: Removed benchmark (saturated) model for nominal data. With many items and categories, every examinee has a unique response pattern, making the benchmark model trivially saturated (`ell_B = 0`) and chi-square based indices (NFI, RFI, IFI, TLI, CFI, RMSEA) meaningless. These indices are now reported as `NA` for nominal data. Information criteria (AIC, BIC, CAIC) are computed directly from the model log-likelihood and remain available. Also fixed a `length(benchGroup)` → `length(unique(benchGroup))` bug in `Biclustering.nominal()` (the `unique()` call was missing).

## Improvements

- **Added empty field warning for all Biclustering models**: When some fields have no items assigned (e.g., due to specifying too many fields), a warning message is now emitted suggesting to reduce `nfld`. Affects `Biclustering()` (binary/ordinal/nominal), `Biclustering_IRM()` (binary/ordinal/nominal).
- **Renamed `field` to `fld` in `Biclustering_IRM.binary()`**: Internal variable name changed from `field` to `fld` for consistency with other Biclustering models. No change to the public API (`FieldEstimated` output name is unchanged).

## Documentation Fix

- **Fixed J35S5000 UseCase in data format tables**: Corrected from "Nominal LRA" to "Rated LRA" in getting-started vignette, and from "名義LRA" to "評定LRA" in Japanese guide. J35S5000 is a rated (multiple-choice with correct answers) dataset, not nominal.
- **Fixed J15S3810 roxygen documentation**: The `@format` field incorrectly stated "nominal responses" but the dataset is ordinal (`response.type=ordinal`). Also fixed "A ordinal" to "An ordinal" in `@description`.

# exametrika 1.10.1

## Vignette Build Time Reduction

- **Reduced vignette build time from ~46 minutes to ~5 minutes**: Added `eval=FALSE` to computationally intensive code chunks in vignettes (GridSearch, IRM, LDLRA, LDLRA_PBIL, LDB, BINET, ordinal/nominal Biclustering, ordinal/rated LRA). The Japanese guide (`guide-ja`) now uses `eval=FALSE` for all model estimation chunks to avoid duplicating computation from English vignettes. Full rendered output is available on the [pkgdown site](https://kosugitti.github.io/exametrika/).

# exametrika 1.10.0

## Bug Fixes

### CAIC (Consistent AIC) Formula Correction

- **Fixed CAIC formula to match Bozdogan (1987) original definition**: The CAIC penalty term was `log(n + 1)` but should be `log(n) + 1` per Bozdogan (1987, Psychometrika, 52(3), p.358, Proposition 2, Eq.44). The original Mathematica implementation had this error (`Log[nobs + 1]`), and the R port inherited it. Both the R version (`R/00_ModelFitModule.R`) and the Mathematica version (`develop/mtmk15forVer13/mod/Module_ModelFit.nb`) have been corrected. The numerical difference is approximately 1 (constant), but the corrected formula now matches the published definition: `CAIC(k) = -2 log L + k * (log(n) + 1)`. This affects all models that compute fit indices: IRT, LCA, LRA (binary/ordinal/rated), Biclustering (binary/ordinal/nominal), IRM, BNM, LDLRA, LDB, BINET, and GRM. All 873 tests pass with the corrected formula.

### GRM Example Fix

- **Changed GRM examples from `\donttest` to `\dontrun`**: GRM's multi-panel plot examples caused "invalid graphics state" errors in non-interactive environments (pkgdown, CI). Changed to `\dontrun` to prevent build failures.

### dataFormat Robustness Improvements

- **Fixed auto-detection ignoring `CA` parameter**: When `CA` (correct answer vector) was provided but `response.type` was not explicitly specified, `dataFormat()` incorrectly classified the data as `"ordinal"` instead of `"rated"`. This caused `$U` (binary scoring matrix) to be `NULL`. The auto-detection now checks for `CA` before ordinal/nominal detection: binary → rated (if CA provided) → ordinal → nominal.
- **Fixed `id` parameter not working for column 1**: Changed `id` default from `1` to `NULL`. Previously, `id = 1` (both default and explicit) triggered auto-detection heuristics, making it impossible to explicitly specify the first column as the ID column. Now `id = NULL` triggers auto-detection, and any numeric value (including `1`) forces that column to be used as ID.
- **Improved ID auto-detection for consecutive integers**: Simplified the first-column heuristic to treat unique consecutive integers (e.g., `1:N`) as ID regardless of whether they also look like valid response values. Previously, `1:10` was misclassified as response data because `looks_like_response_data()` returned TRUE.
- **Fixed missing values in rated `U` matrix**: When the response matrix contained missing values (`-1`), the binary scoring matrix `U` incorrectly scored them as `0` (incorrect) instead of `-1` (missing). Now `U[i,j] = -1` when `Q[i,j] = -1`.
- **Fixed `drop=FALSE` missing in item exclusion**: When items were excluded due to invalid variance (e.g., containing `Inf`), the column subsetting `response.matrix[, mask]` dropped matrix dimensions if only one item remained, causing a crash. Added `drop = FALSE`.
- **Added diagnostic messages**: `dataFormat()` now reports via `message()` when it detects problematic data:
  - Items with all missing values
  - Items with zero variance (constant response values)
  - Students with all missing responses

### BINET `g_list` / `adj_list` Input Path Fix

- **Fixed `g_csv` variable undefined error when using `g_list` or `adj_list` input**: `BINET()` crashed with an undefined variable error at the graph construction step when the DAG was specified via `g_list` or `adj_list` parameters. The internal variable `g_csv` (used to build the integrated graph object `all_g`) was only defined in the `adj_file` code path. Added logic to reconstruct `g_csv` from `adj_list` for the `g_list` and `adj_list` input paths, ensuring `all_g` is correctly built with Field edge attributes regardless of input method.
- **Fixed `g_list` / `adj_list` length validation**: The length check for `g_list` and `adj_list` incorrectly compared against `ncls` (number of classes) instead of `nfld` (number of fields). In BINET, each element of `adj_list` represents the DAG structure at a specific field, so the list length should equal `nfld`. Also fixed `g_list` type check from `g_list[[1]]` (always checking the first element) to `g_list[[j]]` (checking each element).

### Biclustering_IRM.binary S3 Method Consistency Fix

- **Added `...` to `Biclustering_IRM.binary()` signature**: The S3 generic `Biclustering_IRM(U, ...)` requires all methods to include `...` in their formal arguments. The `.binary` method was missing it, causing an R CMD check WARNING. Added `...` to match the generic and the `.nominal`/`.ordinal` methods.

### Biclustering_IRM `msg` Field Fix

- **Fixed IRM `msg` field to correctly distinguish Rank vs Class models**: Binary IRM and Ordinal IRM are Ranklustering models (ordered latent classes), so `msg = "Rank"` is correct. Nominal IRM has no Ranklustering concept (unordered latent classes), so `msg = "Class"` is correct. Previously all three methods had inconsistent values; now `Biclustering_IRM.binary` and `Biclustering_IRM.ordinal` return `msg = "Rank"`, while `Biclustering_IRM.nominal` returns `msg = "Class"`. Also updated internal column names of `cls01` and `FRP` from `"Class"` to `"Rank"` for binary/ordinal IRM. The shared Gibbs core (`irm_gibbs_core()`) output column names were also updated.

### Biclustering_IRM.binary Missing `LRD` Alias

- **Added `LRD` (Latent Rank Distribution) alias to `Biclustering_IRM.binary()` return value**: The binary IRM was the only Biclustering model that did not include `LRD` in its return value (it only had `LCD`). Other Biclustering models (binary Biclustering, nominal IRM, ordinal IRM) all return both `LRD` and `LCD`. Added `LRD = clsdist` as an alias for `LCD` to ensure consistency across all Biclustering-family models. This improves interoperability with downstream packages (ggExametrika) that look up `LRD` when `msg == "Rank"`.

### Biclustering_IRM Seed Default

- **Reverted `Biclustering_IRM()` seed default back to 123**: Ensures reproducibility by default.

### Biclustering_IRM `t(apply())` Dimension Drop Fix

- **Fixed `t(apply(log_S, 1, irm_log_to_prob))` dimension drop in Nominal/Ordinal IRM**: When the Gibbs sampler converged to `ncls=1` or `nfld=1`, `apply()` on a single-column matrix returned a vector instead of a matrix, causing `t()` to produce a 1-row matrix instead of an N-row matrix. This led to incorrect class/field membership assignment. Replaced all 6 instances of `t(apply(mat, 1, fun))` with explicit for-loops in both `Biclustering_IRM.nominal()` (3 locations: EM E-step, final class membership, final field membership) and `Biclustering_IRM.ordinal()` (3 locations: same). Consistent with the project's `apply()` caution policy.

### Biclustering_IRM Log-Probability Normalization Fix

- **Fixed log-to-probability conversion in `Biclustering_IRM()` Gibbs sampler**: Changed `exp(ptab - min(ptab))` to `exp(ptab - max(ptab))` for numerical stability. The previous implementation subtracted the minimum log-probability, which could cause overflow (`exp(large positive) = Inf`) when the range of log-probabilities was large, resulting in `NaN` after normalization. Subtracting the maximum ensures the largest value becomes `exp(0) = 1` and all others underflow harmlessly to near-zero values.

### Biclustering.nominal Model Fit Fix

- **Fixed `Biclustering.nominal()` using stale log-likelihood in model fit indices**: When the EM algorithm exited due to log-likelihood decrease, `BCRM` was correctly reverted to the previous iteration's value, but `test_log_lik` was not. The model fit section recalculated the correct log-likelihood as `testell`, but `chi_A`, `model_log_like`, `log_lik`, and `LogLik` all referenced the stale `test_log_lik` instead of `testell`. Also removed a duplicated model fit code block (copy-paste error).

### J20S400 Dataset Fix

- **Fixed `J20S400` response type from `nominal` to `binary`**: The `J20S400.rda` dataset was incorrectly stored with `response.type = "nominal"` despite being a binary (0/1) dataset with -99 as missing values. Missing values were not properly handled (Z was all 1s, Q contained raw -99 values). Regenerated from the original CSV source (`develop/sampleData/J20S400.csv`) with `dataFormat(..., na = -99)`, now correctly typed as `binary` with 86 missing values properly masked in Z.

## New Features

### Polytomous IRM (Biclustering_IRM.nominal / Biclustering_IRM.ordinal)

- **New `Biclustering_IRM.nominal()` for nominal/polytomous data**: Extends the Infinite Relational Model (IRM) from binary to nominal scale data using a Dirichlet-Multinomial collapsed Gibbs sampler. The Chinese Restaurant Process (CRP) automatically determines the optimal number of classes and fields. After the Gibbs sampling phase, small classes are consolidated and refined with an EM algorithm. The Dirichlet prior concentration parameter `alpha` controls smoothing of category probabilities.
- **New `Biclustering_IRM.ordinal()` for ordinal/polytomous data**: Extends IRM to ordinal scale data. Shares the same Dirichlet-Multinomial collapsed Gibbs sampler as nominal IRM (Phase 1), then applies ordinal-specific EM refinement (Phase 2) with cumulative normalization to enforce monotonic category boundaries. The `mic` parameter (default `TRUE`) enforces monotone increasing class ordering by expected score sum. Reports BFRP (Bicluster Field Reference Profile), FRPIndex, TRP, and Strongly/Weakly Ordinal Alignment Conditions (SOACflg/WOACflg).
- **`Biclustering_IRM()` is now an S3 generic**: Dispatches to `Biclustering_IRM.binary` (existing binary IRM), `Biclustering_IRM.nominal` (new), and `Biclustering_IRM.ordinal` (new). Raw data is automatically formatted and dispatched based on `response.type`.
- **Shared Gibbs sampler core**: The collapsed Gibbs sampler has been extracted into a shared internal function `irm_gibbs_core()` (in `R/00_IRM_Gibbs_CORE.R`), used by both nominal and ordinal IRM. Helper functions (`irm_calc_Ufcq`, `irm_lmvbeta`, `irm_log_to_prob`, `irm_bic_calc`, etc.) are also shared.
- **Performance optimization**: The Gibbs sampler uses differential updates for the sufficient statistics array `U_fcq`, computing only the contribution of the target student/item rather than recalculating the full array at each step.

### Confirmatory LCA/LRA (Test Equating)

- **`LCA()` and `LRA()` now support a `conf` parameter for confirmatory analysis**: The `conf` argument accepts an IRP matrix (ncls/nrank x testlength) where non-NA values are held fixed throughout EM estimation and NA values are freely estimated. This enables test equating scenarios where anchor items retain their known IRPs while new items are calibrated against them.
- **Label-based item matching**: When `conf` has column names, items are matched by label rather than by position. This allows `conf` to contain a subset of items (anchor items only) or items in a different order than the data. Items in the data but not in `conf` are automatically set to freely estimated. Unknown labels in `conf` produce an informative error.
- **Works with both GTM and SOM methods** for LRA.

### Internal Refactoring: SOM Estimation

- **Extracted SOM estimation into `somclus()` internal function**: The Self-Organizing Maps estimation code previously inlined in `LRA.binary()` (~100 lines) has been extracted into a standalone internal function `somclus()` in `00_EMclus.R`. This parallels `emclus()` (GTM/EM) and returns the same structure. Also fixed a pre-existing typo (`h_cout` → `h_count`) and another (`oldsBIC` → `oldBIC`).

## Test Suite Modernization

- **Complete migration from Excel to CSV fixtures**: Removed all 14 legacy test files that depended on `tidyverse` and `readxl` for reading Excel-based Mathematica reference data. Replaced with 24 modern test files using base R `read.csv()` and `test_path()` to load CSV fixtures from `tests/testthat/fixtures/mathematica_reference/`. The new test suite has zero external package dependencies beyond `testthat`.
- **Removed `readxl`/`tidyverse` from test dependencies**: DESCRIPTION `Suggests` no longer requires any packages beyond `knitr`, `rmarkdown`, and `testthat`.
- **Fixture file reorganization**: Shortened overly long CSV fixture filenames to comply with CRAN's 100-byte portable path requirement.
- **Test coverage**: 26 test files, 321 test blocks, covering all models (CTT, IRT 2PL/3PL/4PL, LCA, LRA binary/ordinal/nominal, Biclustering binary/ordinal/nominal, IRM binary/nominal/ordinal, BNM, LDLRA, LDB, BINET, GRM, GridSearch, dataFormat, polychoric correlation, scoring, student/test analysis, confirmatory LCA/LRA). 85 Mathematica reference CSV files for cross-validation.
- **Added Nominal IRM tests** (`test-irm-nominal.R`): 18 test blocks for `Biclustering_IRM.nominal()` using J20S600 data — basic execution, dimensions, FRP validity, membership, fit indices, backward compatibility, seed reproducibility, alpha validation.
- **Added Ordinal IRM tests** (`test-irm-ordinal.R`): 25 test blocks for `Biclustering_IRM.ordinal()` using J35S500 data — basic execution, dimensions, FRP validity, expected scores, TRP, BFRP, FRPIndex, SOAC/WOAC flags, mic parameter, fit indices, backward compatibility, seed reproducibility, alpha validation.

## Documentation

- **TestStatistics example**: Added stem-and-leaf plot example using `stem(nrs(dataFormat(J15S500)))` to demonstrate score distribution visualization.

## Internal Improvements

- **pkgdown migration**: Migrated documentation site from Jekyll (main/docs) to pkgdown (gh-pages branch via GitHub Actions).
- **CI/CD**: Added GitHub Actions workflows for automated R CMD check, test coverage reporting, and pkgdown site deployment. Removed Codecov upload step from test-coverage workflow.
- **.Rbuildignore cleanup**: Removed `^tests$` and `^inst$` entries that were incorrectly excluding tests and vignettes from the built package.

---

# exametrika 1.9.0

## Bug Fixes

### GridSearch `index` Parameter Fixes

- **Index alias support**: `GridSearch()` now accepts common aliases for fit indices. `"loglik"`, `"log_lik"`, `"LogLik"`, and `"LL"` are mapped to `"model_log_like"`. `"Chi_sq"` and `"chi_sq"` are mapped to `"model_Chi_sq"`. Previously, using these aliases caused silent `NULL` extraction and eventual errors.
- **Early validation**: Invalid index names are now caught immediately at the start of `GridSearch()`, before the computationally expensive grid search loop runs. The error message lists all valid options including available aliases.
- **Log-likelihood optimization direction**: `model_log_like` is now correctly treated as a maximization target (larger log-likelihood = better fit). Previously, it was incorrectly placed in the minimization group, which would have selected the worst-fitting model.

### Test Tolerance Fix

- **Q3 matrix test**: Changed the 2PL Q3 matrix test from relative tolerance (`tolerance = 1e-2` in `expect_equal`) to absolute difference comparison (threshold 0.005). Q3 residual correlations include values near zero where relative tolerance comparisons are unreliable.

### NAMESPACE Fix

- **Missing imports**: Added `layout` and `plot.new` from `graphics` to NAMESPACE. These functions are used by the legend strip layout helpers for polytomous Biclustering plots.

### LRA.ordinal / LRA.rated Category Computation Fix

- **Fixed `apply(U$Q, 2, unique)` returning matrix instead of list**: When all items have the same number of response categories (e.g., all 5-point Likert), `apply()` returns a matrix rather than a list. The subsequent `lapply()` then iterates over individual elements instead of per-column vectors, causing `ncat` to be a vector of 1s and crashing the algorithm. Replaced with `lapply(seq_len(nitems), function(j) sort(unique(U$Q[, j])))` which always returns a proper list. Same fix applied to `catfreq999` computation. Both `LRA.ordinal` and `LRA.rated` were affected.

### GRM ItemFitIndices Computation Fix

- **Fixed `apply(tmp$Q, 2, table)` returning matrix instead of list**: Same class of bug as the LRA.ordinal/LRA.rated fix above. In `GRM()`, the null model log-likelihood computation used `apply(tmp$Q, 2, table)` to compute per-item category frequencies. When all items have the same number of response categories (e.g., all 5-point Likert), `apply()` returns a matrix instead of a list, causing `response_list[[j]]` to extract a single number rather than the full frequency table. This produced incorrect `null_log_like` values and consequently wrong chi-square statistics, RMSEA, TLI, and CFI in `ItemFitIndices`. Replaced with `lapply(seq_len(nitems), function(j) table(tmp$Q[, j]))` which always returns a proper list.

### LRA.ordinal Mixed Category Count Validation

- **Added input validation for mixed category counts**: `LRA.ordinal()` now raises an informative error when items have different numbers of response categories (e.g., some items with 3 categories and others with 5). The internal matrix algebra uses fixed-stride indexing that assumes uniform category counts. The error message suggests alternatives (`LRA.rated`, `Biclustering.ordinal`) that support mixed category counts via list-based designs.

### LCA/LRA FRP Plot Type Removal

- **Removed "FRP" from valid plot types for LCA and LRA**: Field Reference Profile (FRP) requires a field structure (item grouping), which LCA and LRA do not have. Previously, `plot(lca_result, type = "FRP")` passed validation but failed at runtime because the `$FRP` field does not exist in LCA/LRA return values. Now properly rejected with an informative error message at the validation stage.

## New Features

### New Sample Datasets for Polytomous Biclustering

- **J35S500**: Simulated ordinal dataset (500 students, 35 items, 5 categories) with a cumulative staircase pattern (5 latent classes, 5 fields). Contains approximately 0.5% missing values.
- **J20S600**: Simulated nominal dataset (600 students, 20 items, 4 categories) with a cyclic category preference pattern (5 latent classes, 4 fields). Contains approximately 0.5% missing values.

### New Plot Types for Polytomous Biclustering

- **FRP** (Field Reference Profile): Expected score line plot per field, with `stat` parameter supporting `"mean"` (default), `"median"`, and `"mode"`.
- **FCRP** (Field Category Response Profile): Category probability plot per field, with `style` parameter supporting `"line"` (default) and `"bar"` (stacked bar chart).
- **FCBR** (Field Cumulative Boundary Reference): Boundary probability plot per field (ordinal Biclustering only).
- **ScoreField**: Heatmap of expected scores across fields and latent classes/ranks.
- **RRV** (Rank Reference Vector): Transposed view with fields on x-axis and expected scores on y-axis, with `stat` parameter.

### FRPIndex for Ordinal Biclustering

- Ordinal Biclustering now computes `FRPIndex` (Field Reference Profile indices) including location parameters (Alpha, Beta), slope parameters (A, B), and monotonicity indices (Gamma, C).

### New Parameters for `plot.exametrika()`

- `stat`: Controls the summary statistic for FRP and RRV plots (`"mean"`, `"median"`, `"mode"`).
- `style`: Controls the display style for FCRP plots (`"line"`, `"bar"`).

### Array Plot Improvements

- **Missing value display**: Array plots now display missing values in a distinct color. Binary data uses gray (`#808080`) to distinguish from white (incorrect) and black (correct). Polytomous data uses black (`#000000`) to distinguish from the category color palette.

### Print Method Improvements

- **Ordinal Biclustering**: `print()` now displays FRPIndex (Field Reference Profile Indices) with a note that the values are based on normalized expected scores `(E[score]-1)/(maxQ-1)`.

### Documentation Improvements

- **FRPIndex**: Expanded documentation for the 6 profile shape indices (Alpha, A, Beta, B, Gamma, C) in `?Biclustering`, including detailed definitions and polytomous adaptation logic.

### Return Value Structure Unification

Systematic unification of return value structures across all analysis functions for consistency and interoperability.

#### snake_case Field Names Extended to All Functions

- **LDLRA**: Added `n_class` (retaining `Nclass` for backward compatibility)
- **LDB**: Added `n_rank`, `n_field` (retaining `Nrank`, `Nfield`)
- **BINET**: Added `n_class`, `n_field` (retaining `Nclass`, `Nfield`)
- **Biclustering.nominal**: Added `n_class`, `n_field`, `n_cycle` (retaining `Nclass`, `Nfield`, `N_Cycle`)
- **Biclustering.ordinal**: Added `n_class`, `n_field`, `n_cycle` (retaining `Nclass`, `Nfield`, `N_Cycle`)
- **Biclustering_IRM**: Added `n_cycle`, `N_Cycle` (retaining `em_cycle`, `EM_Cycle` as IRM-specific aliases)

#### Top-Level `log_lik` Added to All Functions

- All analysis functions now consistently provide `log_lik` at the top level of the return object, matching `TestFitIndices$model_log_like`.
- Functions updated: IRT, LCA, LRA.binary, BNM, LDLRA, LDB, BINET, Biclustering.binary, Biclustering_IRM

#### TestFitIndices Structure Unified

- **GRM, Biclustering.nominal, Biclustering.ordinal**: TestFitIndices now uses the full 16-field structure with `ModelFit` class, matching the format used by IRT/LCA/LRA and other functions. Previously these used bare `calcFitIndices()` output (9 fields, no class). Added fields: `model_log_like`, `bench_log_like`, `null_log_like`, `model_Chi_sq`, `null_Chi_sq`, `model_df`, `null_df`.
- **GRM ItemFitIndices**: Also unified to the full 16-field structure with `ModelFit` class.
- **BINET**: `TestFitIndices` added as primary name for multigroup fit indices (previously only `MG_FitIndices`). `MG_FitIndices` retained as backward-compatible alias. `SM_FitIndices` (saturated model) remains unchanged.

#### Students Matrix Enhanced

- **Biclustering.nominal**: Added `Estimate` column (most probable class assignment) to the Students matrix.
- **Biclustering.ordinal**: Added `Estimate` column (most probable class assignment) to the Students matrix.

#### Other Structural Improvements

- **Biclustering_IRM**: Added `FRPIndex` (Field Reference Profile indices) for consistency with Biclustering.binary and LDB.
- **LDB**: Fixed `TRP` from `matrix(1×ncls)` to `numeric vector`, consistent with all other functions.
- **GridSearch**: Added `class = c("exametrika", "GridSearch")` to return value for method dispatch support.

### Bug Fixes

- **LCA `msg` field assignment**: Fixed `msg <- "Class"` to `msg = "Class"` inside `structure()` call (line 150 of `05_LCA.R`). The `<-` operator was being interpreted as a standalone assignment rather than a named list element, causing the `msg` field name to be empty.
- **RMP/CMP single student plot error**: Fixed dimension drop error when plotting RMP or CMP for a single student (e.g., `plot(r, type="RMP", students=1)`). Added `drop = FALSE` to prevent matrix-to-vector coercion when extracting a single row from the Students matrix.
- **LRA.ordinal / LRA.rated `TestFitIndices` log-likelihood fields**: Fixed `null_log_like` which incorrectly stored the saturated model log-likelihood (`log_lik_satu`) instead of the null model log-likelihood (`sum(null_itemll)`). Added missing `bench_log_like` field containing the saturated model log-likelihood (`sum(satu_itemll2)`). Note: the chi-square-based fit indices (NFI, CFI, TLI, RMSEA, AIC, BIC, etc.) were always computed correctly; only the stored log-likelihood labels were affected.
- **LRA.ordinal / LRA.rated FitIndices structure**: Unified `TestFitIndices` and `ItemFitIndices` to the standard 16-field structure with `ModelFit` class (`c("exametrika", "ModelFit")`), matching all other analysis functions. Added `model_log_like`, `bench_log_like`, `null_log_like` to `ItemFitIndices`. Removed `ScoreRankCorr` / `RankQuantCorr` from `TestFitIndices` (already available at the top level of the return object).
- **LRA.ordinal / LRA.rated test updates**: Updated `test-12OLR.R` and `test-13NLR.R` to handle the new `ModelFit` class (add `unclass()` before `as.data.frame()`) and adjusted column/index references to match the unified 16-field structure.

### Plot Layout Improvements

- **Legend strip layout**: Moved per-panel legends to a shared legend strip below the plot area for FCRP (line/bar), FCBR, GRM IRF, and IRT overlay (IRF/IIF) plots. Uses `layout()` with a thin dedicated row (height ratio 0.2) to reduce visual clutter in data panels. Added `setup_legend_layout()` and `draw_legend_strip()` internal helper functions.
- **FCBR reference line**: Added `P(Q>=1)=1.0` reference line at the top of FCBR plots for visual completeness.

---

# exametrika 1.8.1

## Bug Fixes

### dataFormat Function

- **Fixed factor ID column detection**: `dataFormat()` now correctly identifies factor-type ID columns before converting factors to numeric. Previously, the factor-to-numeric conversion occurred before ID detection, causing factor ID columns with many levels (>=20) to trigger a "Too many categories" error instead of being recognized as IDs.
- **Removed unused helper function**: Removed dead code (`is_response_data()`) that was defined but never called.

### GridSearch Function

- **Fixed ordinal data support**: GridSearch now correctly handles ordinal data by using `obj$Q` instead of `obj$U` for test length calculation
- **Resolved nfld=1 parameter issue**: Eliminated invalid parameter ranges that caused crashes with ordinal datasets

### Biclustering.ordinal Function

- **Enhanced numerical stability**: Added `pmax(Ufcq_prior, 1e-10)` to prevent division by zero and NaN errors
- **Fixed convergence failures**: Resolved "missing value where TRUE/FALSE needed" errors in specific parameter combinations (e.g., ncls=4 with nfld=5)
- **Improved robustness**: Algorithm now handles edge cases where field membership probabilities approach zero

### Impact

- GridSearch now works reliably with ordinal data across all parameter combinations
- Biclustering analysis completes successfully for previously problematic parameter settings
- Enhanced overall stability for large-scale grid searches

---

# exametrika 1.8.0

## Naming Convention Improvements

This release improves naming consistency across the package while maintaining full backward compatibility through a deprecation path.

### New Field Names (Recommended)

All analysis functions now return results with snake_case field names for better consistency:

+ `n_class` - Number of latent classes (replaces `Nclass`)
+ `n_field` - Number of latent fields (replaces `Nfield`)
+ `n_rank` - Number of latent ranks (replaces `Nrank`)
+ `n_cycle` - Number of EM iterations (replaces `N_Cycle`)
+ `log_lik` - Log-likelihood value (replaces `LogLik`)

### Deprecated Field Names (Still Supported)

The old field names continue to work for backward compatibility:

+ `Nclass`, `Nfield`, `Nrank`, `N_Cycle`, `LogLik` - **Deprecated but functional**
+ These fields will be removed in version 2.0.0
+ Please update your code to use the new snake_case names

### Example Migration

```r
# Old code (deprecated but still works)
result <- LCA(data, ncls = 3)
n_classes <- result$Nclass

# New code (recommended)
result <- LCA(data, ncls = 3)
n_classes <- result$n_class
```

### Output Formatting Improvements

Progress messages now display properly in R Markdown documents:

+ **GridSearch()** - Added `verbose` parameter (default: `TRUE`)
  + Output organized by row: `ncls = 2: nfld=2 nfld=3 nfld=4 ...`
  + Set `verbose = FALSE` to suppress all progress messages
+ **Biclustering_IRM()** - Improved iteration display
  + Format: `iter 1: match=0 nfld=15 ncls=30`
  + Class adjustment messages: `Adjusting classes: BIC=-99592.5 ncls=21 (min size < 20)`
+ **LRA.ordinal()** - Fixed verbose behavior
  + Changed default from `verbose = TRUE` to `verbose = FALSE` (consistent with binary/rated versions)
  + Saturation Model: `Saturation Model - iter 1: log_lik=-1.234567`
  + Restricted Model: `Restricted Model - iter 1: log_lik=-0.987654`
+ **LRA.rated()** - Fixed verbose behavior
  + Changed default from `verbose = TRUE` to `verbose = FALSE` (consistent with binary/ordinal versions)
  + Same improved format as LRA.ordinal

All progress messages now use proper line breaks instead of carriage returns, ensuring clean output in R Markdown/knitr documents and web documentation.

### Bug Fixes

+ Fixed Array plot color mapping for binary data
  + Used `sort(unique(...))` to ensure consistent ordering: 0 (white/incorrect) → 1 (black/correct)
  + Previously, color mapping could be reversed depending on data order
+ Fixed Ranklustering Array plot sorting order
  + Students with higher correct response rates now appear at the bottom of the plot
  + Previously, high performers were incorrectly placed at the top

### Internal Improvements

+ Standardized internal variable naming (e.g., `testEll` → `test_log_lik`)
+ Improved code readability and maintainability
+ All output messages now use consistent `log_lik` terminology

# exametrika 1.7.0

+ Renamed IRM() to Biclustering_IRM() for consistency with structure learning naming conventions
  + Follows the same pattern as BNM_GA(), LDLRA_PBIL(), etc. (model_method naming)
  + IRM() function still works but is now deprecated with a warning using .Deprecated()
  + The new name clarifies that this function performs Biclustering structure learning using the Infinite Relational Model
  + All documentation and examples updated to use Biclustering_IRM()
+ Added .Deprecated() warnings to renamed functions from version 1.6.5
  + StrLearningGA_BNM() now shows deprecation warning, recommending BNM_GA()
  + StrLearningPBIL_BNM() now shows deprecation warning, recommending BNM_PBIL()
  + StrLearningPBIL_LDLRA() now shows deprecation warning, recommending LDLRA_PBIL()
  + Old function names still work for backward compatibility but display warnings

# exametrika 1.6.5

+ Critical bugfix for LCA() response type validation
  + Fixed incorrect variable reference in response type checking
+ Added beta1 and beta2 parameters to all Beta distribution-based functions
  + LRA.binary(): beta1=1, beta2=1 (GTM method)
  + LCA(): beta1=1, beta2=1
  + Biclustering.binary(): beta1=1, beta2=1
  + BNM(): beta1=1, beta2=1
  + LDB(): beta1=1, beta2=1
  + LDLRA(): beta1=2, beta2=2
  + LD_param_est(): beta1=2, beta2=2 (internal helper function)
  + LDLRA_PBIL(): beta1=2, beta2=2
  + These parameters control the prior density parameters in Bayesian parameter estimation
  + Users can now customize Beta distribution parameters for EM algorithm parameter updating
  + Default values preserve backward compatibility with previous versions
+ Added alpha parameter to polytomous models for Dirichlet prior control
  + Biclustering.ordinal(): alpha=1 (flat Dirichlet prior)
  + Biclustering.nominal(): alpha=1 (flat Dirichlet prior)
  + These parameters control the concentration parameter for Dirichlet priors in category probability estimation
  + Users can customize Dirichlet parameters to adjust prior strength (alpha > 0)
  + Default values (alpha=1) preserve backward compatibility with previous versions
+ Simplified function names for structure learning functions
  + StrLearningGA_BNM() renamed to BNM_GA()
  + StrLearningPBIL_BNM() renamed to BNM_PBIL()
  + StrLearningPBIL_LDLRA() renamed to LDLRA_PBIL()
  + Shorter, more intuitive function names for improved usability
  + All documentation and examples updated accordingly

# exametrika 1.6.4

+ Critical bugfix for Biclustering() field initialization
  + Fixed subscript out of bounds error when nfld values cause ceiling() to exceed nfld
  + Added pmin() constraint to ensure fld0 values never exceed nfld parameter
  + Resolves crashes with specific nfld/testlength combinations (e.g., nfld=15, testlength=21)
+ Enhanced GridSearch() fit index optimization logic
  + Added support for all fit indices returned by TestFitIndices()
  + Correctly handles minimization indices: model_log_like, model_Chi_sq, RMSEA, AIC, CAIC, BIC
  + Correctly handles maximization indices: NFI, RFI, IFI, TLI, CFI
  + Added validation to prevent unknown index specification

# exametrika 1.6.3

+ Major performance enhancement for GRM (Graded Response Model)
  + Replaced R implementation with high-performance C++ code using Rcpp
  + Implemented analytical gradient computation for significant speed improvements
  + Achieved 5-6x faster convergence compared to numerical differentiation
  + Maintains identical mathematical accuracy to previous implementation
  + Full compatibility with existing GRM() function interface
+ Added `converge` variable to all EM-based functions to indicate algorithm convergence status
  + Functions affected: Biclustering(), LCA(), LRA(), and related methods
  + Returns TRUE if converged within maxiter iterations, FALSE otherwise
  + Displays convergence warning messages when maxiter is reached
+ Enhanced GridSearch() function with convergence handling
  + Automatically excludes non-converged results from optimization
  + Displays warning messages for parameter combinations that failed to converge
  + Returns list of failed settings in output
  + Terminates with error message if all parameter combinations fail to converge
+ Improved numerical stability in Biclustering()
  + Implemented conditional pmax() application to avoid unnecessary log-likelihood inflation
  + Applied numerical correction only when NaN/Inf values are detected
+ High-performance polychoric correlation computation
  + Implemented C++ acceleration for polychoric correlation calculations
  + Achieved significant speed improvements over R-based implementation
+ Improved Array-type plot visualization with enhanced color palette
  + Replaced dull default colors with vibrant, high-contrast color palette
  + Added colorblind-friendly color scheme for better accessibility
  + Binary data (2 categories) now uses black and white for optimal contrast
  + Multi-category data uses enhanced colorblind-accessible palette


# exametrika 1.6.2 

+ Biclustering: Fixed floating-point arithmetic errors causing NaN results

# exametrika 1.6.1 on Aug 26, 2025

+ Biclustering: Fixed improper handling of missing values

# exametrika 1.6.0 on Aug 12, 2025

+ Biculustring.norminal is available!
+ Biclustering.ordinal is available!
+ New function GridSerch() for grid search optimization of model parameters
+ Bugfix: Fix output typos(class/rank)
+ Added duplicate ID validation to dataFormat()
+ Bugfix: Fix ID column detection in dataFormat()
+ Bugfix: Fix stanine division error when unable to split data

# exameterika 1.5.2 on March 29, 2025

+ Bugfix: Fix output typos(TestStatisticsFunction,GRMs)

# exameterika 1.5.1 on March 8, 2025

+ Field analysis for Biclustering is included in the Biclustering() function
+ Class/Rank Reference Vector plot is now available.
+ Bug Fix for polychoric correlations

# exametrika 1.5.0 on March 5, 2025

+ New function GRM is available!

# exametrika 1.4.4 on March 3, 2025

+ In Exametrika 1.4.1, bug fixes were made.
+ In 1.4.2, it became possible to calculate polychoric correlation and polyserial correlation.
+ In 1.4.3, item analysis for polytomous items became available.
+ In 1.4.4, we renamed "ICC" to "IRF," although they refer to the same concept (Item Characteristic Curves and Item Response Functions are interchangeable terms). 
The function will interpret "ICC" input as "IRF" automatically. Additionally, Test Response Function (TRF) output was also made available.

# exametrika 1.4.0 on Feb 25, 2025

+ New function LRA() now supports rated data
+ Hex icon available

# exametrika 1.3.0 on Feb 11, 2025

* Added implementation of latent rank model for ordinal scale data

+ New function LRA() now supports ordinal response data
  + Added visualization methods for ordinal scale analysis:
    + Score frequency with rank thresholds (ScoreFreq)
    + Score-rank probability heatmap (ScoreRank)
    + Item category boundary reference (ICBR)
    + Item category response profile (ICRP)

+ Bug fixes and improvements
+ Standardized terminology: unified the usage of "class" and "rank" throughout the package
+ Various minor bug fixes

# Exametrika 1.2.0 on Jan 30, 2025.

* Improved numerical stability for model estimation
* Bug fixes for log-likelihood calculation 

# Exametrika 1.1.0 on Oct 30, 2024.

* Added support for polytomous response data

# Exametrika 1.0.2 on Aug 17, 2024.

* Bug fix for Issue #12

# Exametrika 1.0.1 on July 31, 2024.

* Bug fix for Item Total Correlation
* Bug fix for Fit indices
* New function called ItemStatistics

# Exametrika 1.0.0 on June 9, 2024.

* Initial Release

# Exametrika 0.11.5 on Mar 30, 2024.

* Bug fix for plot labels for TRP/LRD

# Exametrika 0.11.4 on Jan 27, 2024.

* Bug fix for LCD/LRD for IRM

# Exametrika 0.11.3 on December 15, 2023.

* Bug fix for item information curve

# Exametrika 0.11.2 on October 27, 2023.

* Plot manual update

# Exametrika 0.11.1 on October 27, 2023.

* All model tested by testthat environment

# Exametrika 0.11.0 on October 26, 2023.

* Added BINET model

# Exametrika 0.10.0 on October 18, 2023.

* Added LDB model

# Exametrika 0.9.0 on October 17, 2023.

* Added Structure Learning for LDLRA using PBIL
* Added LDLRA model

# Exametrika 0.8.1 on October 10, 2023.

* Added Structure Learning for BMN with simple GA and PBIL model

# Exametrika 0.8.0 on October 4, 2023.

* Added BNM model

# Exametrika 0.7.0 on October 4, 2023.

* Added IRM model

# Exametrika 0.6.1

* Added Q3 matrix output to the IRT function
