# Changelog

## exametrika 1.10.0

### Bug Fixes

#### CAIC (Consistent AIC) Formula Correction

- **Fixed CAIC formula to match Bozdogan (1987) original definition**:
  The CAIC penalty term was `log(n + 1)` but should be `log(n) + 1` per
  Bozdogan (1987, Psychometrika, 52(3), p.358, Proposition 2, Eq.44).
  The original Mathematica implementation had this error
  (`Log[nobs + 1]`), and the R port inherited it. Both the R version
  (`R/00_ModelFitModule.R`) and the Mathematica version
  (`develop/mtmk15forVer13/mod/Module_ModelFit.nb`) have been corrected.
  The numerical difference is approximately 1 (constant), but the
  corrected formula now matches the published definition:
  `CAIC(k) = -2 log L + k * (log(n) + 1)`. This affects all models that
  compute fit indices: IRT, LCA, LRA (binary/ordinal/rated),
  Biclustering (binary/ordinal/nominal), IRM, BNM, LDLRA, LDB, BINET,
  and GRM. All 873 tests pass with the corrected formula.

#### GRM Example Fix

- **Changed GRM examples from `\donttest` to `\dontrun`**: GRM’s
  multi-panel plot examples caused “invalid graphics state” errors in
  non-interactive environments (pkgdown, CI). Changed to `\dontrun` to
  prevent build failures.

#### dataFormat Robustness Improvements

- **Fixed auto-detection ignoring `CA` parameter**: When `CA` (correct
  answer vector) was provided but `response.type` was not explicitly
  specified,
  [`dataFormat()`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  incorrectly classified the data as `"ordinal"` instead of `"rated"`.
  This caused `$U` (binary scoring matrix) to be `NULL`. The
  auto-detection now checks for `CA` before ordinal/nominal detection:
  binary → rated (if CA provided) → ordinal → nominal.
- **Fixed `id` parameter not working for column 1**: Changed `id`
  default from `1` to `NULL`. Previously, `id = 1` (both default and
  explicit) triggered auto-detection heuristics, making it impossible to
  explicitly specify the first column as the ID column. Now `id = NULL`
  triggers auto-detection, and any numeric value (including `1`) forces
  that column to be used as ID.
- **Improved ID auto-detection for consecutive integers**: Simplified
  the first-column heuristic to treat unique consecutive integers (e.g.,
  `1:N`) as ID regardless of whether they also look like valid response
  values. Previously, `1:10` was misclassified as response data because
  `looks_like_response_data()` returned TRUE.
- **Fixed missing values in rated `U` matrix**: When the response matrix
  contained missing values (`-1`), the binary scoring matrix `U`
  incorrectly scored them as `0` (incorrect) instead of `-1` (missing).
  Now `U[i,j] = -1` when `Q[i,j] = -1`.
- **Fixed `drop=FALSE` missing in item exclusion**: When items were
  excluded due to invalid variance (e.g., containing `Inf`), the column
  subsetting `response.matrix[, mask]` dropped matrix dimensions if only
  one item remained, causing a crash. Added `drop = FALSE`.
- **Added diagnostic messages**:
  [`dataFormat()`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  now reports via [`message()`](https://rdrr.io/r/base/message.html)
  when it detects problematic data:
  - Items with all missing values
  - Items with zero variance (constant response values)
  - Students with all missing responses

#### BINET `g_list` / `adj_list` Input Path Fix

- **Fixed `g_csv` variable undefined error when using `g_list` or
  `adj_list` input**:
  [`BINET()`](https://kosugitti.github.io/exametrika/reference/BINET.md)
  crashed with an undefined variable error at the graph construction
  step when the DAG was specified via `g_list` or `adj_list` parameters.
  The internal variable `g_csv` (used to build the integrated graph
  object `all_g`) was only defined in the `adj_file` code path. Added
  logic to reconstruct `g_csv` from `adj_list` for the `g_list` and
  `adj_list` input paths, ensuring `all_g` is correctly built with Field
  edge attributes regardless of input method.
- **Fixed `g_list` / `adj_list` length validation**: The length check
  for `g_list` and `adj_list` incorrectly compared against `ncls`
  (number of classes) instead of `nfld` (number of fields). In BINET,
  each element of `adj_list` represents the DAG structure at a specific
  field, so the list length should equal `nfld`. Also fixed `g_list`
  type check from `g_list[[1]]` (always checking the first element) to
  `g_list[[j]]` (checking each element).

#### Biclustering_IRM.binary S3 Method Consistency Fix

- **Added `...` to
  [`Biclustering_IRM.binary()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
  signature**: The S3 generic `Biclustering_IRM(U, ...)` requires all
  methods to include `...` in their formal arguments. The `.binary`
  method was missing it, causing an R CMD check WARNING. Added `...` to
  match the generic and the `.nominal`/`.ordinal` methods.

#### Biclustering_IRM `msg` Field Fix

- **Fixed IRM `msg` field to correctly distinguish Rank vs Class
  models**: Binary IRM and Ordinal IRM are Ranklustering models (ordered
  latent classes), so `msg = "Rank"` is correct. Nominal IRM has no
  Ranklustering concept (unordered latent classes), so `msg = "Class"`
  is correct. Previously all three methods had inconsistent values; now
  `Biclustering_IRM.binary` and `Biclustering_IRM.ordinal` return
  `msg = "Rank"`, while `Biclustering_IRM.nominal` returns
  `msg = "Class"`. Also updated internal column names of `cls01` and
  `FRP` from `"Class"` to `"Rank"` for binary/ordinal IRM. The shared
  Gibbs core (`irm_gibbs_core()`) output column names were also updated.

#### Biclustering_IRM.binary Missing `LRD` Alias

- **Added `LRD` (Latent Rank Distribution) alias to
  [`Biclustering_IRM.binary()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
  return value**: The binary IRM was the only Biclustering model that
  did not include `LRD` in its return value (it only had `LCD`). Other
  Biclustering models (binary Biclustering, nominal IRM, ordinal IRM)
  all return both `LRD` and `LCD`. Added `LRD = clsdist` as an alias for
  `LCD` to ensure consistency across all Biclustering-family models.
  This improves interoperability with downstream packages (ggExametrika)
  that look up `LRD` when `msg == "Rank"`.

#### Biclustering_IRM Seed Default

- **Reverted
  [`Biclustering_IRM()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
  seed default back to 123**: Ensures reproducibility by default.

#### Biclustering_IRM Log-Probability Normalization Fix

- **Fixed log-to-probability conversion in
  [`Biclustering_IRM()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
  Gibbs sampler**: Changed `exp(ptab - min(ptab))` to
  `exp(ptab - max(ptab))` for numerical stability. The previous
  implementation subtracted the minimum log-probability, which could
  cause overflow (`exp(large positive) = Inf`) when the range of
  log-probabilities was large, resulting in `NaN` after normalization.
  Subtracting the maximum ensures the largest value becomes `exp(0) = 1`
  and all others underflow harmlessly to near-zero values.

#### Biclustering.nominal Model Fit Fix

- **Fixed
  [`Biclustering.nominal()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
  using stale log-likelihood in model fit indices**: When the EM
  algorithm exited due to log-likelihood decrease, `BCRM` was correctly
  reverted to the previous iteration’s value, but `test_log_lik` was
  not. The model fit section recalculated the correct log-likelihood as
  `testell`, but `chi_A`, `model_log_like`, `log_lik`, and `LogLik` all
  referenced the stale `test_log_lik` instead of `testell`. Also removed
  a duplicated model fit code block (copy-paste error).

#### J20S400 Dataset Fix

- **Fixed `J20S400` response type from `nominal` to `binary`**: The
  `J20S400.rda` dataset was incorrectly stored with
  `response.type = "nominal"` despite being a binary (0/1) dataset with
  -99 as missing values. Missing values were not properly handled (Z was
  all 1s, Q contained raw -99 values). Regenerated from the original CSV
  source (`develop/sampleData/J20S400.csv`) with
  `dataFormat(..., na = -99)`, now correctly typed as `binary` with 86
  missing values properly masked in Z.

### New Features

#### Polytomous IRM (Biclustering_IRM.nominal / Biclustering_IRM.ordinal)

- **New
  [`Biclustering_IRM.nominal()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
  for nominal/polytomous data**: Extends the Infinite Relational Model
  (IRM) from binary to nominal scale data using a Dirichlet-Multinomial
  collapsed Gibbs sampler. The Chinese Restaurant Process (CRP)
  automatically determines the optimal number of classes and fields.
  After the Gibbs sampling phase, small classes are consolidated and
  refined with an EM algorithm. The Dirichlet prior concentration
  parameter `alpha` controls smoothing of category probabilities.
- **New
  [`Biclustering_IRM.ordinal()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
  for ordinal/polytomous data**: Extends IRM to ordinal scale data.
  Shares the same Dirichlet-Multinomial collapsed Gibbs sampler as
  nominal IRM (Phase 1), then applies ordinal-specific EM refinement
  (Phase 2) with cumulative normalization to enforce monotonic category
  boundaries. The `mic` parameter (default `TRUE`) enforces monotone
  increasing class ordering by expected score sum. Reports BFRP
  (Bicluster Field Reference Profile), FRPIndex, TRP, and
  Strongly/Weakly Ordinal Alignment Conditions (SOACflg/WOACflg).
- **[`Biclustering_IRM()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
  is now an S3 generic**: Dispatches to `Biclustering_IRM.binary`
  (existing binary IRM), `Biclustering_IRM.nominal` (new), and
  `Biclustering_IRM.ordinal` (new). Raw data is automatically formatted
  and dispatched based on `response.type`.
- **Shared Gibbs sampler core**: The collapsed Gibbs sampler has been
  extracted into a shared internal function `irm_gibbs_core()` (in
  `R/00_IRM_Gibbs_CORE.R`), used by both nominal and ordinal IRM. Helper
  functions (`irm_calc_Ufcq`, `irm_lmvbeta`, `irm_log_to_prob`,
  `irm_bic_calc`, etc.) are also shared.
- **Performance optimization**: The Gibbs sampler uses differential
  updates for the sufficient statistics array `U_fcq`, computing only
  the contribution of the target student/item rather than recalculating
  the full array at each step.

#### Confirmatory LCA/LRA (Test Equating)

- **[`LCA()`](https://kosugitti.github.io/exametrika/reference/LCA.md)
  and [`LRA()`](https://kosugitti.github.io/exametrika/reference/LRA.md)
  now support a `conf` parameter for confirmatory analysis**: The `conf`
  argument accepts an IRP matrix (ncls/nrank x testlength) where non-NA
  values are held fixed throughout EM estimation and NA values are
  freely estimated. This enables test equating scenarios where anchor
  items retain their known IRPs while new items are calibrated against
  them.
- **Label-based item matching**: When `conf` has column names, items are
  matched by label rather than by position. This allows `conf` to
  contain a subset of items (anchor items only) or items in a different
  order than the data. Items in the data but not in `conf` are
  automatically set to freely estimated. Unknown labels in `conf`
  produce an informative error.
- **Works with both GTM and SOM methods** for LRA.

#### Internal Refactoring: SOM Estimation

- **Extracted SOM estimation into `somclus()` internal function**: The
  Self-Organizing Maps estimation code previously inlined in
  [`LRA.binary()`](https://kosugitti.github.io/exametrika/reference/LRA.md)
  (~100 lines) has been extracted into a standalone internal function
  `somclus()` in `00_EMclus.R`. This parallels `emclus()` (GTM/EM) and
  returns the same structure. Also fixed a pre-existing typo (`h_cout` →
  `h_count`) and another (`oldsBIC` → `oldBIC`).

### Test Suite Modernization

- **Complete migration from Excel to CSV fixtures**: Removed all 14
  legacy test files that depended on `tidyverse` and `readxl` for
  reading Excel-based Mathematica reference data. Replaced with 24
  modern test files using base R
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html) and
  `test_path()` to load CSV fixtures from
  `tests/testthat/fixtures/mathematica_reference/`. The new test suite
  has zero external package dependencies beyond `testthat`.
- **Removed `readxl`/`tidyverse` from test dependencies**: DESCRIPTION
  `Suggests` no longer requires any packages beyond `knitr`,
  `rmarkdown`, and `testthat`.
- **Fixture file reorganization**: Shortened overly long CSV fixture
  filenames to comply with CRAN’s 100-byte portable path requirement.
- **Test coverage**: 26 test files, 321 test blocks, covering all models
  (CTT, IRT 2PL/3PL/4PL, LCA, LRA binary/ordinal/nominal, Biclustering
  binary/ordinal/nominal, IRM binary/nominal/ordinal, BNM, LDLRA, LDB,
  BINET, GRM, GridSearch, dataFormat, polychoric correlation, scoring,
  student/test analysis, confirmatory LCA/LRA). 85 Mathematica reference
  CSV files for cross-validation.
- **Added Nominal IRM tests** (`test-irm-nominal.R`): 18 test blocks for
  [`Biclustering_IRM.nominal()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
  using J20S600 data — basic execution, dimensions, FRP validity,
  membership, fit indices, backward compatibility, seed reproducibility,
  alpha validation.
- **Added Ordinal IRM tests** (`test-irm-ordinal.R`): 25 test blocks for
  [`Biclustering_IRM.ordinal()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
  using J35S500 data — basic execution, dimensions, FRP validity,
  expected scores, TRP, BFRP, FRPIndex, SOAC/WOAC flags, mic parameter,
  fit indices, backward compatibility, seed reproducibility, alpha
  validation.

### Documentation

- **TestStatistics example**: Added stem-and-leaf plot example using
  `stem(nrs(dataFormat(J15S500)))` to demonstrate score distribution
  visualization.

### Internal Improvements

- **pkgdown migration**: Migrated documentation site from Jekyll
  (main/docs) to pkgdown (gh-pages branch via GitHub Actions).
- **CI/CD**: Added GitHub Actions workflows for automated R CMD check,
  test coverage reporting, and pkgdown site deployment. Removed Codecov
  upload step from test-coverage workflow.
- **.Rbuildignore cleanup**: Removed `^tests$` and `^inst$` entries that
  were incorrectly excluding tests and vignettes from the built package.

------------------------------------------------------------------------

## exametrika 1.9.0

### Bug Fixes

#### GridSearch `index` Parameter Fixes

- **Index alias support**:
  [`GridSearch()`](https://kosugitti.github.io/exametrika/reference/GridSearch.md)
  now accepts common aliases for fit indices. `"loglik"`, `"log_lik"`,
  `"LogLik"`, and `"LL"` are mapped to `"model_log_like"`. `"Chi_sq"`
  and `"chi_sq"` are mapped to `"model_Chi_sq"`. Previously, using these
  aliases caused silent `NULL` extraction and eventual errors.
- **Early validation**: Invalid index names are now caught immediately
  at the start of
  [`GridSearch()`](https://kosugitti.github.io/exametrika/reference/GridSearch.md),
  before the computationally expensive grid search loop runs. The error
  message lists all valid options including available aliases.
- **Log-likelihood optimization direction**: `model_log_like` is now
  correctly treated as a maximization target (larger log-likelihood =
  better fit). Previously, it was incorrectly placed in the minimization
  group, which would have selected the worst-fitting model.

#### Test Tolerance Fix

- **Q3 matrix test**: Changed the 2PL Q3 matrix test from relative
  tolerance (`tolerance = 1e-2` in `expect_equal`) to absolute
  difference comparison (threshold 0.005). Q3 residual correlations
  include values near zero where relative tolerance comparisons are
  unreliable.

#### NAMESPACE Fix

- **Missing imports**: Added `layout` and `plot.new` from `graphics` to
  NAMESPACE. These functions are used by the legend strip layout helpers
  for polytomous Biclustering plots.

#### LRA.ordinal / LRA.rated Category Computation Fix

- **Fixed `apply(U$Q, 2, unique)` returning matrix instead of list**:
  When all items have the same number of response categories (e.g., all
  5-point Likert), [`apply()`](https://rdrr.io/r/base/apply.html)
  returns a matrix rather than a list. The subsequent
  [`lapply()`](https://rdrr.io/r/base/lapply.html) then iterates over
  individual elements instead of per-column vectors, causing `ncat` to
  be a vector of 1s and crashing the algorithm. Replaced with
  `lapply(seq_len(nitems), function(j) sort(unique(U$Q[, j])))` which
  always returns a proper list. Same fix applied to `catfreq999`
  computation. Both `LRA.ordinal` and `LRA.rated` were affected.

#### GRM ItemFitIndices Computation Fix

- **Fixed `apply(tmp$Q, 2, table)` returning matrix instead of list**:
  Same class of bug as the LRA.ordinal/LRA.rated fix above. In
  [`GRM()`](https://kosugitti.github.io/exametrika/reference/GRM.md),
  the null model log-likelihood computation used
  `apply(tmp$Q, 2, table)` to compute per-item category frequencies.
  When all items have the same number of response categories (e.g., all
  5-point Likert), [`apply()`](https://rdrr.io/r/base/apply.html)
  returns a matrix instead of a list, causing `response_list[[j]]` to
  extract a single number rather than the full frequency table. This
  produced incorrect `null_log_like` values and consequently wrong
  chi-square statistics, RMSEA, TLI, and CFI in `ItemFitIndices`.
  Replaced with `lapply(seq_len(nitems), function(j) table(tmp$Q[, j]))`
  which always returns a proper list.

#### LRA.ordinal Mixed Category Count Validation

- **Added input validation for mixed category counts**:
  [`LRA.ordinal()`](https://kosugitti.github.io/exametrika/reference/LRA.md)
  now raises an informative error when items have different numbers of
  response categories (e.g., some items with 3 categories and others
  with 5). The internal matrix algebra uses fixed-stride indexing that
  assumes uniform category counts. The error message suggests
  alternatives (`LRA.rated`, `Biclustering.ordinal`) that support mixed
  category counts via list-based designs.

#### LCA/LRA FRP Plot Type Removal

- **Removed “FRP” from valid plot types for LCA and LRA**: Field
  Reference Profile (FRP) requires a field structure (item grouping),
  which LCA and LRA do not have. Previously,
  `plot(lca_result, type = "FRP")` passed validation but failed at
  runtime because the `$FRP` field does not exist in LCA/LRA return
  values. Now properly rejected with an informative error message at the
  validation stage.

### New Features

#### New Sample Datasets for Polytomous Biclustering

- **J35S500**: Simulated ordinal dataset (500 students, 35 items, 5
  categories) with a cumulative staircase pattern (5 latent classes, 5
  fields). Contains approximately 0.5% missing values.
- **J20S600**: Simulated nominal dataset (600 students, 20 items, 4
  categories) with a cyclic category preference pattern (5 latent
  classes, 4 fields). Contains approximately 0.5% missing values.

#### New Plot Types for Polytomous Biclustering

- **FRP** (Field Reference Profile): Expected score line plot per field,
  with `stat` parameter supporting `"mean"` (default), `"median"`, and
  `"mode"`.
- **FCRP** (Field Category Response Profile): Category probability plot
  per field, with `style` parameter supporting `"line"` (default) and
  `"bar"` (stacked bar chart).
- **FCBR** (Field Cumulative Boundary Reference): Boundary probability
  plot per field (ordinal Biclustering only).
- **ScoreField**: Heatmap of expected scores across fields and latent
  classes/ranks.
- **RRV** (Rank Reference Vector): Transposed view with fields on x-axis
  and expected scores on y-axis, with `stat` parameter.

#### FRPIndex for Ordinal Biclustering

- Ordinal Biclustering now computes `FRPIndex` (Field Reference Profile
  indices) including location parameters (Alpha, Beta), slope parameters
  (A, B), and monotonicity indices (Gamma, C).

#### New Parameters for `plot.exametrika()`

- `stat`: Controls the summary statistic for FRP and RRV plots
  (`"mean"`, `"median"`, `"mode"`).
- `style`: Controls the display style for FCRP plots (`"line"`,
  `"bar"`).

#### Array Plot Improvements

- **Missing value display**: Array plots now display missing values in a
  distinct color. Binary data uses gray (`#808080`) to distinguish from
  white (incorrect) and black (correct). Polytomous data uses black
  (`#000000`) to distinguish from the category color palette.

#### Print Method Improvements

- **Ordinal Biclustering**:
  [`print()`](https://rdrr.io/r/base/print.html) now displays FRPIndex
  (Field Reference Profile Indices) with a note that the values are
  based on normalized expected scores `(E[score]-1)/(maxQ-1)`.

#### Documentation Improvements

- **FRPIndex**: Expanded documentation for the 6 profile shape indices
  (Alpha, A, Beta, B, Gamma, C) in
  [`?Biclustering`](https://kosugitti.github.io/exametrika/reference/Biclustering.md),
  including detailed definitions and polytomous adaptation logic.

#### Return Value Structure Unification

Systematic unification of return value structures across all analysis
functions for consistency and interoperability.

##### snake_case Field Names Extended to All Functions

- **LDLRA**: Added `n_class` (retaining `Nclass` for backward
  compatibility)
- **LDB**: Added `n_rank`, `n_field` (retaining `Nrank`, `Nfield`)
- **BINET**: Added `n_class`, `n_field` (retaining `Nclass`, `Nfield`)
- **Biclustering.nominal**: Added `n_class`, `n_field`, `n_cycle`
  (retaining `Nclass`, `Nfield`, `N_Cycle`)
- **Biclustering.ordinal**: Added `n_class`, `n_field`, `n_cycle`
  (retaining `Nclass`, `Nfield`, `N_Cycle`)
- **Biclustering_IRM**: Added `n_cycle`, `N_Cycle` (retaining
  `em_cycle`, `EM_Cycle` as IRM-specific aliases)

##### Top-Level `log_lik` Added to All Functions

- All analysis functions now consistently provide `log_lik` at the top
  level of the return object, matching `TestFitIndices$model_log_like`.
- Functions updated: IRT, LCA, LRA.binary, BNM, LDLRA, LDB, BINET,
  Biclustering.binary, Biclustering_IRM

##### TestFitIndices Structure Unified

- **GRM, Biclustering.nominal, Biclustering.ordinal**: TestFitIndices
  now uses the full 16-field structure with `ModelFit` class, matching
  the format used by IRT/LCA/LRA and other functions. Previously these
  used bare
  [`calcFitIndices()`](https://kosugitti.github.io/exametrika/reference/calcFitIndices.md)
  output (9 fields, no class). Added fields: `model_log_like`,
  `bench_log_like`, `null_log_like`, `model_Chi_sq`, `null_Chi_sq`,
  `model_df`, `null_df`.
- **GRM ItemFitIndices**: Also unified to the full 16-field structure
  with `ModelFit` class.
- **BINET**: `TestFitIndices` added as primary name for multigroup fit
  indices (previously only `MG_FitIndices`). `MG_FitIndices` retained as
  backward-compatible alias. `SM_FitIndices` (saturated model) remains
  unchanged.

##### Students Matrix Enhanced

- **Biclustering.nominal**: Added `Estimate` column (most probable class
  assignment) to the Students matrix.
- **Biclustering.ordinal**: Added `Estimate` column (most probable class
  assignment) to the Students matrix.

##### Other Structural Improvements

- **Biclustering_IRM**: Added `FRPIndex` (Field Reference Profile
  indices) for consistency with Biclustering.binary and LDB.
- **LDB**: Fixed `TRP` from `matrix(1×ncls)` to `numeric vector`,
  consistent with all other functions.
- **GridSearch**: Added `class = c("exametrika", "GridSearch")` to
  return value for method dispatch support.

#### Bug Fixes

- **LCA `msg` field assignment**: Fixed `msg <- "Class"` to
  `msg = "Class"` inside
  [`structure()`](https://rdrr.io/r/base/structure.html) call (line 150
  of `05_LCA.R`). The `<-` operator was being interpreted as a
  standalone assignment rather than a named list element, causing the
  `msg` field name to be empty.
- **RMP/CMP single student plot error**: Fixed dimension drop error when
  plotting RMP or CMP for a single student (e.g.,
  `plot(r, type="RMP", students=1)`). Added `drop = FALSE` to prevent
  matrix-to-vector coercion when extracting a single row from the
  Students matrix.
- **LRA.ordinal / LRA.rated `TestFitIndices` log-likelihood fields**:
  Fixed `null_log_like` which incorrectly stored the saturated model
  log-likelihood (`log_lik_satu`) instead of the null model
  log-likelihood (`sum(null_itemll)`). Added missing `bench_log_like`
  field containing the saturated model log-likelihood
  (`sum(satu_itemll2)`). Note: the chi-square-based fit indices (NFI,
  CFI, TLI, RMSEA, AIC, BIC, etc.) were always computed correctly; only
  the stored log-likelihood labels were affected.
- **LRA.ordinal / LRA.rated FitIndices structure**: Unified
  `TestFitIndices` and `ItemFitIndices` to the standard 16-field
  structure with `ModelFit` class (`c("exametrika", "ModelFit")`),
  matching all other analysis functions. Added `model_log_like`,
  `bench_log_like`, `null_log_like` to `ItemFitIndices`. Removed
  `ScoreRankCorr` / `RankQuantCorr` from `TestFitIndices` (already
  available at the top level of the return object).
- **LRA.ordinal / LRA.rated test updates**: Updated `test-12OLR.R` and
  `test-13NLR.R` to handle the new `ModelFit` class (add
  [`unclass()`](https://rdrr.io/r/base/class.html) before
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)) and
  adjusted column/index references to match the unified 16-field
  structure.

#### Plot Layout Improvements

- **Legend strip layout**: Moved per-panel legends to a shared legend
  strip below the plot area for FCRP (line/bar), FCBR, GRM IRF, and IRT
  overlay (IRF/IIF) plots. Uses
  [`layout()`](https://rdrr.io/r/graphics/layout.html) with a thin
  dedicated row (height ratio 0.2) to reduce visual clutter in data
  panels. Added `setup_legend_layout()` and `draw_legend_strip()`
  internal helper functions.
- **FCBR reference line**: Added `P(Q>=1)=1.0` reference line at the top
  of FCBR plots for visual completeness.

------------------------------------------------------------------------

## exametrika 1.8.1

CRAN release: 2026-02-17

### Bug Fixes

#### dataFormat Function

- **Fixed factor ID column detection**:
  [`dataFormat()`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  now correctly identifies factor-type ID columns before converting
  factors to numeric. Previously, the factor-to-numeric conversion
  occurred before ID detection, causing factor ID columns with many
  levels (\>=20) to trigger a “Too many categories” error instead of
  being recognized as IDs.
- **Removed unused helper function**: Removed dead code
  (`is_response_data()`) that was defined but never called.

#### GridSearch Function

- **Fixed ordinal data support**: GridSearch now correctly handles
  ordinal data by using `obj$Q` instead of `obj$U` for test length
  calculation
- **Resolved nfld=1 parameter issue**: Eliminated invalid parameter
  ranges that caused crashes with ordinal datasets

#### Biclustering.ordinal Function

- **Enhanced numerical stability**: Added `pmax(Ufcq_prior, 1e-10)` to
  prevent division by zero and NaN errors
- **Fixed convergence failures**: Resolved “missing value where
  TRUE/FALSE needed” errors in specific parameter combinations (e.g.,
  ncls=4 with nfld=5)
- **Improved robustness**: Algorithm now handles edge cases where field
  membership probabilities approach zero

#### Impact

- GridSearch now works reliably with ordinal data across all parameter
  combinations
- Biclustering analysis completes successfully for previously
  problematic parameter settings
- Enhanced overall stability for large-scale grid searches

------------------------------------------------------------------------

## exametrika 1.8.0

CRAN release: 2025-12-10

### Naming Convention Improvements

This release improves naming consistency across the package while
maintaining full backward compatibility through a deprecation path.

#### New Field Names (Recommended)

All analysis functions now return results with snake_case field names
for better consistency:

- `n_class` - Number of latent classes (replaces `Nclass`)
- `n_field` - Number of latent fields (replaces `Nfield`)
- `n_rank` - Number of latent ranks (replaces `Nrank`)
- `n_cycle` - Number of EM iterations (replaces `N_Cycle`)
- `log_lik` - Log-likelihood value (replaces `LogLik`)

#### Deprecated Field Names (Still Supported)

The old field names continue to work for backward compatibility:

- `Nclass`, `Nfield`, `Nrank`, `N_Cycle`, `LogLik` - **Deprecated but
  functional**
- These fields will be removed in version 2.0.0
- Please update your code to use the new snake_case names

#### Example Migration

``` r
# Old code (deprecated but still works)
result <- LCA(data, ncls = 3)
n_classes <- result$Nclass

# New code (recommended)
result <- LCA(data, ncls = 3)
n_classes <- result$n_class
```

#### Output Formatting Improvements

Progress messages now display properly in R Markdown documents:

- **GridSearch()** - Added `verbose` parameter (default: `TRUE`)
  - Output organized by row: `ncls = 2: nfld=2 nfld=3 nfld=4 ...`
  - Set `verbose = FALSE` to suppress all progress messages
- **Biclustering_IRM()** - Improved iteration display
  - Format: `iter 1: match=0 nfld=15 ncls=30`
  - Class adjustment messages:
    `Adjusting classes: BIC=-99592.5 ncls=21 (min size < 20)`
- **LRA.ordinal()** - Fixed verbose behavior
  - Changed default from `verbose = TRUE` to `verbose = FALSE`
    (consistent with binary/rated versions)
  - Saturation Model: `Saturation Model - iter 1: log_lik=-1.234567`
  - Restricted Model: `Restricted Model - iter 1: log_lik=-0.987654`
- **LRA.rated()** - Fixed verbose behavior
  - Changed default from `verbose = TRUE` to `verbose = FALSE`
    (consistent with binary/ordinal versions)
  - Same improved format as LRA.ordinal

All progress messages now use proper line breaks instead of carriage
returns, ensuring clean output in R Markdown/knitr documents and web
documentation.

#### Bug Fixes

- Fixed Array plot color mapping for binary data
  - Used `sort(unique(...))` to ensure consistent ordering: 0
    (white/incorrect) → 1 (black/correct)
  - Previously, color mapping could be reversed depending on data order
- Fixed Ranklustering Array plot sorting order
  - Students with higher correct response rates now appear at the bottom
    of the plot
  - Previously, high performers were incorrectly placed at the top

#### Internal Improvements

- Standardized internal variable naming (e.g., `testEll` →
  `test_log_lik`)
- Improved code readability and maintainability
- All output messages now use consistent `log_lik` terminology

## exametrika 1.7.0

CRAN release: 2025-11-22

- Renamed IRM() to Biclustering_IRM() for consistency with structure
  learning naming conventions
  - Follows the same pattern as BNM_GA(), LDLRA_PBIL(), etc.
    (model_method naming)
  - IRM() function still works but is now deprecated with a warning
    using .Deprecated()
  - The new name clarifies that this function performs Biclustering
    structure learning using the Infinite Relational Model
  - All documentation and examples updated to use Biclustering_IRM()
- Added .Deprecated() warnings to renamed functions from version 1.6.5
  - StrLearningGA_BNM() now shows deprecation warning, recommending
    BNM_GA()
  - StrLearningPBIL_BNM() now shows deprecation warning, recommending
    BNM_PBIL()
  - StrLearningPBIL_LDLRA() now shows deprecation warning, recommending
    LDLRA_PBIL()
  - Old function names still work for backward compatibility but display
    warnings

## exametrika 1.6.5

CRAN release: 2025-11-06

- Critical bugfix for LCA() response type validation
  - Fixed incorrect variable reference in response type checking
- Added beta1 and beta2 parameters to all Beta distribution-based
  functions
  - LRA.binary(): beta1=1, beta2=1 (GTM method)
  - LCA(): beta1=1, beta2=1
  - Biclustering.binary(): beta1=1, beta2=1
  - BNM(): beta1=1, beta2=1
  - LDB(): beta1=1, beta2=1
  - LDLRA(): beta1=2, beta2=2
  - LD_param_est(): beta1=2, beta2=2 (internal helper function)
  - LDLRA_PBIL(): beta1=2, beta2=2
  - These parameters control the prior density parameters in Bayesian
    parameter estimation
  - Users can now customize Beta distribution parameters for EM
    algorithm parameter updating
  - Default values preserve backward compatibility with previous
    versions
- Added alpha parameter to polytomous models for Dirichlet prior control
  - Biclustering.ordinal(): alpha=1 (flat Dirichlet prior)
  - Biclustering.nominal(): alpha=1 (flat Dirichlet prior)
  - These parameters control the concentration parameter for Dirichlet
    priors in category probability estimation
  - Users can customize Dirichlet parameters to adjust prior strength
    (alpha \> 0)
  - Default values (alpha=1) preserve backward compatibility with
    previous versions
- Simplified function names for structure learning functions
  - StrLearningGA_BNM() renamed to BNM_GA()
  - StrLearningPBIL_BNM() renamed to BNM_PBIL()
  - StrLearningPBIL_LDLRA() renamed to LDLRA_PBIL()
  - Shorter, more intuitive function names for improved usability
  - All documentation and examples updated accordingly

## exametrika 1.6.4

- Critical bugfix for Biclustering() field initialization
  - Fixed subscript out of bounds error when nfld values cause ceiling()
    to exceed nfld
  - Added pmin() constraint to ensure fld0 values never exceed nfld
    parameter
  - Resolves crashes with specific nfld/testlength combinations (e.g.,
    nfld=15, testlength=21)
- Enhanced GridSearch() fit index optimization logic
  - Added support for all fit indices returned by TestFitIndices()
  - Correctly handles minimization indices: model_log_like,
    model_Chi_sq, RMSEA, AIC, CAIC, BIC
  - Correctly handles maximization indices: NFI, RFI, IFI, TLI, CFI
  - Added validation to prevent unknown index specification

## exametrika 1.6.3

CRAN release: 2025-09-30

- Major performance enhancement for GRM (Graded Response Model)
  - Replaced R implementation with high-performance C++ code using Rcpp
  - Implemented analytical gradient computation for significant speed
    improvements
  - Achieved 5-6x faster convergence compared to numerical
    differentiation
  - Maintains identical mathematical accuracy to previous implementation
  - Full compatibility with existing GRM() function interface
- Added `converge` variable to all EM-based functions to indicate
  algorithm convergence status
  - Functions affected: Biclustering(), LCA(), LRA(), and related
    methods
  - Returns TRUE if converged within maxiter iterations, FALSE otherwise
  - Displays convergence warning messages when maxiter is reached
- Enhanced GridSearch() function with convergence handling
  - Automatically excludes non-converged results from optimization
  - Displays warning messages for parameter combinations that failed to
    converge
  - Returns list of failed settings in output
  - Terminates with error message if all parameter combinations fail to
    converge
- Improved numerical stability in Biclustering()
  - Implemented conditional pmax() application to avoid unnecessary
    log-likelihood inflation
  - Applied numerical correction only when NaN/Inf values are detected
- High-performance polychoric correlation computation
  - Implemented C++ acceleration for polychoric correlation calculations
  - Achieved significant speed improvements over R-based implementation
- Improved Array-type plot visualization with enhanced color palette
  - Replaced dull default colors with vibrant, high-contrast color
    palette
  - Added colorblind-friendly color scheme for better accessibility
  - Binary data (2 categories) now uses black and white for optimal
    contrast
  - Multi-category data uses enhanced colorblind-accessible palette

## exametrika 1.6.2

- Biclustering: Fixed floating-point arithmetic errors causing NaN
  results

## exametrika 1.6.1 on Aug 26, 2025

CRAN release: 2025-08-27

- Biclustering: Fixed improper handling of missing values

## exametrika 1.6.0 on Aug 12, 2025

CRAN release: 2025-08-19

- Biculustring.norminal is available!
- Biclustering.ordinal is available!
- New function GridSerch() for grid search optimization of model
  parameters
- Bugfix: Fix output typos(class/rank)
- Added duplicate ID validation to dataFormat()
- Bugfix: Fix ID column detection in dataFormat()
- Bugfix: Fix stanine division error when unable to split data

## exametrika 1.5.0 on March 5, 2025

- New function GRM is available!

## exametrika 1.4.4 on March 3, 2025

- In Exametrika 1.4.1, bug fixes were made.
- In 1.4.2, it became possible to calculate polychoric correlation and
  polyserial correlation.
- In 1.4.3, item analysis for polytomous items became available.
- In 1.4.4, we renamed “ICC” to “IRF,” although they refer to the same
  concept (Item Characteristic Curves and Item Response Functions are
  interchangeable terms). The function will interpret “ICC” input as
  “IRF” automatically. Additionally, Test Response Function (TRF) output
  was also made available.

## exametrika 1.4.0 on Feb 25, 2025

- New function LRA() now supports rated data
- Hex icon available

## exametrika 1.3.0 on Feb 11, 2025

- Added implementation of latent rank model for ordinal scale data

- New function LRA() now supports ordinal response data

  - Added visualization methods for ordinal scale analysis:
    - Score frequency with rank thresholds (ScoreFreq)
    - Score-rank probability heatmap (ScoreRank)
    - Item category boundary reference (ICBR)
    - Item category response profile (ICRP)

- Bug fixes and improvements

- Standardized terminology: unified the usage of “class” and “rank”
  throughout the package

- Various minor bug fixes

## Exametrika 1.2.0 on Jan 30, 2025.

CRAN release: 2025-01-31

- Improved numerical stability for model estimation
- Bug fixes for log-likelihood calculation

## Exametrika 1.1.0 on Oct 30, 2024.

CRAN release: 2024-11-22

- Added support for polytomous response data

## Exametrika 1.0.2 on Aug 17, 2024.

- Bug fix for Issue
  [\#12](https://github.com/kosugitti/exametrika/issues/12)

## Exametrika 1.0.1 on July 31, 2024.

- Bug fix for Item Total Correlation
- Bug fix for Fit indices
- New function called ItemStatistics

## Exametrika 1.0.0 on June 9, 2024.

- Initial Release

## Exametrika 0.11.5 on Mar 30, 2024.

- Bug fix for plot labels for TRP/LRD

## Exametrika 0.11.4 on Jan 27, 2024.

- Bug fix for LCD/LRD for IRM

## Exametrika 0.11.3 on December 15, 2023.

- Bug fix for item information curve

## Exametrika 0.11.2 on October 27, 2023.

- Plot manual update

## Exametrika 0.11.1 on October 27, 2023.

- All model tested by testthat environment

## Exametrika 0.11.0 on October 26, 2023.

- Added BINET model

## Exametrika 0.10.0 on October 18, 2023.

- Added LDB model

## Exametrika 0.9.0 on October 17, 2023.

- Added Structure Learning for LDLRA using PBIL
- Added LDLRA model

## Exametrika 0.8.1 on October 10, 2023.

- Added Structure Learning for BMN with simple GA and PBIL model

## Exametrika 0.8.0 on October 4, 2023.

- Added BNM model

## Exametrika 0.7.0 on October 4, 2023.

- Added IRM model

## Exametrika 0.6.1

- Added Q3 matrix output to the IRT function
