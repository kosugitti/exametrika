# exametrika Work Log

Detailed development log. User-facing changes go in `NEWS.md`; this file
captures the per-session internal narrative (why a change was made, what
was investigated, what was ruled out). Entries are newest-first.

## 2026-04-21 — v1.12.0 planning: vectorize Biclustering.ordinal EM

### Motivation

Profiling of the Phase3 simulation (A3 polytomous Biclustering paper)
showed that
[`Biclustering.ordinal()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
is 5x slower per GridSearch call than
[`Biclustering.nominal()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
on the same data. On typical simulation conditions (e.g. C10F5S200J50K7,
200 trials), the ordinal EM grid search accounts for ~50% of total trial
wall-clock. Vectorizing the ordinal EM hot path is the single
highest-leverage optimization for the simulation pipeline.

### Hotspot analysis (R/16_Biclustering_ordinal.R)

Per-EM-iteration hotspots identified:

- **H1** line 199:
  `apply(Ufcq_prior, c(1,2), function(x) rev(cumsum(rev(x))))` — reverse
  cumulative sum along the 3rd axis via per-cell apply. Slowest single
  line; `apply` with a user function dispatches `nfld*ncls` R-level
  calls per EM iteration.
- **H2** lines 172, 185, 321, 323: `apply(X, 1, min/max)` — O(nrow)
  R-level function calls. Replaceable with
  `do.call(pmin.int, as.data.frame(X))` (C-level, log-factor fewer
  calls).
- **H3** lines 166, 182: `log((BBRM[, , q] - BBRM[, , q + 1]) + const)`
  is computed independently for E-step class and E-step field each q per
  iter. Identical values can be precomputed once per iter.
- **H4** Z\*Uq\[,,q\] recomputed at lines 170, 182, 194, 225 every iter.
  Precomputable via column-major recycling: `ZU <- Uq * as.vector(Z)`.
- **3D apply sum**: `apply(X, c(1,2), sum)` / `apply(X, c(2,3), sum)`
  can be replaced with `rowSums(X, dims=2)` / `colSums(X, dims=1)` (base
  R, no new imports).

### Plan

Phase 1 of v1.12.0: apply H1-H4 + 3D-apply replacements. Phase 2 of
v1.12.0 (deferred): collapse E-step/log-lik q-loops into fused matmul
(may alter floating-point summation order, requires looser tolerance).
Evaluate after Phase 1 measurement.

### Decisions

- **No new Imports.** `matrixStats` was initially considered for
  rowMins/ rowMaxs but `do.call(pmin.int, as.data.frame(X))` achieves
  comparable performance in pure base R; `rowSums/colSums` have an
  undocumented-ish `dims` argument that replaces
  `apply(X, c(k1,k2), sum)` on 3D arrays.
- **H5/H6 deferred.** Fusing the q-loop matmuls changes summation order
  and can introduce O(1e-14) floating-point drift. Per project policy
  (“same results are paramount”), this is left for a follow-up release
  or kept out of v1.12.0 entirely.
- **CRAN timing.** v1.11.0 was accepted 2026-04-15; CRAN dislikes short
  intervals. Simulation servers install v1.12.0 via
  `remotes::install_github("kosugitti/exametrika")`. Formal CRAN
  submission is deferred until after paper submission.

### Validation strategy

- Numerical identity: for a fixed seed on J35S500 (ordinal) and a
  synthetic polytomous dataset, record the full log-lik trajectory and
  final BCRM / BBRM / ClassMembership / FieldMembership under the old
  implementation (tag `baseline_v1.11.0_HEAD`). New implementation must
  match to `max(abs(old - new)) < 1e-12` for every EM step.
- All 3,532 tests in `tests/testthat/` must pass (FAIL 0, WARN 0).
- `R CMD check --as-cran` must remain 0/0/0.

### Implementation and results (same day)

All five hot spots implemented incrementally, validated against the
pre-refactor baseline after each step. Baseline captured on commit
bbafdbe (pkg 1.11.0) via
`develop/vectorization_ordinal_v1_12_0/capture_baseline.R` over 8
configurations spanning ordinal B/R methods, ncls in {5, 8, 10}, nfld in
{4, 5}, J35S500 and J15S3810 datasets, plus mic=TRUE and a
non-converging case, and two nominal configurations on J20S600.

`compare_to_baseline.R` reports IDENTICAL (max abs diff == 0) for all
eight configurations across every monitored field (BCRM, BBRM,
ClassMembership, FieldMembership, TestFitIndices, FRP, TRP, LFD, LRD,
FieldEstimated, ClassEstimated, log_lik, n_cycle, converge) after every
incremental step.

Per-step wall-clock ratios measured on those same 8 configurations
(single-run, not averaged; noise dominates for \<0.1s runs):

- After H1 alone: 0.78x to 1.02x (within noise)
- After H1+H2: 0.81x to 1.09x
- After H1+H2+H3: 0.91x to 1.21x (O6 J15S3810 ncls=8 nfld=4)
- After H1+H2+H3+H4: 0.91x to 1.28x
- After all 5: 1.04x (N1) to 1.33x (O6)

Larger single-cell benchmark on J35S500 with variable grid cells,
against baseline bbafdbe:

``` R
ncls=5  nfld=5   : 0.073s -> 0.059s  (1.24x)
ncls=10 nfld=10  : 0.161s -> 0.127s  (1.27x)
ncls=15 nfld=10  : 0.065s -> 0.051s  (1.27x)
ncls=20 nfld=15  : 0.071s -> 0.061s  (1.16x)
```

log_lik identical to every digit in all four cases.

testthat: FAIL 0, WARN 0, SKIP 0, PASS 4750 (test count higher than the
3,532 figure in the outdated CLAUDE.md because of rated Biclustering and
DistractorAnalysis additions in 1.11.0; all pass).

### Phase 2 stance for v1.12.0

The E-step and log-lik q-loops (H5 in the original plan; fuse Q matmuls
into one big matmul over reshaped arrays) were *not* applied in this
release because the summation order would change and break the
`max(abs(old - new)) == 0` gate. Deferred to a future release or a
separate experiment branch.

The project-level ask for v1.12.0 mentions the simulation pipeline also
benefits from further work in `Biclustering.ordinal`; that work (if any)
should preserve the same identity gate and be tracked in a new WORKLOG
entry.
