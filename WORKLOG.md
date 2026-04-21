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
