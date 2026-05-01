# exametrika Work Log

Detailed development log. User-facing changes go in `NEWS.md`; this file
captures the per-session internal narrative (why a change was made, what
was investigated, what was ruled out). Entries are newest-first.

## 2026-04-29/30 — v1.13.0: Graphical Lasso + Chatterjee’s ξ 実装と DAG 試作

v1.13.0 のメインフィーチャ2系統を実装し，両者を接続する DAG 構築の
試作まで進めた。コミット 7bf5ce9 で R/22_GlassoUnit.R,
R/23_Chatterjee.R, print.exametrika の Glasso ケース，両者のテストを
main に push 済み。

### Graphical Lasso (R/22_GlassoUnit.R)

学習モードでスライド (BoChang 2015) と ESL 17.2 を読みながら段階的に
組み上げた。最終形は3層構造:

- `soft_thresholding(y, lambda)` — ESL 17.26 の soft-threshold 演算子
- `glasso_one(S, lambda, W_init, Beta_init, eps, max_iter)` — 単一 λ
  でブロック座標降下＋内側 lasso 座標降下
- `compute_EBIC_glasso(S, Theta, n, p, gamma)` — Foygel-Drton 2010
- `Glasso(U, ...)` — 公開関数。dataFormat 経由で polychoric 計算 → λ
  グリッド自動構築 → 各 λ で glasso_one を呼び EBIC 比較 → 最適 Θ

設計上踏んだ典型バグ: - Jacobi vs Gauss-Seidel: 内側 CD で beta
を別配列に書き込んで一括 swap する Jacobi にしていたら，bfi
のような実データの共分散で発散 → in-place 更新 (Gauss-Seidel) に修正 -
整数オーバーフロー（修正は Chatterjee 側） - `n * sum(...)` で R の int
max を超え NA 化 → `as.numeric(length(x))` -
ホットスタート時の対角不整合: 前 λ の W を持ち越すと対角が古い λ で
固定され，新 λ で発散する場合あり → diag(W_init) を新 λ で再設定 -
print.exametrika の switch にケース追加が必要（trailing comma で
「argument is missing」エラー）→ Glasso ケース追加で解消

EBIC 設計の現実的な観察: J15S3810 (N=3810, p=15) のような N≫p データ
では γ をどれだけ上げても罰則が尤度に勝てず最小 λ が選ばれる。
lambda_ratio で探索下限を制御する手が実用的。CLAUDE.md にも v1.13.0
要素として「manual lambda override の必要性」を残しておく価値あり。

テスト 9件パス，命名・class 順は exametrika 慣習 c(“exametrika”, X)
に揃えた（specific→general ではない。これは tail(class(x), 1) で
ディスパッチするため）。

### Chatterjee’s xi (R/23_Chatterjee.R)

3関数を新規実装: - `chatterjee_xi(x, y, ties_method)` — 単発計算 -
`xi_stable(x, y, B, seed)` — タイ破りを B 回平均，list(xi, sd, se, B) -
`chatterjee_matrix(U, B, seed, verbose)` — pairwise 非対称 ξ 行列

B のデフォルトを peas データで実験的に校正: B=1000 で SE ≈ 0.001
となり構造学習の枝判定には十分との確認。テスト 9件パス。

### DAG 構築の試作 (develop/dag_test_20260429.R)

Glasso + Chatterjee の出力を結合する `direct_edges()` を試作。
asym_thresh = 0.01 で「絶対値で大きい方が親」ルールを実装。

J15S3810 で実行した結果，Glasso スケルトンが 98 エッジ（15×14/2=105 中の
98）と密。EBIC は罰則が効かず最小 λ を選び続ける。lambda_ratio を 0.3
に上げて 61 エッジ，さらに \|theta\| \> 0.1 の閾値カットで 10
エッジまで絞り込み。10 エッジのうち 3 が方向確定（V4→V8, V8→V6,
V4→V11），7 が双方向（ξ 差 \< 0.01 で方向不明）。

3パターンの DAG 比較も試作: - A: 純平均ベース（low → high） — 必ず DAG -
B: 純 ξ ベース — 双方向多数で DAG ならず - C: ξ 優先・平均 fallback —
一見 DAG ぽいが ξ が mean 順序に
逆らうエッジを生むためサイクルが発生（V4→V8→V6→V13→V4）

### 設計上の方向転換: 累積リンクモデル本流

セッション後半で v2.0.0 構造学習の本流が「累積リンクモデル」である
ことが明確化。Chatterjee ξ は構造学習の入口（XICOR の代替）として
有用だが，方向決定の本流は累積リンクで X→Y と Y→X の尤度比較を する
LiNGAM の順序版が筋が良い。先生発案の Q\_{XY}(x) = P(Y ≥ x \| X = x)
アプローチは累積リンクの「対角成分」の sub-case として位置づけ，
補助プロファイル/解釈ツールとして温存する方針。

memory `project_exametrika_future.md` のアイデア5に詳細記録。

### 試作の到達点と未着手

- v1.13.0 commit: 7bf5ce9（push 済み）。Glasso と Chatterjee 群と print
  method を含む
- direct_edges() は develop に試作残置。exametrika に組み込みは
  累積リンク版 (v2.0.0) と並行検討
- v2.0.0 多値BNM の構造学習は「入口=ξ → スケルトン=Glasso →
  方向=累積リンク」の三段構成で進める方針

## 2026-04-28 — v2.0.0 多値BNM準備: Chatterjee’s ξ プロトタイプ + ブートストラップ設計

`develop/Chaterjee20260428.R` で Chatterjee (2021) の ξ_n
を素のRで再実装し、 XICOR
との数値一致を取った後、タイ多発データでの安定化方策を実験的に決定した。

### ξ_n 手計算の検証

peas データ（n=700, parent のユニーク値 7 個 = 693 タイ）で
`XICOR::xicor()` と完全一致を確認。途中で踏んだ典型バグを記録しておく:

- **X のタイ崩し**: `order(x)` ではなく
  `order(rank(x, ties.method = "random"))`。 R
  の安定ソートは元の出現順を保つが、Chatterjee
  の定義は一様ランダム順序。
- **\|r\_{i+1} - r_i\| の `abs` 抜け**: `sum(diff(r_i))` は望遠鏡和で
  `r_n - r_1` に潰れる。`sum(abs(diff(r_i)))` が正しい。
- **l_i の対象**: l_i は **Y に対する降順ランク**
  `rank(-sorted.y, "max")`。 X のランクではない。
- **τ_n² 推定の u_i**: 論文 Theorem 2.2 直後の式で「u_i は R(1),…,R(n)
  を **昇順並べ替え**したもの」と明記されている (correlation43_ja.tex
  L325)。 XICOR の `qfr = sort(fr)`
  と同じ。`y[r_i]`（生Yをランクで引く）ではない。

最終的に xi/sd/pval が XICOR と bit-identical (差 1.78e-15) で一致。

### ブートストラップ B 数の目安

タイの多いデータでは ξ_n
がタイ破りごとにブレるので、複数回平均で安定化する。
（行サンプリングではなく、[`set.seed()`](https://rdrr.io/r/base/Random.html)
を振り直して `rank(x, "random")` を B
回引き直すだけの軽量版。サンプルサイズが変わらない利点あり。）

n とタイ密度を変えて sd(ξ_iter) を測定:

| データ         | n   | sd(ξ_iter) | B=1000 SE |
|----------------|-----|------------|-----------|
| peas           | 700 | 0.024      | 0.00075   |
| peas subsample | 100 | 0.060      | 0.00191   |
| Likert 5件     | 300 | 0.040      | 0.00127   |
| Likert 5件     | 100 | 0.071      | 0.00223   |

おおむね sd(ξ_iter) ∝ 1/√n（タイ密度同程度なら）。

**結論**: 5件法 n=100 の最悪ケースでも B=1000 で SE ≒ 0.002、ξ が
**小数点以下 2 桁で安定**する。BNM 構造学習の枝符号判定には十分。 v2.0.0
多値 BNM 実装時のデフォルトは `B = 1000`、論文用の高精度報告は
`B = 5000` を引数で指定可能にする方針。

設計上の TODO（v2.0.0 多値 BNM 実装時）: - `xi_stable(x, y, B = 1000)`
のラッパー関数を用意 - 「最初に B=200 で sd(ξ) を測って目標精度から B
を自動決定」案も検討 - ブートストラップは ξ 推定値だけでなく p
値も同様に平均化 - 累積平均の SE は `sd(xi)/sqrt(B)`（標本 SD
ではない点に注意）

### 成果物

- `develop/Chaterjee20260428.R` — ξ_n 手計算 + B=10000
  のブートストラップ実験
- `develop/Chaterjee_bootstrap_convergence.png` — 累積平均 ± SE プロット
- `develop/Chatterjee2021/correlation43_ja.tex` —
  日本語訳（既存）の参照箇所 L308-329（定理 2.2 周辺、$`\hat{\tau}_n^2`$
  推定式）

## 2026-04-27 — v1.12.0: C++ Gibbs sampler, conf_class, CRAN prep

### Confirmatory Biclustering bug audit

Started from a known issue: `.claude/CLAUDE.md` flagged that
`R/07_Biclustering.R` (binary) and `R/15_Biclustering_nominal.R`
(nominal) had the same `NCOL(U)` length-check pattern that was fixed for
ordinal in the v1.12.x prep work, but with the qualifier “not
catastrophic because of the in-loop guard”. Empirical check on `J20S600`
with `length(conf) = 20` showed:

- Binary: works at runtime.
  [`Biclustering.binary()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
  rebinds `U <- tmp$U * tmp$Z` in line 169 *before* the conf block, so
  `NCOL(U)` returns the item count. Cosmetic-only fix to `NCOL(tmp$U)`
  for readability.
- Nominal: **broken since v1.10.0**.
  [`Biclustering.nominal()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
  does not rebind `U`, so `NCOL(U) == 1` (it is an `exametrika` list),
  and every well-formed `conf` vector hits the “size does NOT match”
  stop. The in-loop guard never runs because the conf block aborts the
  call up front. Fixed to `NCOL(U$Q)`.
- New bug found while writing matrix-form regression tests: the
  `is.matrix(conf) | is.data.frame(conf)` branch validated the matrix
  but never assigned it to `conf_mat`. The next line,
  `nfld <- NCOL(conf_mat)`, then died with “object ‘conf_mat’ not
  found”. Same omission in all three implementations (binary, ordinal,
  nominal). Fixed by adding `conf_mat <- as.matrix(conf)` in each matrix
  branch.

Tests: 10 new test_that blocks across `test-biclustering.R` and
`test-polytomous-biclustering.R` covering vector accept, wrong-length
reject, matrix accept, and wrong-rows reject for each of the three data
types. Full suite: PASS 4775 -\> 4790.

Committed as
`60bace9: fix: confirmatory Biclustering bugs across binary/ordinal/nominal`.

### Class-side confirmatory clustering (`conf_class`)

User asked to add the symmetric counterpart to `conf` that fixes
respondents to classes (instead of items to fields). Design (per the
TODO note in `.claude/CLAUDE.md`):

- Reuse the existing `conf` argument; add a new `conf_class` argument.
  Fully additive, no breaking changes.
- Mirror the conf type-check block: vector or 0/1 matrix, length check
  against `nobs`, build `conf_class_mat`, override `ncls` from
  `NCOL(conf_class_mat)`. ncls/nfld mismatch with the supplied conf is
  silently overridden (intentional, matches existing conf semantics).
- In the EM loop, after `clsmemb` is updated by softmax/normalisation,
  if `conf_class_mat` is non-null, force `clsmemb <- conf_class_mat`.
  For Ranklustering (`method = "R"`), also set
  `smoothed_memb <- clsmemb` because the neighbour-smoothing filter
  would otherwise re-spread mass off the fixed labels.
- `nparam` is not adjusted: only PiFR / BCRM cell probabilities are
  parameters in this code, and membership matrices are latent
  posteriors, not parameters. Recorded the rationale in NEWS.md.
- For rated data the dispatch is via
  [`Biclustering.rated()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
  which passes `...` to
  [`Biclustering.nominal()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
  internally, then re-orders output classes by correct rate. So
  `conf_class` works through rated but the output class labels are a
  permutation of the input labels (the individual-to-class mapping is
  preserved). Test verifies the bijective invariant rather than
  equality.

Tests: 13 new test_that blocks across the same two files plus a combined
`conf + conf_class` test on ordinal data.

### IRM Gibbs sampler in C++ (the headline change)

Profiled the R Gibbs core at 66-99 ms/iter on J20S600 and 71 ms/iter on
J35S500. Hotspots (`Rprof`, line-level): `irm_lmvbeta()` 20.6%, 3-D
`U_fcq[f, c, ]` slicing and `nume <- ... + alpha_vec` arithmetic 13-16%
each, `t(fld01) %*% (Z[target, ] * Uq[target, , q])` row matmul 13%.
Inner CRP loop dominates total time.

Wrote `src/irm_gibbs_core.cpp` as a direct, line-by-line translation of
`irm_gibbs_core()`. State held in flat `std::vector<double>` /
`std::vector<int>` to avoid an `RcppArmadillo` dependency. No
algorithmic changes: same loop nest, same accumulation order.

The RNG-compatibility journey was the most time-consuming part:

1.  First pass used `Rcpp::sample(n, n, false)`. Output diverged from R
    immediately. Direct check: `set.seed(42); sample(1:10, 10)` !=
    `Rcpp::sample(10, 10, false)`. Confirmed `Rcpp::sample` does not
    share the same RNG-consumption order as base R.
2.  Switched the perm to `Function f("sample.int"); f(n)`. With this
    alone, the class-side Gibbs loop produced bit-identical Nc and cls
    to the R reference (verified on `J20S600`, max_iter = 1).
3.  Field-side loop still diverged. After auditing the implementation
    line by line and finding no algorithmic difference, added a
    class-only debug build (`#if 0` around the field-side loop) and
    confirmed the post-class-loop RNG state matched: `runif(3)` in R and
    in C++ returned the same three values.
4.  Despite same RNG state, full-run jRand still differed from the R
    reference. The remaining suspect was `R::rmultinom` (the C-level
    entry). Replaced it with
    `Function r_rmultinom("rmultinom"); m = r_rmultinom(1, 1, p)`.
    Bit-identical match for max_iter = 1, 5, 10, 50.

Hypothesis for why `R::rmultinom` desynchronises while `R::sample`
(well, `Rcpp::sample`) was at least RNG-consistent internally is that
`do_rmultinom` runs `FixupProb` and possibly other
validation/normalisation steps that `R::rmultinom` (the C-level form)
skips. We tried explicit normalisation (`p[k] /= sum(p)`) before
`R::rmultinom` and it still desynchronised, so there is more going on
than just the FixupProb division. Did not investigate further; the
R-level path is fast enough.

Performance after the switch:

- nominal J20S600: 82 -\> 20 ms/iter (4.0x)
- ordinal J35S500: 76 -\> 19 ms/iter (4.1x)

End-to-end `Biclustering_IRM(J20S600, max_iter = 100)` drops from a few
seconds to ~0.3 s. Numerical equivalence verified by 6 unit tests in
`test-irm-gibbs-cpp.R` covering several seeds and iteration counts (cls,
fld, Nc, Nf, cls01, fld01, U_fcq all bit-identical).

R integration: added `use_cpp = TRUE` argument to `irm_gibbs_core()`.
Default dispatches to `irm_gibbs_core_cpp()`; `use_cpp = FALSE`
preserves the R reference for cross-checking. The two callers
([`Biclustering_IRM.nominal()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
and
[`Biclustering_IRM.ordinal()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md))
need no changes; rated dispatch flows through nominal automatically.

Committed (with conf_class and bg-agent work bundled) as
`d485a18: feat: C++ IRM Gibbs sampler + class-side confirmatory + vignette/test polish`.
Pushed to origin/main.

### CRAN submission prep

Pending A3 Phase3 simulation completion (per `.claude/CLAUDE.md`
policy), but staged the docs-only prep work so the submission can go out
as soon as Phase3 lands:

- `.Rbuildignore` += `^figure$` (knitr vignette artefact, was being
  picked up as untracked).
- `roxygen2::roxygenise()` to regenerate `man/Biclustering.Rd` with the
  new `conf_class` argument; added a proper `@param conf_class` block in
  `R/07_Biclustering.R` so the description body is non-empty.
- `NEWS.md`: consolidated the duplicate “## Bug Fixes” subsection into
  the single “## Bug fixes” block, and corrected the “## Notes” entry
  which had previously claimed “no API changes” (conf_class is an
  additive API change).
- `cran-comments.md`: rewrote for v1.12.0 (was still v1.11.0 text).

Committed as `86e7b6b: docs: prep CRAN submission for v1.12.0`.

### Outstanding before CRAN submission

- A3 Phase3 simulation must finish (Al-Khwarizmi, expected ~4/25
  evening, slipped). Notify on completion.
- GitHub Actions: confirm `R-CMD-check.yaml`, `rhub.yaml`, and
  `pkgdown.yaml` all pass on `main` after the two recent pushes.
- `R CMD check --as-cran` locally on macOS arm64 (R 4.5.3).
- `rhub::check_for_cran()` and `devtools::check_win_devel()`.
- Then submit; existing CRAN-SUBMISSION points to v1.11.0 and will be
  refreshed by the submission tooling.

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

### Uq one-hot vectorization + maxiter bug fix (same day)

Discovered while preparing simulation deployment that the `Uq` one-hot
encoding in both
[`Biclustering.ordinal()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
and
[`Biclustering.nominal()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
is still a nobs\*nitems nested R loop:

``` R
for (i in 1:nobs) for (j in 1:nitems)
  Uq[i, j, tmp$Q[i, j]] <- 1
```

Replaced with a single matrix-index assignment using masked
`cbind(i_idx, j_idx, q_idx)` restricted to `tmp$Z == 1`. The old loop
also had a silent bug: for missing entries (`tmp$Q[i,j] == -1` set by
[`dataFormat()`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)),
`Uq[i, j, -1] <- 1` sets every Uq\[i, j, k\] for k in 2..maxQ to 1 due
to R’s negative-subscript semantics. The values were never read (every
consumer applies the `tmp$Z` mask), but the vectorized version correctly
leaves those positions at zero. Downstream outputs (BCRM,
ClassMembership, TestFitIndices, log-lik) remain bit-identical to the
pre-refactor baseline.

Wall-clock impact on the 8-configuration validation matrix:

``` R
O1 (J35S500,  B, 5,5)   0.068s -> 0.033s  (2.06x)
O2 (J35S500,  B, 10,5)  0.178s -> 0.147s  (1.21x)
O3 (J35S500,  R, 5,5)   0.060s -> 0.038s  (1.58x)
O4 (J35S500,  B, 5,5, mic=TRUE)
                        0.049s -> 0.024s  (2.04x)
O5 (J15S3810, B, 5,5)   0.364s -> 0.241s  (1.51x)
O6 (J15S3810, R, 8,4, non-converge)
                        0.802s -> 0.552s  (1.45x)
N1 (J20S600,  -, 4,3)   0.021s -> 0.004s  (5.25x)
N2 (J20S600,  -, 6,4)   0.058s -> 0.049s  (1.18x)
```

Independent maxiter bug: ordinal also had `maxemt <- 100` hardcoded at
line 39, identical in spirit to the nominal bug fixed in 1.11.0. The
user-supplied `maxiter` argument was being ignored. Fixed in the same
commit. testthat passes (4750, FAIL 0).

### GridSearch() per-cell error tolerance (same day)

While preparing the simulation-harness refactor, we discovered that
[`GridSearch()`](https://kosugitti.github.io/exametrika/reference/GridSearch.md)
does not wrap its inner `do.call(fun, args_list)` call in `tryCatch`:
any error raised by
[`Biclustering()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
at a single grid cell terminates the whole grid search, and the caller’s
higher-level `tryCatch` then turns the entire configuration into an
all-NA row. This has a disproportionate effect on small `(nobs, nitems)`
conditions (empty-cluster edge cases at corners of the grid), which we
observed wiping out the Biclustering-branch rows for small conditions in
the Phase3 simulation.

The fix is a two-line `tryCatch` wrapper in both branches (Biclustering
and LCA/LRA) of `R/00_GridSearch.R`, treating errors exactly like
non-convergence (NA in the index matrix, record in `failed_settings`).
All 4750 testthat tests still pass. This behavior change is non-breaking
for successful grids but is a genuine bug fix for pathological grids,
and is shipped as part of v1.12.0.
