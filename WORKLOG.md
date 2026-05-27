# exametrika Work Log

Detailed development log. User-facing changes go in `NEWS.md`; this file
captures the per-session internal narrative (why a change was made, what
was investigated, what was ruled out). Entries are newest-first.

## 2026-05-27 (午後) — v1.14.0 Bug \#2 IRM binary 欠測 NaN 修正

A3 論文 (polytomous_biclustering) の 4.4 節 実データ適用作業中に発見した
バグの調査と修正。

### 発見の経緯

論文 4.4 用に HCI (Homeostasis Concept Inventory, 651×20 4肢択一) と
SAT12 (mirt, 600×32 5肢択一) を二値化して IRM にかけたところ，両者
で同じエラー:

    log(CcfPlus[i, j] + gamp - 1 - s + const) で:
      計算結果が NaN になりました
    rmultinom(1, 1, ptab) でエラー:
      確率ベクトル中にNAがあります

シミュレーションでは binary IRM の収束率が 93.7% と他データ型 (96-98%)
より 低かったのもおそらく同根 (Phase3
シミュは欠測なしだったので顕在化せず)。

### 真因

`Biclustering_IRM.binary` (R/07_IRM.R) は `tmp$U %*% fld01` で `Cif` を
集計するが，[`dataFormat()`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
は欠測セルを `tmp$U` に **-1** として残し 別途 `tmp$Z`
で観測マスクを持つ設計。`tmp$U` をマスク前のまま積算すると -1 が伝播して
`CcfPlus` が負 → [`log()`](https://rdrr.io/r/base/Log.html) で NaN →
`ptab` 全体が NaN → `rmultinom` クラッシュ。

line 120 に `U <- tmp$U * tmp$Z` という**未使用**変数があった (代入先が
誰にも参照されないデッドコード)。リファクタの取り残しと思われる。

### 修正

`R/07_IRM.R` 2行修正:

1.  line 120: `U <- tmp$U * tmp$Z` → `tmp$U <- tmp$U * tmp$Z` 下流の
    `tmp$U %*% fld01` 系 (line 165, 204, 324, 339) 全てが Z マスク済み
    値で計算されるようになる。
2.  line 610: `U = U` → `U = tmp$U` 旧コードは line 120 のデッド代入が
    function parameter `U` を上書き していた偶然に依存して
    `U = masked_matrix` を返していた。新コードは `tmp$U`
    から明示的に取る (これがないと `plot_array` が dataFormat
    オブジェクトを matrix と誤解して「次元不正」エラーになる; R CMD
    check examples で発覚)。

### テスト

`tests/testthat/test-irm.R` 末尾に Bug \#2 regression test を追加。
500×20 二値データに 2% の欠測 (-1) を入れ，`Biclustering_IRM` が
クラッシュせず通常の `exametrika`/`IRM` クラスのオブジェクトを返す
ことを検証。`skip_on_cran()` 付き (推定に数秒かかるため)。

### Bug \#3 (CFI clip による benchmark inversion 隠蔽) は見送り

同じ作業の副産物として `calcFitIndices` の `pmax(., 0)` クリップが
`chi_A < 0` の異常を `CFI = 1.0` として隠蔽する挙動を発見 (bfi binary
で再現)。これは Bentler-Bonett の伝統的仕様であり Mathematica reference
も同じ挙動。仕様逸脱 (NA + warning 化) を一度実装してテスト通過まで
持っていったが，

- Mathematica reference と CSV fixture が CFI=1.0 / RMSEA=0 を出している
- 「コードは正せず Mathematica .nb
  も同期する」と「クランプは設計と認める」 の判断が必要

ため，先生判断で v1.14.0 では見送り。`develop/20260527_bug_reproduce.R`
に再現コードを残し，将来 (v1.15+) で Mathematica 同期込みで再検討する
材料として保留。

### 検証

- `devtools::test()`: 4899 PASS / 0 FAIL / 0 WARN / 0 SKIP
- `R CMD check --as-cran`: 0 ERROR / 0 WARNING / 1 NOTE (NOTE は HTML
  Tidy 環境依存; CRAN サーバでは出ない)
- `R/07_IRM.R` の plot サンプル (Biclustering_IRM(J35S515)) が
  `plot(result, type="Array")` まで完走することを確認

## 2026-05-27 — PR \#30 (socialistic.ai 誘導 PR) 拒否・周辺ハウスキーピング

コードには触らず，GitHub 上の宛先処理だけのセッション。

### PR \#30 / issue \#29 — community-hosted リンク追加 PR を close

shesl-tinkerland (shesonglin) から README に「Try it online
(community-hosted)」セクションを追加する PR が到着 (5/26 14:19 UTC)。
リンク先は `socialistic.ai/skill/exametrika-test-analysis-ebe9d8` で
`utm_source=github&utm_medium=readme&utm_campaign=exametrika` 付き。

中身を吟味した結果，以下の理由で **マージ拒否 → close** を判断:

1.  **socialistic.ai は exametrika を走らせていない** — LLM ベースの
    skill marketplace で，ブラウザ上で Rcpp の GRM optimizer や IRM
    Gibbs sampler を再現できるわけがない。CSV アップで「IRT/潜在ランク
    /学習経路/Biclustering/TDE レポート」が出ると謳っているが，実体は
    LLM が exametrika 風の出力を捏造している可能性が極めて高い
2.  **ゼミ生・同僚への誤誘導リスク** — 「これが exametrika の出力です」
    とハルシネーション結果を信じさせると reputational damage。 5/23 に
    ラマヌジャン bot のハルシ対策を入れたばかりの立場で，同じ落とし穴の
    サービスを公式 README から推奨するのは矛盾
3.  **README 同期ルール違反** — PR 自体が `README.md` 直編集 (CLAUDE.md
    で禁止)。PR 本人も「R toolchain 入れてないので手で sync した」と
    告白
4.  **OSS を利用した UTM トラフィック誘導** — 「community-hosted」「not
    affiliated」と書いてリスクを著者に押し付けつつバッジで集客する手口

対応:

- PR \#30: 1, 3 を理由として丁寧に説明するコメント投稿 → close
- issue \#29: 元の「面白い取り組みですね！」コメントを撤回するのは
  気まずいが，PR コメントを参照する短い follow-up を投稿 → close

両方 close 済み。文面は受け入れの可能性を残さない一方で，研究室外で
個人として使う分には問題ないことを明示。

### branch protection バナー — dismiss

GitHub のリポジトリトップに出ていた「Your main branch isn’t protected」
バナーについて質問あり。説明:

- 1 人開発 (committer は kosugitti + claude) で PR 必須化・review
  必須化を入れると自分が自分にレビューを強要するだけで生産性低下
- 普段から `git push origin main` 直 push 運用前提で，CRAN 提出フロー
  (`devtools::release()` の打つ tag や `cran-comments.md` の commit)
  と相性が悪い
- force push 事故リスクは低い (単独開発で `--force` は意図的にしか
  打たない)

→ Dismiss で OK。外部 PR が増えてきた段階で「force push 禁止 + 削除
禁止」最小構成だけ入れれば十分，という方針確認。

### issue \#28 (Release exametrika 1.14.0) の出自確認

「これ何で立ってる？」と質問あり。調査結果:

- 作成日時 2026-05-17 20:52 = v1.13.0 を CRAN に提出した直後
- 中身は `usethis::use_release_issue()` の標準テンプレート (git pull →
  NEWS polish → win-devel → revdepcheck → submit → tag → blog → social
  の checkbox 列)
- `devtools::release()` フロー or 直接
  `usethis::use_release_issue('minor')` が次の minor として 1.14.0
  を提案して自動作成

今 ちょうど進めている v1.14.0 (Glasso graceful divergence handling，
ターゲット 6/15 CRAN 提出) の release tracking issue として **そのまま
使える** ことを確認。close せず残置。

### 今回得た知見

- **LLM-marketplace 系の README リンク追加 PR は要警戒**。UTM 付き
  バッジ + 「community-hosted/not affiliated」の組み合わせは典型的な OSS
  スパムパターン。memory に拒否方針を記録
- **`usethis::use_release_issue()` は次バージョンの checklist を
  自動生成する** — 提出時に立つので「これ俺立てたっけ？」と混乱
  しがち。memory 化

------------------------------------------------------------------------

## 2026-05-23 — v1.13.1 後フォローアップの取りこぼし確認

5/20 セッションで Discussions follow-up と smoke test を実施した記録が
WORKLOG にあるにも関わらず，`.claude/CLAUDE.md` の TODO リストが `[ ]`
のままだった (前回セッションが TODO 更新を忘れていた)。 別マシン (Mac
mini 側) から「メール来たぞ」の連絡があり， GitHub API で 5/19 08:24 UTC
に kosugitti アカウントで follow-up 投稿 されていることを確認 → TODO
`[x]` に更新。

ついでに smoke test を再確認:

- **ggExametrika** `devtools::test()`: 611/611 PASS, WARN 1 (既知の
  stanine 9 分割警告で 1.13.1 無関係), SKIP 0
- **shinyExametrika** `devtools::test()`: 67/67 PASS, WARN 0 (前回 5/20
  は PR#14 マージ作業だけで smoke test 単独の結果は WORKLOG に未記載
  だったので，1.13.1 と shinyExametrika の組み合わせの初回正式確認)

`.claude/CLAUDE.md` の「優先度：高（v1.13.1 受理後のフォローアップ）」 3
件全て `[x]` クローズ。次は v2.0.0 (StepBNM / 案 C) 着手フェーズ。 CRAN
cadence で提出は 6/18 以降。

### 教訓

セッション終了時に WORKLOG.md と `.claude/CLAUDE.md` の TODO の同期を
忘れると，後続セッションで「やったのか，やってないのか」の二度手間が
発生する。「ログ書いておいて」ルーチンの 2. (CLAUDE.md 更新) には **TODO
リストのチェック更新も含む**ことを明示しておく方が安全。

------------------------------------------------------------------------

## 2026-05-20 — v1.13.1 受理後の周辺整備（Discussions / smoke test / sv1+sv2 展開）

5/18 v1.13.1 CRAN
受理を受けて，残っていた周辺タスクを片付けるセッション。

### Discussions \#26/#27 への follow-up

v1.13.0 リリース告知 (日本語 \#26 / 英語 \#27, 5/17 投稿) には CRAN
反映の 顛末がまだ付いていなかったので，1.13.1
受理を短くまとめてコメント追加:

- \#26 (JP):
  <https://github.com/kosugitti/exametrika/discussions/26#discussioncomment-16972892>
- \#27 (EN):
  <https://github.com/kosugitti/exametrika/discussions/27#discussioncomment-16972893>

要旨: v1.13.0 が CRAN auto-check の r-devel-windows-x86_64 で 「Overall
checktime 11 min \> 10 min」NOTE で auto-reject されたこと， 1.13.1 は
test-grm.R / test-irm.R の重いブロックを `skip_on_cran()` で除外する
hotfix で **ユーザ可視変更なし**，スキップ対象は CRAN だけで `NOT_CRAN`
経由のローカル/R-hub/win-devel では従来通り走るためカバレッジ不変。

### ggExametrika smoke test (v1.13.1 連動)

Newton で `install.packages("exametrika")` で 1.12.2 → 1.13.1
に上げてから ggExametrika (現 v1.1.1 dev) の `devtools::test()` を実行:
**611/611 PASS**。 警告は BINET (Class 1→Class 2 のフィールド数チェック)
の caution が 1 件 だけで，機能上は問題なし。1.13.1 の API 変更は
ggExametrika 側に影響なし。

### shinyExametrika PR#14 マージ (別リポジトリ)

arimune-san の “DAG plot height slider for BNM and LDLRA” を smoke test
後マージ。 詳細は shinyExametrika/WORKLOG.md 参照。

### sv1/sv2 への R 4.6.0 + exametrika 1.13.1 展開

EPEL 9 に R 4.6.0-1.el9 が 5/5 以降に到着していたので，sv1/sv2 を一気に
4.5.3 → 4.6.0 + exametrika 1.13.1 へ。詳細は
`~/Dropbox/.claude-system-inventory.md` の作業ログ 2026-05-20
セクション参照。 両機とも
[`library(exametrika); IRT(J15S500)`](https://kosugitti.github.io/exametrika/)
で 11 iter 収束， LogLik -3893.03 で完全一致。

### 次フォローアップ

- 3.  v2.0.0 BNM 着手 (CRAN cadence で 6/18 以降 提出目安)
- sv1/sv2 で 1 pass で取りこぼした依存順失敗パッケージ 5-6 個の 2nd pass
- Newton id_ed25519.pub を sv1/sv2 へ登録して二段 SSH 解消

## 2026-05-17 — v1.13.0 CRAN 提出（5/15 予定から 2 日遅れ）

5/8-9 の案 C BNM プロト確認以降 A5 お遍路さん側に注力していたため 予定日
(5/15) を 2 日過ぎての提出。残作業は (1) 提出前チェック群, (2) tag, (3)
GitHub Release / Discussions, (4) `devtools::release()`, (5)
confirmation。全部消化。

### A. 提出前チェック

`tools/build_pkg.R` を一気通貫で実行 (styler → document → spell_check →
check(cran=TRUE) → rhub → check_win_devel → release):

- **ローカル `R CMD check --as-cran`**: 0 errors / 0 warnings / 0 notes
  (6m 20s, macOS arm64, R 4.6.0)
- **win-devel** (R-devel 2026-05-15 r90061 ucrt): Status OK (build 34s,
  check 640s, log <https://win-builder.r-project.org/DA3rl6RAg6Bn>)
- **R-hub v2** (workflow_dispatch ラン
  `velveteen-asiaticlesserfreshwaterclam`, run id 25984718083):
  - linux (R-devel): 14m26s success
  - macos-arm64 (R-devel): 15m11s success
  - windows (R-devel): 19m52s success

R-hub の Annotations は全部 J*-S* フィクスチャ向けの想定済み
[`message()`](https://rdrr.io/r/base/message.html) (zero variance /
missing responses など) で check failure ではない。

### B. 未コミット作業ログを掃除

`WORKLOG.md` に 5/12-13 セッション (ξ map + StepReg/StepNet 命名) の
追記が staged 前段で置いたままだったので `5fa6142` で先にコミット。
v1.13.0 のリリース内容自体には影響しないが台帳としては落としておく。

### C. タグ運用

- **v1.12.2** (`26855f1`, “chore: re-version 1.13.1 → 1.12.2”): 既に
  打ってあった。A3 polytomous_biclustering 論文のベースライン用に CRAN
  未公開のまま git tag だけ残しておく方針 (NEWS は 1.13.0 が
  ロールアップ)
- **v1.13.0** (`5fa6142` = WORKLOG 追記後の main HEAD): 今日打った。
  WORKLOG は `.Rbuildignore` で除外されるので tar.gz には含まれない

両方を `origin` に push (main も同時)。

### D. GitHub Release + Discussions 投稿

ハイライト本文を作成し:

- **GitHub Release** v1.13.0: `gh release create --latest`,
  <https://github.com/kosugitti/exametrika/releases/tag/v1.13.0>
- **Discussions (日本語)** \#26 (Announcements):
  <https://github.com/kosugitti/exametrika/discussions/26>
- **Discussions (英語)** \#27 (Announcements):
  <https://github.com/kosugitti/exametrika/discussions/27>

Discussions は `gh discussion create` が存在しないので GraphQL
`createDiscussion` mutation 直接叩き。`repositoryId` は
`gh api graphql -f query='query { repository(...) { id } }'` で,
`categoryId` は `discussionCategories` クエリで取得 (Announcements =
`DIC_kwDOJS6L084CZbQk`)。本文は `-F body=$(cat file.md)` で多バイト
そのまま渡せる。

### E. CRAN 提出

RStudio から `devtools::release()` 実行 → CRAN にアップロード →
confirmation メールのリンクをクリック済み。受理は incoming queue の
自動チェック後 (数日〜1週間)。

`release()` 副作用: - `CRAN-SUBMISSION` を 1.11.0 → 1.13.0 (SHA
`5fa6142`) に書き換え - `tools/build_pkg.R` の最終行を
`devtools::release()` → `usethis::use_release_issue()` に置換
(次回サイクル用の usethis 提案)

`9122685` でコミット & push。

### 次へ

- CRAN 受理メール待ち (来たら本ホーム CLAUDE.md A4+C1 ステータスを
  「v1.13.0 CRAN 受理」に書き換え)
- v2.0.0 BNM (StepBNM = 案 C, DAG 所与スコープ) に着手可能
- A5 お遍路さん 5/20 締切の社会心理学会原稿に復帰

------------------------------------------------------------------------

## 2026-05-12〜13 — ξ map 法の確立 + 「ステップ回帰/ステップネット」命名

5/8-9 で案 C BNM プロトタイプの動作確認まで進んだ後, 「変数選択を恣意性
から救う」目的でデータ駆動 DAG 抽出パイプラインを構築。発展的に
ステップ回帰の概念整理と命名確定まで。

### A. Chatterjee Map (ξ map) プロトタイプ

`02_研究/お遍路さん/2025本調査データ/20260513_chatterjee_map.R`:

着想: ξ 行列 (Chatterjee の非対称相関) から (1) 距離成分 → MDS で 2D
埋め込み, (2) 非対称成分 → 各項目の方向ベクトル場, を分解。これに
よって「項目地図 + 方向場」として可視化し, DAG 候補を地理的に
読み取る方法。

お遍路さん N=143, 98 項目 (E=32, M=66) で実行:

- 距離 d_jk = 1 − max(ξ_jk, ξ_kj) で MDS → 2D
- 方向 v_j = Σ_k (ξ_jk − ξ_kj) · (pos_k − pos_j) / \|\|·\|\|
- 結果: **E 群 (体験) と M 群 (意味づけ) が地図上できれいに分離** (xi
  within-group \>\> xi between-group のため)。E と M の独立性 (A5
  論文の柱 1) を視覚的に補強。

先生提案 19 辺 DAG との重ね合わせ: cos alignment 整合性チェックで **58%
(11/19) が方向場と一致**, 残り 8/19 は逆向き。「データ駆動 ξ 方向 ≠
因果的時間 ordering」の差を定量化。例: E19→M07 (衣装→達成感)
は理論的に妥当だが ξ では逆向きに出る (M07 が衣装着用を予測する側で ξ
大)。

### B. ξ map からの DAG 自動抽出

`20260513_extract_edges.R` (v1, 失敗) → `20260513_extract_edges_v2.R`
(成功):

- v1: 全ペアで上位 5% フィルタ + 相対非対称度 \> 5% → 73 辺だが **E-M
  クロスが 0 辺** (E-M の絶対値が M-M に勝てない)
- v2: **edge type 別 quota** (E-E:10, M-M:15, E-M:30) で 54 辺 (うち
  E-M:29) を救う。サイクル除去後 DAG 化

E-M 分布の比較で問題が判明:

                  q50    q90    q95    q99
    E-E (496)    0.133  0.250  0.282  0.398
    E-M (2112)   0.024  0.088  0.107  0.137   ← M-M の 1/8
    M-M (2145)   0.202  0.318  0.356  0.421

E-M クロスは絶対値で 1 桁弱い。「弱くても E-M なら重要」(先生の研究
仮説) を実現するには **type 別フィルタが必須**。

### C. 抽出 DAG の StepBNM fit

`20260513_caseC_fit_extracted.R`:

E-M 29 辺を子ノードでグループ化 (22 child) し, brms `mo()` + cumulative
probit で fit。結果:

- **26 / 29 (90%) が CI で 0 を跨がず有意**
- 上位辺: E01 → M23 (β=2.84), E01 → M37 (β=2.74), E05 → M29 (β=2.43)
- 負の β: **M19 → E27 (β=−2.38)** “リフレッシュ → トラブル少ない”
  という非自明な発見。M19 を見ればトラブル経験者でないことが予測される
- 非有意 3 辺: E05→M28, M35→E02, M42→E02 → triple filter (GGM + ξ +
  StepBNM) で更に絞り込みが効く

子ノード別 log_lik 改善 (vs null):

    M28 (3 親): +28.4    最も改善
    M23 (E01 単親): +25.7
    E02 (M10+M48+M35+M42 4 親): +22.7
    M10 (E01+E05): +19.8

E01 が圧倒的ハブ性を再確認 (GGM, ξ map, StepBNM 全て一致)。

### D. 命名確定: ステップ回帰 / ステップネット

「案 C」が呼びにくいため命名議論。先生から「**案 C は network model
じゃなくて regression model だろ. 多項条件付き確率の関数形だ**」と
本質的指摘 → 二層化命名:

    ステップ回帰 (StepReg) : P(Y | X_1,...,X_k) の関数形
                             monotonic step (mo() in brms) + cumulative probit + 加法
                             Bürkner-Charpentier 2020 そのもの, 独立した
                             回帰モデルとして使える
                             ↓
    ステップネット (StepBNM) : StepReg を各 child ノードに置いた DAG
                              ベース BNM。荘島 binary BNM の多値拡張

カタカナでも英略でも通用するのが採用理由。荘島先生面会用の
プレゼンでもこの 2 階層で説明する筋。

### E. 5/29 荘島先生面会用の整理

メモリ `shojima_meeting_5_29.md` 更新済。発表ストーリー候補:

1.  ステップ回帰 (StepReg) と SEM 流 β の対応 (β = bsp_moX × D)
2.  ステップネット (StepBNM) の動作確認 (お遍路さん 6 ノード)
3.  ξ map 法 (本セッションの新ネタ) → AMISESCAL との接続可能性
4.  データ駆動 DAG 抽出 → StepBNM 検証 (triple filter pipeline)

### 次へ

- **5/20 締切の社会心理学会原稿**へ復帰 (Fig 1 パス図, Fig 2 E×M
  heatmap, Fig 3 forest plot)
- **5/29 面会用プレゼン資料**の下書き
- **v1.13.0 CRAN 提出 (今日, 5/15)**: 別途実施

------------------------------------------------------------------------

## 2026-05-08〜09 — 案 C BNM プロトタイプ動作確認 + v2.0.0 設計スコープ確定

5/1 で 3 段構え (Glasso → ξ → 案 C) の方針を固めた後,「案 C が実用に
耐えるか」をデータで確認するセッション。プロトは exametrika
リポジトリ内の `develop/` (gitignored) と `02_研究/お遍路さん/` の
両方で並行進行。

### A. 案 C は brms `mo()` で 1 行で書ける既存技術と判明

Bürkner & Charpentier (2020, BJMSP 73(3) 420-451) の monotonic effects
がそのまま案 C のパラメータ化と一致することを実験で確認。
`brm(Y ~ mo(X), family = cumulative("probit"))` で済む。

決定的な実証: `develop/20260508_caseC_diagnose.R` で brms 内部の
パラメータ化を逆算し, 案 C の μ_x と brms の `bsp_moX` の関係が **μ_x =
bsp_moX × D × cumsum(c(0, ζ))** であることを数値検証。 RMSE 0.085 (D
倍を含む) vs 0.785 (D 倍なし) で確定。

→ 翻訳ルール: **案 C のパス係数 β = bsp_moX × D**。SEM 流の path diagram
表記が直接できる。

### B. Phase A/B シミュレーション (J15S3810 ベース)

`develop/20260508_caseC_proto.R`, `phaseA2.R`, `phaseB.R`:

- **Phase A1**: 単親リカバリ (μ=(0,0.3,0.7,1.2,1.8) 真値) → 全パラメータ
  CrI 内, β = 1.67 \[1.45, 1.90\] vs 真値 1.80
- **Phase A2**: 4 形状 (linear/convex/concave/step) で安定リカバリ (RMSE
  0.05〜0.09)。step は Dirichlet(1,…) 事前で境界を僅かに膨らます (5/5 中
  4/5 カバレッジ)
- **Phase B**: 多親加法 vs 相互作用シナリオで LOO が正しく判別 (additive
  truth: m_add 勝, ΔLOO=−1.5; interaction truth: m_int 勝, ΔLOO=−62.8)

### C. Phase C (J15S3810 で全パイプライン)

- C-1: Glasso → 98 辺スケルトン
- C-2:
  [`xi_stable()`](https://kosugitti.github.io/exametrika/reference/xi_stable.md)
  で方向判定 → 78 辺の DAG. ただし **xi_stable の SE
  はタイブレーキング由来 (B 反復で √B → 0)** なので z
  検定は無意味。CLAUDE.md の 5/1 の警告と整合
- C-4: paired data bootstrap (B=500, idx 共有で xi_jk と xi_kj を
  同一リサンプル内で計算) → **本物の有意辺は 11**。残り 67 辺は
  「タイブレーキングに対して安定」というだけだった
- C-5: 11 辺の DAG 可視化 (Item13 がソース, Item14→Item11←Item15
  v-structure)
- C-6: 11 辺それぞれ案 C で β fit。Item5→Item12 だけ β CI が 0 跨ぎ
  (10/11 が triple-filter 通過)

### D. お遍路さんデータ N=143 で 6 ノード DAG fit

`02_研究/お遍路さん/2025本調査データ/20260509.R`:

DAG: E05 → {E07, M60}, E07 → M50, M60 → M50, M50 → M03, M03 → M14

主結果 (タスク 1〜4): - E07 → M50 が NULL (β = −0.36 \[−2.09, 0.52\]) →
削除で AIC/BIC 改善 - DAG 全反転で ΔAIC = +17 (forward 勝ち,
**方向情報が立っている**) J15S3810 は同質尺度で Δ=±5 だったが,
お遍路さんは多次元尺度で明確 - M50 の親候補比較で E05 を新規支持 (β =
1.16 \[0.40, 2.02\]) - 最終 6 辺 DAG: E05 が 3 子のハブ (E07, M60, M50)

理論的補強: BNM 全体尤度 (飽和周辺込み) では H(X) と H(Y) の
エントロピー項がキャンセルし, **方向差は KL(P̂(Y\|X) \|\| P_caseC(Y\|X))
の差**として残る (5/1 棄却された “ll 差はエントロピー差に支配” は
条件付き尤度単独比較の話。BNM 文脈では別)。

### E. GGM (お遍路さん 全 98 項目)

`02_研究/お遍路さん/2025本調査データ/20260509_t5_GGM.R`:

p=98 一括 Glasso は BCD 内で破綻 (`if (diff < eps)` で NaN) → 3
段階に分割:

- E 内 (32) Glasso: 219 辺 (密度 44%)
- M 内 (66) Glasso: 584 辺 (密度 27%, near-synonym ペア多数, M07-M08
  \|θ\| = 0.985)
- E-M クロス (32×66) は polychoric 相関の上位採用 (\|ρ\|\>0.4 で 245 辺)

発見: - **E01 (地元の人と挨拶) が E-M クロスの真のハブ** (5/9 朝の DAG
で E05 を選んだのは恣意的。E01 中心の方が情報量が大きい) - **M-M の
near-synonym ペアは Glasso でも残る** → field 化が必須

### F. v2.0.0 設計スコープを「DAG 所与」に絞り込み

「DAG 探索的推定は後回し, 荘島モデルも DAG ありき」と先生判断。 v2.0.0
BNM API は:

``` r

BNM_polytomous(data, adj_matrix,
               model = "caseC",       # 加法 + 単調制約
               ...)
# 出力: τ, μ プロファイル, β, log_lik, fit_indices
```

3 モデル比較: - M_0: 全独立 (β=0 / 周辺確率) - M_t: 案 C 加法 - M_s: DAG
内飽和 (各 child で free CPT)

NFI / CFI / RMSEA / AIC / BIC / CAIC を
[`calcFitIndices()`](https://kosugitti.github.io/exametrika/reference/calcFitIndices.md)
形式で。

ξ 行列の活用案: 局所診断 (DAG で繋がっていないペアの ξ_residual
が大きければ修正指数相当) として使う。

### G. README.md pkgdown 描画修正 (cc54ab3)

セッション中に先生が pkgdown サイトの LDB セクションが崩れている
ことに気付く。原因: 4 か所で prose 直後に ```` ```{r ...} ```` が来て
おり, パーサが状態を間違えていた。空行を入れて修正・push。 (その後
9787904 で README.Rmd → README.md ビルドフローに転換)

### H. データ修復: お遍路さん 00_dataHandling.R

`item_definitions.rds` に M11-M17 (宗教性 7 項目) のラベルが欠落
していたのを発見。`00_dataHandling.R` の `meaning_labels` 定義で M10
の次が M18 になっていた。アンケート PDF と照合して 7 項目を 挿入, rds
再生成 (60 → 67 ラベル)。

### I. brms 出力の解説 (E01 ~ mo(E05), M24 ~ mo(E01)\*mo(E05))

セッション末に先生が自分で brms を回し, 出力の各行の意味を確認:

- `Intercept[q]` = τ_q
- `bsp_moX` = per-step slope, **× D で β_caseC**
- `simo_moX1[j]` = ζ シンプレックス
- `disc = 1.00` = 識別性のために固定
- `mo(X1):mo(X2)` = 2 つのシンプレックスを持つ交互作用項 (1 b + 3 + 3 =
  7 パラメータ追加)
- 加法 vs 交互作用は LOO 比較で判定。先生例 (M24 ~ E01\*E05) は 交互作用
  NULL (CI 跨ぎ) で加法で十分

### 次へ

- **バイクラスタリング融合 (タスク 2)**: フィールド代表値スコアを
  作って案 C BNM の上位構造として乗せる。研究的本丸
- **MLE 実装**: brms は遅すぎ (1 fit 30s〜2m)。`optim` + 単調 / 順序
  reparametrize で最尤推定 (推定 100 倍速)。先生が brms 体験中なので
  急がない
- **19 辺 DAG full fit**: 先生の理論的提案 DAG を案 C で全 fit + β
  - 全体適合度
- **荘島先生メール**: 今日の整理を要約して送付の予定 (先生作業)
- **A3 polytomous_biclustering 論文**: Phase3 完走済 (5/3) なので
  v1.13.0 CRAN 提出 (5/15) と並行で論文 4 節の結果反映へ

------------------------------------------------------------------------

## 2026-05-01 — v1.13.0 CRAN check 通過対応 + 多値 BNM 構造学習の本格議論

このセッションは2部構成。前半は v1.12.1 → v1.13.0 のバージョン上げと
GitHub Actions R-CMD-check 失敗の修正。後半は多値 BNM 構造学習の
方針確定に向けた長い議論で，案を複数比較しながら最終的に「3 段構え」
(Glasso スケルトン → Chatterjee ξ で方向 → 単調制約累積リンクで条件付き
分布) を採用する見通しを立てた。本ログは後半の議論を丁寧に残す。

### A. v1.12.1 → v1.13.0 バージョン上げ

A3 Phase3 シミュレーション中に Biclustering EM ループの非有限
test_log_lik 事故が発覚し，先に v1.12.1 として hotfix (コミット 0ca95e6)
を切り出して Phase3 を継続走行させていた。Phase3 が動いている間は
DESCRIPTION のバージョンも 1.12.1 のまま据え置いていたが，4/30 に
v1.13.0 の主要機能 (Glasso + Chatterjee ξ, コミット 7bf5ce9) が main に
着地した上に pkgdown 登録 (258e208) も済んでいたため，本セッションで
DESCRIPTION の Version を 1.13.0 に上げた。NEWS.md には既に 1.13.0
セクションが書かれていたので変更なし。WORKLOG.md には v1.13.0 の
詳細経緯 (Glasso の Jacobi → Gauss-Seidel 修正，整数オーバーフロー
対応，DAG 試作の方向転換等) を追記。コミット 67ec9a1 で push。

### B. R-CMD-check の連続失敗を解消

push 直後に GitHub Actions R-CMD-check が落ちていることが判明。実は
v1.12.0 IRM Gibbs C++ コミット d485a18 から 3 連続で failure に
なっていた:

- d485a18 (v1.12.0 IRM Gibbs C++) failure
- 0ca95e6 (v1.12.1 hotfix) failure
- 258e208 (v1.13.0 pkgdown register) failure

ジョブの Annotation から 2 WARNING + 1 NOTE で `--as-cran` が exit 1
になっていたことを特定。

**問題 1 (WARNING): roxygen Markdown の角括弧自動リンク化**

`chatterjee_matrix.Rd` で `[j, k]`, `[i, j]`, `[i, k]` が `\link{}`
として 解釈され，`Glasso.Rd` で `[0, 1]` が同様に解釈されていた。これは
roxygen2 が Markdown モードで `[xxx]` を自動的に cross-reference に
変換する仕様のため。バックティックで囲んで `\verb{}` / `\code{}` 化
されるよう修正:

- `R/22_GlassoUnit.R`: `EBIC tuning parameter in [0, 1]` →
  `` EBIC tuning parameter in `[0, 1]` ``
- `R/23_Chatterjee.R`: `Entry [j, k] is xi(item_j, item_k)` →
  `` Entry `[j, k]` is xi(item_j, item_k) ``，`Q[i, j] or Q[i, k]` を
  `` `Q[i, j]` or `Q[i, k]` `` に。

**問題 2 (WARNING): Glasso() の `...` が undocumented**

関数定義に `...` があるが `@param ...` 行がなかった。
`@param ... Additional arguments (currently unused; reserved for future extensions).`
を追加。

**問題 3 (NOTE): partial argument match**

`R/22_GlassoUnit.R:113` で `determinant(Theta, log = TRUE)` の `log` が
`logarithm` に部分マッチしていた。`logarithm = TRUE` に修正。

`roxygen2::roxygenise()` で man/Glasso.Rd と man/chatterjee_matrix.Rd を
再生成 (`\verb{}`, `\code{}` に変わる)。`devtools::check_man()` で警告
なし確認。NEWS.md の 1.13.0 セクションに「Documentation and CRAN check
cleanup」サブセクションを追加。コミット 620c11e で push。

(なおこのセッションで一度 main 直 push が Claude Code の harness
permission に弾かれたが，2 回目以降は通った。1 回目はユーザに `!` で
シェル直叩きしてもらって解消した。)

### C. 多値 BNM 構造学習の本格議論

ここからが本セッションの主題。4/30 確定とされていた「累積リンクモデル
尤度比較 (LiNGAM 順序版) を方向決定本流にする」方針が本当に妥当か， 順序
BNM の構造学習をどう設計すべきかを長く検討した。最終的に
ユーザの整理で「3 段構え」が確定した。経緯を詳しく残す。

#### C.1 累積リンク尤度比較の限界発見

J35S500 (5 件法 ordinal, N=500, J=35) で
[`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html) probit を使った
最小実験を実施。

**実験 1** (`develop/20260501.R`): Item01-Item02 ペアで双方向 polr。

- factor 渡しだと係数 4 個 (ダミー)
- numeric 渡しだと係数 1 個 (X 連続化)
- ll(Item02 → Item01) = -380.57, ll(Item01 → Item02) = -761.23, Δ =
  +380.66

問題提起: Δ が +380 もあるのは怪しい。手計算でエントロピー差を出すと n ×
(H(Item02) - H(Item01)) = 380.56。実測差 +379.82 と誤差 0.7 で
ほぼ一致。これは累積リンクの漸近形

ℓ(X → Y) ≈ -n · H(Y \| X)

から差を取ると ℓ(X→Y) - ℓ(Y→X) ≈ n · (H(X) - H(Y)) になることに
対応。**生の対数尤度比較は「エントロピー小が下流」ルールしか出して
いない**。LiNGAM 順序版にはなっていない。

**実験 2** (`develop/20260501b.R`): MI 上位 10 ペアで同じ実験を回し，
残差 (delta_ll - delta_pred_ent) の挙動を確認。

- Item01-Item02 (MI ≈ 0.013) では残差 +0.7 (誤差レベル)
- MI 上位ペア (MI ≈ 0.20-0.24) では残差 -26〜+60 (支配的)
- residual / \|delta_ll\| が 1.66〜4.45
- ll 判定と ent 判定が一致 9/10 (ペア 22-34 のみ符号反転)
- β の絶対値は両方向ほぼ同じ (-0.0007〜-0.0680 / 0.27〜0.56)

つまり方向情報は β にではなく対数尤度差の残差に存在し，MI が小さい
ペアでは残差が消える。「累積リンク尤度差 = エントロピー差」は MI ≈ 0
の極限の話で，MI 大では残差として方向情報が出る可能性がある。
ただし単独で実用するには残差を抽出する別の指標化が必要で，
「方向決定の本流」と呼ぶのは過大評価だった。

#### C.2 ユーザによる重要な指摘 1: エントロピー誤用

「項目エントロピー H(j) って何だ。それって順序でもできるのか?」という
指摘。

確かに `H(j) = -Σ P(q) log P(q)` は順序を入れ替えても不変で，順序
情報を一切使わない。二値の H(j) = -(p log p + (1-p) log(1-p)) が正答率 p
の単調関数だったので，二値 BNM では「H 小 = 正答率極端 = 下流」
が偶然成立していたが，多値ではこの偶然は崩れる。 20260118 アイデアノート
5.3 節「方向は周辺エントロピー差で決まる」は
**二値限定の言明として訂正すべき**。

順序を尊重する情報量指標として:

- 累積エントロピー (Rao 2004): H̄(X) = -∫ F(x) log F(x) dx
- 累積残存エントロピー (Crescenzo & Longobardi 2002)

があるが心理計量で標準的でない。素直に項目平均/中央値の方が解釈
しやすい。

#### C.3 ユーザの発想 1: 順序 MDS で項目を空間化

「項目間の順序 MDS のような応用ができないか。距離構造を Chatterjee
相関で見て，距離の大小関係を維持する空間に入れる。非対称 MDS で von
Mises を使って」とのアイデア。

これに対し以下を整理して提示:

- Hill-climbing model (Borg & Groenen 2005 章 23): d(j → k) = \|\|x_j -
  x_k\|\| + α (h_k - h_j)
- Slide-Vector model (Zielman & Heiser 1996)
- Drift Vector model + von Mises 角度
- ノンメトリック MDS (Kruskal) の disparity

Hill-climbing の `h_j` を何にするかで `develop/20260501c.R` で実験:

- 平均: (Pearson 0.004, Spearman 0.016)
- 中央値: (0.031, 0.038)
- -H エントロピー: (-0.014, -0.048)
- drift = ξ 非対称行列の行平均: (-0.497, -0.431)

**重要発見**: 項目の周辺特性 (平均/中央値/-H) は ξ 非対称を**全く
説明しない**。drift だけが説明するが，これは ξ 非対称から定義したので
構成上当然 (循環論)。つまり「ξ が捉えている方向情報は周辺特性と
独立な構造」を示している。ξ 非対称性は項目の中心傾向や不確実性
では再現できない別物。

#### C.4 荘島先生 AMISESCAL モデルの読み込み

ユーザから <https://sh0j1ma.stars.ne.jp/ams/shojimaBSJ11.pdf> を共有。

**AMISESCAL = Asymmetric von Mises Scaling** (Kojiro Shojima, NCUEE
Research Division)

- 入力: 非対称 N×N 近接行列 G = {g_ij} (g_ij 小 = i が j
  を好ましく感じる)
- 各点 i に: 座標 x_i ∈ R^p, von Mises 平均方向 μ_i, 集中度 κ_i
- 縮尺率: π_ij = f(θ_ij \| μ_i, κ_i) (von Mises 密度値)
- モデル距離: ξ_ij = (1 - π_ij) δ_ij where δ_ij = \|\|x_i - x_j\|\|
- Stress: S(X, μ, κ) = Σ Σ (g_ij - ξ_ij)²
- 引用: Chino (1997), Mardia & Jupp (2000), Okada & Imaizumi (1987)

これは私が独立に提案した「Drift Vector + von Mises」と完全に一致する
モデル。荘島先生がすでに発表していた発想を再発見した形。

#### C.5 Abelson 電磁場モデル (小杉 2004) の歴史的位置づけ

ユーザから
<https://www.jstage.jst.go.jp/article/jbhmk/31/1/31_1_17/_pdf> も 共有。

**小杉考司・藤原武弘 (2004) 等高線マッピングによる態度布置モデル.
行動計量学 31(1) 17-24.**

Abelson (1954-55) Contour Map モデルの応用:

V(P) = Σ V(j) / (1 + d²\_{Pj})

二次元 MDS 空間 + 高さ次元 = スカラー場。電位アナロジー。論文末尾
で当時のユーザ自身が:

> 高度という第三の次元に「高いところから低いところへ力が加わる」
> といった力学的仮定を付与する十分な理論的背景がないのである。
> すなわち，本研究で扱ったモデルは，勾配ベクトル場ではなくスカラー場
> としてしか捉えられず，力動的な系として表現できない。

と限界を明記。AMISESCAL がこの限界を方向統計 (von Mises) で解決
した形になっている (22 年越しの発展)。

#### C.6 ユーザによる重要な指摘 2: 方向場と DAG は別物

「荘島モデル，Abelson モデル，いずれも DAG を作る話には向いてない。
向きがわかるだけで」というご指摘。

確かに:

- AMISESCAL は項目空間に方向場 (μ_j, κ_j) を出すが「j の親は具体的に
  誰か」を直接答えない。μ_j 方向の最近傍を子と見なす後付けルールが
  必要で，それは AMISESCAL 本来のモデル外
- DAG 構築には「ペア局所判定 + DAG 整合性」の二段構成が標準 (PC, GES,
  LiNGAM, NOTEARS)
- AMISESCAL/Abelson は最終出力の可視化・解釈補助に位置づける のが妥当

mdspace パッケージ的な空間モデルは BNM の DAG 学習エンジンでは
なく，事後の可視化器として理解しておく。

#### C.7 ユーザによる 3 段構えの提案 (確定)

    段 1  GGM (Glasso) で無向スケルトン           ← エッジ有無
    段 2  Chatterjee ξ の値の大小で方向決定       ← エッジ方向
    段 3  単調制約付き累積リンク等で条件付き分布   ← パス係数

ξ(j, k) \> ξ(k, j) なら j → k (大きいほうが親)。

各段の役割が綺麗に分離。段 1 (Glasso) と段 2 (chatterjee_matrix) は
v1.13.0 で実装済みなので，段 3 が次の主戦場。

#### C.8 段 3 の検討: 案 C と派生アイデア

**案 C (単調制約付きカテゴリ係数累積リンク)**:

P(Y ≤ q \| X = x) = Φ(τ_q - μ_x), μ_1 ≤ μ_2 ≤ … ≤ μ_Q

- X カテゴリごとに別の μ_x (X 連続化なし)
- 単調制約で順序情報尊重
- パラメータ数 2(Q-1) (Q=5 で 8)
- 親複数の加法モデル: τ_q - Σ_k μ\_{x_k}^{(k)} で Q×\|pa\| 線形
- 累積リンク (Q 個) と分割表 (Q² 個) の中間
- 加法性仮定で交互作用なし

**ユーザの派生アイデア: 「X_k で条件付けた Y\_{k+1}」**

D\_{X→Y}(k) = P(Y ≥ k+1 \| X = k)

X が k のとき Y が 1 段階上に行く確率を全 k で見る。GRM の閾値
対応のような発想。

これは案 C の対角隣セル `1 - Φ(τ_k - μ_k)` で，案 C のサブセット。
新しいモデルではなく案 C の補助プロファイル/可視化として位置づけ
られる。「+1 シフト」の恣意性 (なぜ +1, +2 や +0.5 はどうか)， 端点問題
(X = Q で Y\_{Q+1} は定義不能)，対角隣だけ見る情報損失， の懸念がある。

**結論**: パス係数は案 C を採用して β = μ_Q - μ_1 で取り，対角プロファ
イル D(k) は診断・可視化用に併用する。

順序 MDS との違いも明確化:

- 順序 MDS: 全項目を空間化，stress 最小化，項目に座標が付く
- 案 C: ペアの条件付き分布推定，対数尤度最大化，項目に座標は 付かない
  (Y\* は応答スケール内の潜在連続体)

#### C.9 議論の整理スライド作成

`develop/20260501_BNM_3steps.qmd` (Quarto reveal.js, 17 枚) を作成
してレンダリング。HTML 版は `develop/20260501_BNM_3steps.html`。
スライド構成:

1.  二値 BNM の前提
2.  多値で何が壊れるか
3.  全体像 (3 段構えの mermaid 図)
4.  段 1: GGM (Glasso)
5.  段 2: Chatterjee ξ
6.  段 2: 判定ルール
7.  段 3 の問題提起 (なぜ累積リンクではダメか)
8.  段 3: 案 C
9.  案 C の潜在変数イメージ
10. パラメータ数比較
11. 親複数の加法モデル
12. パス係数の取り出し方
13. 3 段の役割マップ (mermaid)
14. 残された設計判断
15. 補足: 順序 MDS との違い
16. まとめ

### D. 残された論点 (次セッション以降)

- **段 2 判定閾値**: 絶対値 / SE 正規化 / 経験的カットオフ。 xi_stable()
  の SE で正規化が一番ロバストの見立て
- **段 2 計算範囲**: スケルトン上のみ (高速) vs 全ペア (安全)
- **段 3 単調制約の実装**: 強制最適化 (差分 log 再パラメータ化) vs PAVA
  事後投影
- **段 3 加法性仮定**: 交互作用なしで OK か実データで検証
- **サイクル処理**: 最弱辺削除 vs 反転
- **パス係数の表現**: スカラー β / プロファイル D(k) / 両方を返す
- **BNM_GA / BNM_PBIL は 3 段構えで不要**: スコア探索が要らない
  (Glasso + ξ で構造が決まる)。既存の二値 BNM_GA / BNM_PBIL は
  二値専用として残すのが自然

### E. ユーザの思いつきメモ (将来役立つ可能性あり)

このセッションで発生したアイデアを後の参照用にまとめておく:

1.  **項目の順序 MDS + Chatterjee ξ**: 項目を ξ 距離で空間化し， 非対称
    MDS (von Mises) で方向場を出す。BNM の DAG 学習器
    ではないが，可視化・解釈補助に有効
2.  **AMISESCAL の応用**: 各項目に座標 x_j, 方向角 μ_j, 集中度 κ_j を
    割り当て，BNM 完成後に大局的な流れを確認する事後ツールとして
3.  **3 次元 Abelson 電磁場モデル**: ユーザ自身の 2004 年論文の発想
    (態度の認知次元 + 感情強度の場)。今回の DAG には不向きだが， 多値
    BNM の出力を「態度場」として可視化するアイデアの源泉に なりうる
4.  **GRM 並べでの両側閾値モデル**: X と Y の両方に GRM 風の閾値を
    割り当て，β\_{X,k} と β\_{Y,k+1} の対応で項目間関係を読む。これは 1
    次元 IRT の項目困難度プロファイルとして使えるが，BNM の親子
    関係とは別軸の話
5.  **「X_k で条件付けた Y\_{k+1}」プロファイル**: パス係数の補助
    可視化として，ペアごとに k = 1, …, Q-1 の折れ線で「動きやすさ」を
    見せる
6.  **対数尤度差の残差** (= delta_ll - n\*(H_X - H_Y)): MI 大ペアでは
    方向情報を含むかもしれない量。単独使用は不安定だが，ξ 非対称
    との組み合わせで補強できる可能性

### F. 作成・更新ファイル

- `DESCRIPTION` (1.12.1 → 1.13.0)
- `NEWS.md` (1.13.0 セクションに Documentation and CRAN check cleanup
  サブセクション追加)
- `WORKLOG.md` (本ログ)
- `R/22_GlassoUnit.R` (logarithm = TRUE, バックティック化, @param …)
- `R/23_Chatterjee.R` (バックティック化)
- `man/Glasso.Rd`, `man/chatterjee_matrix.Rd` (roxygen 再生成)
- `develop/20260501.R` (新規, 累積リンク基本実験)
- `develop/20260501b.R` (新規, MI 上位ペア比較)
- `develop/20260501c.R` (新規, Hill-climbing h 候補比較)
- `develop/20260501c_hill_h_candidates.pdf` (新規, 散布図)
- `develop/20260501_BNM_3steps.qmd` (新規, スライド)
- `develop/20260501_BNM_3steps.html` (新規, レンダリング結果)

### G. コミット

- `67ec9a1` chore: bump DESCRIPTION to 1.13.0 and record v1.13.0 worklog
- `620c11e` fix: clear R CMD check WARNINGs and NOTE introduced by
  v1.13.0

両方とも origin/main へ push 済み。620c11e の R-CMD-check 結果は push
後に走行開始 (本ログ作成時点では未確認)。

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
