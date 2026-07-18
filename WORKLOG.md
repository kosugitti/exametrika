# exametrika Work Log

Detailed development log. User-facing changes go in `NEWS.md`; this file
captures the per-session internal narrative (why a change was made, what
was investigated, what was ruled out). Entries are newest-first.

## 2026-07-02〜03 — v1.15.0: 第2次監査（横断4系統）→ 回帰テスト → API統一 → 提出前チェック一式

「他にまだゴミ・バグ・不統一がないか」という依頼で、前日の縦割り監査
（ファイル番号帯4分割）と切り口を変えた横断監査を4系統並行agentで実施:
API一貫性 / ドキュメント整合性 / 前日8コミットの回帰レビュー / 残ゴミ・テスト衛生。
全指摘は実測検証つき。主な発見: Biclustering系のmaxiter/max_iter混在
（...に吸われ無警告無視）、mic既定がordinal IRMだけTRUE、BNMだけplotなし、
DistractorAnalysisの0始まりカテゴリ脱落（前日修正の同型取りこぼし、実測319件消失）、
前日21件修正に回帰テストゼロ、GridSearch @examplesのgrid_serch誤記、
verbose @paramの記述逆転4系統、死にコード約120行、cat_labelXXX、
tarballへのRplots.pdf混入、ファイル名Biclucter誤記、git追跡の「.png」等。

### コミット構成（全push済）

1. `88c8f38` 前セッション未コミットのdataFormat分散ゼロ修正一式を救済コミット
2. `f9889e1` print系再インデント（空白のみ、diff -wで検証）
3. `d412b68` 回帰テスト21本（test-regression-1150.R）+ Alpha/Omega/Biserial
   スモークテスト（test-smoke-reliability.R）。テスト作成agentが
   build_conf_mat()のdata.frame conf拒否バグ（%in%が列単位比較）を追加発見
4. `fbb7ca4` 掃除: R/00_BiclucterUtils.R→00_BiclusterUtils.R改名、
   ルートの「.png」git rm、.RbuildignoreにRplots.pdf非アンカー版追加
5. `661c70d` API統一+バグ修正（NEWS「API consistency changes」節新設）:
   - verbose既定を全関数FALSEに統一（IRT/GRM/LCA/GridSearch/Biclustering系/
     IRM系/BNM_GA/BNM_PBILがTRUEだった）+ 記述逆転doc 4系統修正
   - Biclustering_IRMのmax_iter→maxiter改名（resolve_deprecated_max_iter()
     シムで旧名も警告つきで受理、v2.0.0で削除予定）
   - ordinal IRMのmic既定TRUE→FALSE・EM_limit 100→20（兄弟と統一）
   - BNM/LDLRA/LDB/BINET/GA系/IRM.binaryの引数順を(na,Z,w)に統一
   - Biclustering.ratedにconf_classをシグネチャ明示（...経由の隠し渡し解消）、
     rated既定method="R"をdocに明記
   - LDLRA beta=2,2は本家Module_LDLRA.wl確認で本家由来と確定→docに根拠明記
     （BNM/Biclusは本家も1,1。BNMのbeta0エイリアスも本家由来だった）
   - DistractorAnalysisのdistractor_coreでQ/CAをremap（0始まり/飛び番対応、
     モデル側FRPと整合）
   - build_conf_matにas.matrix(conf)を検証前に追加
   - grid_serch→GridSearch例修正、LDLRAに0/0時の明示エラー
     （beta_posterior_modeのNaN仕様をdocstring化、BNMはis.nan表示に依存
     するため戻り値表現は不変更）
   - print typo: Dimensionality Analyeis/Cummurative/Conditonal×2
6. `48d8ad6` LRA.ratedのminFreqRatioカテゴリ潰し機能を復活: 判定条件が
   度数と正答コードを比較しており一度も発火せず、発火すれば組立部で
   長さ不一致クラッシュという二重の死に機能だった。「頻度が閾値以上
   または正答なら残す」に修正、ICRPはcat_label（旧cat_labelXXX、位置対応
   のmatch引きに改良）で潰しバケットを「CatX」表示。既定0は挙動不変。
   合成データで単一/複数カテゴリプールを実測検証、回帰テスト追加
7. `31905c4` styler（新規テスト2ファイルの空白のみ）
8. `383567b` cran-comments.md 1.15.0化

### 提出前チェック（tools/build_pkg.R をステップ実行）

- **submit_cran()だけは実行していない**（スクリプト最終行に入っており、
  実行すると即提出＝クールダウン7/14明け前になるため。7/15に手動実行）
- local devtools::check(cran=TRUE): 0/0/0（5m22s）
- rhub v2: linux/macos-arm64/windows（全てR-devel）全green
- win-builder R-devel: 投入済み → **結果メール（kosugitti@gmail.com）の確認が残タスク**
- spell_check: 日本語ビネットの既知誤検知のみ
- 下流: dev版をローカルinstallして ggExametrika 611全パス（WARN1は2024年からの
  BINET Cautionで無関係）、shinyExametrika 183全パス
- GitHub Actions: R-CMD-check/pkgdown/test-coverage 全green
- フルスイート 5137テスト FAIL 0

### 保留（意図的に未対応）

- BNMのplotメソッドなし（やるならv1.16+の機能追加）
- qBiNormal/polychoric_likelihoodのR参照実装・コメントアウト旧コードは
  思い出として温存
- 直接テストなしエクスポート残り約26本（GA/PBIL系は意図的、他は間接被覆）
- 診断用C++（compare_gradients_grm等）のdev専用コード同梱（低優先）

### 次回（7/15前後）

1. win-builderの結果メール確認
2. `devtools::submit_cran()`（cran-comments.mdは更新済み）
3. CRAN受理後: git tag v1.15.0 → GitHub Release → Discussions告知（日英）

## 2026-07-01（続き） — v1.15.0: dataFormatの分散ゼロ項目検出漏れ修正（IRT() "object 'result' not found"）

class_statistics（先生の別プロジェクト、試験問題プールのIRT分析）で
`IRT(dat)`が「Error in IRT(dat) : object 'result' not found」で落ちる
という報告を受けて調査。

原因は`IRT()`内のEMのMステップで、項目の傾き初期値をNaN（分散ゼロ項目の
item-total correlationがNaNになるため）で`optim()`に渡すと、tryCatchで
100回リトライしても毎回失敗し、`result`が一度も代入されないまま
`result$value`を参照してクラッシュすること。再現データ（定数列を1本混ぜた
ダミー行列）で確認済み。

本家Mathematica実装（`develop/mtmk15forVer13/mod/Module_CTT.wl`の
`dataformat[]`）を確認したところ、`Total[uuu]`が`samplesize`（全員正解）
または`0`（全員不正解）の項目は、全ての分析の入口で無条件に除外される
仕様だった（wolframscriptで実データ相当のテストケースを実際に実行し確認
済み）。一方R版`dataFormat()`は同種の項目を`message()`で警告するだけで
除外しておらず、ここが本家との差分。

実際にclass_statisticsの`res_pool`（複数年度の試験回答を縦積みしたプール）
を`dataFormat()`に通して調べたところ、M0033（12人中12人正解）・M0048
（28人中28人正解）の2項目が該当。ただし既存の分散ゼロ検出（`sd.check`）
はこれを検出できていなかった。原因は`sd.check`が生の応答行列（欠測は`-1`
に埋められている）に対して`sd()`を計算していたため。回答者が少なく欠測
(-1)が大半を占める項目では、-1と実際の回答値が混在することで見かけ上
分散が生じてしまい、実際には回答者全員が同じ答えでも検出をすり抜けていた。

ユーザと相談の上、本家に倣う方針（除外して継続）に決定。対応:

- `R/01_dataFormat.R`: `sd.check`を欠測マスク（`Z == 1`）済みの有効回答
  のみで計算するよう修正。既存の「NA sd」除外ブロックと「分散ゼロ」警告
  のみブロックを1本に統合し、両方とも除外して継続する仕様に変更
  （メッセージは表示）。rated型の`CA`引数も項目除外に連動して詰め直す
  ように修正（従来は除外時にCAがずれる潜在バグがあった）。
- 「全員未回答」の項目（有効回答0件）は今まで通り除外せず警告のみ
  （本家とは意図的に異なる、既存の挙動を維持）。
- `tests/testthat/test-dataFormat.R`/`test-dataFormat-edge.R`:
  新しい除外仕様に合わせて3件更新、2件追加（全項目が定数の場合は
  エラーになること、一部項目が定数の場合はCAがずれずに除外されること）。
- フルテストスイート（4900件超）を実行し、FAIL 0を確認。
- `NEWS.md`の1.15.0 Bug fixesに追記。

この過程で、ユーザから「RStudioでテスト実行中に'No ID column detected'
という警告が大量に出る、前回直したのでは」という指摘もあったが調査の
結果、これは前回（同日）修正した`Biclustering.rated()`/`GridSearch()`
の**二重呼び出し**バグとは別物で、IDカラムを持たないテスト用行列を
意図的に使っているテスト（6ファイル・計26回）が仕様通りメッセージを
出しているだけと判明。`test-dataFormat-edge.R`はこの文言自体を
`expect_message()`でテストしている。バグの再発ではないため変更なし。

次: class_statistics側（M0033/M0048を含む再推定の実行）に対応。

## 2026-07-01（セッション締め） — v1.15.0: NEWS.md整形 + 寝かせ入り

最後に2点:

- NEWS.mdの1.15.0セクション（今回書いた分だけ）から`**太字**`強調を全除去
  （commit `1a1b90a`）。ユーザから「NEWS.mdに強調表示は使わないでほしい、
  生成AI使ったことが丸わかり」と指摘を受けた。各バグ項目の1文目を太字にする
  書き方は生成AIにありがちな定型パターンなので、以後NEWS.md含む配布物に
  書く文章は最初からプレーンに書く（memory `feedback_no_bold.md`に追記済み）。
  1.14.0以前の既存セクションは触っていない（既に公開済みの記録なので）。
- 今日一日で積んだv1.15.0の変更一式（列名対応・欠測データ系バグ21件・
  リファクタリング4件・巨大ファイル分割2件、計8コミット）はここで一旦
  「寝かせる」ことに決定。追加の変更が入るかもしれないため、git tag /
  GitHub Release / Discussions告知はまだ作らず、7/15の提出タイミングで
  まとめて行う方針（過去のv1.13.0/v1.14.0と同じ流儀）。コードは全て
  `origin/main`にpush済み・作業ツリークリーン。

### 次回セッションで最初にやること

- 追加の変更依頼があれば通常通り対応
- 変更が無ければ7/15前後で: cran-comments.md 1.15.0化 → R CMD check
  --as-cran → rhub/win-devel → git tag v1.15.0 → GitHub Release →
  Discussions告知(JA/EN) → CRAN提出

## 2026-07-01（さらにさらに続き） — v1.15.0: 巨大ファイル2本を分割

先の4件リファクタリングに続き、監査時に「今回は未着手」としていた最後の2件
（`R/02_TestItemFunctions.R`1748行と`R/00_exametrikaPrint.R`775行の分割）を
「進めていい」「他のも考えたいな」というやり取りの後、正式に依頼されて実施。

### `R/02_TestItemFunctions.R` → 3分割（commit `fd81c37`）

`InterItemAnalysis()`/`ItemStatistics()`/`StudentAnalysis()`が実際にどの
低レベル関数を呼んでいるかで自然な境界線を確認（grep実測）し、既存の
`02B_TestStatistics.R`/`02C_ItemStatistics.R`/`02D_StudentAnalysis.R`に続く
`02E`/`02F`/`02G`として:
- `02E_ItemAssociation.R`: JointSampleSize〜TetrachoricCorrelationMatrix +
  Dimensionality（相関行列の固有値分解なのでこちらに分類）
- `02F_ItemDifficulty.R`: crr〜ITBiserial
- `02G_StudentScore.R`: nrs〜stanine

`sed -n`で元ファイルから正確な行範囲を切り出す方式で実施（手打ちでの
書き写しミスを避けるため）。1回だけ、Dimensionalityセクション（ファイル末尾、
次のセクションによる境界がない）を`wc -l`の値1749ではなく最後の内容行1748で
区切ってしまい、閉じ`}`を1行落とすミスがあった → `devtools::load_all()`の
パースエラーで即座に検出、`git show HEAD:...`から正しい範囲を再取得して修正。
検証は「関数名の並び替えソート結果が移動前後で完全一致」「全5030テストFAIL 0」
の2段階。

### `R/00_exametrikaPrint.R` → 4分割+本体化（commit `8a39c62`）

`00_exametrikaPlot.R`が既に`plot_irt_model()`のような小関数を`00_plot_irt.R`
等に切り出し、本体はswitch()ディスパッチだけにする構造を採っていたので、
これをそのままprintにも適用。switch文の各`X = { ... }`ブロックの閉じ括弧
`^    },$`（4スペースインデント）をgrepで全27箇所特定し、本体側のインデント
（6スペース以上）と区別することで、sedでの正確な行範囲抽出を実現（前段の
TestItemFunctions分割での1行ズレ教訓を活かし、今回は最初から`grep -n "^    },$"`
で閉じ括弧を機械的に特定してから範囲を計算）:
- `00_print_ctt_irt.R`: TestStatistics〜GRM（10ケース）
- `00_print_lca_lra.R`: LCA〜LRArated（4ケース）
- `00_print_biclustering.R`: Biclustering〜IRM（5ケース）
- `00_print_network.R`: BNM〜BINET（4ケース）
- 本体`00_exametrikaPrint.R`はswitch()ディスパッチ＋小さい残り
  （ModelFit/Glasso/matrix/GridSearch/all）のみ、775行→124行に圧縮

検証は3段階: (1) 全`cat("...")`/`print(x$...)`呼び出しの集合が移動前後で
完全一致、(2) `git stash`で分割前後のコードを切り替えながらIRT/LCA/BNM/
TestStatistics/ItemStatistics/InterItemAnalysis/Dimensionalityの実際の
print()出力をファイルにキャプチャして`diff`で完全一致確認、(3) 全5030テスト
FAIL 0。

これで2026-07-01の全体監査で出た「改善提案」6件（`\r`統一・beta共通化・
conf共通化・GridSearch統合・TestItemFunctions分割・exametrikaPrint分割）が
全て完了。`.claude/CLAUDE.md`の将来課題リストからも全て除去済み。

## 2026-07-01（さらに続き） — v1.15.0: 監査で見つけた重複ロジックの4件をリファクタリング

直前の全コードベース監査で「改善提案（今回は未着手）」としていた6件のうち、
ユーザから「他のも考えたいな。進捗表示の規約違反とは？重複統合とは？」と
質問され、`\r`進捗表示と`GridSearch`分岐重複の具体例を提示。ユーザが「進めよう。
これら4つの修正だ」と、以下4件（巨大ファイル分割2件は対象外、リスクの低い
共通化4件）をまとめて実施することに決定:

1. **`\r`進捗表示を`\n`に統一**（8箇所: `00_EMclus.R`, `04C_ParameterEstimation.R`,
   `07_Biclustering.R`, `08C_BNM_GA.R`×2, `09B_LDLRA_GA.R`,
   `15_Biclustering_nominal.R`, `16_Biclustering_ordinal.R`）。単純な文字列置換、
   計算結果への影響なし。
2. **`beta_posterior_mode()`共通ヘルパー化**（`R/08A_BNM.R`に新設）。
   `(count+beta1-1)/(total+beta1+beta2-2)`という式がBNM/LD_param_est(LDLRA)/BINET
   で重複していたのを統合。BNMには`beta0<-beta2`という無駄な別名変数もあったので
   ついでに削除。LDBは分母にゼロ観測セル穴埋め処理（`denom0`トリック）が別途
   組み込まれており構造が異なるため対象外（既にbeta1修正済みで数値は正しい）。
3. **`build_conf_mat()`共通ヘルパー化**（`R/07_Biclustering.R`に新設）。
   `conf`(vector/matrix/data.frame)→`conf_mat`変換ロジックがBiclustering/
   .nominal/.ordinal/LDBで重複していたのを統合。LDBは`conf`必須（NULL不可）・
   他は`conf=NULL`で探索的モード、という既存の分岐差はヘルパー呼び出し側の
   `if(!is.null(conf))`ラッパー有無で温存（ヘルパー自体はNULLを渡すと
   `is.vector/is.matrix/is.data.frame`いずれにも該当せず自然にstopする設計）。
4. **`GridSearch()`のBiclustering/LCA分岐の重複統合**。全滅チェック
   (`stop_if_all_grid_failed`)・失敗設定警告(`report_failed_grid_settings`)・
   最適値選択(`select_optimal_grid_index`)の3ヘルパーに切り出し。最適値選択は
   `which(ret==target,arr.ind=TRUE)`が1次元ベクトルでも2次元行列でも動く性質
   （1次元だと`arr.ind`が無視されて素の位置ベクトルが返る）を利用して1つの
   関数で両分岐に対応。

各修正後に関連テスト、最後に全5030テストFAIL 0を確認（値は前回コミット時と
完全一致、GridSearchのoptimal_ncls/nfld等で数値比較して確認済み）。
NEWS.mdに「Internal (no user-visible behavior change)」節を新設して記録。

commit: `16ed16b`

### 残った改善提案（未着手のまま）

- `R/02_TestItemFunctions.R`（1748行）・`R/00_exametrikaPrint.R`（775行）の
  巨大ファイル分割は今回もスコープ外（今回の4件よりリスクが高いと判断）

## 2026-07-01（続き） — v1.15.0: 全コードベース監査（4系統並行）+ 21件のバグ修正

前段（BINET欠測クラッシュ修正）の後、ユーザから「パッケージ全体を見渡して改良点を
洗い出してほしい。明らかなバグや死にコードは直したいが、効率化案はまずプランとして
提案してほしい」という依頼。R/のファイル名の数字プレフィックス（00→23）が開発時系列
に対応しているという前提のもと、4系統の並行general-purpose agentで全R/ファイル
（約18,000行、src/のC++含む）を監査した。

### 監査体制

- 00番台agent: ユーティリティ群（plot/print/GridSearch/EMclus等15ファイル）
- 01-06番台agent: dataFormat/項目分析関数群/CTT/IRT/LCA/LRA
- 07-14番台agent: Biclustering/IRM/BNM/LDLRA/LDB/BINET/GRM（主要モデル群）
- 15-23番台agent: 多値Biclustering派生系 + Glasso/Chatterjee + src/のC++

各agentに「バグ確定/死にコード/設計の不整合/効率化の余地/改善提案」の5カテゴリで
報告させ、バグ・死にコード候補は必ず`devtools::load_all()`後に実測検証させた
（憶測での報告を禁止）。直前セッションのtmp剥がれ系バグ探索は既に別途完了済みと
伝え、重複探索を避けた。

結果、Tier1（実害大・9件）+ Tier2（実害限定的・12件）+ 死にコード（8件）の
合計29件相当の指摘が集まった。ユーザ判断で「全部今回直す」ことに決定。

### 発見のうち特に重かったもの

- **`R/02_TestItemFunctions.R`(1748行) longdataFormat**: 数値IDが1始まり連番で
  ないと`nrow(U)`が実際の学生数と食い違う重大バグ（ID={10,20,30}で3人なのに
  30行のUができる）。`w=`重み引数も項目IDを行番号として誤用する別バグ。
- **15/16/17/18番台のBiclustering/Biclustering_IRM**: 0始まりカテゴリコード
  （0,1,2,3）が無警告で脱落するバグ。自パッケージ同梱J15S3810（まさに0始まり）
  で3,854件の応答が消えていた。17/18番台はさらに欠測(-1)がRの負インデックス
  仕様でone-hot配列を汚染し、実測で7%のクラス割当が変わるバグも併発。
  GRM()には既に同種の対策（カテゴリ再割当）があったのでそれを`R/00_BiclucterUtils.R`
  の`remap_category_codes()`として切り出し共通化、15/16/17/18全てに適用。
  Uqの1の総数=非欠測セル数、という恒等式で修正を数値検証。
- **`R/00_exametrikaPrint.R`**: `print(GridSearch(...))`がswitch文の末尾カンマ
  で必ずクラッシュ（GridSearchは戻り値を返すだけの主要公開関数で対話利用では
  自動printされるため影響大）。`CCRR_tabje`という綴りミス2箇所で`digits=`引数
  も無効化されていた。IIAnalysis.ordinalではCSRが一度も表示されずJSRが二重表示。
- **`R/08A_BNM.R`/`R/11_BINET.R`**: `adj^i`がRでは要素ごとの累乗であり行列累乗
  にならないため、非巡回性・連結性チェックが実質プラシーボだった。BINETの
  「Your graph is not a DAG」エラーは巡回グラフに対して発火しない状態。
  `igraph::is_dag()`/`is_connected(mode="weak")`に置き換えて解決（igraphは
  既存の依存パッケージ）。
- **`R/10_LDB.R`**: `conf`をmatrix/data.frameで渡すとクラッシュ（4/27に
  Biclustering.ordinal/nominalで直した同型バグの直し忘れ）。beta1/beta2の
  分子役割もBNM/LDLRA/BINETと逆（本家Mathematica `Module_LDB.nb`のソースを
  確認したところ本家自体が`beta2`を使っており、移植バグでなく原典の記述ゆれと
  判明。数学的にはBeta(α,β)の慣習でα=成功側=beta1が正しいので、パッケージ内
  一貫性を優先してbeta1に統一）。

### 修正しなかったもの（設計改善プランとして提案のみ）

- `R/02_TestItemFunctions.R`(1748行)/`R/00_exametrikaPrint.R`(775行)の巨大
  ファイル分割
- `conf`/`conf_mat`解析、beta事後平均式の共通ヘルパー化（LDBの件で一度顕在化
  したパターン）
- GridSearchのBiclustering/LCA分岐の重複統合
- `\r`進捗表示の規約違反（CLAUDE.mdは`\n`推奨）の全体清掃

### コミット構成

監査バッチと対応させて4コミットに分割:
1. `143c35a` 00番台（print/plot/GridSearch）
2. `c01a2d2` 01-06番台（dataFormat/longdataFormat/項目分析関数）
3. `7a7d891` 07-14番台（BNM/LDLRA/LDB/BINETのDAGチェック・ラベリング・死にコード）
4. `5c1b09f` 15-23番台（カテゴリコード処理・Glasso edge_tol）+ NEWS.md

各バッチ後に該当テストファイルで検証、最終コミット後に全5030テストFAIL 0を確認
（開始時点の4926から増加＝一部テストが従来クラッシュで途中終了していた分を含む）。

### 次回への引き継ぎ

- cran-comments.md 1.15.0化・R CMD check --as-cran・rhub/win-devel実行が
  7/15提出前に必要（バグ修正で内容が大きく増えたので再確認強めに）
- 上記「修正しなかったもの」は次のクリーンアップ機会に

## 2026-07-01 — v1.15.0: dataFormat列名対応 + 欠測データ系バグ一斉修正（BINET含む）

きっかけは「dataFormatのid列は列番号でしか指定できない」という一言の指摘。
そこから列名対応 → 隣接コードのバグ探索 → 全モデル関数の並行監査、と芋づる式に
広がり、最終的に2コミットでv1.15.0（7/15提出予定）分の修正が積み上がった。

### コミット1: `2bf3857` — dataFormat列名対応 + tmp剥がれ系バグ一式

**列名対応**
- `dataFormat()`の`id`引数が数値の列番号しか受け付けず，文字列を渡すと即
  `stop()`していた。列名（文字列）でも指定できるように修正。存在しない列名・
  複数マッチはエラーで明示。
- `longdataFormat()`のSid/Qid/Respは元々R側の`[[`/`[,]`が汎用的なので列名でも
  動いてはいたが，Sid/Qid/Resp/w全てに列存在チェック・エラーメッセージを追加して
  dataFormatと足並みを揃えた。
- ついでに発見: `longdataFormat()`は「1学生が複数項目に回答する」通常のロング
  形式データで必ず「Duplicated IDs found」を誤検出していた（生のSid列に対して
  重複チェックしていたのが原因、ロング形式ではSidが複数行に渡って重複するのが
  正常）。(Sid, Qid)ペアの重複チェックに変更。

**tmp剥がれ系バグ（本セッションの主戦場）**
`IRT()`で「`U <- tmp$U * tmp$Z`でexametrikaクラスを剥がし，欠測(-1)を0に
潰した後，その`U`を`ItemTotalCorr()`/`ItemThreshold()`に渡している」というバグを
発見。これらの関数は`UseMethod`ディスパッチで，`inherits(U,"exametrika")`が
falseだと`na`/`Z`/`w`なしで`dataFormat()`を再実行してしまうため，欠測が
「誤答」として再解釈される。実測（20名5項目・欠測2件）で`tau`/`rho`が実際に
ずれることを確認。

この型のバグを全ファイルで洗い出すため，4系統の並行Explore/general-purpose
agentを立てて監査（IRT/GRM系，LCA/LRA系，Biclustering/IRM系，BNM/LDLRA/LDB/
BINET系）。結果，同型のバグが`crr(U)`という形で5箇所に伝播していたことが判明:
`Biclustering.binary`の`FieldAnalysis$CRR`，`BNM`/`LDLRA`/`LDB`/`BINET`の
`$crr`出力フィールド。いずれも推定計算本体（EM/Gibbs, IRP/FRP, CCRR_table等）
は`tmp$U`/`tmp$Z`を直接使っており無事，汚染されるのは`$crr`フィールドのみ
（BNMは`print.BNM`のDAG描画ノード縦位置ソートにも影響）。全て`crr(tmp)`に
差し替えて解決。

隣接して，`CTT()`/`BNM()`/`LDLRA()`/`LDB()`/`BINET()`で「`if (U$response.type
!= "binary")`」のように**exametrikaクラスに変換する前の生引数**をチェックして
いるバグも発見。生のdata.frame/matrixを直接渡すと`U$response.type`がNULLになり
即クラッシュ（`tmp$response.type`に統一）。`CTT()`だけは`inherits()`の条件が
そもそも反転していて，生データを渡すと`dataFormat()`自体をスキップして即死する
より重いバグだった。

副産物として，同じ「`U <- tmp$U * tmp$Z`だが以降未使用」という死んだコードが
`LRA.binary`/`BNM_GA`/`BNM_PBIL`/`LD_param_est`/`LDLRA_PBIL`の5箇所に見つかり
削除（実害なし，可読性のみ）。

全4926テストがFAIL 0で通ることを確認してコミット・プッシュ。

### コミット2: `8e186e0` — BINET()の欠測データクラッシュ修正

コミット1のBINET監査中，ユーザから「BINETが欠測データでクラッシュする件も
今回合わせて直す」と指示。原因は2つの複合:

1. `Ccj`（クラス別正答カウント）が`t(clsmemb) %*% tmp$U`と，欠測(-1)を
   Zマスクせずそのまま行列積に混入させていた。対になる`Fcj`（誤答カウント）は
   `tmp$Z * (1 - tmp$U)`と正しくマスクしているのに`Ccj`だけ漏れていた。
   欠測が多いと`Ccj`が負値になり，条件付き正答率`Pcf`/`pap`/`chp`が破綻。
2. 平滑化定数`gamp <- 1`（Beta(1,1)事前分布のMAP式）がハードコードで引数化
   されておらず，あるクラス×フィールドの組み合わせで非欠測観測数がゼロになると
   0/0でNaNになる。姉妹関数のBNM/LDLRA/LDBには同じ役割の`beta1`/`beta2`引数が
   あるのにBINETだけ無かった。

ユーザが本家Mathematica実装（`estbinet[d0_, ai0_, gp0_]`）のソースを提示して
くれて，(2)は本家も`estbinet[uuu, structmat, 1]`とgamma=1固定で呼んでいる
ことが判明＝移植バグではなく本家から継承した構造的弱点。一方(1)は本家の
`Ccj = Transpose[clsmemb] . dat`が`dat`側で欠測を0に前処理済みという前提に
依存しており，R側は`tmp$U`が欠測を`-1`で持つ設計なので前提が合わずR特有の
移植バグと判断。

対処: `Ccj`/`irp`の計算を`tmp$Z * tmp$U`でマスク，`BINET()`に`beta1 = 1,
beta2 = 1`引数を追加（既定値は現状維持で非破壊），それでも0/0になる場合は
謎の`'dimnames'の長さ...`エラーではなく「beta1/beta2を上げてください」という
分かりやすいエラーに変更。欠測30件注入のトイデータで再現・修正確認，
Mathematica参照値を使った既存test-binet.R（40件）・全4926テストとも
FAIL 0を確認してコミット・プッシュ。

### バージョニング

ユーザ判断で，今回の修正一式をv1.15.0として2026-07-15にCRAN提出する方針に
確定（前回1.14.0受理2026-06-14からの1ヶ月クールダウンにちょうど収まる）。
NEWS.mdの`(development version)`見出しを`1.15.0`に確定し，DESCRIPTIONの
Versionも`1.14.0.9000`→`1.15.0`にbump（このタイミングでbumpするのは1.13.0の
時と同じ前例）。cran-comments.mdは提出直前に作業する慣例のため今回は未着手。

### 次回への引き継ぎ

- cran-comments.md 1.15.0化，R CMD check --as-cran / rhub / win-devel 実行は
  7/15提出前に必要
- 今回のtmp剥がれ監査は「危険な関数リスト」（ItemTotalCorr/ItemThreshold/crr/
  JointSampleSize/JCRR/CCRR/ItemLift/MutualInformation/PhiCoefficient/
  PolychoricCorrelationMatrix/TetrachoricCorrelationMatrix/ItemOdds/
  ItemEntropy/ITBiserial/nrs/passage/sscore/percentile/stanine/
  Dimensionality/TestStatistics/ItemStatistics/InterItemAnalysis/CTT）
  への呼び出し箇所を対象にしたもので，パッケージ全体を網羅した形跡は
  ある（4系統agentで03〜23番のRファイルほぼ全て担当済み）が，将来新しい
  モデル関数を足すときは同じ「tmp剥がれ→UseMethod関数に渡す」パターンを
  混入させないよう要注意（`.claude/CLAUDE.md`にコーディング規約として
  一文残す価値あり）

## 2026-06-15 — v1.14.0 CRAN 提出・受理・リリース一式

v1.14.0 を CRAN に提出し，同日中に受理・公開された。前回 1.13.0 を落とした
Windows checktime の壁を今回はクリアし，auto-check 一発通過からそのまま
公開まで通った。

### 提出前チェック
- `R CMD check --as-cran`: 当初 1 NOTE（top-level に `Rplots.pdf`）。6/8 の
  プロット作業中にデフォルトデバイスが生成した残骸が tarball に混入していた。
  **対処**: 実ファイル削除 + `.Rbuildignore` に `^Rplots\.pdf$` を追加（.gitignore
  だけでは R CMD build が見ないため不十分＝根本対処は .Rbuildignore）。再チェックで
  0/0/0。
- win-devel: Status OK, check time 276s（10 分制限内）。
- rhub v2: linux / macos-arm64 / windows 全 green（13 分）。
- `cran-comments.md` を 1.14.0 用に全面書き換え（plot `...` 伝播 / Glasso 発散対応 /
  二値 IRM 欠測 / typo 群，Windows checktime が skip_on_cran で収まる旨も明記）。

### 提出と公開
- commit `4bd5ebb`（cran-comments・.Rbuildignore・WORKLOG・styler 整形）から提出。
  styler が 07_IRM.R / 22_GlassoUnit.R / test-irm.R のコメント空白を整形（コスメ）。
- `devtools::submit_cran()` → auto-check 両 flavor Result: OK → CRAN 公開
  （Date/Publication 2026-06-14 23:40:02 UTC）。
- `CRAN-SUBMISSION`（SHA 4bd5ebb）を記録（commit `02a5ae2`）。同コミットで
  `tools/build_pkg.R` 最終行を `use_release_issue()` → `submit_cran()` に修正
  （use_release_issue はリリース開始時にチェックリスト issue を作るコマンドで，
  提出ステップではない）。

### 受理後の公開作業
- git tag `v1.14.0`（注釈付き，push 済み）。
- GitHub Release 公開・latest 昇格。
- Discussions 2 本: JA #33 / EN #34（Announcements）。
- `usethis::use_dev_version()` → DESCRIPTION 1.14.0.9000 + NEWS dev 見出し
  （非対話 Rscript は commit しないので手動 commit `b716384`）。
- release issue #28 を全項目反映の上クローズ。未チェックで残したのは
  urlchecker / build_readme / revdepcheck / blog post（バグ修正主体の release の
  ため意図的にスキップ）。

### 付随した意思決定
- **DESCRIPTION Title 変更を決定**（次リリース適用）: `Test Theory Analysis and
  Biclustering` → `Test Data Engineering`。book(Shojima 2022)・R Journal 論文
  （2025-66, "exametrika: Test Data Engineering with R"）と語を揃える。Biclustering
  だけ名指しする粒度の不揃いを解消。1.14.0 は提出済みのため変更不可，**issue #32**
  で次リリースに向け追跡。Title 規約（title case / no period / no pkg name・"R"）も
  クリア。
- **ggExametrika 1.1.1** は exametrika 1.14.0 CRAN 着地により提出可能に。中身は
  完成済み（Clusterd→Clustered 破壊的リネーム他）。styler を回したが 35 ファイル
  全 unchanged（既に準拠）。未コミット 5 ファイル（NAMESPACE 再生成等）の整理 →
  check → 提出が次の作業。

### 次への引き継ぎ
- ggExametrika 1.1.1 CRAN 提出（依存チェーン順で exametrika の後）。
- shinyExametrika の plotArray_gg 破壊的リネーム追従確認。
- R Journal 編集長 Emi Tanaka へ 1.14.0 CRAN 受理報告 + タイトル変更承認依頼。
- 次リリースで DESCRIPTION Title 変更（issue #32）。

## 2026-06-08 — plot.exametrika() の `...` 伝播修正 (R Journal 査読対応, v1.14.0 目玉)

R Journal (2025-66) の編集長 Emi Tanaka から 6/8 にフォローアップ。査読者の
指摘は1点のみ: 「`plot.exametrika()` の help には `...` で下層プロット関数へ
引数を渡せると書いてあるが実際には効かない。`plot(result.LRA, type="IRP",
items=1:4, nc=2, nr=2, las=2, pch=16)` で `las` も `pch` も変わらない。原因は
`plot_irt_model()`/`plot_grm_model()`/`plot_common_profiles()` 等が引数を
受け取らず伝播もしないこと」。前回 revision2 では「軽量設計だから ggExametrika
で対応」と"言い訳"で返していたが今回は実修正する方針に。

### 調査で確定した事実
- ディスパッチャ `plot.exametrika()` は `...` を捕捉するが，内部13関数の
  シグネチャに `...` が無く一切渡していなかった（査読者の診断は完全に正確）。
- `las` が効かない真因: 多くのプロットが `plot(..., xaxt="n")` で軸を消して
  `axis(1, ...)` で**手描き**しており，`las` は plot() ではなくこの手描き
  `axis()` に渡さないと効かない（IRP がまさにこれ）。
- 検証(`warn=2`): base 描画関数 plot/barplot/image/axis/lines は余計な
  グラフィカル引数を渡してもエラーも警告も出さない → フィルタ不要で全 dots を
  そのまま流せる。axis に pch を渡しても無警告。
- `curve()` は第一引数が非標準評価 + ローカル変数(a,b,c,d)参照のため do.call
  での合流が環境解決で詰む → グリッド評価に置換。IRT モデル関数は theta
  ベクトルでベクトル化済みを実機確認。

### 実装
- 共有ヘルパー3つを `R/00_plot_biclustering.R` に追加:
  `merge_plot_dots(defaults, dots)`(modifyList でユーザ優先合流),
  `call_plot(.fun, defaults, dots)`(do.call), `draw_curve(fn,...)`(グリッド評価).
  注: `graphics::plot` は存在しない(plot ジェネリックは base へ移動)ので
  call_plot には素の `plot` を渡す。
- 内部13関数すべてに `dots = list()` 引数を追加し，各 plot/barplot/image/
  lines/axis 呼び出しを call_plot 経由に。手描き axis にも dots 転送。
- 副次: IRT TIF タイトル typo "Test Informaiton" -> "Test Information".
- `tests/testthat/test-plot-dots.R` 新規27件 (merge/call_plot/draw_curve の
  機構 + LRA/IRT/GRM/Biclustering 統合, モデル fit する統合テストは
  skip_on_cran)。call_plot スパイで las/pch が3関数の下層描画まで到達する
  ことを実証 (las reached:TRUE / pch reached:TRUE)。

### 結果・コミット
- 全 4926 テスト PASS / 0 FAIL / 0 WARN。
- commit `71171e9` を main に push。NEWS.md の v1.14.0 先頭に `## Improvements`
  として記載。`man/plot.exametrika.Rd` の `@param ...` を詳細化。
- **罠**: ローカル roxygen2 が 8.0.0 で `roxygenise()` が DESCRIPTION の
  `RoxygenNote: 7.3.3` を `Config/roxygen2/version: 8.0.0` に書き換え +
  Biclustering.Rd を再整形してしまう。`devtools::test()` でも誘発。該当2
  ファイルは `git checkout` で都度戻す（plot.exametrika.Rd は意味的差分のみで
  クリーンなので保持）。
- CRAN は cadence 通り 6/15 以降に 1.14.0 を提出予定（前回受理 5/18 から）。

## 2026-06-02 (PM) — exametrika 一家 ユーザ可視文字列 typo 総点検

`Clusterd` -> `Clustered` の修正がきっかけで，殿から「人様の目に触れる
plot/message/print/cat 等の出力に他にも typo がないか総点検せよ」と指示。
exametrika/ggExametrika/shinyExametrika の3プロジェクトに Explore エージェント
を割り当てて並列実行。

### 検出と修正 (exametrika 4 件)

ファイル: 行 / 該当文字列 / 修正内容

- `R/04C_ParameterEstimation.R:337` — IRT slope warning
  - `"... exceeds 10.Please exercise caution ..."` (ピリオド後のスペース欠落)
  - → `"... exceeds 10. Please exercise caution ..."`
- `R/02_TestItemFunctions.R:251` — `CCRR.nominal()` の情報メッセージ
  - `"CCRR is for binary data only. Conditional Selection Rate for your polytomous data instead."` (動詞 "Using" 欠落)
  - → `"... Using Conditional Selection Rate for your polytomous data instead."`
  - L88 (ordinal), L115 (rated), L172 (binary) の parallel メッセージは全て
    "Using" を含んでおり L251 のみが欠落していた
- `R/09_LDLRA.R:36`, `R/10_LDB.R:251` — max-parents warning
  - `"[Caution!] The maximum number of parents per item is N, Please check."` (カンマで継いで Please)
  - → `"... is N. Please check."`

### 検出結果 (他プロジェクト)

- **ggExametrika**: typo なし。`Clusterd` 系は v1.1.1 で既に直しており，
  R/*.R, man/*.Rd, vignettes/*.Rmd, NEWS.md, README.md すべてクリーン
- **shinyExametrika**: typo なし。ただし `inst/i18n/translation.json` の
  日本語訳で「英字 と 日本語 の間のスペース」が GRM/BNM/LDLRA の 6 行だけ
  欠落 (CTT/IRT/LCA/LRA/Biclustering/IRM など他 8 行はスペースあり) →
  shinyExametrika 側で修正

### 影響範囲

すべて文字列の体裁修正のみで挙動変更なし。回帰テスト再生成不要。NEWS.md
1.14.0 に "User-facing message typos and missing-word fixes" として記載。

### 専門用語の low 確信度候補

各エージェントの low セクションは空 (biclustering/ranklustering/IRM/LDLRA
等の造語は除外指示済み)。確認を仰ぐ案件なし。

## 2026-06-02 — v1.14.0 Biclustering plot のtypo修正 (Clusterd -> Clustered)

殿（プロジェクトオーナー）から ggExametrika 経由で「ggArrayプロットに
Clustered Data とすべきところが Clusterd Data になっている」との指摘。
ggExametrika の引数名 (`Clusterd*`) を辿ると本家 exametrika 内部
(`R/00_plot_biclustering.R`) にも同種のtypoが存在していた。

### 修正対象

`R/00_plot_biclustering.R` のみ:

- L151 コメント: `## Clusterd Plot` -> `## Clustered Plot`
- L156 panel title: `main = "Clusterd Data"` -> `"Clustered Data"`
- L81, L160 内部変数: `clusterd_data` -> `clustered_data`

### 影響範囲

引数名にはtypoなし。`plot.exametrika()` の Biclustering 経路は表示文字列
のみが変わるユーザ可視変更で，API 影響はゼロ。Mathematica reference fixture
には main title 文字列は含まれないので回帰テストの再生成は不要。

### 連動修正 (ggExametrika 1.1.1)

ggExametrika 側 `plotArray_gg()` は本家を移植した際に引数化された段で
typoが伝染しており，こちらは引数名 (`Clusterd` / `Clusterd_lines` /
`Clusterd_lines_color`) も含む破壊的変更。v1.14.0 リリースと同時に
ggExametrika 1.1.1 を出す方針 (殿確認済)。

### 次にやること

- v1.14.0 リリース時にこの修正を含めて CRAN 提出 (6/15 目標)
- 連動して ggExametrika 1.1.1 で `roxygen2::roxygenise()` + pkgdown 再ビルド
  + R CMD check 後に CRAN 提出

## 2026-05-27 (午後) — v1.14.0 Bug #2 IRM binary 欠測 NaN 修正

A3 論文 (polytomous_biclustering) の 4.4 節 実データ適用作業中に発見した
バグの調査と修正。

### 発見の経緯

論文 4.4 用に HCI (Homeostasis Concept Inventory, 651×20 4肢択一) と
SAT12 (mirt, 600×32 5肢択一) を二値化して IRM にかけたところ，両者
で同じエラー:

```
log(CcfPlus[i, j] + gamp - 1 - s + const) で:
  計算結果が NaN になりました
rmultinom(1, 1, ptab) でエラー:
  確率ベクトル中にNAがあります
```

シミュレーションでは binary IRM の収束率が 93.7% と他データ型 (96-98%) より
低かったのもおそらく同根 (Phase3 シミュは欠測なしだったので顕在化せず)。

### 真因

`Biclustering_IRM.binary` (R/07_IRM.R) は `tmp$U %*% fld01` で `Cif` を
集計するが，`dataFormat()` は欠測セルを `tmp$U` に **-1** として残し
別途 `tmp$Z` で観測マスクを持つ設計。`tmp$U` をマスク前のまま積算すると
-1 が伝播して `CcfPlus` が負 → `log()` で NaN → `ptab` 全体が NaN →
`rmultinom` クラッシュ。

line 120 に `U <- tmp$U * tmp$Z` という**未使用**変数があった (代入先が
誰にも参照されないデッドコード)。リファクタの取り残しと思われる。

### 修正

`R/07_IRM.R` 2行修正:

1. line 120: `U <- tmp$U * tmp$Z` → `tmp$U <- tmp$U * tmp$Z`
   下流の `tmp$U %*% fld01` 系 (line 165, 204, 324, 339) 全てが Z マスク済み
   値で計算されるようになる。
2. line 610: `U = U` → `U = tmp$U`
   旧コードは line 120 のデッド代入が function parameter `U` を上書き
   していた偶然に依存して `U = masked_matrix` を返していた。新コードは
   `tmp$U` から明示的に取る (これがないと `plot_array` が dataFormat
   オブジェクトを matrix と誤解して「次元不正」エラーになる; R CMD check
   examples で発覚)。

### テスト

`tests/testthat/test-irm.R` 末尾に Bug #2 regression test を追加。
500×20 二値データに 2% の欠測 (-1) を入れ，`Biclustering_IRM` が
クラッシュせず通常の `exametrika`/`IRM` クラスのオブジェクトを返す
ことを検証。`skip_on_cran()` 付き (推定に数秒かかるため)。

### Bug #3 (CFI clip による benchmark inversion 隠蔽) は見送り

同じ作業の副産物として `calcFitIndices` の `pmax(., 0)` クリップが
`chi_A < 0` の異常を `CFI = 1.0` として隠蔽する挙動を発見 (bfi binary
で再現)。これは Bentler-Bonett の伝統的仕様であり Mathematica reference
も同じ挙動。仕様逸脱 (NA + warning 化) を一度実装してテスト通過まで
持っていったが，

- Mathematica reference と CSV fixture が CFI=1.0 / RMSEA=0 を出している
- 「コードは正せず Mathematica .nb も同期する」と「クランプは設計と認める」
  の判断が必要

ため，先生判断で v1.14.0 では見送り。`develop/20260527_bug_reproduce.R`
に再現コードを残し，将来 (v1.15+) で Mathematica 同期込みで再検討する
材料として保留。

### 検証

- `devtools::test()`: 4899 PASS / 0 FAIL / 0 WARN / 0 SKIP
- `R CMD check --as-cran`: 0 ERROR / 0 WARNING / 1 NOTE
  (NOTE は HTML Tidy 環境依存; CRAN サーバでは出ない)
- `R/07_IRM.R` の plot サンプル (Biclustering_IRM(J35S515)) が
  `plot(result, type="Array")` まで完走することを確認

## 2026-05-27 — PR #30 (socialistic.ai 誘導 PR) 拒否・周辺ハウスキーピング

コードには触らず，GitHub 上の宛先処理だけのセッション。

### PR #30 / issue #29 — community-hosted リンク追加 PR を close

shesl-tinkerland (shesonglin) から README に「Try it online
(community-hosted)」セクションを追加する PR が到着 (5/26 14:19 UTC)。
リンク先は `socialistic.ai/skill/exametrika-test-analysis-ebe9d8` で
`utm_source=github&utm_medium=readme&utm_campaign=exametrika` 付き。

中身を吟味した結果，以下の理由で **マージ拒否 → close** を判断:

1. **socialistic.ai は exametrika を走らせていない** — LLM ベースの
   skill marketplace で，ブラウザ上で Rcpp の GRM optimizer や IRM
   Gibbs sampler を再現できるわけがない。CSV アップで「IRT/潜在ランク
   /学習経路/Biclustering/TDE レポート」が出ると謳っているが，実体は
   LLM が exametrika 風の出力を捏造している可能性が極めて高い
2. **ゼミ生・同僚への誤誘導リスク** — 「これが exametrika の出力です」
   とハルシネーション結果を信じさせると reputational damage。 5/23 に
   ラマヌジャン bot のハルシ対策を入れたばかりの立場で，同じ落とし穴の
   サービスを公式 README から推奨するのは矛盾
3. **README 同期ルール違反** — PR 自体が `README.md` 直編集 (CLAUDE.md
   で禁止)。PR 本人も「R toolchain 入れてないので手で sync した」と
   告白
4. **OSS を利用した UTM トラフィック誘導** — 「community-hosted」「not
   affiliated」と書いてリスクを著者に押し付けつつバッジで集客する手口

対応:

- PR #30: 1, 3 を理由として丁寧に説明するコメント投稿 → close
- issue #29: 元の「面白い取り組みですね！」コメントを撤回するのは
  気まずいが，PR コメントを参照する短い follow-up を投稿 → close

両方 close 済み。文面は受け入れの可能性を残さない一方で，研究室外で
個人として使う分には問題ないことを明示。

### branch protection バナー — dismiss

GitHub のリポジトリトップに出ていた「Your main branch isn't protected」
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

### issue #28 (Release exametrika 1.14.0) の出自確認

「これ何で立ってる？」と質問あり。調査結果:

- 作成日時 2026-05-17 20:52 = v1.13.0 を CRAN に提出した直後
- 中身は `usethis::use_release_issue()` の標準テンプレート (git pull
  → NEWS polish → win-devel → revdepcheck → submit → tag → blog →
  social の checkbox 列)
- `devtools::release()` フロー or 直接 `usethis::use_release_issue('minor')`
  が次の minor として 1.14.0 を提案して自動作成

今 ちょうど進めている v1.14.0 (Glasso graceful divergence handling，
ターゲット 6/15 CRAN 提出) の release tracking issue として **そのまま
使える** ことを確認。close せず残置。

### 今回得た知見

- **LLM-marketplace 系の README リンク追加 PR は要警戒**。UTM 付き
  バッジ + 「community-hosted/not affiliated」の組み合わせは典型的な
  OSS スパムパターン。memory に拒否方針を記録
- **`usethis::use_release_issue()` は次バージョンの checklist を
  自動生成する** — 提出時に立つので「これ俺立てたっけ？」と混乱
  しがち。memory 化

---

## 2026-05-24 — v1.14.0 着手 (Glasso 数値発散 graceful handling)

索引から転記: CLAUDE.md A4+C1 ステータス欄に記録されていた内容。

### 背景

お遍路さんデータ (N=143, p=98) で全項目一括 Glasso を実行したところ，
`glasso_one()` 内部の BCD ループで `diff` が NaN になりイテレーションが
停止せず，最終的に NaN だらけの精度行列が返ってきてエラー。

### 修正 (A+B)

- **修正 A** — `glasso_one()` に `converged` フラグを追加。`beta`/`diff`/`W`
  のいずれかに non-finite 値を検出した時点で `converged = FALSE` を立て
  ループを抜ける (発散の graceful detection)。
- **修正 B** — `Glasso()` の lambda ループで diverged フラグを参照し，
  warning を出して early break + 収束直前の best 解を fallback 返却。
  path 内の該当 lambda は NA のまま残す (caller が識別可能)。

### ファイル

- `R/22_GlassoUnit.R`: `glasso_one()` に diverged フラグ追加，`Glasso()` に
  warning + early break + best 解 fallback
- `tests/testthat/test-glasso.R`: 発散シナリオ 4 件追加
- `DESCRIPTION`: Version 1.13.1 → 1.14.0
- `NEWS.md`: v1.14.0 セクション追加

### 結果

全 4894 テスト PASS。`R CMD check --as-cran` 0/0/0。

---

## 2026-05-23 — v1.13.1 後フォローアップの取りこぼし確認

5/20 セッションで Discussions follow-up と smoke test を実施した記録が
WORKLOG にあるにも関わらず，`.claude/CLAUDE.md` の TODO リストが
`[ ]` のままだった (前回セッションが TODO 更新を忘れていた)。
別マシン (Mac mini 側) から「メール来たぞ」の連絡があり，
GitHub API で 5/19 08:24 UTC に kosugitti アカウントで follow-up 投稿
されていることを確認 → TODO `[x]` に更新。

ついでに smoke test を再確認:

- **ggExametrika** `devtools::test()`: 611/611 PASS, WARN 1 (既知の
  stanine 9 分割警告で 1.13.1 無関係), SKIP 0
- **shinyExametrika** `devtools::test()`: 67/67 PASS, WARN 0 (前回 5/20
  は PR#14 マージ作業だけで smoke test 単独の結果は WORKLOG に未記載
  だったので，1.13.1 と shinyExametrika の組み合わせの初回正式確認)

`.claude/CLAUDE.md` の「優先度：高（v1.13.1 受理後のフォローアップ）」
3 件全て `[x]` クローズ。次は v2.0.0 (StepBNM / 案 C) 着手フェーズ。
CRAN cadence で提出は 6/18 以降。

### 教訓

セッション終了時に WORKLOG.md と `.claude/CLAUDE.md` の TODO の同期を
忘れると，後続セッションで「やったのか，やってないのか」の二度手間が
発生する。「ログ書いておいて」ルーチンの 2. (CLAUDE.md 更新) には
**TODO リストのチェック更新も含む**ことを明示しておく方が安全。

---

## 2026-05-20 — v1.13.1 受理後の周辺整備（Discussions / smoke test / sv1+sv2 展開）

5/18 v1.13.1 CRAN 受理を受けて，残っていた周辺タスクを片付けるセッション。

### Discussions #26/#27 への follow-up

v1.13.0 リリース告知 (日本語 #26 / 英語 #27, 5/17 投稿) には CRAN 反映の
顛末がまだ付いていなかったので，1.13.1 受理を短くまとめてコメント追加:

- #26 (JP): <https://github.com/kosugitti/exametrika/discussions/26#discussioncomment-16972892>
- #27 (EN): <https://github.com/kosugitti/exametrika/discussions/27#discussioncomment-16972893>

要旨: v1.13.0 が CRAN auto-check の r-devel-windows-x86_64 で
「Overall checktime 11 min > 10 min」NOTE で auto-reject されたこと，
1.13.1 は test-grm.R / test-irm.R の重いブロックを `skip_on_cran()`
で除外する hotfix で **ユーザ可視変更なし**，スキップ対象は CRAN だけで
`NOT_CRAN` 経由のローカル/R-hub/win-devel では従来通り走るためカバレッジ不変。

### ggExametrika smoke test (v1.13.1 連動)

Newton で `install.packages("exametrika")` で 1.12.2 → 1.13.1 に上げてから
ggExametrika (現 v1.1.1 dev) の `devtools::test()` を実行: **611/611 PASS**。
警告は BINET (Class 1→Class 2 のフィールド数チェック) の caution が 1 件
だけで，機能上は問題なし。1.13.1 の API 変更は ggExametrika 側に影響なし。

### shinyExametrika PR#14 マージ (別リポジトリ)

arimune-san の "DAG plot height slider for BNM and LDLRA" を smoke test 後マージ。
詳細は shinyExametrika/WORKLOG.md 参照。

### sv1/sv2 への R 4.6.0 + exametrika 1.13.1 展開

EPEL 9 に R 4.6.0-1.el9 が 5/5 以降に到着していたので，sv1/sv2 を一気に
4.5.3 → 4.6.0 + exametrika 1.13.1 へ。詳細は
`~/Dropbox/.claude-system-inventory.md` の作業ログ 2026-05-20 セクション参照。
両機とも `library(exametrika); IRT(J15S500)` で 11 iter 収束，
LogLik -3893.03 で完全一致。

### 次フォローアップ

- (c) v2.0.0 BNM 着手 (CRAN cadence で 6/18 以降 提出目安)
- sv1/sv2 で 1 pass で取りこぼした依存順失敗パッケージ 5-6 個の 2nd pass
- Newton id_ed25519.pub を sv1/sv2 へ登録して二段 SSH 解消

## 2026-05-18 — v1.13.1 hotfix, CRAN 再提出・受理, git tag, GitHub Release

索引から転記: CLAUDE.md A4+C1 ステータス欄に記録されていた内容。

### 経緯

2026-05-17 に提出した v1.13.0 が CRAN auto-check の r-devel-windows-x86_64 で
`Overall checktime 11 min > 10 min` という単一 NOTE により auto-reject。
パッケージ本体の Windows/Debian チェックは Status: OK だったが，
checktime 制限で弾かれた。

### v1.13.1 hotfix

commit `69a1008` で以下のテストを `skip_on_cran()` で除外:

- `test-grm.R`: J15S3810 GRM (~60s) + nitems≥8 underflow regression (~8s)
- `test-irm.R`: J35S515 Gibbs 共有 fixture (~23s) + 再現性テスト 2 件 (~22s)

ローカル / R-hub / win-devel では `NOT_CRAN` 経由で従来通り走るためカバレッジ不変。
**ユーザ可視変更なし**。NEWS.md に 1.13.1 セクション追加，
cran-comments.md に 1.13.1 の説明追加，DESCRIPTION バンプ。

### CRAN 再提出・受理

`devtools::release()` で 1.13.1 を再提出 → 同日 2026-05-18 に受理。

### git tag + GitHub Release

- annotated tag `v1.13.1` を commit `69a1008` に打って origin に push
  (v1.13.0 と同じ流儀でリリースノートを tag message に同梱)
- GitHub Release v1.13.1 作成 (latest に昇格)
  https://github.com/kosugitti/exametrika/releases/tag/v1.13.1

### 恒久ルール

**実データ (J*S*) を使った大規模 fit テストには必ず `skip_on_cran()` を入れること。**

---

## 2026-05-17 — v1.13.0 CRAN 提出（5/15 予定から 2 日遅れ）

5/8-9 の案 C BNM プロト確認以降 A5 お遍路さん側に注力していたため
予定日 (5/15) を 2 日過ぎての提出。残作業は (1) 提出前チェック群,
(2) tag, (3) GitHub Release / Discussions, (4) `devtools::release()`,
(5) confirmation。全部消化。

### A. 提出前チェック

`tools/build_pkg.R` を一気通貫で実行 (styler → document → spell_check
→ check(cran=TRUE) → rhub → check_win_devel → release):

- **ローカル `R CMD check --as-cran`**: 0 errors / 0 warnings / 0 notes
  (6m 20s, macOS arm64, R 4.6.0)
- **win-devel** (R-devel 2026-05-15 r90061 ucrt): Status OK
  (build 34s, check 640s, log <https://win-builder.r-project.org/DA3rl6RAg6Bn>)
- **R-hub v2** (workflow_dispatch ラン `velveteen-asiaticlesserfreshwaterclam`,
  run id 25984718083):
  - linux (R-devel): 14m26s success
  - macos-arm64 (R-devel): 15m11s success
  - windows (R-devel): 19m52s success

R-hub の Annotations は全部 J*-S* フィクスチャ向けの想定済み
`message()` (zero variance / missing responses など) で check failure
ではない。

### B. 未コミット作業ログを掃除

`WORKLOG.md` に 5/12-13 セッション (ξ map + StepReg/StepNet 命名) の
追記が staged 前段で置いたままだったので `5fa6142` で先にコミット。
v1.13.0 のリリース内容自体には影響しないが台帳としては落としておく。

### C. タグ運用

- **v1.12.2** (`26855f1`, "chore: re-version 1.13.1 → 1.12.2"): 既に
  打ってあった。A3 polytomous_biclustering 論文のベースライン用に
  CRAN 未公開のまま git tag だけ残しておく方針 (NEWS は 1.13.0 が
  ロールアップ)
- **v1.13.0** (`5fa6142` = WORKLOG 追記後の main HEAD): 今日打った。
  WORKLOG は `.Rbuildignore` で除外されるので tar.gz には含まれない

両方を `origin` に push (main も同時)。

### D. GitHub Release + Discussions 投稿

ハイライト本文を作成し:

- **GitHub Release** v1.13.0: `gh release create --latest`,
  <https://github.com/kosugitti/exametrika/releases/tag/v1.13.0>
- **Discussions (日本語)** #26 (Announcements):
  <https://github.com/kosugitti/exametrika/discussions/26>
- **Discussions (英語)** #27 (Announcements):
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

`release()` 副作用:
- `CRAN-SUBMISSION` を 1.11.0 → 1.13.0 (SHA `5fa6142`) に書き換え
- `tools/build_pkg.R` の最終行を `devtools::release()` →
  `usethis::use_release_issue()` に置換 (次回サイクル用の usethis 提案)

`9122685` でコミット & push。

### 次へ

- CRAN 受理メール待ち (来たら本ホーム CLAUDE.md A4+C1 ステータスを
  「v1.13.0 CRAN 受理」に書き換え)
- v2.0.0 BNM (StepBNM = 案 C, DAG 所与スコープ) に着手可能
- A5 お遍路さん 5/20 締切の社会心理学会原稿に復帰

---

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
- 方向 v_j = Σ_k (ξ_jk − ξ_kj) · (pos_k − pos_j) / ||·||
- 結果: **E 群 (体験) と M 群 (意味づけ) が地図上できれいに分離**
  (xi within-group >> xi between-group のため)。E と M の独立性
  (A5 論文の柱 1) を視覚的に補強。

先生提案 19 辺 DAG との重ね合わせ: cos alignment 整合性チェックで
**58% (11/19) が方向場と一致**, 残り 8/19 は逆向き。「データ駆動 ξ
方向 ≠ 因果的時間 ordering」の差を定量化。例: E19→M07 (衣装→達成感)
は理論的に妥当だが ξ では逆向きに出る (M07 が衣装着用を予測する側で
ξ 大)。

### B. ξ map からの DAG 自動抽出

`20260513_extract_edges.R` (v1, 失敗) → `20260513_extract_edges_v2.R`
(成功):

- v1: 全ペアで上位 5% フィルタ + 相対非対称度 > 5% → 73 辺だが
  **E-M クロスが 0 辺** (E-M の絶対値が M-M に勝てない)
- v2: **edge type 別 quota** (E-E:10, M-M:15, E-M:30) で 54 辺
  (うち E-M:29) を救う。サイクル除去後 DAG 化

E-M 分布の比較で問題が判明:
```
              q50    q90    q95    q99
E-E (496)    0.133  0.250  0.282  0.398
E-M (2112)   0.024  0.088  0.107  0.137   ← M-M の 1/8
M-M (2145)   0.202  0.318  0.356  0.421
```

E-M クロスは絶対値で 1 桁弱い。「弱くても E-M なら重要」(先生の研究
仮説) を実現するには **type 別フィルタが必須**。

### C. 抽出 DAG の StepBNM fit

`20260513_caseC_fit_extracted.R`:

E-M 29 辺を子ノードでグループ化 (22 child) し, brms `mo()` + cumulative
probit で fit。結果:

- **26 / 29 (90%) が CI で 0 を跨がず有意**
- 上位辺: E01 → M23 (β=2.84), E01 → M37 (β=2.74), E05 → M29 (β=2.43)
- 負の β: **M19 → E27 (β=−2.38)** "リフレッシュ → トラブル少ない"
  という非自明な発見。M19 を見ればトラブル経験者でないことが予測される
- 非有意 3 辺: E05→M28, M35→E02, M42→E02 → triple filter
  (GGM + ξ + StepBNM) で更に絞り込みが効く

子ノード別 log_lik 改善 (vs null):
```
M28 (3 親): +28.4    最も改善
M23 (E01 単親): +25.7
E02 (M10+M48+M35+M42 4 親): +22.7
M10 (E01+E05): +19.8
```

E01 が圧倒的ハブ性を再確認 (GGM, ξ map, StepBNM 全て一致)。

### D. 命名確定: ステップ回帰 / ステップネット

「案 C」が呼びにくいため命名議論。先生から「**案 C は network model
じゃなくて regression model だろ. 多項条件付き確率の関数形だ**」と
本質的指摘 → 二層化命名:

```
ステップ回帰 (StepReg) : P(Y | X_1,...,X_k) の関数形
                         monotonic step (mo() in brms) + cumulative probit + 加法
                         Bürkner-Charpentier 2020 そのもの, 独立した
                         回帰モデルとして使える
                         ↓
ステップネット (StepBNM) : StepReg を各 child ノードに置いた DAG
                          ベース BNM。荘島 binary BNM の多値拡張
```

カタカナでも英略でも通用するのが採用理由。荘島先生面会用の
プレゼンでもこの 2 階層で説明する筋。

### E. 5/29 荘島先生面会用の整理

メモリ `shojima_meeting_5_29.md` 更新済。発表ストーリー候補:

1. ステップ回帰 (StepReg) と SEM 流 β の対応 (β = bsp_moX × D)
2. ステップネット (StepBNM) の動作確認 (お遍路さん 6 ノード)
3. ξ map 法 (本セッションの新ネタ) → AMISESCAL との接続可能性
4. データ駆動 DAG 抽出 → StepBNM 検証 (triple filter pipeline)

### 次へ

- **5/20 締切の社会心理学会原稿**へ復帰 (Fig 1 パス図, Fig 2 E×M heatmap,
  Fig 3 forest plot)
- **5/29 面会用プレゼン資料**の下書き
- **v1.13.0 CRAN 提出 (今日, 5/15)**: 別途実施

---

## 2026-05-08〜09 — 案 C BNM プロトタイプ動作確認 + v2.0.0 設計スコープ確定

5/1 で 3 段構え (Glasso → ξ → 案 C) の方針を固めた後,「案 C が実用に
耐えるか」をデータで確認するセッション。プロトは exametrika
リポジトリ内の `develop/` (gitignored) と `02_研究/お遍路さん/` の
両方で並行進行。

### A. 案 C は brms `mo()` で 1 行で書ける既存技術と判明

Bürkner & Charpentier (2020, BJMSP 73(3) 420-451) の monotonic
effects がそのまま案 C のパラメータ化と一致することを実験で確認。
`brm(Y ~ mo(X), family = cumulative("probit"))` で済む。

決定的な実証: `develop/20260508_caseC_diagnose.R` で brms 内部の
パラメータ化を逆算し, 案 C の μ_x と brms の `bsp_moX` の関係が
**μ_x = bsp_moX × D × cumsum(c(0, ζ))** であることを数値検証。
RMSE 0.085 (D 倍を含む) vs 0.785 (D 倍なし) で確定。

→ 翻訳ルール: **案 C のパス係数 β = bsp_moX × D**。SEM 流の
path diagram 表記が直接できる。

### B. Phase A/B シミュレーション (J15S3810 ベース)

`develop/20260508_caseC_proto.R`, `phaseA2.R`, `phaseB.R`:

- **Phase A1**: 単親リカバリ (μ=(0,0.3,0.7,1.2,1.8) 真値) → 全パラメータ
  CrI 内, β = 1.67 [1.45, 1.90] vs 真値 1.80
- **Phase A2**: 4 形状 (linear/convex/concave/step) で安定リカバリ
  (RMSE 0.05〜0.09)。step は Dirichlet(1,...) 事前で境界を僅かに膨らます
  (5/5 中 4/5 カバレッジ)
- **Phase B**: 多親加法 vs 相互作用シナリオで LOO が正しく判別
  (additive truth: m_add 勝, ΔLOO=−1.5; interaction truth: m_int 勝,
  ΔLOO=−62.8)

### C. Phase C (J15S3810 で全パイプライン)

- C-1: Glasso → 98 辺スケルトン
- C-2: `xi_stable()` で方向判定 → 78 辺の DAG. ただし
  **xi_stable の SE はタイブレーキング由来 (B 反復で √B → 0)** なので
  z 検定は無意味。CLAUDE.md の 5/1 の警告と整合
- C-4: paired data bootstrap (B=500, idx 共有で xi_jk と xi_kj を
  同一リサンプル内で計算) → **本物の有意辺は 11**。残り 67 辺は
  「タイブレーキングに対して安定」というだけだった
- C-5: 11 辺の DAG 可視化 (Item13 がソース, Item14→Item11←Item15
  v-structure)
- C-6: 11 辺それぞれ案 C で β fit。Item5→Item12 だけ β CI が 0
  跨ぎ (10/11 が triple-filter 通過)

### D. お遍路さんデータ N=143 で 6 ノード DAG fit

`02_研究/お遍路さん/2025本調査データ/20260509.R`:

DAG: E05 → {E07, M60}, E07 → M50, M60 → M50, M50 → M03, M03 → M14

主結果 (タスク 1〜4):
- E07 → M50 が NULL (β = −0.36 [−2.09, 0.52]) → 削除で AIC/BIC 改善
- DAG 全反転で ΔAIC = +17 (forward 勝ち, **方向情報が立っている**)
  J15S3810 は同質尺度で Δ=±5 だったが, お遍路さんは多次元尺度で明確
- M50 の親候補比較で E05 を新規支持 (β = 1.16 [0.40, 2.02])
- 最終 6 辺 DAG: E05 が 3 子のハブ (E07, M60, M50)

理論的補強: BNM 全体尤度 (飽和周辺込み) では H(X) と H(Y) の
エントロピー項がキャンセルし, **方向差は KL(P̂(Y|X) || P_caseC(Y|X))
の差**として残る (5/1 棄却された "ll 差はエントロピー差に支配" は
条件付き尤度単独比較の話。BNM 文脈では別)。

### E. GGM (お遍路さん 全 98 項目)

`02_研究/お遍路さん/2025本調査データ/20260509_t5_GGM.R`:

p=98 一括 Glasso は BCD 内で破綻 (`if (diff < eps)` で NaN) →
3 段階に分割:

- E 内 (32) Glasso: 219 辺 (密度 44%)
- M 内 (66) Glasso: 584 辺 (密度 27%, near-synonym ペア多数,
  M07-M08 \|θ\| = 0.985)
- E-M クロス (32×66) は polychoric 相関の上位採用 (\|ρ\|>0.4 で 245 辺)

発見:
- **E01 (地元の人と挨拶) が E-M クロスの真のハブ** (5/9 朝の DAG で
  E05 を選んだのは恣意的。E01 中心の方が情報量が大きい)
- **M-M の near-synonym ペアは Glasso でも残る** → field 化が必須

### F. v2.0.0 設計スコープを「DAG 所与」に絞り込み

「DAG 探索的推定は後回し, 荘島モデルも DAG ありき」と先生判断。
v2.0.0 BNM API は:

```r
BNM_polytomous(data, adj_matrix,
               model = "caseC",       # 加法 + 単調制約
               ...)
# 出力: τ, μ プロファイル, β, log_lik, fit_indices
```

3 モデル比較:
- M_0: 全独立 (β=0 / 周辺確率)
- M_t: 案 C 加法
- M_s: DAG 内飽和 (各 child で free CPT)

NFI / CFI / RMSEA / AIC / BIC / CAIC を `calcFitIndices()` 形式で。

ξ 行列の活用案: 局所診断 (DAG で繋がっていないペアの
ξ_residual が大きければ修正指数相当) として使う。

### G. README.md pkgdown 描画修正 (cc54ab3)

セッション中に先生が pkgdown サイトの LDB セクションが崩れている
ことに気付く。原因: 4 か所で prose 直後に ```` ```{r ...} ```` が来て
おり, パーサが状態を間違えていた。空行を入れて修正・push。
(その後 9787904 で README.Rmd → README.md ビルドフローに転換)

### H. データ修復: お遍路さん 00_dataHandling.R

`item_definitions.rds` に M11-M17 (宗教性 7 項目) のラベルが欠落
していたのを発見。`00_dataHandling.R` の `meaning_labels` 定義で
M10 の次が M18 になっていた。アンケート PDF と照合して 7 項目を
挿入, rds 再生成 (60 → 67 ラベル)。

### I. brms 出力の解説 (E01 ~ mo(E05), M24 ~ mo(E01)*mo(E05))

セッション末に先生が自分で brms を回し, 出力の各行の意味を確認:

- `Intercept[q]` = τ_q
- `bsp_moX` = per-step slope, **× D で β_caseC**
- `simo_moX1[j]` = ζ シンプレックス
- `disc = 1.00` = 識別性のために固定
- `mo(X1):mo(X2)` = 2 つのシンプレックスを持つ交互作用項
  (1 b + 3 + 3 = 7 パラメータ追加)
- 加法 vs 交互作用は LOO 比較で判定。先生例 (M24 ~ E01*E05) は
  交互作用 NULL (CI 跨ぎ) で加法で十分

### 次へ

- **バイクラスタリング融合 (タスク 2)**: フィールド代表値スコアを
  作って案 C BNM の上位構造として乗せる。研究的本丸
- **MLE 実装**: brms は遅すぎ (1 fit 30s〜2m)。`optim` + 単調 / 順序
  reparametrize で最尤推定 (推定 100 倍速)。先生が brms 体験中なので
  急がない
- **19 辺 DAG full fit**: 先生の理論的提案 DAG を案 C で全 fit + β
  + 全体適合度
- **荘島先生メール**: 今日の整理を要約して送付の予定 (先生作業)
- **A3 polytomous_biclustering 論文**: Phase3 完走済 (5/3) なので
  v1.13.0 CRAN 提出 (5/15) と並行で論文 4 節の結果反映へ

---

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

ジョブの Annotation から 2 WARNING + 1 NOTE で `--as-cran` が
exit 1 になっていたことを特定。

**問題 1 (WARNING): roxygen Markdown の角括弧自動リンク化**

`chatterjee_matrix.Rd` で `[j, k]`, `[i, j]`, `[i, k]` が `\link{}` として
解釈され，`Glasso.Rd` で `[0, 1]` が同様に解釈されていた。これは
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
`@param ... Additional arguments (currently unused; reserved for future
extensions).` を追加。

**問題 3 (NOTE): partial argument match**

`R/22_GlassoUnit.R:113` で `determinant(Theta, log = TRUE)` の
`log` が `logarithm` に部分マッチしていた。`logarithm = TRUE` に修正。

`roxygen2::roxygenise()` で man/Glasso.Rd と man/chatterjee_matrix.Rd を
再生成 (`\verb{}`, `\code{}` に変わる)。`devtools::check_man()` で警告
なし確認。NEWS.md の 1.13.0 セクションに「Documentation and CRAN
check cleanup」サブセクションを追加。コミット 620c11e で push。

(なおこのセッションで一度 main 直 push が Claude Code の harness
permission に弾かれたが，2 回目以降は通った。1 回目はユーザに `!` で
シェル直叩きしてもらって解消した。)

### C. 多値 BNM 構造学習の本格議論

ここからが本セッションの主題。4/30 確定とされていた「累積リンクモデル
尤度比較 (LiNGAM 順序版) を方向決定本流にする」方針が本当に妥当か，
順序 BNM の構造学習をどう設計すべきかを長く検討した。最終的に
ユーザの整理で「3 段構え」が確定した。経緯を詳しく残す。

#### C.1 累積リンク尤度比較の限界発見

J35S500 (5 件法 ordinal, N=500, J=35) で `MASS::polr` probit を使った
最小実験を実施。

**実験 1** (`develop/20260501.R`): Item01-Item02 ペアで双方向 polr。

- factor 渡しだと係数 4 個 (ダミー)
- numeric 渡しだと係数 1 個 (X 連続化)
- ll(Item02 → Item01) = -380.57, ll(Item01 → Item02) = -761.23,
  Δ = +380.66

問題提起: Δ が +380 もあるのは怪しい。手計算でエントロピー差を出すと
n × (H(Item02) - H(Item01)) = 380.56。実測差 +379.82 と誤差 0.7 で
ほぼ一致。これは累積リンクの漸近形

  ℓ(X → Y) ≈ -n · H(Y | X)

から差を取ると ℓ(X→Y) - ℓ(Y→X) ≈ n · (H(X) - H(Y)) になることに
対応。**生の対数尤度比較は「エントロピー小が下流」ルールしか出して
いない**。LiNGAM 順序版にはなっていない。

**実験 2** (`develop/20260501b.R`): MI 上位 10 ペアで同じ実験を回し，
残差 (delta_ll - delta_pred_ent) の挙動を確認。

- Item01-Item02 (MI ≈ 0.013) では残差 +0.7 (誤差レベル)
- MI 上位ペア (MI ≈ 0.20-0.24) では残差 -26〜+60 (支配的)
- residual / |delta_ll| が 1.66〜4.45
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
情報を一切使わない。二値の H(j) = -(p log p + (1-p) log(1-p)) が正答率
p の単調関数だったので，二値 BNM では「H 小 = 正答率極端 = 下流」
が偶然成立していたが，多値ではこの偶然は崩れる。
20260118 アイデアノート 5.3 節「方向は周辺エントロピー差で決まる」は
**二値限定の言明として訂正すべき**。

順序を尊重する情報量指標として:

- 累積エントロピー (Rao 2004): H̄(X) = -∫ F(x) log F(x) dx
- 累積残存エントロピー (Crescenzo & Longobardi 2002)

があるが心理計量で標準的でない。素直に項目平均/中央値の方が解釈
しやすい。

#### C.3 ユーザの発想 1: 順序 MDS で項目を空間化

「項目間の順序 MDS のような応用ができないか。距離構造を Chatterjee
相関で見て，距離の大小関係を維持する空間に入れる。非対称 MDS で
von Mises を使って」とのアイデア。

これに対し以下を整理して提示:

- Hill-climbing model (Borg & Groenen 2005 章 23):
  d(j → k) = ||x_j - x_k|| + α (h_k - h_j)
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

ユーザから https://sh0j1ma.stars.ne.jp/ams/shojimaBSJ11.pdf を共有。

**AMISESCAL = Asymmetric von Mises Scaling** (Kojiro Shojima, NCUEE
Research Division)

- 入力: 非対称 N×N 近接行列 G = {g_ij} (g_ij 小 = i が j を好ましく感じる)
- 各点 i に: 座標 x_i ∈ R^p, von Mises 平均方向 μ_i, 集中度 κ_i
- 縮尺率: π_ij = f(θ_ij | μ_i, κ_i) (von Mises 密度値)
- モデル距離: ξ_ij = (1 - π_ij) δ_ij where δ_ij = ||x_i - x_j||
- Stress: S(X, μ, κ) = Σ Σ (g_ij - ξ_ij)²
- 引用: Chino (1997), Mardia & Jupp (2000), Okada & Imaizumi (1987)

これは私が独立に提案した「Drift Vector + von Mises」と完全に一致する
モデル。荘島先生がすでに発表していた発想を再発見した形。

#### C.5 Abelson 電磁場モデル (小杉 2004) の歴史的位置づけ

ユーザから https://www.jstage.jst.go.jp/article/jbhmk/31/1/31_1_17/_pdf も
共有。

**小杉考司・藤原武弘 (2004) 等高線マッピングによる態度布置モデル.
行動計量学 31(1) 17-24.**

Abelson (1954-55) Contour Map モデルの応用:

  V(P) = Σ V(j) / (1 + d²_{Pj})

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
- DAG 構築には「ペア局所判定 + DAG 整合性」の二段構成が標準
  (PC, GES, LiNGAM, NOTEARS)
- AMISESCAL/Abelson は最終出力の可視化・解釈補助に位置づける
  のが妥当

mdspace パッケージ的な空間モデルは BNM の DAG 学習エンジンでは
なく，事後の可視化器として理解しておく。

#### C.7 ユーザによる 3 段構えの提案 (確定)

```
段 1  GGM (Glasso) で無向スケルトン           ← エッジ有無
段 2  Chatterjee ξ の値の大小で方向決定       ← エッジ方向
段 3  単調制約付き累積リンク等で条件付き分布   ← パス係数
```

ξ(j, k) > ξ(k, j) なら j → k (大きいほうが親)。

各段の役割が綺麗に分離。段 1 (Glasso) と段 2 (chatterjee_matrix) は
v1.13.0 で実装済みなので，段 3 が次の主戦場。

#### C.8 段 3 の検討: 案 C と派生アイデア

**案 C (単調制約付きカテゴリ係数累積リンク)**:

  P(Y ≤ q | X = x) = Φ(τ_q - μ_x), μ_1 ≤ μ_2 ≤ … ≤ μ_Q

- X カテゴリごとに別の μ_x (X 連続化なし)
- 単調制約で順序情報尊重
- パラメータ数 2(Q-1) (Q=5 で 8)
- 親複数の加法モデル: τ_q - Σ_k μ_{x_k}^{(k)} で Q×|pa| 線形
- 累積リンク (Q 個) と分割表 (Q² 個) の中間
- 加法性仮定で交互作用なし

**ユーザの派生アイデア: 「X_k で条件付けた Y_{k+1}」**

  D_{X→Y}(k) = P(Y ≥ k+1 | X = k)

X が k のとき Y が 1 段階上に行く確率を全 k で見る。GRM の閾値
対応のような発想。

これは案 C の対角隣セル `1 - Φ(τ_k - μ_k)` で，案 C のサブセット。
新しいモデルではなく案 C の補助プロファイル/可視化として位置づけ
られる。「+1 シフト」の恣意性 (なぜ +1, +2 や +0.5 はどうか)，
端点問題 (X = Q で Y_{Q+1} は定義不能)，対角隣だけ見る情報損失，
の懸念がある。

**結論**: パス係数は案 C を採用して β = μ_Q - μ_1 で取り，対角プロファ
イル D(k) は診断・可視化用に併用する。

順序 MDS との違いも明確化:

- 順序 MDS: 全項目を空間化，stress 最小化，項目に座標が付く
- 案 C: ペアの条件付き分布推定，対数尤度最大化，項目に座標は
  付かない (Y* は応答スケール内の潜在連続体)

#### C.9 議論の整理スライド作成

`develop/20260501_BNM_3steps.qmd` (Quarto reveal.js, 17 枚) を作成
してレンダリング。HTML 版は `develop/20260501_BNM_3steps.html`。
スライド構成:

1. 二値 BNM の前提
2. 多値で何が壊れるか
3. 全体像 (3 段構えの mermaid 図)
4. 段 1: GGM (Glasso)
5. 段 2: Chatterjee ξ
6. 段 2: 判定ルール
7. 段 3 の問題提起 (なぜ累積リンクではダメか)
8. 段 3: 案 C
9. 案 C の潜在変数イメージ
10. パラメータ数比較
11. 親複数の加法モデル
12. パス係数の取り出し方
13. 3 段の役割マップ (mermaid)
14. 残された設計判断
15. 補足: 順序 MDS との違い
16. まとめ

### D. 残された論点 (次セッション以降)

- **段 2 判定閾値**: 絶対値 / SE 正規化 / 経験的カットオフ。
  xi_stable() の SE で正規化が一番ロバストの見立て
- **段 2 計算範囲**: スケルトン上のみ (高速) vs 全ペア (安全)
- **段 3 単調制約の実装**: 強制最適化 (差分 log 再パラメータ化) vs
  PAVA 事後投影
- **段 3 加法性仮定**: 交互作用なしで OK か実データで検証
- **サイクル処理**: 最弱辺削除 vs 反転
- **パス係数の表現**: スカラー β / プロファイル D(k) / 両方を返す
- **BNM_GA / BNM_PBIL は 3 段構えで不要**: スコア探索が要らない
  (Glasso + ξ で構造が決まる)。既存の二値 BNM_GA / BNM_PBIL は
  二値専用として残すのが自然

### E. ユーザの思いつきメモ (将来役立つ可能性あり)

このセッションで発生したアイデアを後の参照用にまとめておく:

1. **項目の順序 MDS + Chatterjee ξ**: 項目を ξ 距離で空間化し，
   非対称 MDS (von Mises) で方向場を出す。BNM の DAG 学習器
   ではないが，可視化・解釈補助に有効
2. **AMISESCAL の応用**: 各項目に座標 x_j, 方向角 μ_j, 集中度 κ_j を
   割り当て，BNM 完成後に大局的な流れを確認する事後ツールとして
3. **3 次元 Abelson 電磁場モデル**: ユーザ自身の 2004 年論文の発想
   (態度の認知次元 + 感情強度の場)。今回の DAG には不向きだが，
   多値 BNM の出力を「態度場」として可視化するアイデアの源泉に
   なりうる
4. **GRM 並べでの両側閾値モデル**: X と Y の両方に GRM 風の閾値を
   割り当て，β_{X,k} と β_{Y,k+1} の対応で項目間関係を読む。これは
   1 次元 IRT の項目困難度プロファイルとして使えるが，BNM の親子
   関係とは別軸の話
5. **「X_k で条件付けた Y_{k+1}」プロファイル**: パス係数の補助
   可視化として，ペアごとに k = 1, …, Q-1 の折れ線で「動きやすさ」を
   見せる
6. **対数尤度差の残差** (= delta_ll - n*(H_X - H_Y)): MI 大ペアでは
   方向情報を含むかもしれない量。単独使用は不安定だが，ξ 非対称
   との組み合わせで補強できる可能性

### F. 作成・更新ファイル

- `DESCRIPTION` (1.12.1 → 1.13.0)
- `NEWS.md` (1.13.0 セクションに Documentation and CRAN check
  cleanup サブセクション追加)
- `WORKLOG.md` (本ログ)
- `R/22_GlassoUnit.R` (logarithm = TRUE, バックティック化, @param ...)
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
- `620c11e` fix: clear R CMD check WARNINGs and NOTE introduced by v1.13.0

両方とも origin/main へ push 済み。620c11e の R-CMD-check 結果は
push 後に走行開始 (本ログ作成時点では未確認)。

## 2026-04-29/30 — v1.13.0: Graphical Lasso + Chatterjee's ξ 実装と DAG 試作

v1.13.0 のメインフィーチャ2系統を実装し，両者を接続する DAG 構築の
試作まで進めた。コミット 7bf5ce9 で R/22_GlassoUnit.R, R/23_Chatterjee.R,
print.exametrika の Glasso ケース，両者のテストを main に push 済み。

### Graphical Lasso (R/22_GlassoUnit.R)

学習モードでスライド (BoChang 2015) と ESL 17.2 を読みながら段階的に
組み上げた。最終形は3層構造:

- `soft_thresholding(y, lambda)` — ESL 17.26 の soft-threshold 演算子
- `glasso_one(S, lambda, W_init, Beta_init, eps, max_iter)` — 単一 λ
  でブロック座標降下＋内側 lasso 座標降下
- `compute_EBIC_glasso(S, Theta, n, p, gamma)` — Foygel-Drton 2010
- `Glasso(U, ...)` — 公開関数。dataFormat 経由で polychoric 計算 →
  λ グリッド自動構築 → 各 λ で glasso_one を呼び EBIC 比較 → 最適 Θ

設計上踏んだ典型バグ:
- Jacobi vs Gauss-Seidel: 内側 CD で beta を別配列に書き込んで一括
  swap する Jacobi にしていたら，bfi のような実データの共分散で発散
  → in-place 更新 (Gauss-Seidel) に修正
- 整数オーバーフロー（修正は Chatterjee 側）
- `n * sum(...)` で R の int max を超え NA 化 → `as.numeric(length(x))`
- ホットスタート時の対角不整合: 前 λ の W を持ち越すと対角が古い λ で
  固定され，新 λ で発散する場合あり → diag(W_init) を新 λ で再設定
- print.exametrika の switch にケース追加が必要（trailing comma で
  「argument is missing」エラー）→ Glasso ケース追加で解消

EBIC 設計の現実的な観察: J15S3810 (N=3810, p=15) のような N≫p データ
では γ をどれだけ上げても罰則が尤度に勝てず最小 λ が選ばれる。
lambda_ratio で探索下限を制御する手が実用的。CLAUDE.md にも v1.13.0
要素として「manual lambda override の必要性」を残しておく価値あり。

テスト 9件パス，命名・class 順は exametrika 慣習 c("exametrika", X)
に揃えた（specific→general ではない。これは tail(class(x), 1) で
ディスパッチするため）。

### Chatterjee's xi (R/23_Chatterjee.R)

3関数を新規実装:
- `chatterjee_xi(x, y, ties_method)` — 単発計算
- `xi_stable(x, y, B, seed)` — タイ破りを B 回平均，list(xi, sd, se, B)
- `chatterjee_matrix(U, B, seed, verbose)` — pairwise 非対称 ξ 行列

B のデフォルトを peas データで実験的に校正: B=1000 で SE ≈ 0.001
となり構造学習の枝判定には十分との確認。テスト 9件パス。

### DAG 構築の試作 (develop/dag_test_20260429.R)

Glasso + Chatterjee の出力を結合する `direct_edges()` を試作。
asym_thresh = 0.01 で「絶対値で大きい方が親」ルールを実装。

J15S3810 で実行した結果，Glasso スケルトンが 98 エッジ（15×14/2=105
中の 98）と密。EBIC は罰則が効かず最小 λ を選び続ける。lambda_ratio
を 0.3 に上げて 61 エッジ，さらに |theta| > 0.1 の閾値カットで
10 エッジまで絞り込み。10 エッジのうち 3 が方向確定（V4→V8, V8→V6,
V4→V11），7 が双方向（ξ 差 < 0.01 で方向不明）。

3パターンの DAG 比較も試作:
- A: 純平均ベース（low → high） — 必ず DAG
- B: 純 ξ ベース — 双方向多数で DAG ならず
- C: ξ 優先・平均 fallback — 一見 DAG ぽいが ξ が mean 順序に
  逆らうエッジを生むためサイクルが発生（V4→V8→V6→V13→V4）

### 設計上の方向転換: 累積リンクモデル本流

セッション後半で v2.0.0 構造学習の本流が「累積リンクモデル」である
ことが明確化。Chatterjee ξ は構造学習の入口（XICOR の代替）として
有用だが，方向決定の本流は累積リンクで X→Y と Y→X の尤度比較を
する LiNGAM の順序版が筋が良い。先生発案の Q_{XY}(x) = P(Y ≥ x | X = x)
アプローチは累積リンクの「対角成分」の sub-case として位置づけ，
補助プロファイル/解釈ツールとして温存する方針。

memory `project_exametrika_future.md` のアイデア5に詳細記録。

### 試作の到達点と未着手

- v1.13.0 commit: 7bf5ce9（push 済み）。Glasso と Chatterjee 群と
  print method を含む
- direct_edges() は develop に試作残置。exametrika に組み込みは
  累積リンク版 (v2.0.0) と並行検討
- v2.0.0 多値BNM の構造学習は「入口=ξ → スケルトン=Glasso →
  方向=累積リンク」の三段構成で進める方針

## 2026-04-28 — v2.0.0 多値BNM準備: Chatterjee's ξ プロトタイプ + ブートストラップ設計

`develop/Chaterjee20260428.R` で Chatterjee (2021) の ξ_n を素のRで再実装し、
XICOR との数値一致を取った後、タイ多発データでの安定化方策を実験的に決定した。

### ξ_n 手計算の検証

peas データ（n=700, parent のユニーク値 7 個 = 693 タイ）で `XICOR::xicor()`
と完全一致を確認。途中で踏んだ典型バグを記録しておく:

- **X のタイ崩し**: `order(x)` ではなく `order(rank(x, ties.method = "random"))`。
  R の安定ソートは元の出現順を保つが、Chatterjee の定義は一様ランダム順序。
- **|r_{i+1} - r_i| の `abs` 抜け**: `sum(diff(r_i))` は望遠鏡和で `r_n - r_1`
  に潰れる。`sum(abs(diff(r_i)))` が正しい。
- **l_i の対象**: l_i は **Y に対する降順ランク** `rank(-sorted.y, "max")`。
  X のランクではない。
- **τ_n² 推定の u_i**: 論文 Theorem 2.2 直後の式で「u_i は R(1),...,R(n) を
  **昇順並べ替え**したもの」と明記されている (correlation43_ja.tex L325)。
  XICOR の `qfr = sort(fr)` と同じ。`y[r_i]`（生Yをランクで引く）ではない。

最終的に xi/sd/pval が XICOR と bit-identical (差 1.78e-15) で一致。

### ブートストラップ B 数の目安

タイの多いデータでは ξ_n がタイ破りごとにブレるので、複数回平均で安定化する。
（行サンプリングではなく、`set.seed()` を振り直して `rank(x, "random")` を
B 回引き直すだけの軽量版。サンプルサイズが変わらない利点あり。）

n とタイ密度を変えて sd(ξ_iter) を測定:

| データ | n | sd(ξ_iter) | B=1000 SE |
|---|---|---|---|
| peas | 700 | 0.024 | 0.00075 |
| peas subsample | 100 | 0.060 | 0.00191 |
| Likert 5件 | 300 | 0.040 | 0.00127 |
| Likert 5件 | 100 | 0.071 | 0.00223 |

おおむね sd(ξ_iter) ∝ 1/√n（タイ密度同程度なら）。

**結論**: 5件法 n=100 の最悪ケースでも B=1000 で SE ≒ 0.002、ξ が
**小数点以下 2 桁で安定**する。BNM 構造学習の枝符号判定には十分。
v2.0.0 多値 BNM 実装時のデフォルトは `B = 1000`、論文用の高精度報告は
`B = 5000` を引数で指定可能にする方針。

設計上の TODO（v2.0.0 多値 BNM 実装時）:
- `xi_stable(x, y, B = 1000)` のラッパー関数を用意
- 「最初に B=200 で sd(ξ) を測って目標精度から B を自動決定」案も検討
- ブートストラップは ξ 推定値だけでなく p 値も同様に平均化
- 累積平均の SE は `sd(xi)/sqrt(B)`（標本 SD ではない点に注意）

### 成果物

- `develop/Chaterjee20260428.R` — ξ_n 手計算 + B=10000 のブートストラップ実験
- `develop/Chaterjee_bootstrap_convergence.png` — 累積平均 ± SE プロット
- `develop/Chatterjee2021/correlation43_ja.tex` — 日本語訳（既存）の参照箇所
  L308-329（定理 2.2 周辺、$\hat{\tau}_n^2$ 推定式）

## 2026-04-27 — v1.12.0: C++ Gibbs sampler, conf_class, CRAN prep

### Confirmatory Biclustering bug audit

Started from a known issue: `.claude/CLAUDE.md` flagged that
`R/07_Biclustering.R` (binary) and `R/15_Biclustering_nominal.R`
(nominal) had the same `NCOL(U)` length-check pattern that was fixed
for ordinal in the v1.12.x prep work, but with the qualifier "not
catastrophic because of the in-loop guard". Empirical check on
`J20S600` with `length(conf) = 20` showed:

- Binary: works at runtime. `Biclustering.binary()` rebinds
  `U <- tmp$U * tmp$Z` in line 169 *before* the conf block, so
  `NCOL(U)` returns the item count. Cosmetic-only fix to `NCOL(tmp$U)`
  for readability.
- Nominal: **broken since v1.10.0**. `Biclustering.nominal()` does not
  rebind `U`, so `NCOL(U) == 1` (it is an `exametrika` list), and every
  well-formed `conf` vector hits the "size does NOT match" stop. The
  in-loop guard never runs because the conf block aborts the call up
  front. Fixed to `NCOL(U$Q)`.
- New bug found while writing matrix-form regression tests: the
  `is.matrix(conf) | is.data.frame(conf)` branch validated the matrix
  but never assigned it to `conf_mat`. The next line,
  `nfld <- NCOL(conf_mat)`, then died with "object 'conf_mat' not
  found". Same omission in all three implementations (binary, ordinal,
  nominal). Fixed by adding `conf_mat <- as.matrix(conf)` in each
  matrix branch.

Tests: 10 new test_that blocks across `test-biclustering.R` and
`test-polytomous-biclustering.R` covering vector accept, wrong-length
reject, matrix accept, and wrong-rows reject for each of the three
data types. Full suite: PASS 4775 -> 4790.

Committed as `60bace9: fix: confirmatory Biclustering bugs across
binary/ordinal/nominal`.

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
- For rated data the dispatch is via `Biclustering.rated()` which
  passes `...` to `Biclustering.nominal()` internally, then re-orders
  output classes by correct rate. So `conf_class` works through rated
  but the output class labels are a permutation of the input labels
  (the individual-to-class mapping is preserved). Test verifies the
  bijective invariant rather than equality.

Tests: 13 new test_that blocks across the same two files plus a
combined `conf + conf_class` test on ordinal data.

### IRM Gibbs sampler in C++ (the headline change)

Profiled the R Gibbs core at 66-99 ms/iter on J20S600 and 71 ms/iter
on J35S500. Hotspots (`Rprof`, line-level): `irm_lmvbeta()` 20.6%,
3-D `U_fcq[f, c, ]` slicing and `nume <- ... + alpha_vec` arithmetic
13-16% each, `t(fld01) %*% (Z[target, ] * Uq[target, , q])` row
matmul 13%. Inner CRP loop dominates total time.

Wrote `src/irm_gibbs_core.cpp` as a direct, line-by-line translation
of `irm_gibbs_core()`. State held in flat `std::vector<double>` /
`std::vector<int>` to avoid an `RcppArmadillo` dependency. No
algorithmic changes: same loop nest, same accumulation order.

The RNG-compatibility journey was the most time-consuming part:

1. First pass used `Rcpp::sample(n, n, false)`. Output diverged from R
   immediately. Direct check: `set.seed(42); sample(1:10, 10)` !=
   `Rcpp::sample(10, 10, false)`. Confirmed `Rcpp::sample` does not
   share the same RNG-consumption order as base R.
2. Switched the perm to `Function f("sample.int"); f(n)`. With this
   alone, the class-side Gibbs loop produced bit-identical Nc and cls
   to the R reference (verified on `J20S600`, max_iter = 1).
3. Field-side loop still diverged. After auditing the implementation
   line by line and finding no algorithmic difference, added a
   class-only debug build (`#if 0` around the field-side loop) and
   confirmed the post-class-loop RNG state matched: `runif(3)` in R
   and in C++ returned the same three values.
4. Despite same RNG state, full-run jRand still differed from the R
   reference. The remaining suspect was `R::rmultinom` (the C-level
   entry). Replaced it with `Function r_rmultinom("rmultinom"); m =
   r_rmultinom(1, 1, p)`. Bit-identical match for max_iter = 1, 5,
   10, 50.

Hypothesis for why `R::rmultinom` desynchronises while
`R::sample` (well, `Rcpp::sample`) was at least RNG-consistent
internally is that `do_rmultinom` runs `FixupProb` and possibly other
validation/normalisation steps that `R::rmultinom` (the C-level form)
skips. We tried explicit normalisation (`p[k] /= sum(p)`) before
`R::rmultinom` and it still desynchronised, so there is more going on
than just the FixupProb division. Did not investigate further; the
R-level path is fast enough.

Performance after the switch:

- nominal J20S600: 82 -> 20 ms/iter (4.0x)
- ordinal J35S500: 76 -> 19 ms/iter (4.1x)

End-to-end `Biclustering_IRM(J20S600, max_iter = 100)` drops from a
few seconds to ~0.3 s. Numerical equivalence verified by 6 unit tests
in `test-irm-gibbs-cpp.R` covering several seeds and iteration counts
(cls, fld, Nc, Nf, cls01, fld01, U_fcq all bit-identical).

R integration: added `use_cpp = TRUE` argument to `irm_gibbs_core()`.
Default dispatches to `irm_gibbs_core_cpp()`; `use_cpp = FALSE`
preserves the R reference for cross-checking. The two callers
(`Biclustering_IRM.nominal()` and `Biclustering_IRM.ordinal()`) need
no changes; rated dispatch flows through nominal automatically.

Committed (with conf_class and bg-agent work bundled) as `d485a18:
feat: C++ IRM Gibbs sampler + class-side confirmatory + vignette/test
polish`. Pushed to origin/main.

### CRAN submission prep

Pending A3 Phase3 simulation completion (per `.claude/CLAUDE.md`
policy), but staged the docs-only prep work so the submission can go
out as soon as Phase3 lands:

- `.Rbuildignore` += `^figure$` (knitr vignette artefact, was being
  picked up as untracked).
- `roxygen2::roxygenise()` to regenerate `man/Biclustering.Rd` with
  the new `conf_class` argument; added a proper `@param conf_class`
  block in `R/07_Biclustering.R` so the description body is non-empty.
- `NEWS.md`: consolidated the duplicate "## Bug Fixes" subsection
  into the single "## Bug fixes" block, and corrected the "## Notes"
  entry which had previously claimed "no API changes" (conf_class is
  an additive API change).
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
showed that `Biclustering.ordinal()` is 5x slower per GridSearch call
than `Biclustering.nominal()` on the same data. On typical simulation
conditions (e.g. C10F5S200J50K7, 200 trials), the ordinal EM grid
search accounts for ~50% of total trial wall-clock. Vectorizing the
ordinal EM hot path is the single highest-leverage optimization for
the simulation pipeline.

### Hotspot analysis (R/16_Biclustering_ordinal.R)

Per-EM-iteration hotspots identified:

- **H1** line 199: `apply(Ufcq_prior, c(1,2), function(x) rev(cumsum(rev(x))))`
  — reverse cumulative sum along the 3rd axis via per-cell apply. Slowest
  single line; `apply` with a user function dispatches `nfld*ncls` R-level
  calls per EM iteration.
- **H2** lines 172, 185, 321, 323: `apply(X, 1, min/max)` — O(nrow) R-level
  function calls. Replaceable with `do.call(pmin.int, as.data.frame(X))`
  (C-level, log-factor fewer calls).
- **H3** lines 166, 182: `log((BBRM[, , q] - BBRM[, , q + 1]) + const)` is
  computed independently for E-step class and E-step field each q per iter.
  Identical values can be precomputed once per iter.
- **H4** Z*Uq[,,q] recomputed at lines 170, 182, 194, 225 every iter.
  Precomputable via column-major recycling: `ZU <- Uq * as.vector(Z)`.
- **3D apply sum**: `apply(X, c(1,2), sum)` / `apply(X, c(2,3), sum)` can be
  replaced with `rowSums(X, dims=2)` / `colSums(X, dims=1)` (base R, no
  new imports).

### Plan

Phase 1 of v1.12.0: apply H1-H4 + 3D-apply replacements.
Phase 2 of v1.12.0 (deferred): collapse E-step/log-lik q-loops into
fused matmul (may alter floating-point summation order, requires
looser tolerance). Evaluate after Phase 1 measurement.

### Decisions

- **No new Imports.** `matrixStats` was initially considered for rowMins/
  rowMaxs but `do.call(pmin.int, as.data.frame(X))` achieves comparable
  performance in pure base R; `rowSums/colSums` have an undocumented-ish
  `dims` argument that replaces `apply(X, c(k1,k2), sum)` on 3D arrays.
- **H5/H6 deferred.** Fusing the q-loop matmuls changes summation order
  and can introduce O(1e-14) floating-point drift. Per project policy
  ("same results are paramount"), this is left for a follow-up release
  or kept out of v1.12.0 entirely.
- **CRAN timing.** v1.11.0 was accepted 2026-04-15; CRAN dislikes short
  intervals. Simulation servers install v1.12.0 via
  `remotes::install_github("kosugitti/exametrika")`. Formal CRAN
  submission is deferred until after paper submission.

### Validation strategy

- Numerical identity: for a fixed seed on J35S500 (ordinal) and a
  synthetic polytomous dataset, record the full log-lik trajectory and
  final BCRM / BBRM / ClassMembership / FieldMembership under the old
  implementation (tag `baseline_v1.11.0_HEAD`). New implementation
  must match to `max(abs(old - new)) < 1e-12` for every EM step.
- All 3,532 tests in `tests/testthat/` must pass (FAIL 0, WARN 0).
- `R CMD check --as-cran` must remain 0/0/0.

### Implementation and results (same day)

All five hot spots implemented incrementally, validated against the
pre-refactor baseline after each step. Baseline captured on commit
bbafdbe (pkg 1.11.0) via
`develop/vectorization_ordinal_v1_12_0/capture_baseline.R` over 8
configurations spanning ordinal B/R methods, ncls in {5, 8, 10}, nfld
in {4, 5}, J35S500 and J15S3810 datasets, plus mic=TRUE and a
non-converging case, and two nominal configurations on J20S600.

`compare_to_baseline.R` reports IDENTICAL (max abs diff == 0) for all
eight configurations across every monitored field (BCRM, BBRM,
ClassMembership, FieldMembership, TestFitIndices, FRP, TRP, LFD, LRD,
FieldEstimated, ClassEstimated, log_lik, n_cycle, converge) after
every incremental step.

Per-step wall-clock ratios measured on those same 8 configurations
(single-run, not averaged; noise dominates for <0.1s runs):

- After H1 alone: 0.78x to 1.02x (within noise)
- After H1+H2: 0.81x to 1.09x
- After H1+H2+H3: 0.91x to 1.21x (O6 J15S3810 ncls=8 nfld=4)
- After H1+H2+H3+H4: 0.91x to 1.28x
- After all 5: 1.04x (N1) to 1.33x (O6)

Larger single-cell benchmark on J35S500 with variable grid cells,
against baseline bbafdbe:

    ncls=5  nfld=5   : 0.073s -> 0.059s  (1.24x)
    ncls=10 nfld=10  : 0.161s -> 0.127s  (1.27x)
    ncls=15 nfld=10  : 0.065s -> 0.051s  (1.27x)
    ncls=20 nfld=15  : 0.071s -> 0.061s  (1.16x)

log_lik identical to every digit in all four cases.

testthat: FAIL 0, WARN 0, SKIP 0, PASS 4750 (test count higher than
the 3,532 figure in the outdated CLAUDE.md because of rated
Biclustering and DistractorAnalysis additions in 1.11.0; all pass).

### Phase 2 stance for v1.12.0

The E-step and log-lik q-loops (H5 in the original plan; fuse Q
matmuls into one big matmul over reshaped arrays) were *not* applied
in this release because the summation order would change and break
the `max(abs(old - new)) == 0` gate. Deferred to a future release or
a separate experiment branch.

The project-level ask for v1.12.0 mentions the simulation pipeline
also benefits from further work in `Biclustering.ordinal`; that work
(if any) should preserve the same identity gate and be tracked in a
new WORKLOG entry.

### Uq one-hot vectorization + maxiter bug fix (same day)

Discovered while preparing simulation deployment that the `Uq`
one-hot encoding in both `Biclustering.ordinal()` and
`Biclustering.nominal()` is still a nobs*nitems nested R loop:

    for (i in 1:nobs) for (j in 1:nitems)
      Uq[i, j, tmp$Q[i, j]] <- 1

Replaced with a single matrix-index assignment using masked
`cbind(i_idx, j_idx, q_idx)` restricted to `tmp$Z == 1`. The old
loop also had a silent bug: for missing entries (`tmp$Q[i,j] == -1`
set by `dataFormat()`), `Uq[i, j, -1] <- 1` sets every Uq[i, j, k]
for k in 2..maxQ to 1 due to R's negative-subscript semantics. The
values were never read (every consumer applies the `tmp$Z` mask),
but the vectorized version correctly leaves those positions at zero.
Downstream outputs (BCRM, ClassMembership, TestFitIndices, log-lik)
remain bit-identical to the pre-refactor baseline.

Wall-clock impact on the 8-configuration validation matrix:

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

Independent maxiter bug: ordinal also had `maxemt <- 100` hardcoded at
line 39, identical in spirit to the nominal bug fixed in 1.11.0. The
user-supplied `maxiter` argument was being ignored. Fixed in the same
commit. testthat passes (4750, FAIL 0).

### GridSearch() per-cell error tolerance (same day)

While preparing the simulation-harness refactor, we discovered that
`GridSearch()` does not wrap its inner `do.call(fun, args_list)` call
in `tryCatch`: any error raised by `Biclustering()` at a single grid
cell terminates the whole grid search, and the caller's higher-level
`tryCatch` then turns the entire configuration into an all-NA row.
This has a disproportionate effect on small `(nobs, nitems)`
conditions (empty-cluster edge cases at corners of the grid), which
we observed wiping out the Biclustering-branch rows for small
conditions in the Phase3 simulation.

The fix is a two-line `tryCatch` wrapper in both branches
(Biclustering and LCA/LRA) of `R/00_GridSearch.R`, treating errors
exactly like non-convergence (NA in the index matrix,
record in `failed_settings`). All 4750 testthat tests still pass.
This behavior change is non-breaking for successful grids but is a
genuine bug fix for pathological grids, and is shipped as part of
v1.12.0.

---

## 2026-06-01 — 多値DAG制約格子モデル アイデアノート追記

- `develop/20250529meetingMemo.md` → `develop/20260529meetingMemo.md` にリネーム（年号誤り修正）
- 荘島先生ミーティング（2026-05-29）のフォローアップとして，多値DAG制約格子モデルの構想を末尾に追記
  - フィールド間OR制約格子，有効状態の列挙，Stress最小DAG探索アルゴリズム
  - 詳細は `02_研究/お遍路さん/WORKLOG.md` 2026-06-01 参照
- 将来の exametrika v3.0 系機能候補として保留

## 2026-07-13 アイソトニック潜在ランクモデル（第3のLRA・develop/・v1.16.0候補）

A3方法論文の順序版の弱さ（平均で並べ替え＝実質空虚。AIレビュー2本が指摘）を直す過程で，LRA/ランクラスタリングの順序性を根本から作り直す新モデルが派生。SOM→フィルタ(GTM)に続く**第3のやり方＝順序制約(アイソトニック)を制約として課す確率モデル**。

- **定式化**: 各項目の正答率 π*_jc がランクで単調非減少。段差 δ_jc=π*_{j,c+1}−π*_jc≥0 を (C+1)-単体に載せ，**累積和で自動単調**（単調制約を明示的に書かない）。段差に**ディリクレ**（濃度 a＝ダイヤル: a=1純アイソトニック/a>1タイなし・均等）。名義のカテゴリ確率と同じディリクレ-多項の器を「ランク差分」に効かせる＝魂の統一（ただし累積和が非線形なので共役ではない）。
- **推定**: E-step現行同一（順序制約はパラメタ空間の話でM-stepにしか入らない）。M-step＝ランク方向の**重み付きPAVA**（a=1で制約付き最尤の厳密解・Ayer et al. 1955）。a>1は単体上の凸最適化。ベイズ版は**Stan**（段差 simplex＋cumulative_sum→単調自動）。
- **実証(J15S500, S500/J15/C6)**: (1)EM**大域最適**（多初期値で既定超え0/30）, (2)Stan動く・**必ず単調**・収束Rhat≈1.02, (3)aダイヤルで段差ばらつき 0.187(EM MAP)→0.082(a=1)→0.038(a=30), (4)**タイは点推定(モード)の癖・事後平均は滑らか**（潰れはMAPの性質でベイズなら a=1でも滑らか）, (5)**piR(ランク分布)**: 現行GTM(emclus)/バイクラのE-stepに事前項なし＝**一様仮定**（SOMのみheuristicなprior_list）・実データ非一様(rank5≈40%,rank1≈1.4%)・`piR<-colMeans(pd)`で推定するとfit↑(−4131.8→−4124.7)だがAIC/BICはトレードオフ, (6)**適合度: Isotonic>GTM**（loglik −4131.8/−4124.7 vs GTM −4148.5, AIC/BICもIsotonic勝ち＝GTM劣位。目玉）。
- **現行多値順序版のバグ確認**: 16/18等は「平均で並べ替え」で実質空虚（カテゴリ点推定＝名義と恒等・フィルタは死にコード・trace(Fil)不整合）。→v1.16.0で非推奨化対象。
- **files(develop/)**: `OrderRestrictedLRA_ja.tex/.pdf`(荘島先生への報告書6p), `_prototype.R`(EM+自作PAVA), `isotonic_LRA.stan`＋`_run.R`, `isotonic_compare.R/.png`(EM vs MCMC・aダイヤル図), `isotonic_diagnose.R`(多初期値・piR), `isotonic_fit_compare.R`(GTM比較)。`myBiber.bib`に`ayer1955`追加, 文献PDF=`02_研究/多値バイクラスタリング/文献/ayer1955.pdf`。
- **計画(未確定)**: **v1.16.0**で`method="isotonic"`新設＋現行多値順序版の非推奨化（R JournalのSOM/GTM記述と衝突しない追加。2.0.0は多値BNM温存）。math→dev→exametrika→論文を荘島先生と共同開発。次＝荘島先生の反応待ち／ランクラスタリング(2モード)へ持ち上げ。

### 2026-07-13 追記：適合度計算のレシピ確定＋報告書仕上げ（荘島先生へ報告）

- **適合度(AIC/BIC)のレシピ確定**：(1)**推定後に観測周辺対数尤度を計算し直す**(log-sum-exp・M-stepのQでない)，(2)有効パラメタ数 df ＝ **各項目のuniqueな正答率の数(タイで潰れたブロック数)** ＝ 形状制約回帰のdf(Meyer & Woodroofe 2000: shape-restricted回帰のdf＝ブロック数)。piR推定版は **+(C-1)**。GTMだけは平滑スムーザなので df＝**trace(Fil)×J**。(3)**BICのN＝人数S**。→ `isotonic_fit_compare.R` は最初からこの定義で計算しており，(f)表は再実行で完全一致(GTM −4148.5/71.5, Iso一様 −4131.8/49, Iso piR推定 −4124.7/69)＝**変更不要**を確認。
- **piR推定をLRA_3rd_20260712.Rに実装(殿が)**：3行(init `piR<-rep(1/ncls,ncls)` ／E-step `llmat<-sweep(llmat,2,log(piR),"+")` をexp前に ／M-step `piR<-colMeans(postDist)`)。ベースレートとして割り振りに効く＝事後計算(現行GTMの一様)とは別物・IRPが変わる。
- **報告書 `OrderRestrictedLRA_ja.pdf` 仕上げ**：APAスタイルのbiber配線(`myBiber.bib`, PAVA初出でAyer1955を`\parencite`＋参考文献リスト＋PAVAフルネーム)，(d)を「タイはモード(MAP)の癖・EAPなら滑らか」に精密化(StanのoptimizeでMCMC-MAP≒EM+PAVAを数値確認：populated ranks一致・空rank1のみ非識別で分岐)，(f)にdf数え方の注記追加。殿が文体を平場化(表記"Isotonic"に統一・節短縮)。5ページ・未定義0。**殿が荘島先生に報告予定**。
- 追加ファイル: `isotonic_map_check.R`(MCMC-MAP vs EM+PAVA検証)。

### 2026-07-13 追記2：文献スキャン結果・出版戦略の解決・実装ラダー

- **文献スキャン(裏で実行・完了)**: 二値アイソトニックLRA＋EM+PAVAは**Croon(1990, BJMSP)が既出**(順序潜在クラス+PAVA)。増分事前=Yang-O'Brien-Dunson(2011, JASA)。Dirichlet増分累積和=Ferguson/Sethuraman。**Ligtvoet(2012 Psychometrika)「An isotonic partial credit model」=タイトル衝突**(要区別)。→**単独の二値isotonicは新規性で撃墜リスク大**。白地は**(1)二モード(バイクラ)・(2)多値統一・(3)SOM/GTMフィルタ→isotonic置換テーゼ+実データhead-to-head勝利・(4)LRA国際化**の組み合わせのみ。Related Workで Croon/Vermunt/van Onna/Ligtvoet/Yang を明示配置し新規性をnarrowに。要原典照合=Croon1990のPAVA使用(現状二次ソース)。
- **出版戦略の解決**: 単独isotonicは諦め，**アイソトニックをA3の組織原理として統合＝旗艦「多値アイソトニック・バイクラスタリング」(Psychometrika本命→Behaviormetrika→和文の二/三の矢)**。旗艦はプレプリ先行(優先権確保)を残す。IRMは数理新規性なし=**研究ノート/資料**で**紀要へ**(arXivは手間嫌で不採用)。紀要=IRM導出+GridSearch vs IRM(**名義・既存結果・再計算なし**)で9月末に安全着地(isotonic非依存)。SWB=JPA。→**3本(旗艦/紀要/SWB)＋旗艦プレプリ**。
- **切り分けの鍵**: isotonicが変えるのは順序emissionのみ→名義/択一式/二値は不変=既存結果流用可。旧「順序54%対28%」はフィルタ産物=無効で落とす。GridSearch vs IRMは構造個数の話でisotonicと直交=紀要は名義で完結。フルisotonicシミュ(1から・同一データ・seed再現可)は**旗艦のため秋に隔離**(締切から外す・計算は背景で放置)。
- **実装ラダー**: フィルタ使用=isotonic対象=06_LRA/07_Biclustering/12_LRA_ordinal/13_LRA_rated/16_Biclustering_ordinal。**易(単一閾値PAVA)=二値LRA(06)・二値ランクラスタリング(07)→8月exametrika**(＋piR・12/16/18は非推奨マーク)。**難(二重単調・Dykstra)=多値順序LRA(12)・多値順序バイクラ(16)→秋**。**多値はLRA(12)から着手(1モードで二重単調を分離)→バイクラ(16)は2モード後乗せ**が筋(殿判断)。択一式(13)の扱い要決定。
- **状態**: 報告書(OrderRestrictedLRA_ja.pdf)送付準備済＝**荘島先生の反応待ちが次のゲート**。反応で次目標確定→実装着手。

## CLAUDE.mdからの退避 (2026-07-17)

ホーム索引(~/Dropbox/CLAUDE.md)のステータスセル圧縮時の退避(退避時点の全文):

CRAN版はv1.15.0(2026-07公開済・GitHub Release v1.15.0/Discussions告知も完了)。開発版v1.16.0(8/15目処)=GRM監査修正済(grm_iif正式Samejima式化・EAP/MAP/PSDの和事後→積事後・回帰テスト追加、5137テスト・check 0/0/0、commit&push済)＋アイソトニック潜在ランクモデル(第3のLRA・develop/で開発中→method="isotonic"新設＋現行多値順序版非推奨化、memory: project_isotonic_latent_rank)。Title変更(issue#32)未着手、v2.0.0多値BNM設計中。R Journal論文は採択・校正対応済。詳細→Git/exametrika/{CLAUDE,WORKLOG}.md

### 2026-07-14〜17 アイソトニック続き：荘島承認・Dykstraメモ・Algorithm_LRA・ソース読解の訂正・順序LRA実装着手

**荘島先生が方針承認(2026-07-14)**: OrderRestrictedLRA_ja報告書に「文句ナッシング・最高」。表現指摘3点を反映→(1)「大域最適」→「局所解は確認されなかった」(大域解は不可知), (2)「勝つ」→「Isotonicのほうが高い」, (3)§3末にディリクレ→isotonic回帰の機序を1段落追記(対数事後=対数尤度+Σ(α-1)lnπ_jk; α=1で罰則消滅=制約付き最尤=PAVA, α>1で均等罰則)。再コンパイル5p・未定義0。

**多値の順序の課し方=確率的順序で確定(→Dykstra)**: Qの扱いを検討し，平均(間隔尺度cheat)・中央値(非平滑)・最適尺度化(Gifi/MDS=確率モデルを捨てる=SOM/GTMと同じ穴)を却下。順序尺度に忠実かつ凸なのは確率的順序(全閾値でランク単調=first-order stochastic dominance)のみ→これがDykstra。不可能の三角形={確率モデル・ノンパラ・楽な計算}から2つ；確率モデル+ノンパラを守る→Dykstra(計算は軽い)。質量移動再パラメタ化はDykstra回避策だがMCMC寄りで保留。

**Dykstraメモ作成** `develop/Dykstra_memo_ja.tex/.pdf`(5p): 確率的順序→二重単調→Dykstra=補正メモ付き交互射影を平易に。2つの壁の角の最近点・「補正メモ=(射影前)-(射影後)を足し戻す」・1巡目=素朴/2巡目から補正・数値追跡((-0.5,-0.5)→…→(0,0))・Q=1でPAVA一発に一致(二値の多値一般化)。図 `develop/dykstra_fig.tex/.pdf`(斜め壁・素朴のドリフト白丸 vs Dykstra青・緑破線=補正で戻る)を§4に埋込。

**Algorithm_LRA.tex作成** `develop/`(7p): Algorithm_Biclustering.texと同じ表記・流れで二値→名義→順序LRA。LRA=各項目が単独フィールド(M_F=I_J)→f→j置換・E-step2削除。順序M-stepは段差展開でπ*_jcq=Σ_{k≥q}γ/Σγ(バイクラと同一証明)=各項目・各ランクで規格化するだけ→その後Dykstra補正(集合A=閾値ごとPAVA∩集合B=各行が生存関数)。Q=1でB自明=PAVA一発。

**ソース読解の重要な訂正(2026-07-14)**: `R/12_LRA_ordinal.R`(順序LRA・荘島)はフィルタをM-stepで実使用(354行 rankProf %*% Fil)→順序はフィルタ(GTM)が作る・空虚でない。`R/16_Biclustering_ordinal.R`(順序ランクラスタリング・小杉自作)はフィルタsmoothed_memb計算するが出力専用(459行)でM-step不使用→EMは名義,順序はmicが「クラスをtotal_expected≒平均得点順にorder()で並べ替えるだけ」(258-271行)=空虚。**「平均で並べ替え」はランクラスタリング(16)だけ・LRA(12)ではない**。AIレビュー「空虚」はランクラスタリングについて正しい。create_filter_matrix=三重対角平滑化カーネル(自分f0+隣(1-f0)/2)。→置換対象: LRAはフィルタ→単調制約, ランクラスタリング(自作)は名義EM→単調制約M-step(飾りから本物へ・改善大)。

**順序LRA実装着手(殿が自分で書く・私は助言のみ)** `develop/Dykstra_20260714.R`: 順序LRA isotonic(フィールドなしで多値PAVAが回るか検証目的)。荘島式の得点分位初期化(score分位でnrank群→群ごと経験比率→単調seed)採用。配列軸はJ×C×Q(π_jcq記法に一致・[,,q]スライスが1閾値のJ×C表でPAVAに好適)に統一。デバッグ済: 軸順(J×C×Qなら代入は[j,r,]・[j,,q]でない)・RBRM全ゼロ(YY充填ループ抜け)・UU(ワンホット=どのカテゴリ)vs YY(累積≥=どこまで)。次=E-step。検証すべき核心=閾値独立PAVA後に各ランクの入れ子(生存関数)が保たれるか=Dykstra要否。

**次**: (a)殿が順序LRA E-step以降を書く, (b)荘島先生への返信(修正版報告書+Dykstraメモ+Algorithm_LRA添付・お礼)は未送, (c)develop一式commit未実施, (d)順序ランクラスタリング(16・自作)のPAVA化。

## 2026-07-18 LRA.ordinal 混在カテゴリ対応（案1・全面ラギッド化）→v1.16.0本採用・コミット

isotonic実装(develop/Dykstra_20260714.R)の前処理を12と共有する検討から派生し，先に`R/12_LRA_ordinal.R`を混在カテゴリ対応へ改修（殿が自筆・私は助言と検証）。CLAUDE.md技術負債「LRA.ordinal mixed category count support (requires matrix→list refactor)」を解決。

- **背景**: 12は各項目のカテゴリ数が等しい前提。混在だと`stop()`で弾いていた（→Biclustering.ordinalへ誘導）。原因は飽和/制約の参照行列・uuMat・null項が`nitems*max(ncat)`固定ストライドで，`sum(ncat)`ベースのdesign行列と混在時に次元衝突するため。
- **方針**: 全て`sum(ncat)`連結（パディングなし・項目ごとncat[j]詰め）に統一。住所録=design1（カテゴリ層sum(ncat)行）とdesignB1（境界層sum(ncat)-nitems行）を`ncat`確定直後に定義。
- **改修点(develop/LRA_ordinal_20260718.R→R/12へ移植)**: (1)design0/design1をncat直後に前倒し＋designB0/designB1新設，(2)`design1 <- cbind(design0 - ncat + 1, design0)`←旧`ncat[1]`の均一前提バグ修正（delete_rows 3箇所も`design0 - ncat + 1`へ統一），(3)uuMatを`sum(ncat)`列化・`UU[, j, 1:ncat[j]]`で列ブロック充填（旧`as.vector(t(UU[i,,]))`のNA混入も解消），(4)ref_mat・catRefMat_satu・refMat/refMat111/refMat000/catRefMatをdesign1/designB1オフセット＋RHS`1:ncat[i]`スライスで充填，(5)null_itemllを`matrix(unlist(catfreq),ncol=ncat)`（均一前提）から項目ごとsapplyへ，(6)`stop()`ガード撤去。
- **境界シフト**（`rbind(refMat111[-1,],..)`＋`[design0,]<-0`）はdesign0が各項目末尾を正しく指すのでラギッドのまま不変で正しい＝触らず。
- **検証**: (a)均一パリティ=本家12とJ15S3810(nrank=5,mic=T)でICBR/ICRP/Students/TRP/null&modelLL/CFI全て`max|diff|=0`，(b)混在=J15S3810の奇数項目を3→2併合(ncat 3/4交互)で本家はstop・12は完走，ICRP行数=sum(ncat)=52・項目ごと=ncat[j]・カテゴリ確率項目内和1・入れ子整合(null≤model≤飽和)・CFI/RMSEA有限。
- **飽和モデルmaxiter**: 混在で「Reached max iterations」が出るが飽和EMの収束の甘さ（元12からの性質・このデータで顕在化）で値は正しい方向。ラギッド化とは無関係。
- **dead code削除**: `refMat`・`refMat_satu`は代入のみで一度も参照されない完全な未使用変数（Mathematica移植の名残）。殿指示で削除実施。付随して`delete_rows`(3組)・`designB0`/`designB1`(refMat専用に新設していた境界住所録)も全てdeadになり除去。`refBox`はrefBox111/000の材料なので残す。削除後もMathematica参照値テスト全通過＝出力不変を確認。delete_rowsのsapply潜在バグ(混在で行列化)はこのdead codeにしか流れていなかった。
- **テスト**: `test-lra-ordinal.R`末尾に混在5テスト追加（データが実際混在・エラーなくLRAordinal返す・ICRP/ICBR行数ラギッド・確率項目内和1・入れ子LL整合）。全通過。
- **ドキュメント**: NEWS.md 1.16.0の## Improvementsに記載。CLAUDE.md技術負債を解決済みに更新。roxygen不変・man再生成不要・man内に均一制約記述なし。
- **コミット**: 14d0e7c（R/12+NEWS+CLAUDE+test）をmainへ。develop/はgitignore(未commit・研究スクラッチ)。

**次**: (a)Dykstra開発(順序LRA isotonic)へ復帰＝Dykstra_20260714.RのE-step以降，(b)method="isotonic"分岐は前処理共有(12のsum(ncat)前処理はそのまま流用可能に)，(c)荘島先生への返信(報告書+Dykstraメモ+Algorithm_LRA)は未送。
