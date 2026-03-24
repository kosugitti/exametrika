# exametrika 日本語ガイド

> 日本語チュートリアルは
> [こちら](https://kosugitti.github.io/slides/BMS2025spring/docs/)
> もご参照ください。
>
> **注意**: CRANビルド時間短縮のため、計算負荷の高いコード例は
> `eval=FALSE` としています。完全な実行結果は
> [pkgdownサイト](https://kosugitti.github.io/exametrika/articles/guide-ja.html)
> をご覧ください。

## 概要

`exametrika`パッケージは、教育テストデータを分析するための包括的なテストデータエンジニアリングツールを提供します。荘島（2022）で説明されている方法に基づき、このパッケージは研究者と実務家に以下の機能を提供します：

- テスト応答パターンと項目特性の分析
- 様々な心理測定モデルを用いた受験者の分類
- テストデータにおける潜在構造の調査
- 項目間の局所依存性の検討
- 項目関係のネットワーク分析

## 機能

本パッケージは以下の心理測定モデルと技法を実装しています：

#### 古典的手法

- 古典的テスト理論（CTT）
  - 項目の難易度と識別力
  - テストの信頼性と妥当性
- 項目反応理論（IRT）
  - 2PL、3PL、4PLモデル
  - 項目特性曲線
  - テスト情報関数

#### 潜在構造分析

- 潜在クラス分析（LCA）
  - クラスメンバーシップの推定
  - 項目応答プロファイル
- 潜在ランク分析（LRA）
  - 順序付き潜在クラス
  - ランク遷移確率
- バイクラスタリングとランククラスタリング
  - 項目と受験者の同時クラスタリング
  - フィールド固有の応答パターン
- 無限関係モデル（IRM）
  - 最適なクラス/フィールド数の決定
  - ノンパラメトリッククラスタリング

## モデルの概要

### 局所依存モデル

本パッケージは、テストデータにおける局所依存性をモデル化する3つの相補的なアプローチを実装しています：

1.  **局所依存潜在ランク分析（LDLRA）**
    - 項目間の依存関係が異なる習熟度ランクでどのように変化するかを分析
    - 項目間の関係が学習者の能力レベルによって変化すると予想される場合に適している
    - LRAとベイジアンネットワークの長所を組み合わせたモデル
2.  **局所依存バイクラスタリング（LDB）**
    - 各ランク内における項目フィールド間の関係に焦点
    - 項目が自然なグループ（フィールド）を形成し、階層的な関係を持つ場合に最適
    - バイクラスタリングとフィールドレベルの依存構造を統合
3.  **バイクラスターネットワークモデル（BINET）**
    - 各フィールド内でのクラス遷移を検討
    - 複雑なクラス進行パターンの理解に最適
    - バイクラスタリングとクラスレベルのネットワーク分析を組み合わせたモデル

### モデル選択ガイド

| モデル | 主な焦点               | 適用場面                                       |
|--------|------------------------|------------------------------------------------|
| LDLRA  | 項目レベルの依存関係   | 項目関係が習熟度によって変化する場合           |
| LDB    | フィールドレベルの構造 | 項目が自然なグループを形成し依存関係を持つ場合 |
| BINET  | クラス進行             | フィールド内に複雑な学習パターンが存在する場合 |

## インストール

``` r
# CRANからインストール
install.packages("exametrika")

# または開発版をGitHubからインストール
if (!require("devtools")) install.packages("devtools")
devtools::install_github("kosugitti/exametrika")
```

### 依存パッケージ

本パッケージには以下が必要です：

- R (\>= 4.1.0)
- igraph (ネットワーク分析用)
- その他の依存パッケージは自動的にインストールされます

## データ形式と使用方法

### 基本的な使用方法

``` r
library(exametrika)
```

### データ要件

Exametrikaは2値データと多値データの両方に対応しています：

- 2値データ（0/1）
  - 0：誤答
  - 1：正答
- 多値データ
  - 順序のある反応カテゴリ
  - 複数の得点レベル
- 欠測値
  - NA値がサポートされています
  - カスタムの欠測値コードを指定可能

### データフォーマット

`dataFormat`関数は分析用の入力データを前処理します：

``` r
data <- dataFormat(J15S500)
str(data)
#> List of 7
#>  $ ID           : chr [1:500] "Student001" "Student002" "Student003" "Student004" ...
#>  $ ItemLabel    : chr [1:15] "Item01" "Item02" "Item03" "Item04" ...
#>  $ Z            : num [1:500, 1:15] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:15] "Item01" "Item02" "Item03" "Item04" ...
#>  $ w            : num [1:15] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ response.type: chr "binary"
#>  $ categories   : Named int [1:15] 2 2 2 2 2 2 2 2 2 2 ...
#>   ..- attr(*, "names")= chr [1:15] "Item01" "Item02" "Item03" "Item04" ...
#>  $ U            : num [1:500, 1:15] 0 1 1 1 1 1 0 0 1 1 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:15] "Item01" "Item02" "Item03" "Item04" ...
#>  - attr(*, "class")= chr [1:2] "exametrika" "exametrikaData"
```

### サンプルデータセット

本パッケージには、荘島（2022）からの様々なサンプルデータセットが学習用に含まれています：

- **命名規則**：JxxSxxx形式（J：項目数、S：サンプルサイズ）

| データセット | 項目数 | 受験者数 | 種別        | 用途                  |
|--------------|-------:|---------:|-------------|-----------------------|
| J5S10        |      5 |       10 | 2値         | クイックテスト        |
| J5S1000      |      5 |    1,000 | 順序        | GRM                   |
| J12S5000     |     12 |    5,000 | 2値         | LDLRA                 |
| J15S500      |     15 |      500 | 2値         | IRT, LCA              |
| J15S3810     |     15 |    3,810 | 順序(4件法) | 順序LRA               |
| J20S400      |     20 |      400 | 2値         | BNM                   |
| J20S600      |     20 |      600 | 名義(4cat)  | 名義Biclustering      |
| J35S500      |     35 |      500 | 順序(5件法) | 順序Biclustering      |
| J35S515      |     35 |      515 | 2値         | Biclustering, Network |
| J35S5000     |     35 |    5,000 | 多肢選択    | 評定LRA               |

## 使用例

### テスト統計量

``` r
TestStatistics(J15S500)
#> Test Statistics
#>                   value
#> TestLength   15.0000000
#> SampleSize  500.0000000
#> Mean          9.6640000
#> SEofMean      0.1190738
#> Variance      7.0892826
#> SD            2.6625707
#> Skewness     -0.4116220
#> Kurtosis     -0.4471624
#> Min           2.0000000
#> Max          15.0000000
#> Range        13.0000000
#> Q1.25%        8.0000000
#> Median.50%   10.0000000
#> Q3.75%       12.0000000
#> IQR           4.0000000
#> Stanine.4%    5.0000000
#> Stanine.11%   6.0000000
#> Stanine.23%   7.0000000
#> Stanine.40%   9.0000000
#> Stanine.60%  11.0000000
#> Stanine.77%  12.0000000
#> Stanine.89%  13.0000000
#> Stanine.96%  14.0000000
```

### 項目統計量

``` r
ItemStatistics(J15S500)
#> Item Statistics
#>    ItemLabel  NR   CRR  ODDs Threshold Entropy ITCrr
#> 1     Item01 500 0.746 2.937    -0.662   0.818 0.375
#> 2     Item02 500 0.754 3.065    -0.687   0.805 0.393
#> 3     Item03 500 0.726 2.650    -0.601   0.847 0.321
#> 4     Item04 500 0.776 3.464    -0.759   0.767 0.503
#> 5     Item05 500 0.804 4.102    -0.856   0.714 0.329
#> 6     Item06 500 0.864 6.353    -1.098   0.574 0.377
#> 7     Item07 500 0.716 2.521    -0.571   0.861 0.483
#> 8     Item08 500 0.588 1.427    -0.222   0.978 0.405
#> 9     Item09 500 0.364 0.572     0.348   0.946 0.225
#> 10    Item10 500 0.662 1.959    -0.418   0.923 0.314
#> 11    Item11 500 0.286 0.401     0.565   0.863 0.455
#> 12    Item12 500 0.274 0.377     0.601   0.847 0.468
#> 13    Item13 500 0.634 1.732    -0.342   0.948 0.471
#> 14    Item14 500 0.764 3.237    -0.719   0.788 0.485
#> 15    Item15 500 0.706 2.401    -0.542   0.874 0.413
```

### CTT

``` r
CTT(J15S500)
#> Realiability
#>                 name value
#> 1  Alpha(Covariance) 0.625
#> 2         Alpha(Phi) 0.630
#> 3 Alpha(Tetrachoric) 0.771
#> 4  Omega(Covariance) 0.632
#> 5         Omega(Phi) 0.637
#> 6 Omega(Tetrachoric) 0.779
#> 
#> Reliability Excluding Item
#>    IfDeleted Alpha.Covariance Alpha.Phi Alpha.Tetrachoric
#> 1     Item01            0.613     0.618             0.762
#> 2     Item02            0.609     0.615             0.759
#> 3     Item03            0.622     0.628             0.770
#> 4     Item04            0.590     0.595             0.742
#> 5     Item05            0.617     0.624             0.766
#> 6     Item06            0.608     0.613             0.754
#> 7     Item07            0.594     0.600             0.748
#> 8     Item08            0.611     0.616             0.762
#> 9     Item09            0.642     0.645             0.785
#> 10    Item10            0.626     0.630             0.773
#> 11    Item11            0.599     0.606             0.751
#> 12    Item12            0.597     0.603             0.748
#> 13    Item13            0.597     0.604             0.753
#> 14    Item14            0.593     0.598             0.745
#> 15    Item15            0.607     0.612             0.759
```

### IRT

IRT関数は、ロジスティックモデルを用いてパラメータを推定します。`model`オプションで指定でき、2PL、3PL、4PLモデルに対応しています。

``` r
result.IRT <- IRT(J15S500, model = 3)
result.IRT
```

``` r
head(result.IRT$ability)
```

``` r
plot(result.IRT, type = "IRF", items = 1:6, nc = 2, nr = 3)
plot(result.IRT, type = "IRF", overlay = TRUE)
plot(result.IRT, type = "IIC", items = 1:6, nc = 2, nr = 3)
plot(result.IRT, type = "TRF")
plot(result.IRT, type = "TIC")
```

### GRM

Graded Response
Model（Samejima,1969）はIRTを多値反応モデルに展開したものです。

``` r
result.GRM <- GRM(J5S1000)
result.GRM
```

``` r
plot(result.GRM, type = "IRF", nc = 2)
plot(result.GRM, type = "IIF", nc = 2)
plot(result.GRM, type = "TIF")
```

### LCA

潜在クラス分析では、データセットとクラス数の指定が必要です。

``` r
result.LCA <- LCA(J15S500, ncls = 5)
head(result.LCA$Students)
```

``` r
plot(result.LCA, type = "IRP", items = 1:6, nc = 2, nr = 3)
plot(result.LCA, type = "CMP", students = 1:9, nc = 3, nr = 3)
plot(result.LCA, type = "TRP")
plot(result.LCA, type = "LCD")
```

### LRA

潜在ランク分析では、データセットとランク数の指定が必要です。

``` r
result.LRA <- LRA(J15S500, nrank = 6)
head(result.LRA$Students)
```

``` r
plot(result.LRA, type = "IRP", items = 1:6, nc = 2, nr = 3)
plot(result.LRA, type = "RMP", students = 1:9, nc = 3, nr = 3)
plot(result.LRA, type = "TRP")
plot(result.LRA, type = "LRD")
```

### LRA 順序尺度データへの適用

``` r
result.LRAord <- LRA(J15S3810, nrank = 3, mic = TRUE)
```

``` r
plot(result.LRAord, type = "ScoreFreq")
plot(result.LRAord, type = "ScoreRank")
```

``` r
plot(result.LRAord, type = "ICBR", items = 1:4, nc = 2, nr = 2)
plot(result.LRAord, type = "ICRP", items = 1:4, nc = 2, nr = 2)
```

``` r
plot(result.LRAord, type = "RMP", students = 1:9, nc = 3, nr = 3)
```

### LRA 名義尺度データへの適用

``` r
result.LRArated <- LRA(J35S5000, nrank = 10, mic = TRUE)
```

``` r
plot(result.LRArated, type = "ScoreFreq")
plot(result.LRArated, type = "ScoreRank")
plot(result.LRArated, type = "ICRP", items = 1:4, nc = 2, nr = 2)
```

### バイクラスタリング

``` r
Biclustering(J35S515, nfld = 5, ncls = 6, method = "B")
```

``` r
result.Ranklustering <- Biclustering(J35S515, nfld = 5, ncls = 6, method = "R")
plot(result.Ranklustering, type = "Array")
plot(result.Ranklustering, type = "FRP", nc = 2, nr = 3)
plot(result.Ranklustering, type = "RRV")
plot(result.Ranklustering, type = "RMP", students = 1:9, nc = 3, nr = 3)
plot(result.Ranklustering, type = "LRD")
```

### グリッドサーチ

``` r
result <- GridSearch(J35S515, method = "R", max_ncls = 10, max_nfld = 10, index = "BIC")
result$optimal_ncls
result$optimal_nfld
plot(result$optimal_result, type = "Array")
```

### 無限関係モデル

``` r
result.IRM <- Biclustering_IRM(J35S515, gamma_c = 1, gamma_f = 1, verbose = TRUE)
plot(result.IRM, type = "Array")
plot(result.IRM, type = "FRP", nc = 3)
plot(result.IRM, type = "TRP")
```

### 多段階反応データ用のバイクラスタリング

#### 順序尺度データ

``` r
result.B.ord <- Biclustering(J35S500, ncls = 5, nfld = 5, method = "R")
result.B.ord
plot(result.B.ord, type = "Array")
plot(result.B.ord, type = "FRP", nc = 3, nr = 2)
plot(result.B.ord, type = "FCRP", nc = 3, nr = 2)
plot(result.B.ord, type = "FCRP", style = "bar", nc = 3, nr = 2)
plot(result.B.ord, type = "FCBR", nc = 3, nr = 2)
plot(result.B.ord, type = "ScoreField")
plot(result.B.ord, type = "RRV")
```

#### 名義尺度データ

``` r
result.B.nom <- Biclustering(J20S600, ncls = 5, nfld = 4)
result.B.nom
plot(result.B.nom, type = "Array")
plot(result.B.nom, type = "FRP", nc = 2, nr = 2)
plot(result.B.nom, type = "FCRP", nc = 2, nr = 2)
plot(result.B.nom, type = "FCRP", style = "bar", nc = 2, nr = 2)
plot(result.B.nom, type = "ScoreField")
plot(result.B.nom, type = "RRV")
```

### ベイジアンネットワークモデル

``` r
library(igraph)
DAG <- matrix(
  c(
    "Item01", "Item02", "Item02", "Item03", "Item02", "Item04",
    "Item03", "Item05", "Item04", "Item05"
  ),
  ncol = 2, byrow = TRUE
)
g <- igraph::graph_from_data_frame(DAG)
adj_mat <- as.matrix(igraph::get.adjacency(g))
```

``` r
result.BNM <- BNM(J5S10, adj_matrix = adj_mat)
result.BNM
```

#### 遺伝的アルゴリズムによる構造学習

``` r
BNM_GA(J5S10,
  population = 20, Rs = 0.5, Rm = 0.002, maxParents = 2,
  maxGeneration = 100, crossover = 2, elitism = 2
)
```

#### PBILによる構造学習

``` r
BNM_PBIL(J5S10,
  population = 20, Rs = 0.5, Rm = 0.005, maxParents = 2,
  alpha = 0.05, estimate = 4
)
```

### 局所依存潜在ランク分析

``` r
DAG_dat <- matrix(c(
  "From", "To", "Rank",
  "Item01", "Item02", 1, "Item04", "Item05", 1,
  "Item01", "Item02", 2, "Item02", "Item03", 2,
  "Item04", "Item05", 2, "Item08", "Item09", 2,
  "Item08", "Item10", 2, "Item09", "Item10", 2, "Item08", "Item11", 2,
  "Item01", "Item02", 3, "Item02", "Item03", 3,
  "Item04", "Item05", 3, "Item08", "Item09", 3,
  "Item08", "Item10", 3, "Item09", "Item10", 3, "Item08", "Item11", 3,
  "Item02", "Item03", 4, "Item04", "Item06", 4,
  "Item04", "Item07", 4, "Item05", "Item06", 4,
  "Item05", "Item07", 4, "Item08", "Item10", 4,
  "Item08", "Item11", 4, "Item09", "Item11", 4,
  "Item02", "Item03", 5, "Item04", "Item06", 5,
  "Item04", "Item07", 5, "Item05", "Item06", 5,
  "Item05", "Item07", 5, "Item09", "Item11", 5,
  "Item10", "Item11", 5, "Item10", "Item12", 5
), ncol = 3, byrow = TRUE)

edgeFile <- tempfile(fileext = ".csv")
write.csv(DAG_dat, edgeFile, row.names = FALSE, quote = TRUE)
```

``` r
result.LDLRA <- LDLRA(J12S5000, ncls = 5, adj_file = edgeFile)
result.LDLRA
```

``` r
plot(result.LDLRA, type = "IRP", nc = 4, nr = 3)
plot(result.LDLRA, type = "TRP")
plot(result.LDLRA, type = "LRD")
```

### 局所依存バイクラスタリング

``` r
conf <- c(
  1, 6, 6, 8, 9, 9, 4, 7, 7, 7, 5, 8, 9, 10, 10,
  9, 9, 10, 10, 10, 2, 2, 3, 3, 5, 5, 6, 9, 9, 10,
  1, 1, 7, 9, 10
)

edges_data <- data.frame(
  "From Field (Parent) >>>" = c(6, 4, 5, 1, 1, 4, 3, 4, 6, 2, 4, 4, 3, 6, 4, 1, 7, 9, 6, 7),
  ">>> To Field (Child)" = c(8, 7, 8, 7, 2, 5, 5, 8, 8, 4, 6, 7, 5, 8, 5, 8, 10, 10, 8, 9),
  "At Class/Rank (Locus)" = c(2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5)
)

edgeFile <- tempfile(fileext = ".csv")
write.csv(edges_data, file = edgeFile, row.names = FALSE)
```

``` r
result.LDB <- LDB(U = J35S515, ncls = 5, conf = conf, adj_file = edgeFile)
result.LDB
```

``` r
plot(result.LDB, type = "Array")
plot(result.LDB, type = "TRP")
plot(result.LDB, type = "LRD")
plot(result.LDB, type = "RMP", students = 1:9, nc = 3, nr = 3)
plot(result.LDB, type = "FRP", nc = 3, nr = 2)
plot(result.LDB, type = "FieldPIRP")
```

### バイクラスターネットワークモデル

``` r
conf <- c(
  1, 5, 5, 5, 9, 9, 6, 6, 6, 6, 2, 7, 7, 11, 11,
  7, 7, 12, 12, 12, 2, 2, 3, 3, 4, 4, 4, 8, 8, 12,
  1, 1, 6, 10, 10
)

edges_data <- data.frame(
  "From Class (Parent) >>>" = c(1, 2, 3, 4, 5, 7, 2, 4, 6, 8, 10, 6, 6, 11, 8, 9, 12),
  ">>> To Class (Child)" = c(2, 4, 5, 5, 6, 11, 3, 7, 9, 12, 12, 10, 8, 12, 12, 11, 13),
  "At Field (Locus)" = c(1, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 7, 8, 8, 9, 9, 12)
)

edgeFile <- tempfile(fileext = ".csv")
write.csv(edges_data, file = edgeFile, row.names = FALSE)
```

``` r
result.BINET <- BINET(
  U = J35S515, ncls = 13, nfld = 12,
  conf = conf, adj_file = edgeFile
)
print(result.BINET)
```

``` r
plot(result.BINET, type = "Array")
plot(result.BINET, type = "TRP")
plot(result.BINET, type = "LRD")
plot(result.BINET, type = "RMP", students = 1:9, nc = 3, nr = 3)
plot(result.BINET, type = "FRP", nc = 3, nr = 2)
plot(result.BINET, type = "LDPSR", nc = 3, nr = 2)
```

## 参考文献

Shojima, K. (2022) *Test Data Engineering: Latent Rank Analysis,
Biclustering, and Bayesian Network* (Behaviormetrics: Quantitative
Approaches to Human Behavior, 13), Springer.
