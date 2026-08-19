# 極小フィクスチャ用データの生成
# ------------------------------------------------------------------
# R と Mathematica の両方が読む同一の CSV を作る。形式は既存の J15S500.csv と同じ
# (1行目=項目ラベル・1列目=ID・欠測は -99)。種を固定してあるので何度でも同じものができる。
#
# 大きさの原則: その章の推定が安定する最小を実測で決める (tiny_fixtures_plan.md)。
# 真のクラス構造を植えて生成する。無構造の一様乱数だと実装間の一致が
# 局所解の偶然に左右されるため。

set.seed(20260819)

# --- tinyLCA: 二値・3クラス・8項目・120名・欠測5% ---
n <- 120
j <- 8
ncls <- 3
cls <- rep(seq_len(ncls), length.out = n)
# クラスごとの正答確率プロファイル(はっきり分離させる)
prof <- rbind(
  c(.9, .9, .8, .8, .7, .3, .2, .2),
  c(.5, .5, .5, .5, .5, .5, .5, .5),
  c(.2, .2, .3, .3, .4, .7, .8, .9)
)
U <- matrix(rbinom(n * j, 1, prof[cls, ]), n, j)
miss <- matrix(runif(n * j) < 0.05, n, j)
U[miss] <- -99
out <- data.frame(ID = sprintf("S%03d", seq_len(n)), U, check.names = FALSE)
colnames(out) <- c("ID", sprintf("Item%02d", seq_len(j)))
write.csv(out, "tinyLCA.csv", row.names = FALSE, quote = FALSE)
cat("tinyLCA.csv:", n, "x", j, " 欠測", sum(miss), "セル\n")

# --- tinyIRT: 二値・2PL の真値から生成・200名x10項目・欠測なし ---
# 困難度を -1.5..1.5 に広く配置して情報を確保する。IRT は LCA と違い，R と
# Mathematica の差はサイズではなく最適化の収束判定の違いで決まり，200名でも
# 500名の実データと同水準(LL 1e-3・パラメタ 1e-4)で一致する。
set.seed(20260819)
n <- 200
j <- 10
a <- runif(j, 0.8, 2.0)
b <- seq(-1.5, 1.5, length.out = j)
th <- rnorm(n)
P <- 1 / (1 + exp(-outer(th, b, "-") * rep(a, each = n)))
U <- matrix(rbinom(n * j, 1, P), n, j)
out <- data.frame(ID = sprintf("S%03d", seq_len(n)), U, check.names = FALSE)
colnames(out) <- c("ID", sprintf("Item%02d", seq_len(j)))
write.csv(out, "tinyIRT.csv", row.names = FALSE, quote = FALSE)
