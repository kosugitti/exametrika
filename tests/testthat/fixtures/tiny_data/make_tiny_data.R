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

# --- tinyCTT: 二値・150名x10項目・欠測なし ---
# 記述統計と相関・信頼性係数なので潜在構造は要らないが，テトラコリック相関と
# Omega の推定が壊れないだけの分散と共通性は要る。1因子の IRT で生成する。
set.seed(20260819)
n <- 150
j <- 10
th <- rnorm(n)
b <- seq(-1.2, 1.2, length.out = j)
P <- 1 / (1 + exp(-outer(th, b, "-") * 1.2))
U <- matrix(rbinom(n * j, 1, P), n, j)
stopifnot(!any(apply(U, 2, function(c) length(unique(c)) < 2)))
out <- data.frame(ID = sprintf("S%03d", seq_len(n)), U, check.names = FALSE)
colnames(out) <- c("ID", sprintf("Item%02d", seq_len(j)))
write.csv(out, "tinyCTT.csv", row.names = FALSE, quote = FALSE)

# --- tinyLRA: 二値・160名x10項目・4ランク・欠測なし ---
# ランクが上がるほど各項目の正答率が単調に上がる構造(GTM の想定)を植える。
set.seed(20260819)
n <- 160
j <- 10
nrank <- 4
rk <- rep(seq_len(nrank), length.out = n)
base <- seq(0.15, 0.85, length.out = j)
prof <- t(sapply(seq_len(nrank), function(r) {
  plogis(qlogis(base) + (r - (nrank + 1) / 2) * 1.1)
}))
U <- matrix(rbinom(n * j, 1, prof[rk, ]), n, j)
stopifnot(!any(apply(U, 2, function(c) length(unique(c)) < 2)))
out <- data.frame(ID = sprintf("S%03d", seq_len(n)), U, check.names = FALSE)
colnames(out) <- c("ID", sprintf("Item%02d", seq_len(j)))
write.csv(out, "tinyLRA.csv", row.names = FALSE, quote = FALSE)

# --- tinyBicl: 二値・150名x12項目・3クラス x 3フィールド・欠測なし ---
# 人と項目の双方に構造を植える。クラスは順序的に並べるので Ranklustering も走る。
set.seed(20260819)
n <- 150
j <- 12
ncls <- 3
nfld <- 3
cls <- rep(seq_len(ncls), length.out = n)
fld <- rep(seq_len(nfld), length.out = j)
pi_fc <- rbind(
  c(0.20, 0.45, 0.70),
  c(0.35, 0.60, 0.85),
  c(0.10, 0.35, 0.60)
)
U <- matrix(rbinom(n * j, 1, t(pi_fc[fld, ][, cls])), n, j)
stopifnot(!any(apply(U, 2, function(c) length(unique(c)) < 2)))
out <- data.frame(ID = sprintf("S%03d", seq_len(n)), U, check.names = FALSE)
colnames(out) <- c("ID", sprintf("Item%02d", seq_len(j)))
write.csv(out, "tinyBicl.csv", row.names = FALSE, quote = FALSE)
