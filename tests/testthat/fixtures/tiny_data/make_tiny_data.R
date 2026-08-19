# 極小フィクスチャの生成
# ==================================================================
# ここで作る CSV は R と Mathematica の両方が読む。だから生成物をリポジトリに置き，
# 種を固定する。設計の経緯は develop/tiny_fixtures_refactor.md。
#
# 実行:  Rscript make_tiny_data.R   (このディレクトリで)
#
# **なぜ4本か。**1本で全部を賄おうとして行き詰まった。要求が互いに逆を向くため。
#
#   ・速く収束させたい   → 潜在構造の分離を強くする
#     しかし分離を強くすると項目対の 2x2 表に空セルが出て，テトラコリック相関が
#     境界(±1)へ飛んで不安定になる(実測: R 0.503 対 Mathematica 0.907)
#   ・平滑化(mic)を検査したい → 標本上に非単調が残っている必要がある
#     しかしそれは「構造を明確にして速く収束させる」と正反対
#
# だから役割で分ける。**mic はデータの属性ではなく当てはめ時の引数**で，同じデータを
# 2通りで当てはめて比べる。ただし標本が完全に単調だと両者が同じ答えを返し検査にならない。
#
# 満点・0点の回答者は**許容する**。当初は避けようとしたが，200名も居れば必ず出るうえ，
# 実データ(J15S500)にも満点が6名いて IRT の EAP は -2.2..1.7 に収まる。事前分布が効くので
# 発散しない。根拠のない制約だった。

# --- tinyCommon: IRT / Biclustering / Ranklustering -------------------------------
# クラス(順序ランク) x フィールドの二重構造。**LCA はここでは使わない**(下の tinyLCA)。
# 探索して sep=2.4 / 240名 / 15項目 / seed=80001 を採用。合計66周期。
#
# 飽和(参照行列のセルが 0 や 1 に張り付くこと)を**ゼロにはできない**。分離を弱めれば
# 飽和は減るがクラスが復元できなくなる。ただし飽和は実データでも起きる——J15S500 の
# 5クラス当てはめでも 75セル中4個(5%)が飽和する——ので，**割合を実データ並みに
# 抑える**方針にした(ここでは 45セル中3個・6.7%)。
set.seed(80001)
n <- 240
j <- 15
sep <- 2.4
cls <- rep(1:3, length.out = n)
fld <- rep(1:3, length.out = j)
# 項目ごとに難易度を揺らす。揺らさないと総得点の分布に山ができ，stanine(9段階)の
# 分位境界が重複して警告が出る。実データでは出ない警告なので，出さない形にする。
jit <- seq(-0.9, 0.9, length.out = j)
P <- plogis(outer(
  seq(-sep, sep, length.out = 3)[fld] + jit,
  seq(-sep, sep, length.out = 3), "+"
))
U <- matrix(rbinom(n * j, 1, t(P[, cls])), n, j)
stopifnot(!any(apply(U, 2, function(col) length(unique(col))) < 2))
common <- data.frame(ID = sprintf("S%03d", seq_len(n)), U, check.names = FALSE)
colnames(common) <- c("ID", sprintf("Item%02d", seq_len(j)))
write.csv(common, "tinyCommon.csv", row.names = FALSE, quote = FALSE)

# --- tinyLCA: LCA 専用 --------------------------------------------------------------
# tinyCommon では LCA の対数尤度が 1.2e-04 までしか合わなかった。原因は飽和したセルで，
# そこでは推定値ではなく対数の中の定数 exp(-testlength) がそのまま出る。
#
# LCA は**フィールド構造も順序も要らない**。その自由度を使い，項目を3群に分けて
# 「クラス k は群 k だけ得意」という名義的な構造にすると，全セルが [0.03, 0.97] に
# 収まって飽和が消える。200名 x 15項目 / seed=96002 で **8周期・クラス復元 ARI 0.99**，
# Mathematica と**対数尤度が完全一致**(IRP 4.7e-16)。
set.seed(96002)
nl <- 200
jl <- 15
cls_l <- rep(1:3, length.out = nl)
grp <- rep(1:3, length.out = jl)
# 群内で正答率に幅を持たせる。均一だと総得点が中央に固まり，異なる得点が9種に届かず
# stanine(9段階)が作れずに警告が出る。検査対象ではないが，警告は本物の警告を埋もれさせる。
wob <- seq(-0.08, 0.08, length.out = jl)
prof_l <- t(sapply(1:3, function(k) pmin(pmax(ifelse(grp == k, 0.90, 0.20) + wob, 0.03), 0.97)))
Ul <- matrix(rbinom(nl * jl, 1, prof_l[cls_l, ]), nl, jl)
stopifnot(
  !any(apply(Ul, 2, function(col) length(unique(col))) < 2),
  length(unique(rowSums(Ul))) >= 9
)
lca <- data.frame(ID = sprintf("S%03d", seq_len(nl)), Ul, check.names = FALSE)
colnames(lca) <- c("ID", sprintf("Item%02d", seq_len(jl)))
write.csv(lca, "tinyLCA.csv", row.names = FALSE, quote = FALSE)

# --- tinyMissing: 欠測処理の照合 ----------------------------------------------------
# tinyLCA に 5% の欠測を入れる。欠測があると R と Mathematica は別の局所解に落ちるので
# 当てはめ結果は比べられないが，**観測データから決まる量は機械精度で一致する**
# ——項目ごとの回答者数と正答率，ベンチマークモデルと帰無モデルの対数尤度。
# それがまさに欠測の扱いなので，ここで数値照合できる(2026-08-19 実測)。
set.seed(96002)
miss <- Ul
miss[matrix(runif(nl * jl) < 0.05, nl, jl)] <- -99
stopifnot(!any(apply(miss, 2, function(col) length(unique(col[col != -99]))) < 2))
missing <- data.frame(ID = sprintf("S%03d", seq_len(nl)), miss, check.names = FALSE)
colnames(missing) <- colnames(lca)
write.csv(missing, "tinyMissing.csv", row.names = FALSE, quote = FALSE)

# --- tinyCTT: CTT 専用 --------------------------------------------------------------
# テトラコリック相関と Omega が安定して推定できることが要件。**全ての項目対の 2x2 表に
# 十分な度数**が要る(空セルがあると相関が境界へ飛ぶ)。潜在クラス構造は不要で1因子で足りる。
# 探索して 250名 / 8項目 / 識別力 0.9 / 困難度 -0.6..0.6 / seed=60003 を採用。
#   最小セル度数 31 ・ テトラコリック相関は 0.2..0.6 に収まり境界に張り付かない
set.seed(60003)
nc <- 250
jc <- 8
th <- rnorm(nc)
bc <- seq(-0.6, 0.6, length.out = jc)
Uc <- matrix(rbinom(nc * jc, 1, plogis(0.9 * (th - rep(bc, each = nc)))), nc, jc)
stopifnot(!any(apply(Uc, 2, function(col) length(unique(col))) < 2))
ctt <- data.frame(ID = sprintf("S%03d", seq_len(nc)), Uc, check.names = FALSE)
colnames(ctt) <- c("ID", sprintf("Item%02d", seq_len(jc)))
write.csv(ctt, "tinyCTT.csv", row.names = FALSE, quote = FALSE)

# --- tinyRough: 平滑化(mic)の検査専用 -----------------------------------------------
# 真の構造は単調にしつつ，人数を絞って標本誤差で凹凸を残す。非単調量が最大になる
# 乱数列を選ぶ。これがないと mic = TRUE と FALSE が同じ答えを返し，検査にならない。
set.seed(20260819)
best <- NULL
for (trial in 1:40) {
  nr <- 120
  jr <- 10
  nrank <- 4
  rk <- rep(seq_len(nrank), length.out = nr)
  base <- seq(0.2, 0.8, length.out = jr)
  prof <- t(sapply(seq_len(nrank), function(r) plogis(qlogis(base) + (r - 2.5) * 0.7)))
  Ur <- matrix(rbinom(nr * jr, 1, prof[rk, ]), nr, jr)
  if (any(apply(Ur, 2, function(col) length(unique(col))) < 2)) next
  emp <- t(sapply(seq_len(nrank), function(r) colMeans(Ur[rk == r, , drop = FALSE])))
  viol <- sum(pmax(0, -apply(emp, 2, diff)))
  if (is.null(best) || viol > best$viol) best <- list(U = Ur, viol = viol)
}
rough <- data.frame(ID = sprintf("S%03d", seq_len(nrow(best$U))), best$U, check.names = FALSE)
colnames(rough) <- c("ID", sprintf("Item%02d", seq_len(ncol(best$U))))
write.csv(rough, "tinyRough.csv", row.names = FALSE, quote = FALSE)

cat(sprintf("tinyCommon  %3d x %2d\n", nrow(common), ncol(common) - 1))
cat(sprintf("tinyLCA     %3d x %2d\n", nrow(lca), ncol(lca) - 1))
cat(sprintf("tinyMissing %3d x %2d  欠測 %d セル\n", nrow(missing), ncol(missing) - 1, sum(miss == -99)))
cat(sprintf("tinyCTT     %3d x %2d\n", nrow(ctt), ncol(ctt) - 1))
cat(sprintf("tinyRough   %3d x %2d  非単調量 %.3f\n", nrow(rough), ncol(rough) - 1, best$viol))
