#' @title IRP/FRP プロファイル形状指標（Kumagai, 2007）
#' @description
#' IRP（Item Reference Profile）またはFRP（Field Reference Profile）の形状を
#' 定量的に記述する6つの指標を計算する。二値・多値 Biclustering 共通で利用。
#'
#' ## 入力行列の仕様
#' - 行: フィールド（またはアイテム）、列: 潜在クラス/ランク
#' - 各セルの値は \[0, 1\] の範囲（確率または正規化された期待得点）
#'
#' ## 6つの指標の定義
#'
#' ### 位置パラメータ（Location parameters）
#' - **Beta**: 値が0.5に最も近いクラス番号。プロファイルの「中心」位置を示す。
#'   `Beta_f = argmin_c |FRP(f,c) - 0.5|`
#' - **B**: Beta位置でのプロファイル値。B ≈ 0.5 なら理想的な位置推定。
#'   `B_f = FRP(f, Beta_f)`
#'
#' ### 傾斜パラメータ（Slope parameters）
#' - **Alpha**: 隣接クラス間の差分（ラグ）が最大となるクラス遷移の位置。
#'   最も急激に上昇する箇所を示す。`Alpha_f = argmax_c lag(f,c) - 1`
#'   （Alpha=1 はクラス1→2の遷移が最急であることを意味する）
#' - **A**: 最大ラグの値。プロファイルの最急傾斜の大きさ。
#'   `A_f = max_c lag(f,c)` ただし `lag(f,c) = FRP(f,c) - FRP(f,c-1)`
#'
#' ### 単調性指標（Monotonicity indices）
#' - **C**: 負のラグの合計。単調性の違反量を表す。
#'   C = 0 ならプロファイルは完全に単調増加。
#'   `C_f = Σ lag(f,c)` （lag < 0 の項のみ合計）
#' - **Gamma**: 非単調遷移の割合。C ≠ 0 のときのみ正値。
#'   `Gamma_f = (負のラグの個数 - 1) / (ncls - 1)`
#'
#' ## 順序整合条件（Ordinal Alignment Condition）との関係
#' - **SOAC**（Strongly Ordinal Alignment Condition）: すべてのフィールドで C = 0、
#'   かつ TRP が単調増加 → 全プロファイルが完全に単調
#' - **WOAC**（Weakly Ordinal Alignment Condition）: TRP が単調増加だが、
#'   一部のフィールドで C ≠ 0 → テスト全体では単調だが局所的に逆転あり
#'
#' ## 多値 Biclustering での利用
#' 多値データ（ordinal Biclustering）では、3次元カテゴリ確率配列 BCRM を
#' 期待得点に変換し \[0,1\] に正規化した行列を入力とする。
#' 詳細は \code{Biclustering.ordinal} のFRPIndex計算部分を参照。
#'
#' @param IRP 数値行列。行=フィールド（またはアイテム）、列=クラス/ランク。
#'   値は \[0,1\] の範囲。二値Biclusteringでは正答確率、
#'   多値Biclusteringでは正規化期待得点 `(E\[score\] - 1) / (maxQ - 1)` を渡す。
#' @return data.frame。行はフィールド、列は Alpha, A, Beta, B, Gamma, C の6指標。
#'
#' @references
#' Kumagai, K. (2007). IRP indices for understanding profile shapes.
#'
#' @noRd

IRPindex <- function(IRP) {
  # Beta: 値が0.5に最も近いクラス位置
  Beta <- apply(abs(IRP - 0.5), 1, which.min)
  NR <- NROW(IRP)
  NC <- NCOL(IRP)
  # B: Beta位置でのプロファイル値
  B <- IRP[cbind(1:NR, Beta)]
  A <- Alpha <- rep(NA, NR)
  C <- Gamma <- rep(0, NR)
  for (i in 1:NR) {
    vec <- IRP[i, ]
    # lag: 隣接クラス間の差分（c と c-1 の差）
    lags <- vec - c(NA, vec[1:(NC - 1)])
    # A: 最大ラグ（最急上昇幅）
    A[i] <- max(lags, na.rm = T)
    # Alpha: 最大ラグの位置（遷移元クラス番号）
    Alpha[i] <- which.max(lags) - 1
    # C: 負のラグの合計（単調性違反量、0なら完全単調）
    C[i] <- sum(lags[lags < 0], na.rm = T)
    if (C[i] != 0) {
      # Gamma: 非単調遷移の割合
      Gamma[i] <- (length(lags[lags < 0]) - 1) / (NC - 1)
    }
  }
  ret <- as.data.frame(cbind(Alpha, A, Beta, B, Gamma, C))
  return(ret)
}
