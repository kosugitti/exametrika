library(exametrika)

# ================================================================
# J35S500: ordinal Biclustering (Ranklustering)
# 理論構造: 累積パターン, 5cls × 5fld × 5cat
# ================================================================

result_ord <- Biclustering(J35S500, ncls = 5, nfld = 5, method = "R", mic = TRUE, maxiter = 200)

test_that("ordinal Biclustering converges", {
  expect_true(result_ord$converge)
})

test_that("ordinal Biclustering FRP dimensions", {
  expect_equal(dim(result_ord$FRP), c(5, 5, 5))
})

test_that("ordinal Biclustering FRP probabilities sum to 1", {
  nfld <- dim(result_ord$FRP)[1]
  ncls <- dim(result_ord$FRP)[2]
  for (f in 1:nfld) {
    for (c in 1:ncls) {
      expect_equal(sum(result_ord$FRP[f, c, ]), 1.0, tolerance = 1e-10)
    }
  }
})

test_that("ordinal Biclustering expected scores in valid range", {
  nfld <- dim(result_ord$FRP)[1]
  ncls <- dim(result_ord$FRP)[2]
  maxQ <- dim(result_ord$FRP)[3]
  for (f in 1:nfld) {
    for (c in 1:ncls) {
      esp <- sum((1:maxQ) * result_ord$FRP[f, c, ])
      expect_gte(esp, 1.0)
      expect_lte(esp, maxQ)
    }
  }
})

test_that("ordinal Biclustering shows score differentiation per field", {
  # 各フィールドにおいて、クラス間で期待得点に差がある（max - min > 0.5）
  nfld <- dim(result_ord$FRP)[1]
  ncls <- dim(result_ord$FRP)[2]
  maxQ <- dim(result_ord$FRP)[3]
  for (f in 1:nfld) {
    esp <- numeric(ncls)
    for (c in 1:ncls) {
      esp[c] <- sum((1:maxQ) * result_ord$FRP[f, c, ])
    }
    expect_gt(max(esp) - min(esp), 0.5)
  }
})

test_that("ordinal Biclustering field/class counts", {
  expect_equal(sum(result_ord$LFD), 35)
  expect_equal(sum(result_ord$LCD), 500)
  expect_equal(length(result_ord$LFD), 5)
  expect_equal(length(result_ord$LCD), 5)
})

test_that("ordinal Biclustering fit indices", {
  expect_equal(result_ord$TestFitIndices$RMSEA, 0.0533156, tolerance = 1e-3)
  expect_equal(result_ord$TestFitIndices$AIC, 7287.635, tolerance = 1)
  expect_equal(result_ord$TestFitIndices$BIC, -66115.85, tolerance = 1)
})


# ================================================================
# J20S600: nominal Biclustering
# 理論構造: 循環パターン, 5cls × 4fld × 4cat
# ================================================================

result_nom <- Biclustering(J20S600, ncls = 5, nfld = 4, maxiter = 200)

test_that("nominal Biclustering converges", {
  expect_true(result_nom$converge)
})

test_that("nominal Biclustering FRP dimensions", {
  expect_equal(dim(result_nom$FRP), c(4, 5, 4))
})

test_that("nominal Biclustering FRP probabilities sum to 1", {
  nfld <- dim(result_nom$FRP)[1]
  ncls <- dim(result_nom$FRP)[2]
  for (f in 1:nfld) {
    for (c in 1:ncls) {
      expect_equal(sum(result_nom$FRP[f, c, ]), 1.0, tolerance = 1e-10)
    }
  }
})

test_that("nominal Biclustering field distribution is uniform", {
  # 理論値: 各フィールドに5項目ずつ
  expect_equal(result_nom$LFD, rep(5, 4))
})

test_that("nominal Biclustering class count sums to nobs", {
  expect_equal(sum(result_nom$LCD), 600)
  expect_equal(length(result_nom$LCD), 5)
})

test_that("nominal Biclustering shows cyclic mode pattern", {
  # 各フィールドで、クラス間の最頻カテゴリが多様（少なくとも3種類以上）
  nfld <- dim(result_nom$FRP)[1]
  ncls <- dim(result_nom$FRP)[2]
  for (f in 1:nfld) {
    modes <- integer(ncls)
    for (c in 1:ncls) {
      modes[c] <- which.max(result_nom$FRP[f, c, ])
    }
    expect_gte(length(unique(modes)), 3)
  }
})

test_that("nominal Biclustering fit indices", {
  # Nominal data has no benchmark model; chi-square based indices are NA
  expect_true(is.na(result_nom$TestFitIndices$bench_log_like))
  expect_true(is.na(result_nom$TestFitIndices$RMSEA))
  expect_true(is.na(result_nom$TestFitIndices$NFI))
  expect_true(is.na(result_nom$TestFitIndices$CFI))
  # Information criteria are computed directly from log-likelihood
  expect_equal(result_nom$TestFitIndices$AIC, 27943.98, tolerance = 1)
  expect_equal(result_nom$TestFitIndices$BIC, 28260.56, tolerance = 1)
})


# ================================================================
# ordinal Biclustering: FRPIndex テスト
# ================================================================

test_that("ordinal Biclustering FRPIndex exists and has correct dimensions", {
  expect_false(is.null(result_ord$FRPIndex))
  # FRPIndex: nfld rows × 6 columns (Alpha, A, Beta, B, Gamma, C)
  expect_equal(nrow(result_ord$FRPIndex), 5)
  expect_equal(ncol(result_ord$FRPIndex), 6)
  expect_equal(names(result_ord$FRPIndex), c("Alpha", "A", "Beta", "B", "Gamma", "C"))
})

test_that("ordinal Biclustering FRPIndex values are valid", {
  # Alpha: location of steepest ascent (integer, 0 to ncls-1)
  expect_true(all(result_ord$FRPIndex$Alpha >= 0))
  expect_true(all(result_ord$FRPIndex$Alpha < 5))
  # A: maximum slope (positive)
  expect_true(all(result_ord$FRPIndex$A > 0))
  # B: normalized expected score at Beta (0 to 1)
  expect_true(all(result_ord$FRPIndex$B >= 0))
  expect_true(all(result_ord$FRPIndex$B <= 1))
  # Gamma: non-monotonicity index (0 to 1)
  expect_true(all(result_ord$FRPIndex$Gamma >= 0))
  expect_true(all(result_ord$FRPIndex$Gamma <= 1))
})

test_that("ordinal Biclustering SOACflg exists", {
  expect_true(is.logical(result_ord$SOACflg))
})


# ================================================================
# ordinal Biclustering: プロットタイプテスト
# ================================================================

test_that("ordinal Biclustering FRP plot works", {
  expect_no_error(plot(result_ord, type = "FRP", nc = 3, nr = 2))
})

test_that("ordinal Biclustering FRP plot with stat=median works", {
  expect_no_error(plot(result_ord, type = "FRP", nc = 3, nr = 2, stat = "median"))
})

test_that("ordinal Biclustering FRP plot with stat=mode works", {
  expect_no_error(plot(result_ord, type = "FRP", nc = 3, nr = 2, stat = "mode"))
})

test_that("ordinal Biclustering FCRP plot (line) works", {
  expect_no_error(plot(result_ord, type = "FCRP", nc = 3, nr = 2))
})

test_that("ordinal Biclustering FCRP plot (bar) works", {
  expect_no_error(plot(result_ord, type = "FCRP", nc = 3, nr = 2, style = "bar"))
})

test_that("ordinal Biclustering FCBR plot works", {
  expect_no_error(plot(result_ord, type = "FCBR", nc = 3, nr = 2))
})

test_that("ordinal Biclustering ScoreField plot works", {
  expect_no_error(plot(result_ord, type = "ScoreField"))
})

test_that("ordinal Biclustering RRV plot works", {
  expect_no_error(plot(result_ord, type = "RRV"))
})

test_that("ordinal Biclustering RRV plot with stat=median works", {
  expect_no_error(plot(result_ord, type = "RRV", stat = "median"))
})

test_that("ordinal Biclustering LCD plot works", {
  expect_no_error(plot(result_ord, type = "LCD"))
})

test_that("ordinal Biclustering RMP plot works", {
  expect_no_error(plot(result_ord, type = "RMP", students = 1:3, nc = 3))
})


# ================================================================
# nominal Biclustering: プロットタイプテスト
# ================================================================

test_that("nominal Biclustering FRP plot works", {
  expect_no_error(plot(result_nom, type = "FRP", nc = 2, nr = 2))
})

test_that("nominal Biclustering FCRP plot (line) works", {
  expect_no_error(plot(result_nom, type = "FCRP", nc = 2, nr = 2))
})

test_that("nominal Biclustering FCRP plot (bar) works", {
  expect_no_error(plot(result_nom, type = "FCRP", nc = 2, nr = 2, style = "bar"))
})

test_that("nominal Biclustering ScoreField plot works", {
  expect_no_error(plot(result_nom, type = "ScoreField"))
})

test_that("nominal Biclustering RRV plot works", {
  expect_no_error(plot(result_nom, type = "RRV"))
})

test_that("nominal Biclustering LCD plot works", {
  expect_no_error(plot(result_nom, type = "LCD"))
})

test_that("nominal Biclustering FCBR is not allowed", {
  expect_error(plot(result_nom, type = "FCBR"))
})


# ================================================================
# J35S5000: rated Biclustering (Ranklustering)
# rated = nominal推定 + 正答率によるクラスソート + 二値/名義二層適合度
# ================================================================

result_rated <- Biclustering(J35S5000, ncls = 3, nfld = 3, method = "R", maxiter = 200)

test_that("rated Biclustering returns correct class", {
  expect_s3_class(result_rated, "exametrika")
  expect_true("ratedBiclustering" %in% class(result_rated))
})

test_that("rated Biclustering converges", {
  expect_true(result_rated$converge)
})

test_that("rated Biclustering FRP dimensions (nominal 3D array)", {
  expect_equal(length(dim(result_rated$FRP)), 3)
  expect_equal(dim(result_rated$FRP)[1], 3) # nfld
  expect_equal(dim(result_rated$FRP)[2], 3) # ncls
})

test_that("rated Biclustering FRP probabilities sum to 1", {
  nfld <- dim(result_rated$FRP)[1]
  ncls <- dim(result_rated$FRP)[2]
  for (f in 1:nfld) {
    for (c in 1:ncls) {
      expect_equal(sum(result_rated$FRP[f, c, ]), 1.0, tolerance = 1e-6)
    }
  }
})

test_that("rated Biclustering has both binary and nominal fit indices", {
  # Binary layer
  expect_false(is.null(result_rated$TestFitIndices))
  expect_s3_class(result_rated$TestFitIndices, "ModelFit")
  expect_false(is.na(result_rated$TestFitIndices$model_log_like))
  expect_false(is.na(result_rated$TestFitIndices$AIC))
  expect_false(is.na(result_rated$TestFitIndices$BIC))
  # Nominal layer
  expect_false(is.null(result_rated$TestFitIndices_nominal))
  expect_s3_class(result_rated$TestFitIndices_nominal, "ModelFit")
  expect_false(is.na(result_rated$TestFitIndices_nominal$AIC))
  expect_false(is.na(result_rated$TestFitIndices_nominal$BIC))
  # Nominal layer has no benchmark
  expect_true(is.na(result_rated$TestFitIndices_nominal$bench_log_like))
  expect_true(is.na(result_rated$TestFitIndices_nominal$RMSEA))
})

test_that("rated Biclustering field/class counts", {
  # LFD sum may exceed nitems when field memberships have ties
  expect_equal(sum(result_rated$LCD), 5000)
  expect_equal(length(result_rated$LFD), 3)
  expect_equal(length(result_rated$LCD), 3)
})

test_that("rated Biclustering classes are sorted by correct rate", {
  # Classes are sorted by correct rate; TRP should generally increase
  trp <- result_rated$TRP
  # Last class should have higher TRP than first
  expect_gt(trp[length(trp)], trp[1])
})

test_that("rated Biclustering has quasiFRP and FieldFRP", {
  expect_equal(dim(result_rated$quasiFRP), c(35, 3))
  # FieldFRP: some fields may be NA if empty
  expect_equal(nrow(result_rated$FieldFRP), 3)
  expect_equal(ncol(result_rated$FieldFRP), 3)
})

test_that("rated Biclustering quasiFRP values in [0, 1]", {
  valid <- result_rated$quasiFRP[!is.na(result_rated$quasiFRP)]
  expect_true(all(valid >= 0 & valid <= 1))
})

test_that("rated Biclustering has FRPIndex", {
  expect_false(is.null(result_rated$FRPIndex))
  expect_equal(ncol(result_rated$FRPIndex), 6)
  expect_equal(names(result_rated$FRPIndex), c("Alpha", "A", "Beta", "B", "Gamma", "C"))
})

test_that("rated Biclustering Students table has rank-up/down odds", {
  expect_true("Rank-Up Odds" %in% colnames(result_rated$Students))
  expect_true("Rank-Down Odds" %in% colnames(result_rated$Students))
  expect_true("Estimate" %in% colnames(result_rated$Students))
  expect_equal(nrow(result_rated$Students), 5000)
})

test_that("rated Biclustering has FieldAnalysis", {
  expect_false(is.null(result_rated$FieldAnalysis))
  expect_equal(nrow(result_rated$FieldAnalysis), 35)
  expect_true("CRR" %in% colnames(result_rated$FieldAnalysis))
  expect_true("LFE" %in% colnames(result_rated$FieldAnalysis))
})

test_that("rated Biclustering has SOAC/WOAC flags", {
  expect_true(is.logical(result_rated$SOACflg))
  expect_true(is.logical(result_rated$WOACflg))
})

test_that("rated Biclustering log_lik fields exist", {
  expect_false(is.na(result_rated$log_lik))
  expect_false(is.na(result_rated$log_lik_nominal))
  # Binary log_lik should be different from nominal
  expect_false(result_rated$log_lik == result_rated$log_lik_nominal)
})

test_that("rated Biclustering backward compatibility fields", {
  expect_equal(result_rated$Nclass, result_rated$n_class)
  expect_equal(result_rated$Nfield, result_rated$n_field)
  expect_equal(result_rated$N_Cycle, result_rated$n_cycle)
  expect_equal(result_rated$LogLik, result_rated$log_lik)
})

test_that("rated Biclustering print works", {
  expect_no_error(capture.output(print(result_rated)))
})


# ================================================================
# Confirmatory Biclustering (nominal / ordinal)
# ================================================================

# nominal: J20S600 = 20項目 → 4フィールド × 5項目
conf_nom <- rep(1:4, each = 5)
result_nom_conf <- Biclustering(J20S600,
  ncls = 5, nfld = 4,
  conf = conf_nom, verbose = FALSE
)

test_that("nominal confirmatory respects field assignments", {
  expect_equal(as.numeric(result_nom_conf$FieldEstimated), conf_nom)
})

test_that("nominal confirmatory rejects wrong-length conf vector", {
  expect_error(
    Biclustering(J20S600,
      ncls = 5, nfld = 4,
      conf = rep(1:4, each = 4), verbose = FALSE
    ),
    "conf vector size does NOT match"
  )
})

test_that("nominal confirmatory accepts membership matrix", {
  conf_mat <- matrix(0, nrow = 20, ncol = 4)
  for (i in seq_len(20)) conf_mat[i, conf_nom[i]] <- 1
  res <- Biclustering(J20S600,
    ncls = 5, nfld = 4,
    conf = conf_mat, verbose = FALSE
  )
  expect_equal(as.numeric(res$FieldEstimated), conf_nom)
})

test_that("nominal confirmatory rejects wrong-row matrix", {
  bad <- matrix(0, nrow = 18, ncol = 4)
  bad[cbind(seq_len(18), rep(1:4, length.out = 18))] <- 1
  expect_error(
    Biclustering(J20S600,
      ncls = 5, nfld = 4,
      conf = bad, verbose = FALSE
    ),
    "conf matrix size does NOT match"
  )
})

# ordinal: J35S500 = 35項目 → 5フィールド × 7項目
conf_ord <- c(rep(1, 7), rep(2, 7), rep(3, 7), rep(4, 7), rep(5, 7))
result_ord_conf <- Biclustering(J35S500,
  ncls = 5, nfld = 5, method = "R",
  conf = conf_ord, verbose = FALSE
)

test_that("ordinal confirmatory respects field assignments", {
  expect_equal(as.numeric(result_ord_conf$FieldEstimated), conf_ord)
})

test_that("ordinal confirmatory rejects wrong-length conf vector", {
  expect_error(
    Biclustering(J35S500,
      ncls = 5, nfld = 5, method = "R",
      conf = rep(1:5, each = 6), verbose = FALSE
    ),
    "conf vector size does NOT match"
  )
})


# ================================================================
# Class-side Confirmatory Biclustering (conf_class)
# ================================================================

# nominal: J20S600, 600 respondents -> 5 classes
nobs_nom <- NROW(dataFormat(J20S600)$Q)
conf_class_nom <- ((seq_len(nobs_nom) - 1) %% 5) + 1
result_nom_cc <- Biclustering(J20S600,
  ncls = 5, nfld = 4,
  conf_class = conf_class_nom, verbose = FALSE
)

test_that("nominal class-side confirmatory respects class assignments", {
  est <- apply(result_nom_cc$ClassMembership, 1, which.max)
  expect_equal(as.numeric(est), conf_class_nom)
})

test_that("nominal class-side confirmatory rejects wrong-length conf_class", {
  expect_error(
    Biclustering(J20S600,
      ncls = 5, nfld = 4,
      conf_class = 1:10, verbose = FALSE
    ),
    "conf_class vector size does NOT match"
  )
})

test_that("nominal class-side confirmatory accepts membership matrix", {
  cm <- matrix(0, nrow = nobs_nom, ncol = 5)
  for (i in seq_len(nobs_nom)) cm[i, conf_class_nom[i]] <- 1
  res <- Biclustering(J20S600,
    ncls = 5, nfld = 4,
    conf_class = cm, verbose = FALSE
  )
  est <- apply(res$ClassMembership, 1, which.max)
  expect_equal(as.numeric(est), conf_class_nom)
})

# ordinal: J35S500, 500 respondents -> 5 classes
nobs_ord <- NROW(dataFormat(J35S500)$Q)
conf_class_ord <- ((seq_len(nobs_ord) - 1) %% 5) + 1
result_ord_cc <- Biclustering(J35S500,
  ncls = 5, nfld = 5, method = "R",
  conf_class = conf_class_ord, verbose = FALSE
)

test_that("ordinal class-side confirmatory respects class assignments", {
  est <- apply(result_ord_cc$ClassMembership, 1, which.max)
  expect_equal(as.numeric(est), conf_class_ord)
})

test_that("ordinal Ranklustering with conf_class skips smoothing", {
  # When conf_class fixes the class memberships, smoothed_memb should equal
  # clsmemb (no neighbour averaging applied).
  expect_equal(
    as.numeric(result_ord_cc$SmoothedMembership),
    as.numeric(result_ord_cc$ClassMembership)
  )
})

# rated: J21S300, 300 respondents -> 4 classes (post-processing reorders classes)
nobs_rat <- NROW(dataFormat(J21S300)$Q)
conf_class_rat <- ((seq_len(nobs_rat) - 1) %% 4) + 1
result_rat_cc <- Biclustering(J21S300,
  ncls = 4, nfld = 3,
  conf_class = conf_class_rat, verbose = FALSE
)

test_that("rated class-side confirmatory keeps individuals in a single class (up to relabeling)", {
  # rated reorders classes by correct rate after estimation, so the input
  # labels do not necessarily match output labels. The invariant we test is
  # that each input class maps bijectively to one output class.
  est <- apply(result_rat_cc$ClassMembership, 1, which.max)
  cross <- table(conf_class_rat, est)
  # Every input class should land in exactly one output class.
  expect_true(all(rowSums(cross > 0) == 1))
  # And every output class should come from exactly one input class.
  expect_true(all(colSums(cross > 0) == 1))
})

# combined: conf + conf_class together (binary-style on ordinal data)
test_that("conf and conf_class can be combined (ordinal)", {
  conf_field_ord <- c(rep(1, 7), rep(2, 7), rep(3, 7), rep(4, 7), rep(5, 7))
  res <- Biclustering(J35S500,
    ncls = 5, nfld = 5, method = "R",
    conf = conf_field_ord,
    conf_class = conf_class_ord, verbose = FALSE
  )
  est <- apply(res$ClassMembership, 1, which.max)
  expect_equal(as.numeric(res$FieldEstimated), conf_field_ord)
  expect_equal(as.numeric(est), conf_class_ord)
})
