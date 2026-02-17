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
  expect_equal(result_nom$TestFitIndices$RMSEA, 0.04719482, tolerance = 1e-3)
  expect_equal(result_nom$TestFitIndices$AIC, 3990.147, tolerance = 1)
  expect_equal(result_nom$TestFitIndices$BIC, -48509.19, tolerance = 1)
})
