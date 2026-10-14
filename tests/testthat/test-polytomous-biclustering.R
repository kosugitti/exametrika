library(exametrika)

# ================================================================
# J35S500: ordinal Biclustering (Ranklustering)
# Planted structure: cumulative pattern, 5 classes x 5 fields x 5 categories
# ================================================================

# Pinned to estimation = "GTM" and init = "legacy": these structural and
# fit-index snapshots track the filter-based path from the start used up to
# 2.2.0. The isotonic default and the new starts are exercised separately below.
result_ord <- Biclustering(J35S500, ncls = 5, nfld = 5, method = "R", estimation = "GTM", mic = TRUE, maxiter = 200, init = "legacy")

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
  # In every field the expected score differs across classes (max - min > 0.5)
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
  # Regression snapshots of the GTM path (no Mathematica reference exists for
  # ordinal Biclustering). Values changed in 2.2.0 when the reported
  # log-likelihood became the evidence lower bound (see NEWS); the EM is also
  # monitored on that quantity, so the stopping point moved slightly.
  expect_equal(result_ord$TestFitIndices$RMSEA, 0.0545661, tolerance = 1e-3)
  expect_equal(result_ord$TestFitIndices$AIC, 8460.093, tolerance = 1e-4)
  expect_equal(result_ord$TestFitIndices$BIC, -64943.395, tolerance = 1e-4)
  expect_equal(result_ord$log_lik, result_ord$elbo)
})


# ---- Isotonic (Fenchel-dual, stochastic-order) ordinal Ranklustering --------

result_ord_iso <- Biclustering(J35S500,
  ncls = 5, nfld = 5, method = "R",
  estimation = "isotonic", maxiter = 200
)

test_that("isotonic is the default estimation for ordinal Ranklustering", {
  res <- Biclustering(J35S500, ncls = 5, nfld = 5, method = "R", maxiter = 200)
  expect_equal(res$estimation, "isotonic")
})

test_that("estimation is ignored (NA) for plain ordinal Biclustering", {
  res <- Biclustering(J35S500, ncls = 5, nfld = 5, method = "B", maxiter = 200)
  expect_true(is.na(res$estimation))
})

test_that("isotonic ordinal Biclustering converges and FRP sums to 1", {
  expect_true(result_ord_iso$converge)
  expect_equal(dim(result_ord_iso$FRP), c(5, 5, 5))
  probsum <- apply(result_ord_iso$FRP, c(1, 2), sum)
  expect_true(all(abs(probsum - 1) < 1e-8))
})

test_that("isotonic ordinal Biclustering enforces stochastic order in every field", {
  # For each field, the upper-cumulative (boundary) probability at every
  # threshold must be non-decreasing across ranks -- this is the genuine
  # stochastic-order restriction the GTM path does not impose.
  nfld <- dim(result_ord_iso$FRP)[1]
  ncls <- dim(result_ord_iso$FRP)[2]
  maxQ <- dim(result_ord_iso$FRP)[3]
  worst <- 0
  for (f in 1:nfld) {
    S <- t(sapply(1:ncls, function(c) {
      rev(cumsum(rev(result_ord_iso$FRP[f, c, ])))[-1]
    })) # ncls x (maxQ-1): P(>= q) per rank
    worst <- max(worst, max(S[-ncls, , drop = FALSE] - S[-1, , drop = FALSE]))
  }
  # violations are bounded by the dual solver tolerance, not exactly zero
  expect_lt(worst, 1e-3)
})

test_that("isotonic ordinal expected scores are monotone across ranks", {
  nfld <- dim(result_ord_iso$FRP)[1]
  ncls <- dim(result_ord_iso$FRP)[2]
  maxQ <- dim(result_ord_iso$FRP)[3]
  for (f in 1:nfld) {
    esp <- sapply(1:ncls, function(c) sum((1:maxQ) * result_ord_iso$FRP[f, c, ]))
    expect_true(all(diff(esp) >= -1e-6))
  }
  # monotone profiles satisfy the ordinal alignment conditions
  expect_true(result_ord_iso$WOACflg)
})

test_that("isotonic ordinal estimation rejects unknown values", {
  expect_error(
    Biclustering(J35S500, ncls = 5, nfld = 5, method = "R", estimation = "bogus"),
    "should be one of"
  )
})


# ================================================================
# J20S600: nominal Biclustering
# Planted structure: cyclic pattern, 5 classes x 4 fields x 4 categories
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
  # Planted: five items per field
  expect_equal(result_nom$LFD, rep(5, 4))
})

test_that("nominal Biclustering class count sums to nobs", {
  expect_equal(sum(result_nom$LCD), 600)
  expect_equal(length(result_nom$LCD), 5)
})

test_that("nominal Biclustering shows cyclic mode pattern", {
  # In every field the modal category varies across classes (at least three kinds)
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
  # Information criteria are computed directly from the reported
  # log-likelihood, which is the evidence lower bound since 2.2.0 (see NEWS)
  expect_equal(result_nom$TestFitIndices$AIC, 29933.173, tolerance = 1e-4)
  expect_equal(result_nom$TestFitIndices$BIC, 30196.989, tolerance = 1e-4)
  expect_equal(result_nom$log_lik, result_nom$elbo)
})


# ================================================================
# ordinal Biclustering: FRPIndex tests
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
# ordinal Biclustering: plot type tests
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
# nominal Biclustering: plot type tests
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
# rated = nominal estimation + classes sorted by correct rate + two-layer (binary/nominal) fit
# ================================================================

# Rows are subset to keep the CRAN Windows check inside its time budget: 1.13.0
# was rejected at "Overall checktime 11 min > 10 min". Only rows are dropped, so
# the object keeps its class and its metadata (response.type, categories, CA),
# and every assertion below is structural -- class, convergence, dimensions,
# sums, finiteness -- rather than a comparison against reference numbers.
head_rows <- function(x, n) {
  x$ID <- x$ID[seq_len(n)]
  for (f in c("Q", "U", "Z")) {
    if (!is.null(x[[f]])) x[[f]] <- x[[f]][seq_len(n), , drop = FALSE]
  }
  return(x)
}

result_rated <- Biclustering(head_rows(J35S5000, 1000),
  ncls = 3, nfld = 3, method = "R", maxiter = 200
)

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
  expect_equal(sum(result_rated$LCD), nrow(result_rated$Q))
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
  expect_equal(nrow(result_rated$Students), nrow(result_rated$Q))
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
  # Removed in 2.0.0: $Nclass is gone, only $n_class remains
  expect_null(result_rated$Nclass)
  expect_false(is.null(result_rated$n_class))
  # Removed in 2.0.0: $Nfield is gone, only $n_field remains
  expect_null(result_rated$Nfield)
  expect_false(is.null(result_rated$n_field))
  # Removed in 2.0.0: $N_Cycle is gone, only $n_cycle remains
  expect_null(result_rated$N_Cycle)
  expect_false(is.null(result_rated$n_cycle))
  # Removed in 2.0.0: $LogLik is gone, only $log_lik remains
  expect_null(result_rated$LogLik)
  expect_false(is.null(result_rated$log_lik))
})

test_that("rated Biclustering print works", {
  expect_no_error(capture.output(print(result_rated)))
})


# ================================================================
# Confirmatory Biclustering (nominal / ordinal)
# ================================================================

# nominal: J20S600 = 20 items -> 4 fields x 5 items
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

# ordinal: J35S500 = 35 items -> 5 fields x 7 items
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


# ---- Starting values (init) ---------------------------------------------

test_that("init = 'auto' runs both data-based starts and keeps the larger ELBO", {
  fit <- Biclustering(J35S500, ncls = 5, nfld = 5, method = "B", init = "auto", nstart = 0)
  expect_equal(fit$init, "auto")
  expect_setequal(names(fit$init_elbo), c("quantile", "ward"))
  expect_equal(fit$init_selected, names(which.max(fit$init_elbo)))
  expect_equal(fit$elbo, max(fit$init_elbo))
})

test_that("random starts are screened, never lower the ELBO, and leave the RNG alone", {
  a <- Biclustering(J35S500, ncls = 6, nfld = 5, method = "B", nstart = 0)
  set.seed(42)
  r0 <- stats::runif(1)
  set.seed(42)
  b <- Biclustering(J35S500, ncls = 6, nfld = 5, method = "B", nstart = 10)
  expect_identical(stats::runif(1), r0)
  expect_length(b$init_elbo, 12)
  # the two data-based starts and the best 3 random starts run to convergence
  expect_equal(sum(!is.na(b$init_elbo)), 5)
  expect_true(all(is.finite(b$init_elbo[c("quantile", "ward")])))
  expect_gte(b$elbo, a$elbo - 1e-8)
  expect_equal(b$elbo, max(b$init_elbo, na.rm = TRUE))
  c2 <- Biclustering(J35S500, ncls = 6, nfld = 5, method = "B", nstart = 10)
  expect_equal(b$elbo, c2$elbo)
})

test_that("each init option runs a single start", {
  for (i in c("quantile", "ward", "legacy")) {
    fit <- Biclustering(J35S500, ncls = 5, nfld = 5, method = "R", init = i)
    expect_equal(names(fit$init_elbo), i)
    expect_equal(fit$elbo, unname(fit$init_elbo))
  }
  expect_error(Biclustering(J35S500, ncls = 5, nfld = 5, init = "random"))
})

test_that("the data-based starts beat the legacy start on J35S500", {
  # A regression check on this data set, not a general guarantee: the legacy
  # start stops at a local optimum here.
  for (arm in list(list(method = "B"), list(method = "R", estimation = "isotonic"))) {
    a <- do.call(Biclustering, c(list(J35S500, ncls = 5, nfld = 5, init = "auto"), arm))
    l <- do.call(Biclustering, c(list(J35S500, ncls = 5, nfld = 5, init = "legacy"), arm))
    expect_gt(a$elbo, l$elbo)
  }
})

test_that("the data-based starts do not depend on the order of the rows", {
  Q <- remap_category_codes(J35S500$Q)
  Z <- J35S500$Z
  perm <- rev(seq_len(NROW(Q)))
  for (g in list(bicl_init_class_quantile, bicl_init_class_ward)) {
    expect_identical(g(Q, Z, 5)[perm], g(Q[perm, ], Z[perm, ], 5))
  }
  a <- Biclustering(J35S500, ncls = 5, nfld = 5, method = "B")
  d <- J35S500
  d$Q <- d$Q[perm, ]
  d$Z <- d$Z[perm, ]
  d$ID <- d$ID[perm]
  b <- Biclustering(d, ncls = 5, nfld = 5, method = "B")
  expect_equal(a$elbo, b$elbo, tolerance = 1e-8)
  expect_equal(unname(a$ClassEstimated[perm]), unname(b$ClassEstimated))
})

test_that("the weighted Ward start matches respondent-level Ward on untied scores", {
  set.seed(1)
  v <- stats::rnorm(200)
  h <- stats::hclust(stats::dist(v), method = "ward.D2")
  for (k in c(3, 6)) {
    expect_identical(
      bicl_init_class_ward(matrix(v), matrix(1, 200, 1), k),
      bicl_init_relabel(as.integer(stats::cutree(h, k)), v)
    )
  }
})

test_that("init keeps the given fields (conf) and classes (conf_class)", {
  conf <- rep(1:5, each = 7)
  fit <- Biclustering(J35S500, ncls = 5, conf = conf, method = "B", init = "auto")
  expect_equal(unname(fit$FieldEstimated), conf)
  cc <- rep(1:5, length.out = NROW(J35S500$Q))
  fit <- Biclustering(J35S500, nfld = 5, conf_class = cc, method = "B", init = "auto")
  expect_equal(names(fit$init_elbo), "conf_class")
  expect_equal(unname(fit$ClassEstimated), cc)
})

test_that("the data-based starts handle missing responses", {
  d <- J35S500
  set.seed(2)
  miss <- matrix(stats::runif(length(d$Q)) < 0.05, nrow = NROW(d$Q))
  d$Q[miss] <- -1
  d$Z[miss] <- 0
  fit <- Biclustering(d, ncls = 5, nfld = 5, method = "R")
  expect_true(is.finite(fit$elbo))
  expect_true(all(is.finite(fit$init_elbo[c("quantile", "ward")])))
})

# ---- Category codes: shifted, never renumbered per item ------------------

test_that("biclustering keeps a category nobody chose in its slot", {
  d <- J35S500
  # item 1 loses its lowest category: recode its 1s as 2s
  q1 <- d$Q[, 1]
  q1[q1 == 1] <- 2
  d$Q[, 1] <- q1
  fit <- suppressWarnings(Biclustering(d, ncls = 4, nfld = 3, method = "B", nstart = 0))
  expect_equal(dim(fit$FRP)[3], max(J35S500$Q[J35S500$Z == 1]))
  # the stored codes of item 1 are not renumbered
  expect_equal(sort(unique(fit$Q[fit$Z[, 1] == 1, 1])), sort(unique(q1[d$Z[, 1] == 1])))
})

test_that("0-based codes are shifted by one for biclustering", {
  fit <- Biclustering(J15S3810, ncls = 3, nfld = 3, method = "B", nstart = 0)
  expect_equal(min(fit$Q[fit$Z == 1]), 1)
  expect_equal(dim(fit$FRP)[3], max(J15S3810$Q[J15S3810$Z == 1]) + 1)
})

test_that("item-level models warn about the categories they drop", {
  raw <- J35S500$Q
  raw[J35S500$Z == 0] <- NA
  raw[, 1][raw[, 1] %in% 2] <- 3
  d <- dataFormat(cbind(ID = J35S500$ID, as.data.frame(raw)), response.type = "ordinal")
  expect_warning(LRA(d, nrank = 3), "category 2")
  expect_warning(GRM(d), "category 2")
  expect_warning(LCA(d, ncls = 3), "category 2")
  # an item that merely has fewer categories is not reported
  expect_silent(warn_dropped_categories(matrix(c(1, 2, 3, 1, 2, 2), ncol = 2), c("a", "b"), "LRA"))
})

test_that("dataFormat codes factors by their declared levels", {
  lv <- c("lo", "mid", "hi")
  df <- data.frame(
    ID = 1:12,
    a = factor(rep(c("lo", "hi"), 6), levels = lv),
    b = factor(rep(c("lo", "mid", "hi"), 4), levels = lv)
  )
  d <- dataFormat(df, response.type = "ordinal")
  expect_equal(sort(unique(d$Q[, "a"])), c(1, 3))
  expect_equal(d$CategoryLabel[["a"]], c("lo", "hi"))
  expect_equal(d$CategoryLabel[["b"]], c("lo", "mid", "hi"))
})

test_that("isotonic Ranklustering satisfies SOAC, judged on the model TRP", {
  # The empirical profile dips between ranks 1 and 2 here; the model TRP does not
  fit <- Biclustering(J35S500, ncls = 8, nfld = 5, method = "R", nstart = 0, verbose = FALSE)
  expect_true(fit$SOACflg)
  expect_true(fit$WOACflg)
  expect_equal(unname(fit$TRP), unname(colSums(apply(fit$FRP, c(1, 2), function(p) sum(seq_along(p) * p)) * fit$LFD)))
  expect_length(fit$TRP_empirical, 8)
  expect_true(all(diff(fit$TRP) >= -1e-6 * 35 * 4))
})
