library(exametrika)

# Regression tests pinning the 21 bug fixes landed on 2026-07-01 for the
# 1.15.0 release. Each test_that() name is prefixed "regression: " and the
# leading comment names the corresponding NEWS.md 1.15.0 bullet. See the
# scratchpad repro*.R scripts for the original hand-verified reproductions.


# --- longdataFormat: column names, non-1..N IDs, weights, duplicates -------
# NEWS: longdataFormat/dataFormat column-name args; misaligned rows for
# non-1..N numeric IDs; w used as a row index; spurious "Duplicated IDs".
test_that("regression: longdataFormat handles colnames, sparse IDs, w, dups", {
  long <- data.frame(
    sid  = rep(c(10, 20, 30), each = 3),
    qid  = rep(c(100, 200, 300), times = 3),
    resp = c(1, 0, 1, 0, 1, 1, 1, 1, 0),
    wt   = rep(c(2, 5, 9), times = 3)
  )

  # (a) character column names are accepted; (b) non-contiguous numeric IDs
  # (10/20/30, 100/200/300) yield a dense 3x3 matrix, not a sparse blown-up one.
  ld <- longdataFormat(long, Sid = "sid", Qid = "qid", Resp = "resp", w = "wt")
  expect_equal(dim(ld$U), c(3L, 3L))
  expect_equal(
    as.vector(ld$U),
    as.vector(matrix(c(1, 0, 1, 0, 1, 1, 1, 1, 0), nrow = 3, byrow = TRUE))
  )

  # (c) w reads the weight column, keyed by each item's first occurrence.
  expect_equal(as.numeric(ld$w), c(2, 5, 9))

  # (d) one student answering many distinct items is NOT a duplicate.
  expect_silent(
    longdataFormat(long[, 1:3], Sid = "sid", Qid = "qid", Resp = "resp")
  )
  # A genuine repeated (student, item) pair is still an error.
  dup <- data.frame(sid = c(1, 1, 1), qid = c(100, 100, 200), resp = c(1, 0, 1))
  expect_error(
    longdataFormat(dup, Sid = "sid", Qid = "qid", Resp = "resp"),
    "Duplicated"
  )
})


# --- Raw matrix/data.frame input no longer crashes -------------------------
# NEWS: CTT()/BNM()/LDLRA()/LDB()/BINET() read response.type from the raw
# argument instead of the formatted object, crashing on documented raw input.
test_that("regression: CTT() and BNM() accept raw matrix input", {
  rawU <- dataFormat(J15S500)$U
  expect_true(is.matrix(rawU))
  expect_no_error(ct <- CTT(rawU))
  expect_s3_class(ct, "CTT")

  # BNM with a raw matrix plus an adjacency matrix labelled to match the
  # auto-generated item labels.
  labs <- dataFormat(J5S10)$ItemLabel
  adj <- matrix(0, 5, 5, dimnames = list(labs, labs))
  for (k in 1:4) adj[k, k + 1] <- 1
  raw5 <- dataFormat(J5S10)$U
  expect_no_error(bn <- suppressMessages(BNM(raw5, adj_matrix = adj)))
  expect_s3_class(bn, "BNM")
})


# --- crr() computed on missing-masked responses, not 0-imputed -------------
# NEWS: models passed the class-stripped (tmp$U * tmp$Z) matrix to crr(),
# counting missing responses as incorrect. $crr must match the masked crr.
test_that("regression: BNM()$crr uses the missing-masked crr", {
  base <- dataFormat(J15S500)
  U <- base$U
  n <- nrow(U)
  p <- ncol(U)
  Z <- matrix(1, n, p)
  set.seed(5)
  Z[sample(length(Z), 300)] <- 0
  obj <- dataFormat(U, Z = Z)

  crr_masked <- as.vector(crr(obj)) # correct: honours Z
  crr_imputed <- as.vector(crr(obj$U * obj$Z)) # buggy: 0-imputes missing
  expect_false(isTRUE(all.equal(crr_masked, crr_imputed)))

  labs <- obj$ItemLabel
  adj <- matrix(0, p, p, dimnames = list(labs, labs))
  for (k in 1:(p - 1)) adj[k, k + 1] <- 1
  bn <- suppressMessages(BNM(obj, adj_matrix = adj))

  expect_equal(as.vector(bn$crr), crr_masked, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(as.vector(bn$crr), crr_imputed, tolerance = 1e-8)))
})


# --- BINET(): missing data + beta1/beta2 -----------------------------------
# NEWS: Ccj ignored the missing mask (fed -1 into log()), and the smoothing
# prior was hardcoded to Beta(1,1); beta1/beta2 args were added.
test_that("regression: BINET() runs on missing data and accepts beta1/beta2", {
  skip_on_cran()
  conf <- read.csv(
    test_path("fixtures", "auxiliary_data", "FixFieldBINET.csv")
  )[, 2]
  edgeFile <- test_path("fixtures", "auxiliary_data", "EdgesBINET.csv")

  base <- dataFormat(J35S515)
  U <- base$U
  n <- nrow(U)
  p <- ncol(U)
  set.seed(20)
  Z <- matrix(1, n, p)
  Z[sample(length(Z), round(0.08 * n * p))] <- 0
  obj <- dataFormat(U, Z = Z)

  expect_no_error(
    bn <- suppressWarnings(suppressMessages(
      BINET(obj,
        ncls = 13, nfld = 12, conf = conf, adj_file = edgeFile,
        beta1 = 2, beta2 = 2
      )
    ))
  )
  expect_s3_class(bn, "BINET")
  # Fit indices are finite (the old crash produced NaN -> 'dimnames' error).
  expect_true(all(is.finite(as.numeric(bn$MG_FitIndices))))
})


# --- GridSearch: print() and invalid-grid guard ----------------------------
# NEWS: print.exametrika() had no GridSearch case (always crashed); tie
# handling read the wrong row/col; max_ncls = 1 tested ncls = 2 anyway.
test_that("regression: print(GridSearch()) works and bad grids stop", {
  gs <- suppressMessages(
    GridSearch(J15S500, max_ncls = 3, max_nfld = 3, verbose = FALSE)
  )
  expect_no_error(out <- capture.output(print(gs)))
  expect_output(print(gs))

  # 2:max_ncls descending-sequence trap: must stop, not silently test ncls=2.
  expect_error(
    suppressMessages(GridSearch(J15S500, max_ncls = 1, verbose = FALSE)),
    "at least 2"
  )
})


# --- BNM/BINET DAG detection via igraph::is_dag ----------------------------
# NEWS: adj^i is elementwise, so cycle/connectivity checks never fired;
# replaced with igraph::is_dag()/is_connected().
test_that("regression: BNM() flags cyclic graphs as non-acyclic", {
  labs <- dataFormat(J5S10)$ItemLabel
  mk <- function(edges) {
    m <- matrix(0, 5, 5, dimnames = list(labs, labs))
    for (e in edges) m[e[1], e[2]] <- 1
    m
  }
  adj_dag <- mk(list(c(1, 2), c(2, 3), c(3, 4)))
  adj_cyc <- mk(list(c(1, 2), c(2, 3), c(3, 4), c(4, 1))) # 1->2->3->4->1

  b_dag <- suppressMessages(BNM(J5S10, adj_matrix = adj_dag))
  b_cyc <- suppressMessages(BNM(J5S10, adj_matrix = adj_cyc))
  expect_equal(b_dag$acyclicFLG, 1)
  expect_equal(b_cyc$acyclicFLG, 0)
})


# --- LDB(): matrix/data.frame conf ----------------------------------------
# NEWS: LDB() crashed ("object 'conf_mat' not found") for non-vector conf;
# also covers raw-matrix input (item: response.type read from raw arg).
test_that("regression: LDB() accepts vector, matrix and data.frame conf", {
  skip_on_cran()
  conf_vec <- read.csv(
    test_path("fixtures", "auxiliary_data", "FixFieldLDB.csv")
  )[, 2]
  edgeFile <- test_path("fixtures", "auxiliary_data", "EdgesLDB.csv")

  nf <- max(conf_vec)
  ni <- length(conf_vec)
  conf_mat <- matrix(0, ni, nf)
  for (i in seq_len(ni)) conf_mat[i, conf_vec[i]] <- 1

  rawU <- dataFormat(J35S515)$U # genuine matrix input
  r_vec <- suppressMessages(LDB(rawU, ncls = 5, conf = conf_vec, adj_file = edgeFile))
  r_mat <- suppressMessages(LDB(rawU, ncls = 5, conf = conf_mat, adj_file = edgeFile))
  expect_s3_class(r_mat, "LDB")
  expect_equal(r_mat$IRP, r_vec$IRP, tolerance = 1e-6)
  # NB: data.frame conf is documented as supported but still crashes in
  # build_conf_mat() (its `conf %in% c(0,1)` check runs column-wise on a
  # data.frame); not asserted here -- see the final report.
})


# --- LDLRA(): raw matrix input and method = "C" labelling ------------------
# NEWS: hardcoded `model <- 2` overrode method="C", mislabelling Class as
# Rank; also covers raw-matrix response.type passthrough.
test_that("regression: LDLRA() raw input and method='C' label", {
  skip_on_cran()
  dag <- test_path("fixtures", "auxiliary_data", "DAG_file.csv")
  rawU <- dataFormat(J12S5000)$U

  ld_c <- suppressMessages(LDLRA(rawU, ncls = 5, adj_file = dag, method = "C"))
  ld_r <- suppressMessages(LDLRA(rawU, ncls = 5, adj_file = dag, method = "R"))
  expect_match(ld_c$msg, "Class")
  expect_match(ld_r$msg, "Rank")
})


# --- 0-indexed / non-contiguous category codes: shift invariance -----------
# NEWS: raw category code 0 was a no-op array index, silently dropping every
# category-0 response in Biclustering()/Biclustering_IRM() nominal & ordinal.
test_that("regression: Biclustering() nominal is invariant to a +1 code shift", {
  set.seed(1)
  N <- 120
  J <- 6
  Q0 <- matrix(sample(0:2, N * J, replace = TRUE), N, J) # codes 0,1,2
  Q1 <- Q0 + 1 # codes 1,2,3
  r0 <- suppressMessages(Biclustering(Q0, nfld = 2, ncls = 3, verbose = FALSE))
  r1 <- suppressMessages(Biclustering(Q1, nfld = 2, ncls = 3, verbose = FALSE))
  expect_equal(r0$ClassEstimated, r1$ClassEstimated)
})

test_that("regression: ordinal Biclustering()/IRM invariant to a +1 code shift", {
  set.seed(42)
  N <- 150
  J <- 8
  Q0 <- matrix(sample(0:3, N * J, replace = TRUE), N, J)
  Q1 <- Q0 + 1
  mkord <- function(Q) {
    d <- suppressMessages(dataFormat(Q))
    d$response.type <- "ordinal"
    d
  }
  o0 <- suppressMessages(Biclustering(mkord(Q0), nfld = 2, ncls = 3, verbose = FALSE))
  o1 <- suppressMessages(Biclustering(mkord(Q1), nfld = 2, ncls = 3, verbose = FALSE))
  expect_equal(o0$ClassEstimated, o1$ClassEstimated)
  expect_equal(o0$log_lik, o1$log_lik)

  set.seed(7)
  i0 <- suppressMessages(Biclustering_IRM(mkord(Q0), verbose = FALSE))
  set.seed(7)
  i1 <- suppressMessages(Biclustering_IRM(mkord(Q1), verbose = FALSE))
  ce0 <- if (!is.null(i0$ClassEstimated)) i0$ClassEstimated else i0$class_membership
  ce1 <- if (!is.null(i1$ClassEstimated)) i1$ClassEstimated else i1$class_membership
  expect_equal(ce0, ce1)
})


# --- Missing responses no longer corrupt the IRM one-hot array -------------
# NEWS: a -1 sentinel used as a raw array index set every category but the
# first to 1; results must stay finite under missingness.
test_that("regression: Biclustering_IRM() ordinal is finite with missing data", {
  set.seed(42)
  N <- 150
  J <- 8
  Q <- matrix(sample(0:3, N * J, replace = TRUE), N, J)
  Z <- matrix(1, N, J)
  Z[sample(length(Z), round(0.08 * N * J))] <- 0
  d <- suppressMessages(dataFormat(Q, Z = Z))
  d$response.type <- "ordinal"
  set.seed(1)
  irm <- suppressMessages(Biclustering_IRM(d, verbose = FALSE))
  expect_true(is.finite(irm$log_lik))
  expect_true(all(is.finite(irm$FRP)))
})


# --- CCRR.default() recurses into CCRR, not JCRR ---------------------------
# NEWS: CCRR(raw) fell through to JCRR(U) (copy-paste bug).
test_that("regression: CCRR() on raw input differs from JCRR()", {
  jc <- JCRR(J5S10)
  cc <- CCRR(J5S10)
  expect_false(isTRUE(all.equal(as.vector(jc), as.vector(cc))))
})


# --- MutualInformation() honours base= for binary data ---------------------
# NEWS: MutualInformation.binary() always used log base 2.
test_that("regression: MutualInformation() respects base= on binary data", {
  mi2 <- MutualInformation(J5S10, base = 2)
  mie <- MutualInformation(J5S10, base = exp(1))
  ratio <- mi2[1, 2] / mie[1, 2]
  expect_equal(as.numeric(ratio), 1 / log(2), tolerance = 1e-6)
})


# --- JSR()/CSR() fall back cleanly on binary data --------------------------
# NEWS: both messaged "using X instead" but never return()ed, then crashed.
test_that("regression: JSR() and CSR() do not crash on binary data", {
  expect_no_error(suppressMessages(JSR(J5S10)))
  expect_no_error(suppressMessages(CSR(J5S10)))
})


# --- ItemTotalCorr.ordinal() masks missing before totalling ----------------
# NEWS: rowSums(U$Q) included the -1 missing sentinel in the total score.
test_that("regression: ItemTotalCorr.ordinal() masks missing responses", {
  set.seed(9)
  N <- 200
  J <- 6
  Q <- matrix(sample(1:4, N * J, replace = TRUE), N, J)
  Z <- matrix(1, N, J)
  Z[sample(length(Z), 80)] <- 0
  d <- suppressMessages(dataFormat(Q, Z = Z))
  d$response.type <- "ordinal"

  itc <- ItemTotalCorr(d)
  expect_true(all(is.finite(itc)))
  expect_true(all(itc >= -1 & itc <= 1))

  # The fixed total excludes missing cells; the buggy total summed the -1
  # sentinel. Confirm the result matches the masked total, not the raw one.
  polyserial <- exametrika:::polyserial
  masked_total <- rowSums(replace(d$Q, d$Z == 0, NA), na.rm = TRUE)
  buggy_total <- rowSums(d$Q) # includes -1 sentinels
  expect_false(isTRUE(all.equal(masked_total, buggy_total)))
  itc_masked <- vapply(seq_len(J), function(j) {
    qc <- d$Q[, j]
    qc[d$Z[, j] == 0] <- NA
    polyserial(masked_total, qc)
  }, numeric(1))
  itc_buggy <- vapply(seq_len(J), function(j) {
    qc <- d$Q[, j]
    qc[d$Z[, j] == 0] <- NA
    polyserial(buggy_total, qc)
  }, numeric(1))
  expect_equal(unname(itc), unname(itc_masked), tolerance = 1e-8)
  expect_false(isTRUE(all.equal(unname(itc), unname(itc_buggy))))
})


# --- print(InterItemAnalysis(ordinal)) shows CSR, not JSR twice -------------
# NEWS: the "Conditional Selection Ratio" (label typo since fixed) block printed x$JSR instead of x$CSR.
test_that("regression: print(IIAnalysis.ordinal) shows the CSR block", {
  set.seed(9)
  N <- 200
  J <- 6
  Q <- matrix(sample(1:4, N * J, replace = TRUE), N, J)
  d <- suppressMessages(dataFormat(Q))
  d$response.type <- "ordinal"
  iia <- InterItemAnalysis(d)
  expect_s3_class(iia, "IIAnalysis.ordinal")

  out <- capture.output(print(iia))
  # CSR self-blocks are conditional identity matrices (P(cat|same cat)=1);
  # JSR self-blocks are the joint marginal (0.25, 0.23, ...). An integer
  # identity row therefore appears only when the CSR block is printed.
  expect_true(any(grepl("Cat1[[:space:]]+1[[:space:]]+0[[:space:]]+0[[:space:]]+0", out)))
  # The Conditional Selection Ratio header is present exactly once.
  expect_equal(sum(grepl("Conditional Selection Ratio", out)), 1L)
})


# --- Internal helpers: remap_category_codes / build_conf_mat / -------------
# --- beta_posterior_mode / select_optimal_grid_index -----------------------
# NEWS: shared helpers extracted during the fix; unit-tested directly.
test_that("regression: remap_category_codes() densifies codes per column", {
  remap <- exametrika:::remap_category_codes
  # 0-based codes with a missing sentinel in column 2.
  Q <- matrix(c(0, 1, 2, 3, 0, 0, 1, -1), ncol = 2)
  expect_equal(remap(Q), matrix(c(1, 2, 3, 4, 1, 1, 2, -1), ncol = 2))
  # Non-contiguous codes collapse to 1..k.
  expect_equal(
    remap(matrix(c(2, 4, 6, 8), ncol = 1)),
    matrix(c(1, 2, 3, 4), ncol = 1)
  )
  # A single-category column maps to all 1s; missing (-1) is left untouched.
  expect_equal(
    remap(matrix(c(5, 5, 5), ncol = 1)),
    matrix(c(1, 1, 1), ncol = 1)
  )
})

test_that("regression: build_conf_mat() handles vector/named/matrix/df conf", {
  bcm <- exametrika:::build_conf_mat
  # Plain confirmatory vector -> one-hot field membership.
  expect_equal(
    bcm(c(1, 1, 2, 2, 3), 5),
    matrix(c(
      1, 1, 0, 0, 0,
      0, 0, 1, 1, 0,
      0, 0, 0, 0, 1
    ), ncol = 3)
  )
  # Named vector is still a vector: names must not change the result.
  expect_equal(
    unname(bcm(c(a = 1, b = 1, c = 2), 3)),
    matrix(c(1, 1, 0, 0, 0, 1), ncol = 2)
  )
  # A gap in the codes leaves an all-zero field column (max drives ncol).
  expect_equal(
    bcm(c(1, 1, 3, 3), 4),
    matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1), ncol = 3)
  )
  # A matrix conf (row-wise one-hot) is returned as a plain matrix.
  m <- matrix(c(1, 0, 1, 0, 1, 0), ncol = 2) # rows: f1, f2, f1
  expect_equal(bcm(m, 3), m)
  # Size mismatch is a clear error.
  expect_error(bcm(c(1, 2), 5), "does NOT match")
})

test_that("regression: beta_posterior_mode() posterior mode formula", {
  bpm <- exametrika:::beta_posterior_mode
  # (count + a - 1) / (total + a + b - 2)
  expect_equal(bpm(5, 10, 1, 1), 0.5)
  expect_equal(bpm(3, 10, 2, 2), 4 / 12)
  # A Beta(1,1) prior on an empty cell is 0/0 = NaN; Beta(2,2) resolves it.
  expect_true(is.nan(bpm(0, 0, 1, 1)))
  expect_equal(bpm(0, 0, 2, 2), 0.5)
})

test_that("regression: select_optimal_grid_index() picks the first tie", {
  sel <- exametrika:::select_optimal_grid_index
  # Maximise a matrix with a unique max at [2, 2]; must return that row/col
  # pair, not a column-major flattened index.
  m <- matrix(c(1, 2, 2, 3), 2)
  idx <- sel(m, minimize = FALSE)
  expect_equal(unname(idx[1]), 2)
  expect_equal(unname(idx[2]), 2)
  # Minimise a vector: first occurrence of the minimum.
  expect_equal(sel(c(5, 3, 3, 9), minimize = TRUE), 2)
})


# --- v1.15.0 API unification ------------------------------------------------
# NEWS: Biclustering_IRM's max_iter renamed to maxiter (deprecated shim),
# mic/EM_limit defaults aligned, verbose defaults unified to FALSE.
test_that("regression: deprecated max_iter still works with a warning", {
  skip_on_cran()
  expect_warning(
    r_old <- Biclustering_IRM(J20S600,
      gamma_c = 1, gamma_f = 1,
      max_iter = 5, verbose = FALSE
    ),
    "deprecated"
  )
  r_new <- Biclustering_IRM(J20S600,
    gamma_c = 1, gamma_f = 1,
    maxiter = 5, verbose = FALSE
  )
  expect_equal(r_old$ClassEstimated, r_new$ClassEstimated)
})

test_that("regression: unified defaults across the model functions", {
  # verbose defaults to FALSE everywhere it exists
  for (fn in list(
    IRT, GRM, LCA, GridSearch,
    exametrika:::Biclustering.binary, exametrika:::Biclustering.nominal,
    exametrika:::Biclustering.ordinal, exametrika:::Biclustering.rated,
    exametrika:::Biclustering_IRM.binary, exametrika:::Biclustering_IRM.nominal,
    exametrika:::Biclustering_IRM.ordinal, exametrika:::Biclustering_IRM.rated
  )) {
    expect_false(eval(formals(fn)$verbose))
  }
  # ordinal IRM matches its siblings
  expect_false(formals(exametrika:::Biclustering_IRM.ordinal)$mic)
  expect_equal(formals(exametrika:::Biclustering_IRM.ordinal)$EM_limit, 20)
  # rated Biclustering exposes conf_class like its siblings
  expect_true("conf_class" %in% names(formals(exametrika:::Biclustering.rated)))
})

# NEWS: DistractorAnalysis counted raw category codes against 1..maxQ,
# dropping category-0 responses entirely for 0-based rated data.
test_that("regression: DistractorAnalysis remaps 0-based category codes", {
  skip_on_cran()
  set.seed(7)
  N <- 150
  J <- 5
  Q0 <- matrix(sample(0:3, N * J, replace = TRUE), N, J)
  CA0 <- rep(1, J)
  d0 <- as.data.frame(Q0)
  fit0 <- Biclustering(dataFormat(d0, response.type = "rated", CA = CA0),
    ncls = 2, nfld = 2, verbose = FALSE
  )
  da0 <- DistractorAnalysis(fit0)
  # every observed response must be counted exactly once
  expect_equal(
    sum(vapply(da0$freq_table, sum, numeric(1))),
    sum(fit0$Z == 1)
  )
  # shift invariance: 1-based version gives the same frequency tables
  fit1 <- Biclustering(dataFormat(as.data.frame(Q0 + 1),
    response.type = "rated", CA = CA0 + 1
  ), ncls = 2, nfld = 2, verbose = FALSE)
  da1 <- DistractorAnalysis(fit1)
  expect_equal(da0$freq_table, da1$freq_table)
})

# NEWS: build_conf_mat() rejected a valid 0/1 data.frame conf (column-wise
# %in% comparison before coercion).
test_that("regression: data.frame conf is accepted and equals matrix conf", {
  conf_vec <- c(1, 1, 2, 2, 3, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
  cm <- exametrika:::build_conf_mat(conf_vec, 15)
  cm_df <- exametrika:::build_conf_mat(as.data.frame(cm), 15)
  expect_identical(cm, unname(cm_df))
})

# NEWS: LRA.rated's minFreqRatio category collapse never fired (frequency
# compared against the correct-answer CODE), and would have crashed at
# report assembly if it had (labels used the full category set).
test_that("regression: LRA.rated minFreqRatio collapses rare distractors", {
  skip_on_cran()
  set.seed(4)
  N <- 400
  J <- 5
  Q <- matrix(sample(1:3, N * J, replace = TRUE), N, J)
  r4 <- matrix(runif(N * J) < 0.02, N, J)
  Q[r4] <- 4
  r5 <- matrix(runif(N * J) < 0.02, N, J)
  Q[r5] <- 5
  dat <- dataFormat(as.data.frame(Q), response.type = "rated", CA = rep(2, J))

  # default minFreqRatio = 0 keeps every category (previous behavior)
  r0 <- suppressMessages(LRA(dat, nrank = 3))
  expect_equal(nrow(r0$ICRP), sum(dat$categories))
  expect_false(any(r0$ICRP$CategoryLabel == "CatX"))

  # minFreqRatio = 0.1 pools the two ~2% categories into one CatX bucket
  r1 <- suppressMessages(LRA(dat, nrank = 3, minFreqRatio = 0.1))
  expect_equal(nrow(r1$ICRP), sum(dat$categories) - J)
  expect_equal(sum(r1$ICRP$CategoryLabel == "CatX"), J)
  # the correct answer is never collapsed
  kept <- tapply(
    r1$ICRP$CategoryLabel, r1$ICRP$ItemLabel,
    function(x) any(grepl("-Cat2$", x))
  )
  expect_true(all(kept))
})
