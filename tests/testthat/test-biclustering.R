library(exametrika)

### GOALS - Mathematica reference data (Biclustering)
test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Biclustering_Test.csv"),
  check.names = FALSE
)
Bicluster <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Biclustering_Bicluster.csv"),
  check.names = FALSE
)
items <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Biclustering_Item.csv"),
  check.names = FALSE
)
student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Biclustering_Student.csv"),
  check.names = FALSE
)


### Setup (Biclustering)
tmp <- dataFormat(J35S515)
Bic <- Biclustering(tmp, ncls = 6, nfld = 5, method = "B", mic = TRUE)

### Tests (Biclustering)
test_that("Biclustering Test Fit", {
  expect <- test[15:30, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- Bic$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("Biclustering Class Info", {
  ## FRP
  expect <- Bicluster[1:5, 2:7] |>
    unlist() |>
    unname() |>
    as.vector()
  result <- Bic$FRP |>
    unname() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  ## TRP
  expect <- Bicluster[6, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$TRP
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- Bicluster[7, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$LRD
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- Bicluster[8, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$ClassMembership |>
    colSums() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("Biclustering Item Info", {
  ## IRP
  expect <- items[, 6:10] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$FieldMembership |>
    unlist() |>
    unname() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Estimated
  expect <- items[, 11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$FieldEstimated |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("Biclustering Students", {
  ## Membership
  expect <- student[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$Students[, 1:6] |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


# Ranklustering ---------------------------------------------------

test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Ranklustering_Test.csv"),
  check.names = FALSE
)
Rankluster <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Ranklustering_Rankluster.csv"),
  check.names = FALSE
)
items <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Ranklustering_Item.csv"),
  check.names = FALSE
)
student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter07Ranklustering_Student.csv"),
  check.names = FALSE
)


### Setup (Ranklustering)
# Pinned to estimation = "GTM": the Mathematica reference uses the original
# filter-based smoothing, not the new isotonic default.
tmp <- dataFormat(J35S515)
Bic <- Biclustering(tmp, ncls = 6, nfld = 5, method = "R", estimation = "GTM", mic = TRUE)

### Tests (Ranklustering)
test_that("Ranklustering Test Fit", {
  expect <- test[15:30, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- Bic$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("Ranklustering Class Info", {
  ## FRP
  expect <- Rankluster[1:5, 2:7] |>
    unlist() |>
    unname() |>
    as.vector()
  result <- Bic$FRP |>
    unname() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  ## TRP
  expect <- Rankluster[6, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$TRP
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- Rankluster[7, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$LRD
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- Rankluster[8, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$ClassMembership |>
    colSums() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Index
  expect <- Rankluster[1:5, 9:14] |>
    unlist() |>
    unname() |>
    as.vector()
  result <- Bic$FRPIndex |>
    unlist() |>
    unname() |>
    as.vector()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("Ranklustering Item Info", {
  ## IRP
  expect <- items[, 6:10] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$FieldMembership |>
    unlist() |>
    unname() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Estimated
  expect <- items[, 11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$FieldEstimated |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("Ranklustering Students", {
  ## Membership
  expect <- student[, 6:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- Bic$Students[, 1:9] |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


### Confirmatory Biclustering (binary)
conf_b <- c(rep(1, 7), rep(2, 7), rep(3, 7), rep(4, 7), rep(5, 7))
Bic_conf <- Biclustering(J35S515, ncls = 5, nfld = 5, method = "B", conf = conf_b, verbose = FALSE)

test_that("binary confirmatory respects field assignments", {
  expect_equal(as.numeric(Bic_conf$FieldEstimated), conf_b)
})

test_that("binary confirmatory rejects wrong-length conf vector", {
  expect_error(
    Biclustering(J35S515,
      ncls = 5, nfld = 5, method = "B",
      conf = rep(1:5, each = 6), verbose = FALSE
    ),
    "conf vector size does NOT match"
  )
})

test_that("binary confirmatory accepts membership matrix", {
  conf_mat <- matrix(0, nrow = 35, ncol = 5)
  for (i in seq_len(35)) conf_mat[i, conf_b[i]] <- 1
  res <- Biclustering(J35S515,
    ncls = 5, nfld = 5, method = "B",
    conf = conf_mat, verbose = FALSE
  )
  expect_equal(as.numeric(res$FieldEstimated), conf_b)
})

test_that("binary confirmatory rejects wrong-row matrix", {
  bad <- matrix(0, nrow = 30, ncol = 5)
  bad[cbind(seq_len(30), rep(1:5, each = 6))] <- 1
  expect_error(
    Biclustering(J35S515,
      ncls = 5, nfld = 5, method = "B",
      conf = bad, verbose = FALSE
    ),
    "conf matrix size does NOT match"
  )
})


### Class-side Confirmatory Biclustering (binary)
nobs_b <- NROW(dataFormat(J35S515)$U)
conf_class_b <- ((seq_len(nobs_b) - 1) %% 5) + 1

test_that("binary B class-side confirmatory respects class assignments", {
  res <- Biclustering(J35S515,
    ncls = 5, nfld = 5, method = "B",
    conf_class = conf_class_b, verbose = FALSE
  )
  est <- apply(res$ClassMembership, 1, which.max)
  expect_equal(as.numeric(est), conf_class_b)
})

test_that("binary R class-side confirmatory respects class assignments and skips smoothing", {
  res <- Biclustering(J35S515,
    ncls = 5, nfld = 5, method = "R",
    conf_class = conf_class_b, verbose = FALSE
  )
  est <- apply(res$ClassMembership, 1, which.max)
  expect_equal(as.numeric(est), conf_class_b)
  expect_equal(
    as.numeric(res$SmoothedMembership),
    as.numeric(res$ClassMembership)
  )
})

test_that("binary class-side confirmatory rejects wrong-length conf_class", {
  expect_error(
    Biclustering(J35S515,
      ncls = 5, nfld = 5, method = "B",
      conf_class = 1:10, verbose = FALSE
    ),
    "conf_class vector size does NOT match"
  )
})

test_that("binary class-side confirmatory accepts membership matrix", {
  cm <- matrix(0, nrow = nobs_b, ncol = 5)
  for (i in seq_len(nobs_b)) cm[i, conf_class_b[i]] <- 1
  res <- Biclustering(J35S515,
    ncls = 5, nfld = 5, method = "B",
    conf_class = cm, verbose = FALSE
  )
  est <- apply(res$ClassMembership, 1, which.max)
  expect_equal(as.numeric(est), conf_class_b)
})

test_that("binary conf and conf_class can be combined", {
  res <- Biclustering(J35S515,
    ncls = 5, nfld = 5, method = "B",
    conf = conf_b,
    conf_class = conf_class_b, verbose = FALSE
  )
  est <- apply(res$ClassMembership, 1, which.max)
  expect_equal(as.numeric(res$FieldEstimated), conf_b)
  expect_equal(as.numeric(est), conf_class_b)
})

### Isotonic (order-restricted) Ranklustering ---------------------

test_that("isotonic is the default estimation for Ranklustering", {
  res <- Biclustering(J35S515, ncls = 6, nfld = 5, method = "R", verbose = FALSE)
  expect_equal(res$estimation, "isotonic")
})

test_that("estimation is ignored (NA) for plain Biclustering", {
  res <- Biclustering(J35S515, ncls = 6, nfld = 5, method = "B", verbose = FALSE)
  expect_true(is.na(res$estimation))
})

test_that("isotonic Ranklustering yields monotone Field Reference Profiles", {
  res <- Biclustering(J35S515,
    ncls = 6, nfld = 5, method = "R",
    estimation = "isotonic", verbose = FALSE
  )
  # each field's profile is non-decreasing across ranks
  expect_true(all(apply(res$FRP, 1, function(r) all(diff(r) >= -1e-9))))
  # monotone estimation trivially satisfies the ordinal alignment conditions
  expect_true(res$WOACflg)
  expect_true(res$SOACflg)
})

test_that("isotonic Ranklustering beats GTM on the log-likelihood", {
  gtm <- Biclustering(J35S515,
    ncls = 6, nfld = 5, method = "R",
    estimation = "GTM", verbose = FALSE
  )
  iso <- Biclustering(J35S515,
    ncls = 6, nfld = 5, method = "R",
    estimation = "isotonic", verbose = FALSE
  )
  expect_gt(iso$log_lik, gtm$log_lik)
})

test_that("isotonic df credit matches the PAVA block count vs GTM", {
  gtm <- Biclustering(J35S515,
    ncls = 6, nfld = 5, method = "R",
    estimation = "GTM", verbose = FALSE
  )
  iso <- Biclustering(J35S515,
    ncls = 6, nfld = 5, method = "R",
    estimation = "isotonic", verbose = FALSE
  )
  # TestFit sets model_df = bench_nparm - nparam, with bench_nparm fixed by the
  # data. So the df difference between two fits equals the nparam difference.
  nparam_gtm <- sum(diag(create_filter_matrix(6))) * 5 # sum(diag(Fil)) * nfld
  nparam_iso <- sum(apply(iso$FRP, 1, function(r) length(unique(round(r, 10)))))
  expect_equal(
    iso$TestFitIndices$model_df - gtm$TestFitIndices$model_df,
    nparam_gtm - nparam_iso
  )
  # each field uses at most ncls monotone blocks
  expect_lte(max(apply(iso$FRP, 1, function(r) length(unique(round(r, 10))))), 6)
})

test_that("estimation argument rejects unknown values", {
  expect_error(
    Biclustering(J35S515, ncls = 6, nfld = 5, method = "R", estimation = "bogus"),
    "should be one of"
  )
})

### mixing = "estimate" (2.3.0): class side only
test_that("Biclustering mixing = 'estimate' estimates the class weights only", {
  tmp <- dataFormat(J35S515)
  eq <- Biclustering(tmp, ncls = 3, nfld = 3)
  expect_equal(eq$mixing, "equal")
  expect_equal(unname(eq$ClassWeight), rep(1 / 3, 3))
  es <- Biclustering(tmp, ncls = 3, nfld = 3, mixing = "estimate")
  expect_equal(es$mixing, "estimate")
  expect_equal(sum(es$ClassWeight), 1)
  expect_true(all(es$ClassWeight > 0))
  expect_equal(es$LogPrior, log(es$ClassWeight))
  # C - 1 weights charged to the test df; the field side is untouched
  expect_equal(es$TestFitIndices$model_df, eq$TestFitIndices$model_df - 2)
  expect_true(es$elbo >= eq$elbo - 1e-8)
  expect_output(print(es), "Class Weight \\(estimated\\)")
  expect_no_match(paste(capture.output(print(eq)), collapse = "\n"), "Weight \\(estimated\\)")
  p <- predict(es, J35S515)
  expect_equal(unname(as.matrix(p$Membership)), unname(as.matrix(es$Students[, 1:3])),
    tolerance = 1e-10
  )
  expect_error(Biclustering(tmp, ncls = 3, nfld = 3, mixing = "free"), "should be one of")
})

test_that("Ranklustering mixing = 'estimate' works with isotonic and GTM", {
  tmp <- dataFormat(J35S515)
  for (est in c("isotonic", "GTM")) {
    eq <- Biclustering(tmp, ncls = 3, nfld = 3, method = "R", estimation = est)
    es <- Biclustering(tmp, ncls = 3, nfld = 3, method = "R", estimation = est, mixing = "estimate")
    expect_equal(sum(es$ClassWeight), 1)
    expect_equal(es$TestFitIndices$model_df, eq$TestFitIndices$model_df - 2)
    expect_output(print(es), "Rank Weight \\(estimated\\)")
    if (est == "isotonic") {
      expect_true(all(apply(es$FRP, 1, function(r) all(diff(r) >= -1e-10))))
    }
  }
})

test_that("Biclustering mixing = 'estimate' does not duplicate a dominant pattern", {
  set.seed(1)
  U <- rbind(
    matrix(0L, nrow = 500, ncol = 6),
    matrix(rbinom(80 * 6, 1, 0.3), nrow = 80, ncol = 6),
    matrix(rbinom(20 * 6, 1, 0.7), nrow = 20, ncol = 6)
  )
  colnames(U) <- paste0("Item", 1:6)
  dat <- dataFormat(U)
  eq <- suppressWarnings(Biclustering(dat, ncls = 3, nfld = 2))
  es <- suppressWarnings(Biclustering(dat, ncls = 3, nfld = 2, mixing = "estimate", maxiter = 3000))
  expect_true(any(eq$LCD == 0))
  expect_true(all(es$LCD > 0))
  expect_true(max(es$ClassWeight) > 0.7)
})
