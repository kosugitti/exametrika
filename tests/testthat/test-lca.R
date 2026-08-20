library(exametrika)

# M2 factorises a matrix whose size is set by the margins, m = sum(cat - 1) plus
# one block per item pair, so it grows with the square of the item count and the
# factorisation with its cube. The respondent count does not enter: subsetting
# rows leaves m unchanged and saves nothing. Dropping to 12 items takes m from
# 1770 to 630 and these blocks from ~20s to ~1.8s under the reference BLAS that
# CRAN's Windows build ships, which is what the 10-minute checktime limit is
# measured against. Every assertion below is structural -- the rank-deficiency
# rule, m recomputed from the data's own category counts, what print() shows --
# so fewer items tests the same thing.
head_cols <- function(x, j) {
  x$ItemLabel <- x$ItemLabel[seq_len(j)]
  x$w <- x$w[seq_len(j)]
  x$categories <- x$categories[seq_len(j)]
  if (!is.null(x$CategoryLabel)) x$CategoryLabel <- x$CategoryLabel[seq_len(j)]
  if (!is.null(x$CA)) x$CA <- x$CA[seq_len(j)]
  for (f in c("Q", "U", "Z")) {
    if (!is.null(x[[f]])) x[[f]] <- x[[f]][, seq_len(j), drop = FALSE]
  }
  return(x)
}


### GOALS - Mathematica reference data
test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter05LCA_Test.csv"),
  check.names = FALSE
)
class <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter05LCA_Class.csv"),
  check.names = FALSE
)
items <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter05LCA_Item.csv"),
  check.names = FALSE
)
student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter05LCA_Student.csv"),
  check.names = FALSE
)


### Setup
# A small binary frame for the tests that only need *a* binary dataset -- which
# method dispatch picks, that a path errors, that two engines agree with each
# other. None of them compares against Mathematica, so J15S500 buys nothing
# there but time, and on CRAN's Windows build time is the scarce thing.
tiny_bin <- dataFormat(
  read.csv(test_path("fixtures", "tiny_data", "tinyLCA.csv"), check.names = FALSE),
  na = -99
)
# Fitted lazily and cached. skip_on_cran() skips a test_that() body but not
# file-scope setup, so leaving the fit out here would have it run on CRAN for
# the sake of four blocks that are skipped there.
tmp <- dataFormat(J15S500, na = -99)
.lca_model <- NULL
full_model <- function() {
  if (is.null(.lca_model)) .lca_model <<- LCA(tmp, ncls = 5)
  return(.lca_model)
}

### Tests
test_that("LCA Test Info", {
  # The tiny fixtures cross-validate the same quantities against the same
  # Mathematica implementation in a fraction of the time (test-lca-tiny.R,
  # test-ctt-tiny.R), including the missing-data handling. This full-size
  # comparison stays for local runs, where the extra seconds cost nothing.
  skip_on_cran()
  expect <- test[14:29, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- full_model()$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LCA Class Info", {
  # The tiny fixtures cross-validate the same quantities against the same
  # Mathematica implementation in a fraction of the time (test-lca-tiny.R,
  # test-ctt-tiny.R), including the missing-data handling. This full-size
  # comparison stays for local runs, where the extra seconds cost nothing.
  skip_on_cran()
  ## TRP
  expect <- class[1, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- full_model()$TRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- class[2, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- full_model()$LCD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- class[3, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- full_model()$CMD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("LCA Item Info", {
  # The tiny fixtures cross-validate the same quantities against the same
  # Mathematica implementation in a fraction of the time (test-lca-tiny.R,
  # test-ctt-tiny.R), including the missing-data handling. This full-size
  # comparison stays for local runs, where the extra seconds cost nothing.
  skip_on_cran()
  ## IRP
  expect <- items[, 6:10] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- full_model()$IRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## FitIndex
  expect <- items[, c(15, 11, 12, 16, 13, 17, 14, 18:26)] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- full_model()$ItemFitIndices |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LCA Students", {
  # The tiny fixtures cross-validate the same quantities against the same
  # Mathematica implementation in a fraction of the time (test-lca-tiny.R,
  # test-ctt-tiny.R), including the missing-data handling. This full-size
  # comparison stays for local runs, where the extra seconds cost nothing.
  skip_on_cran()
  ## Membership
  expect <- student[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- full_model()$Students |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

### S3 dispatch -------------------------------------------------------------

test_that("LCA dispatches on response type", {
  # dispatch is about classes, not estimates, so tiny/subset data suffices
  expect_true(inherits(LCA(tiny_bin, ncls = 3), "LCA"))
  # raw input and a pre-formatted object take the same route
  raw_tiny <- read.csv(test_path("fixtures", "tiny_data", "tinyLCA.csv"), check.names = FALSE)
  expect_equal(LCA(raw_tiny, na = -99, ncls = 3)$log_lik, LCA(tiny_bin, ncls = 3)$log_lik)
  # every response type now has a method, each with its own return class
  expect_true(inherits(LCA(head_rows_dat(J21S300, 100), ncls = 2), "ratedLCA"))
  expect_true(inherits(
    LCA(head_rows_dat(dataFormat(head_cols(J20S600, 12), response.type = "nominal"), 150), ncls = 2),
    "nominalLCA"
  ))
})

### Nominal LCA -------------------------------------------------------------

nominal_data <- dataFormat(head_cols(J20S600, 12), response.type = "nominal")
.nominal <- NULL
nominal_model <- function() {
  if (is.null(.nominal)) .nominal <<- LCA(nominal_data, ncls = 3)
  return(.nominal)
}

test_that("nominal LCA returns its own class and converges", {
  skip_on_cran()
  expect_s3_class(nominal_model(), "exametrika")
  expect_true(inherits(nominal_model(), "nominalLCA"))
  expect_true(nominal_model()$converge)
  expect_equal(nominal_model()$n_class, 3)
})

test_that("nominal LCA category profiles are proper distributions", {
  skip_on_cran()
  probs <- nominal_model()$ICRP[, paste0("class", 1:3)]
  expect_true(all(probs >= 0))
  # every (item, class) column of category probabilities sums to 1
  for (cl in paste0("class", 1:3)) {
    sums <- tapply(nominal_model()$ICRP[[cl]], nominal_model()$ICRP$ItemLabel, sum)
    expect_equal(as.numeric(sums), rep(1, ncol(nominal_data$Q)), tolerance = 1e-8)
  }
  # one row per (item, category), so ragged category counts stay aligned
  expect_equal(nrow(nominal_model()$ICRP), sum(nominal_data$categories))
})

test_that("nominal LCA reports information criteria only", {
  skip_on_cran()
  fit <- nominal_model()$TestFitIndices
  expect_true(is.na(fit$bench_log_like))
  expect_true(all(is.na(c(fit$model_Chi_sq, fit$model_df, fit$CFI, fit$RMSEA))))
  expect_true(all(is.finite(c(fit$AIC, fit$BIC, fit$CAIC))))
  # information criteria follow the -2 log L + k penalty convention here,
  # not the chi-square based one used on the binary path
  nparam <- 3 * sum(nominal_data$categories - 1)
  expect_equal(fit$AIC, -2 * nominal_model()$log_lik + 2 * nparam)
  expect_equal(fit$BIC, -2 * nominal_model()$log_lik + nparam * log(nominal_model()$nobs))
})

test_that("nominal LCA handles ragged category counts", {
  set.seed(123)
  ncat <- c(2, 3, 4, 5, 3)
  raw <- data.frame(ID = paste0("S", 1:200))
  for (j in seq_along(ncat)) {
    raw[[paste0("Item", j)]] <- sample(seq_len(ncat[j]), 200, replace = TRUE)
  }
  dat <- dataFormat(raw, response.type = "nominal")
  m <- LCA(dat, ncls = 2)
  expect_equal(as.vector(dat$categories), ncat)
  expect_equal(nrow(m$ICRP), sum(ncat))
  sums <- tapply(m$ICRP$class1, m$ICRP$ItemLabel, sum)
  expect_equal(as.numeric(sums), rep(1, length(ncat)), tolerance = 1e-8)
})

test_that("ordinal data is routed to the nominal model with a notice", {
  expect_message(m <- LCA(J5S1000, ncls = 2), "Latent classes are unordered")
  expect_true(inherits(m, "nominalLCA"))
})

test_that("binary data recovers the same solution through the nominal engine", {
  # a two-category nominal mixture is the binary LCA model, so the two paths
  # must agree up to label switching and local optima
  db <- tiny_bin
  raw <- cbind(ID = db$ID, as.data.frame(db$U + 1))
  dn <- dataFormat(raw, response.type = "nominal")
  b <- LCA(db, ncls = 3)
  n <- LCA(dn, ncls = 3)
  agreement <- sum(apply(table(b$Students[, "Estimate"], n$Students[, "Estimate"]), 1, max))
  expect_gt(agreement / nrow(db$U), 0.9)
  # the null model does not involve the classes at all, so it must match exactly
  expect_equal(n$TestFitIndices$null_log_like, b$TestFitIndices$null_log_like)
})

test_that("nominal LCA prints", {
  skip_on_cran()
  expect_output(print(nominal_model()), "Item Category Reference Profile")
  expect_output(print(nominal_model()), "Number of Latent class")
})

test_that("nominal LCA plots", {
  skip_on_cran()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot(nominal_model(), type = "ICRP", items = 1:2, nc = 1, nr = 2))
  expect_silent(plot(nominal_model(), type = "LCD"))
  expect_silent(plot(nominal_model(), type = "CMP", students = 1:2, nc = 1, nr = 2))
  # types that need a correct-response rate are not defined without a key
  expect_error(plot(nominal_model(), type = "IRP"), "does not correspond")
})

test_that("nominal data given to LRA is handed to LCA", {
  small_nominal <- head_rows_dat(nominal_data, 150)
  expect_message(m <- LRA(small_nominal, nrank = 4), "Latent ranks require ordered")
  expect_true(inherits(m, "nominalLCA"))
  # nrank is the LRA spelling of ncls and must not be dropped
  expect_equal(m$n_class, 4)
})

### Rated LCA ---------------------------------------------------------------

.rated <- NULL
rated_model <- function() {
  if (is.null(.rated)) .rated <<- LCA(J21S300, ncls = 3)
  return(.rated)
}

test_that("rated LCA estimates through the nominal engine", {
  skip_on_cran()
  expect_true(inherits(rated_model(), "ratedLCA"))
  expect_true(rated_model()$converge)
  # the estimation is the nominal one, so the category profiles must match
  nom <- dataFormat(J21S300)
  nom$response.type <- "nominal"
  n <- LCA(nom, ncls = 3)
  expect_equal(rated_model()$ICRP[, paste0("class", 1:3)], n$ICRP[, paste0("class", 1:3)])
  expect_equal(rated_model()$log_lik_nominal, n$log_lik)
})

test_that("rated LCA IRP is the keyed category probability", {
  skip_on_cran()
  dat <- dataFormat(J21S300)
  ncat <- as.vector(dat$categories)
  offset <- c(0, cumsum(ncat)[-length(ncat)])
  keyed <- as.matrix(rated_model()$ICRP[offset + dat$CA, paste0("class", 1:3)])
  expect_equal(unname(rated_model()$IRP), unname(keyed))
  expect_true(all(rated_model()$IRP >= 0 & rated_model()$IRP <= 1))
  # TRP is the weighted item sum of the IRP
  expect_equal(rated_model()$TRP, as.vector(t(rated_model()$IRP) %*% dat$w))
})

test_that("rated LCA reports both layers of fit", {
  skip_on_cran()
  # binary layer keeps the chi-square based indices
  expect_true(all(is.finite(unlist(rated_model()$TestFitIndices[c(
    "model_Chi_sq", "model_df", "CFI", "RMSEA", "AIC"
  )]))))
  # nominal layer has information criteria only
  expect_true(is.na(rated_model()$TestFitIndicesNominal$model_Chi_sq))
  expect_true(is.finite(rated_model()$TestFitIndicesNominal$BIC))
})

test_that("rated LCA does not sort classes by correct rate", {
  skip_on_cran()
  # Biclustering.rated sorts; LCA must not, because its classes are unordered.
  # The seedless EM is deterministic, so an unsorted TRP is evidence enough.
  expect_false(identical(rated_model()$TRP, sort(rated_model()$TRP)))
})

test_that("rated LCA prints and plots", {
  skip_on_cran()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_output(print(rated_model()), "Binary layer")
  expect_output(print(rated_model()), "Nominal layer")
  for (ty in c("IRP", "TRP", "ICRP", "LCD", "CMP")) {
    expect_silent(plot(rated_model(), type = ty, items = 1:2, students = 1:2, nc = 1, nr = 2))
  }
})

### M2 ----------------------------------------------------------------------

test_that("M2 reproduces the worked example of the design memo", {
  # Independence model = LCA with a single class: J = 3, Q = 2, S = 100.
  # develop/Algorithm_M2.tex gives M2 = 5.52778, df = 3, p = 0.13699, with
  # Xi and Delta written out by hand.
  rho <- c(.5, .4, .6)
  profile <- array(0, dim = c(3, 1, 2))
  profile[, 1, 1] <- rho
  profile[, 1, 2] <- 1 - rho
  p_vec <- c(.5, .4, .6, .25, .32, .26)
  out <- exametrika:::m2_core(profile, ncat = rep(2, 3), nobs = 100, p_vec = p_vec)
  expect_equal(out$M2, 5.52778, tolerance = 1e-5)
  expect_equal(out$df, 3)
  expect_equal(out$p, 0.13699, tolerance = 1e-4)
  expect_equal(out$m, 6)
  expect_equal(out$rank_delta, 3)

  # the margin covariance of the memo, to the last printed digit
  idx <- exametrika:::m2_margin_index(rep(2, 3))
  A <- exametrika:::m2_class_products(profile, idx)
  Xi <- exametrika:::m2_xi(profile, A, idx)
  Xi_memo <- matrix(c(
    .25, 0, 0, .10, .15, 0,
    0, .24, 0, .12, 0, .144,
    0, 0, .24, 0, .12, .096,
    .10, .12, 0, .16, .06, .072,
    .15, 0, .12, .06, .21, .048,
    0, .144, .096, .072, .048, .1824
  ), nrow = 6, byrow = TRUE)
  expect_equal(unname(Xi), Xi_memo, tolerance = 1e-12)

  # and the Jacobian
  Delta <- exametrika:::m2_delta(profile, idx, rep(2, 3))
  Delta_memo <- matrix(c(
    1, 0, 0, 0, 1, 0, 0, 0, 1,
    .4, .5, 0, .6, 0, .5, 0, .6, .4
  ), nrow = 6, byrow = TRUE)
  expect_equal(unname(Delta), Delta_memo, tolerance = 1e-12)
})

test_that("M2 is zero when the data match the model exactly", {
  profile <- array(0, dim = c(4, 2, 3))
  set.seed(9)
  for (j in 1:4) {
    for (cl in 1:2) {
      v <- runif(3, .5, 2)
      profile[j, cl, ] <- v / sum(v)
    }
  }
  idx <- exametrika:::m2_margin_index(rep(3, 4))
  A <- exametrika:::m2_class_products(profile, idx)
  out <- exametrika:::m2_core(profile, rep(3, 4),
    nobs = 500,
    p_vec = exametrika:::m2_pi(A)
  )
  expect_lt(out$M2, 1e-16)
})

test_that("the Jacobian loses rank as (ncls - 1)(ncls - 2) / 2", {
  skip_on_cran()
  # With the class proportions fixed, the second-order margins see the class
  # deviations only through their Gram matrix, which is invariant to rotations
  # of the (ncls - 1)-dimensional class space.
  dat <- dataFormat(head_cols(J20S600, 12), response.type = "nominal")
  for (k in 2:4) {
    fit <- LCA(dat, ncls = k)
    r <- M2(fit, verbose = FALSE)
    expect_equal(r$n_param - r$rank_delta, (k - 1) * (k - 2) / 2)
    expect_equal(r$df, r$m - r$rank_delta)
  }
})

test_that("M2 works from a fitted LCA and prints", {
  dat <- head_rows_dat(dataFormat(head_cols(J20S600, 12), response.type = "nominal"), 150)
  fit <- LCA(dat, ncls = 3)
  r <- M2(fit, verbose = FALSE)
  expect_true(inherits(r, "M2"))
  expect_true(is.finite(r$M2) && r$M2 > 0)
  expect_equal(r$m, sum(dat$categories - 1) +
    sum(outer(dat$categories - 1, dat$categories - 1)[upper.tri(diag(ncol(dat$Q)))]))
  expect_output(print(r), "Limited-information")
  # rated data goes through the same path
  expect_true(inherits(M2(LCA(head_rows_dat(J21S300, 100), ncls = 2), verbose = FALSE), "M2"))
  # models without a maximum likelihood fit of this kind are refused
  expect_error(M2(LCA(tiny_bin, ncls = 3)), "available for models fitted")
})

test_that("add_M2 attaches margin-based fit indices without touching the others", {
  skip_on_cran()
  dat <- dataFormat(head_cols(J20S600, 12), response.type = "nominal")
  fit <- LCA(dat, ncls = 3)
  fit2 <- add_M2(fit, verbose = FALSE)

  # the response-pattern indices are untouched
  expect_equal(fit2$TestFitIndices, fit$TestFitIndices)
  expect_true(inherits(fit2$TestFitIndicesM2, "ModelFitM2"))

  m <- fit2$TestFitIndicesM2
  # the statistic matches a direct call
  expect_equal(m$M2, M2(fit, verbose = FALSE)$M2)
  # the baseline is the independence model: it reproduces the first-order
  # margins, so its degrees of freedom are exactly the second-order cells
  ncat <- as.vector(dat$categories)
  expect_equal(m$df_null, m$n_margin - sum(ncat - 1))
  # a model with classes must fit the cross tables better than independence
  expect_lt(m$M2, m$M2_null)
  # incremental indices only; information criteria stay on the likelihood side
  expect_true(all(is.finite(c(m$NFI, m$RFI, m$IFI, m$TLI, m$CFI, m$RMSEA))))
  expect_null(m$AIC)
})

test_that("print shows both worlds of fit indices, and either alone", {
  skip_on_cran()
  dat <- dataFormat(head_cols(J20S600, 12), response.type = "nominal")
  fit <- add_M2(LCA(dat, ncls = 3), verbose = FALSE)
  expect_output(print(fit), "Response-pattern based")
  expect_output(print(fit), "Margin based")
  expect_output(print(fit, fit_indices = "margin"), "Margin based")
  out <- capture.output(print(fit, fit_indices = "pattern"))
  expect_false(any(grepl("Margin based", out)))
  # before add_M2 the margin block is simply absent
  bare <- LCA(dat, ncls = 2)
  expect_output(print(bare, fit_indices = "margin"), "Call add_M2")
})

test_that("add_M2 refuses models it cannot handle", {
  expect_error(add_M2(LCA(tiny_bin, ncls = 3)), "available for models fitted")
})

### M2 for LRA and Biclustering ---------------------------------------------

test_that("M2 for ordinal LRA matches LCA in structure", {
  skip_on_cran()
  dat <- dataFormat(J5S1000)
  a <- M2(suppressMessages(LRA(dat, nrank = 3, method = "isotonic")), verbose = FALSE)
  b <- M2(suppressMessages(LCA(dat, ncls = 3)), verbose = FALSE)
  # LRA is an ordered LCA: same margins, same parameters, same Jacobian rank.
  # Only the estimator differs, so only the statistic itself may differ.
  expect_equal(c(a$m, a$n_param, a$rank_delta), c(b$m, b$n_param, b$rank_delta))
  expect_true(is.finite(a$M2) && a$M2 > 0)
})

test_that("the Jacobian rank deficiency follows the same rule for LRA", {
  skip_on_cran()
  dat <- dataFormat(J5S1000)
  for (k in 2:4) {
    r <- M2(suppressMessages(LRA(dat, nrank = k, method = "isotonic")), verbose = FALSE)
    expect_equal(r$n_param - r$rank_delta, (k - 1) * (k - 2) / 2)
  }
})

test_that("M2 for biclustering counts field-shared parameters", {
  skip_on_cran()
  # J20S600 (20 items, 4 categories) rather than J35S500: the assertion is about
  # how the parameters are counted, not about the data, and M2 on 35 items costs
  # roughly 130x more under the reference BLAS that CRAN's Windows build uses.
  dat <- dataFormat(J20S600)
  ncat <- 4
  ncls <- 4
  nfld <- 3
  fit <- suppressMessages(Biclustering(dat, ncls = ncls, nfld = nfld, method = "B"))
  r <- M2(fit, verbose = FALSE)
  # one profile per (field, class), not per (item, class)
  expect_equal(r$n_param, nfld * ncls * (ncat - 1))
  expect_equal(r$n_param - r$rank_delta, (ncls - 1) * (ncls - 2) / 2)
  expect_equal(r$df, r$m - r$rank_delta)
})

test_that("the shared Jacobian doubles the derivative within a field", {
  # A pair margin whose two items sit in the same field and name the same
  # category is (1/C) sum_c rho^2, so its derivative is twice rho, not rho.
  profile <- array(0, dim = c(2, 2, 3))
  profile[1, , ] <- profile[2, , ] <- matrix(c(.5, .3, .2, .2, .3, .5), nrow = 2, byrow = TRUE)
  idx <- exametrika:::m2_margin_index(rep(3, 2))
  D <- exametrika:::m2_delta_shared(profile, idx, rep(3, 2), field = c(1, 1))
  # the pair margin (item1 cat1, item2 cat1) is the third row of the pair block
  pair_row <- nrow(idx$single) + which(idx$pair[, "cat1"] == 1 & idx$pair[, "cat2"] == 1)
  # its derivative with respect to (field 1, category 1, class c) is 2 rho / C
  expect_equal(unname(D[pair_row, 1:2]), unname(profile[1, , 1]))
})

test_that("biclustering M2 refuses ragged category counts", {
  # J5S1000 has 4/3/4/3/4 categories; a field profile cannot serve them all
  dat <- head_rows_dat(dataFormat(J5S1000), 200)
  fit <- suppressMessages(Biclustering(dat, ncls = 3, nfld = 2, method = "B"))
  expect_error(M2(fit, verbose = FALSE), "same number of categories")
})

test_that("the M2 object stays small whatever the item count", {
  skip_on_cran()
  # the simulation keeps one of these per fit, so it must not carry the
  # residual vector or anything else that grows with m
  dat <- dataFormat(J20S600)
  fit <- suppressMessages(Biclustering(dat, ncls = 3, nfld = 2, method = "B"))
  r <- M2(fit, verbose = FALSE)
  expect_lt(as.numeric(object.size(r)), 10000)
  expect_null(r$residual)
})

test_that("add_M2 works for LRA and biclustering, with an honest caveat", {
  skip_on_cran()
  dat <- dataFormat(J5S1000)

  # ordinal LRA: order restriction can bind, so the p value is descriptive
  fit <- add_M2(suppressMessages(LRA(dat, nrank = 3, method = "isotonic")), verbose = FALSE)
  m <- fit$TestFitIndicesM2
  expect_true(inherits(m, "ModelFitM2"))
  expect_match(m$caveat, "order restriction")
  expect_output(print(fit), "Margin based")
  expect_output(print(fit), "descriptive only")

  fit_gtm <- add_M2(suppressMessages(LRA(dat, nrank = 3, method = "GTM")), verbose = FALSE)
  expect_match(fit_gtm$TestFitIndicesM2$caveat, "regularisation")

  # the response-pattern indices are untouched, and both blocks can be shown alone
  expect_equal(fit$TestFitIndices, suppressMessages(
    LRA(dat, nrank = 3, method = "isotonic")
  )$TestFitIndices)
  out <- capture.output(print(fit, fit_indices = "pattern"))
  expect_false(any(grepl("Margin based", out)))
})

test_that("biclustering caveats name the reason that applies", {
  # The caveat is a pure function of $model and $estimation (m2_caveat_*), so it
  # needs no particular data -- only a fit that records model 2 and the filter.
  # A generated 120x12 frame does that in 0.3 s; J35S500 took roughly 20 min
  # under the reference BLAS in CRAN's Windows build, which is what sank 1.13.0.
  set.seed(1)
  Q <- matrix(sample(1:4, 12 * 120, replace = TRUE), nrow = 120, ncol = 12)
  colnames(Q) <- paste0("Item", seq_len(12))
  dat <- dataFormat(as.data.frame(cbind(ID = seq_len(120), Q)))
  b <- add_M2(suppressMessages(Biclustering(dat, ncls = 3, nfld = 2, method = "B")),
    verbose = FALSE
  )
  # the field partition is always taken as given
  expect_match(b$TestFitIndicesM2$caveat, "field partition")
  expect_false(grepl("regularisation", b$TestFitIndicesM2$caveat))

  r <- add_M2(suppressMessages(Biclustering(dat,
    ncls = 3, nfld = 2,
    method = "R", estimation = "GTM"
  )), verbose = FALSE)
  expect_match(r$TestFitIndicesM2$caveat, "field partition")
  expect_match(r$TestFitIndicesM2$caveat, "regularisation")
  expect_output(print(b), "Margin based")
})

test_that("the margin baseline does not depend on the model family", {
  skip_on_cran()
  # the independence model is fitted to the data, not to the model, so the
  # same data gives the same baseline whichever arm was run
  dat <- dataFormat(J5S1000)
  a <- add_M2(suppressMessages(LRA(dat, nrank = 3, method = "isotonic")), verbose = FALSE)
  b <- add_M2(suppressMessages(LCA(dat, ncls = 4)), verbose = FALSE)
  expect_equal(a$TestFitIndicesM2$M2_null, b$TestFitIndicesM2$M2_null)
  expect_equal(a$TestFitIndicesM2$df_null, b$TestFitIndicesM2$df_null)
})
