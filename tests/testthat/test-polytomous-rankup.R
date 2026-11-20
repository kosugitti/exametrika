# Rank-up / rank-down odds and RankUpShift for the ordinal Ranklustering
# (2.3.0). The MSPS values of the A3 paper were checked against the analysis
# script outside the package (13_rankup_charts.R); here the definitions are
# pinned on small generated data.

.rankup_cache <- NULL
rankup_data <- function() {
  if (is.null(.rankup_cache)) {
    set.seed(23)
    cls <- rep(1:4, each = 60)
    U <- sapply(1:6, function(j) {
      eta <- (cls - 2.5) * 0.9 + rnorm(length(cls), 0, 0.7) + if (j > 3) 0.4 else 0
      return(as.integer(cut(eta, c(-Inf, -1, 0, 1, Inf))) - 1L) # codes 0..3
    })
    colnames(U) <- paste0("Item", 1:6)
    .rankup_cache <<- suppressMessages(dataFormat(U, response.type = "ordinal"))
  }
  return(.rankup_cache)
}
rfit <- function(...) suppressMessages(suppressWarnings(Biclustering(rankup_data(), ...)))

test_that("ordinal Ranklustering returns the rank-up and rank-down odds", {
  fit <- rfit(ncls = 4, nfld = 2, method = "R", nstart = 0)
  S <- fit$Students
  expect_equal(
    colnames(S),
    c(paste("Membership", 1:4), "Estimate", "Rank-Up Odds", "Rank-Down Odds")
  )
  M <- fit$ClassMembership
  cls <- S[, "Estimate"]
  n <- nrow(M)
  up <- ifelse(cls < 4, M[cbind(1:n, pmin(cls + 1, 4))] / M[cbind(1:n, cls)], NA)
  down <- ifelse(cls > 1, M[cbind(1:n, pmax(cls - 1, 1))] / M[cbind(1:n, cls)], NA)
  expect_equal(unname(S[, "Rank-Up Odds"]), unname(up))
  expect_equal(unname(S[, "Rank-Down Odds"]), unname(down))
  expect_true(all(is.na(S[cls == 4, "Rank-Up Odds"])))
  expect_true(all(is.na(S[cls == 1, "Rank-Down Odds"])))
  # the estimated rank has the largest membership, so the odds are at most 1
  expect_true(all(S[, c("Rank-Up Odds", "Rank-Down Odds")] <= 1, na.rm = TRUE))
})

test_that("RankUpShift is the step of the upper-cumulative FRP", {
  fit <- rfit(ncls = 4, nfld = 2, method = "R", nstart = 0)
  D <- fit$RankUpShift
  maxQ <- dim(fit$FRP)[3]
  expect_equal(dim(D), c(3, 2, maxQ - 1))
  expect_equal(dimnames(D)[[1]], c("Rank1->Rank2", "Rank2->Rank3", "Rank3->Rank4"))
  expect_equal(dimnames(D)[[2]], c("Field1", "Field2"))
  # the data are coded 0..3, so the boundaries carry the raw codes 1..3
  expect_equal(dimnames(D)[[3]], c(">=1", ">=2", ">=3"))
  for (f in 1:2) {
    up <- t(apply(fit$FRP[f, , ], 1, function(p) rev(cumsum(rev(p)))))[, -1]
    expect_equal(unname(D[, f, ]), unname(pmax(diff(up), 0)), tolerance = 1e-6)
  }
  # isotonic: never negative
  expect_true(all(D >= 0))
})

test_that("GTM keeps negative steps and plain Biclustering has no rank outputs", {
  gtm <- rfit(ncls = 4, nfld = 2, method = "R", estimation = "GTM", nstart = 0)
  up <- t(apply(gtm$FRP[1, , ], 1, function(p) rev(cumsum(rev(p)))))[, -1]
  expect_equal(unname(gtm$RankUpShift[, 1, ]), unname(diff(up)))
  expect_true("Rank-Up Odds" %in% colnames(gtm$Students))
  b <- rfit(ncls = 3, nfld = 2, method = "B", nstart = 0)
  expect_null(b$RankUpShift)
  expect_equal(colnames(b$Students), c(paste("Membership", 1:3), "Estimate"))
  expect_error(plot(b, type = "RankUpShift"), "Ranklustering")
  expect_error(plot(b, type = "RankOdds"), "Ranklustering")
})

test_that("plot draws RankUpShift and RankOdds", {
  fit <- rfit(ncls = 4, nfld = 2, method = "R", nstart = 0)
  tf <- tempfile(fileext = ".png")
  on.exit(unlink(tf))
  expect_no_error(plot(fit, type = "RankUpShift", filename = tf))
  expect_true(file.exists(tf))
  expect_no_error(plot(fit, type = "RankOdds", filename = tf))
})
