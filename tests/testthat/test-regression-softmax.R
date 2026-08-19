# Regression tests for the posterior normalisation fix (2026-07-27, v2.0.0)
#
# The E-steps used to subtract the row minimum before exponentiating and then
# clip at exp(700). Rows that spread wider than 700 collapsed onto the clip, so
# memberships that should have differed by hundreds of orders of magnitude came
# out equal. A field posterior sums over respondents, so the spread grows with
# the sample: the failure appeared only above roughly 700 respondents, which is
# why the reference fixtures (515 respondents at most) never caught it.

test_that("row_softmax normalises without overflow at any spread", {
  # A spread of 5000 nats is far past exp(700); the old code returned 0.5/0.5.
  x <- matrix(c(0, 5000, 0, -5000), nrow = 2, byrow = TRUE)
  p <- exametrika:::row_softmax(x)

  expect_equal(rowSums(p), c(1, 1))
  expect_true(all(is.finite(p)))
  expect_equal(p[1, ], c(0, 1))
  expect_equal(p[2, ], c(1, 0))
})

test_that("row_softmax agrees with the direct formula in the safe range", {
  set.seed(20260727)
  x <- matrix(stats::rnorm(200, sd = 3), nrow = 50)
  direct <- exp(x) / rowSums(exp(x))

  expect_equal(exametrika:::row_softmax(x), direct)
})

test_that("ordinal biclustering recovers fields as the sample grows", {
  # Three fields of 4 items, separated by a location shift. Recovery must not
  # get worse with more data; before the fix it collapsed to two fields at 1000.
  set.seed(20260727)
  nitems <- 12
  nfld <- 3
  ncls <- 3
  field <- rep(seq_len(nfld), each = nitems / nfld)
  mu <- c(-1.5, 0, 1.5)
  offset <- c(-1, 0, 1)
  tau <- c(-1.5, 0, 1.5)

  gen <- function(nobs) {
    cls <- rep(seq_len(ncls), length.out = nobs)
    Q <- matrix(0L, nrow = nobs, ncol = nitems)
    for (j in seq_len(nitems)) {
      for (c in seq_len(ncls)) {
        who <- which(cls == c)
        cum <- stats::pnorm(tau - (mu[c] + offset[field[j]]))
        Q[who, j] <- sample.int(4, length(who),
          replace = TRUE,
          prob = diff(c(0, cum, 1))
        )
      }
    }
    return(Q)
  }

  for (nobs in c(300, 1200)) {
    Q <- gen(nobs)
    dat <- suppressMessages(dataFormat(
      cbind(ID = seq_len(nobs), as.data.frame(Q)),
      response.type = "ordinal"
    ))
    fit <- suppressWarnings(suppressMessages(
      Biclustering(dat, ncls = ncls, nfld = nfld, method = "R")
    ))
    est <- as.vector(fit$FieldEstimated)

    # Every field must be used: the failure mode was empty fields.
    expect_length(unique(est), nfld)
    # Items of the same true field must land together, whatever the labels are.
    expect_equal(length(unique(est[field == 1])), 1)
    expect_equal(length(unique(est[field == 2])), 1)
    expect_equal(length(unique(est[field == 3])), 1)
  }
})

test_that("memberships stay finite and normalised on a large ordinal sample", {
  set.seed(20260727)
  nobs <- 1500
  Q <- matrix(sample.int(5, nobs * 15, replace = TRUE), nrow = nobs)
  dat <- suppressMessages(dataFormat(
    cbind(ID = seq_len(nobs), as.data.frame(Q)),
    response.type = "ordinal"
  ))
  fit <- suppressWarnings(suppressMessages(
    Biclustering(dat, ncls = 3, nfld = 3, method = "R")
  ))

  expect_true(all(is.finite(fit$ClassMembership)))
  expect_true(all(is.finite(fit$FieldMembership)))
  expect_equal(rowSums(fit$ClassMembership), rep(1, nobs),
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  expect_equal(rowSums(fit$FieldMembership), rep(1, 15),
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
})

# Order-restricted biclustering on a long test (2026-07-27, v2.0.0)
#
# The M-step pools adjacent categories, so upper-cumulative differences tie
# exactly, and a tie computed as a subtraction lands within a few ulp of zero --
# sometimes below it. `const` is exp(-nitems), which drops under double
# precision noise past about 37 items, so log() of the difference returned NaN,
# the NaN reached the field posterior through the softmax, and every item came
# back unassigned.

test_that("isotonic biclustering assigns every item on a long test", {
  skip_on_cran()
  set.seed(20260727)
  nitems <- 72
  nfld <- 6
  ncls <- 4
  nobs <- 600
  field <- rep(seq_len(nfld), each = nitems / nfld)
  mu <- seq(-2, 2, length.out = ncls)
  offset <- seq(-1.5, 1.5, length.out = nfld)
  tau <- seq(-2.5, 2.5, length.out = 4)

  cls <- rep(seq_len(ncls), length.out = nobs)
  Q <- matrix(0L, nrow = nobs, ncol = nitems)
  for (j in seq_len(nitems)) {
    for (c in seq_len(ncls)) {
      who <- which(cls == c)
      cum <- stats::pnorm(tau - (mu[c] + offset[field[j]]))
      Q[who, j] <- sample.int(5, length(who),
        replace = TRUE,
        prob = diff(c(0, cum, 1))
      )
    }
  }
  dat <- suppressMessages(dataFormat(
    cbind(ID = seq_len(nobs), as.data.frame(Q)),
    response.type = "ordinal"
  ))

  fit <- suppressWarnings(suppressMessages(
    Biclustering(dat,
      ncls = ncls, nfld = nfld, method = "R",
      estimation = "isotonic"
    )
  ))

  est <- as.vector(fit$FieldEstimated)
  expect_false(anyNA(est))
  expect_true(all(est %in% seq_len(nfld)))
  expect_true(all(is.finite(fit$FRP)))
  expect_true(fit$converge)
})

test_that("tied upper-cumulative probabilities survive the log", {
  # A field whose adjacent categories are pooled: the differences are exactly
  # zero, and a subtraction of the cumulative form can undershoot.
  P <- matrix(c(0.25, 0.25, 0.25, 0.25), nrow = 1)
  upper <- exametrika:::iso_upper_cum(P)
  bb <- cbind(1, upper, 0)
  d <- bb[, seq_len(ncol(P)), drop = FALSE] -
    bb[, seq_len(ncol(P)) + 1, drop = FALSE]

  const <- exp(-72)
  expect_true(all(is.finite(log(pmax(d, 0) + const))))
})
