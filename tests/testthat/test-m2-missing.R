# M2 with missing responses (2.3.0). The observed margins are available-case
# proportions, so their covariance scales with the number who answered each
# item (pair), not with the number of respondents. A respondent with every
# response missing changes neither the margins nor the counts, so it must not
# change M2. Before 2.3.0 the statistic was multiplied by the total N, and
# adding 100 empty rows to 300 complete ones inflated it by exactly 400/300.

m2_missing_data <- function() {
  set.seed(4)
  X <- matrix(sample(1:3, 300 * 6, TRUE, prob = c(.5, .3, .2)), 300)
  X[1:150, 1:3] <- X[1:150, 1:3] %% 3 + 1
  colnames(X) <- paste0("V", 1:6)
  return(X)
}

test_that("respondents with every response missing do not change M2 (LCA)", {
  X <- m2_missing_data()
  Xe <- rbind(X, matrix(NA, 100, 6))
  f1 <- suppressMessages(LCA(dataFormat(X, response.type = "nominal"), ncls = 2))
  f2 <- suppressMessages(LCA(dataFormat(Xe, response.type = "nominal"), ncls = 2))
  m1 <- M2(f1, verbose = FALSE)
  m2 <- M2(f2, verbose = FALSE)
  expect_equal(m2$M2, m1$M2, tolerance = 1e-8)
  expect_equal(m2$df, m1$df)
  expect_match(m2$caveat, "missing completely at random")
})

test_that("respondents with every response missing do not change M2 (Biclustering)", {
  X <- m2_missing_data()
  Xe <- rbind(X, matrix(NA, 100, 6))
  q <- function(e) suppressMessages(suppressWarnings(e))
  # With estimated weights the empty rows have the weights as their posterior,
  # which leaves the fixed point where it is but slows the EM, so the two fits
  # stop at slightly different weights; hence the looser tolerance there.
  for (mix in c("equal", "estimate")) {
    f1 <- q(Biclustering(dataFormat(X, response.type = "nominal"), ncls = 2, nfld = 2, mixing = mix))
    f2 <- q(Biclustering(dataFormat(Xe, response.type = "nominal"), ncls = 2, nfld = 2, mixing = mix))
    expect_equal(M2(f2, verbose = FALSE)$M2, M2(f1, verbose = FALSE)$M2,
      tolerance = if (mix == "equal") 1e-8 else 1e-3
    )
  }
})
