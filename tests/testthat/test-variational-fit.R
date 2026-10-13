# Variational fit quantities (elbo, log_lik_marginal, entropy, ICL) ---------
#
# Added in 2.2.0. The identities below pin down what each field means, so a
# later edit cannot silently change one of them into another quantity.

test_that("helpers: entropy, log-sum-exp, hard assignment", {
  M <- matrix(c(0.5, 0.5, 1, 0, 0.2, 0.8), ncol = 2, byrow = TRUE)
  expect_equal(
    exametrika:::membership_entropy(M),
    -(2 * 0.5 * log(0.5)) - (0.2 * log(0.2) + 0.8 * log(0.8))
  )
  L <- matrix(c(-1000, -1001, 0, 1), ncol = 2, byrow = TRUE)
  expect_equal(
    exametrika:::sum_row_logsumexp(L),
    (-1000 + log(1 + exp(-1))) + (1 + log(1 + exp(-1)))
  )
  expect_equal(exametrika:::hard_membership(M), matrix(c(1, 0, 1, 0, 0, 1), ncol = 2, byrow = TRUE))
  expect_equal(exametrika:::completed_mixing_term(c(3, 0, 1)), 3 * log(3 / 4) + 1 * log(1 / 4))
})

test_that("one-mode models: elbo is the observed-data log-likelihood", {
  # Q-function + entropy - n log C = marginal, exactly, when the posterior is
  # the one the EM ended on. The reported log_lik of the binary LCA/LRA is the
  # Q-function (expected log-likelihood) of the reference implementation.
  lca <- LCA(J15S500, ncls = 3)
  expect_equal(lca$elbo, lca$log_lik - lca$nobs * log(3) + unname(lca$entropy["class"]),
    tolerance = 1e-6
  )
  expect_equal(lca$elbo, lca$log_lik_marginal)
  expect_true(lca$entropy["class"] >= 0 && lca$entropy["class"] <= lca$nobs * log(3))
  expect_true(is.finite(lca$ICL))

  lra <- LRA(J15S500, nrank = 3, method = "isotonic")
  expect_equal(lra$elbo, lra$log_lik - lra$nobs * log(3) + unname(lra$entropy["class"]),
    tolerance = 1e-6
  )

  # LCA.nominal already reported the observed-data log-likelihood
  nom <- dataFormat(cbind(ID = seq_len(nrow(J15S3810$Q)), as.data.frame(J15S3810$Q)),
    response.type = "nominal"
  )
  lcn <- LCA(nom, ncls = 3)
  expect_equal(lcn$elbo, lcn$log_lik)
  expect_equal(lcn$elbo, lcn$log_lik_marginal)
})

test_that("binary biclustering: elbo = expected log-likelihood + entropies - prior constants", {
  fit <- Biclustering(J35S515, ncls = 4, nfld = 3, method = "B")
  expect_equal(
    fit$elbo,
    fit$log_lik - fit$nobs * log(4) - fit$testlength * log(3) + sum(fit$entropy),
    tolerance = 1e-8
  )
  expect_named(fit$entropy, c("class", "field"))
  expect_true(all(fit$entropy >= 0))
  expect_true(is.finite(fit$log_lik_marginal) && is.finite(fit$ICL))
})

test_that("polytomous biclustering: log_lik is the elbo and the EM ascends it", {
  dat <- J15S3810
  for (arm in list(list(method = "B"), list(method = "R", estimation = "isotonic"))) {
    out <- capture.output(
      # one start, so the printed trace is a single EM run
      fit <- do.call(Biclustering, c(list(dat, ncls = 3, nfld = 3, verbose = TRUE, init = "quantile"), arm)),
      type = "message"
    )
    expect_equal(fit$log_lik, fit$elbo)
    expect_equal(fit$TestFitIndices$model_log_like, fit$elbo)
    trace <- as.numeric(sub(
      "^iter [0-9]+ elbo (-?[0-9.]+).*$", "\\1",
      grep("^iter [0-9]+ elbo", out, value = TRUE)
    ))
    expect_gt(length(trace), 2)
    # non-decreasing up to the printed precision (6 significant digits)
    expect_true(all(diff(trace) >= -1e-5 * abs(trace[-1])))
  }
})

test_that("polytomous biclustering: log_lik_marginal matches a direct computation", {
  dat <- J5S1000
  fit <- Biclustering(dat, ncls = 3, nfld = 2, method = "B")
  Q <- dat$Q
  Z <- dat$Z
  fld <- fit$FieldEstimated
  const <- exp(-ncol(Q))
  # sum_s log sum_c (1/C) prod_j pi_{f_j c q_sj}
  L <- matrix(0, nrow(Q), 3)
  for (s in seq_len(nrow(Q))) {
    for (j in seq_len(ncol(Q))) {
      if (Z[s, j] == 1) L[s, ] <- L[s, ] + log(fit$FRP[fld[j], , Q[s, j]] + const)
    }
  }
  direct <- sum(log(rowSums(exp(L) / 3)))
  expect_equal(fit$log_lik_marginal, direct, tolerance = 1e-8)
})

test_that("rated models carry the nominal-layer quantities", {
  dat <- dataFormat(cbind(ID = 1:300, as.data.frame(J35S5000$Q[1:300, ])),
    CA = J35S5000$CA, response.type = "rated"
  )
  fit <- Biclustering(dat, ncls = 3, nfld = 2, maxiter = 50)
  expect_equal(fit$log_lik_nominal, fit$elbo_nominal)
  expect_named(fit$entropy_nominal, c("class", "field"))
  expect_true(is.finite(fit$ICL_nominal))
})

test_that("print shows the variational block", {
  fit <- Biclustering(J5S1000, ncls = 3, nfld = 2, method = "B")
  expect_output(print(fit), "Variational Fit")
  expect_output(print(fit), "log_lik_marginal")
})
