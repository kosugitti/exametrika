# mixing = c("equal", "estimate") for the polytomous Biclustering (2.3.0):
# ordinal (isotonic / GTM / plain), nominal and rated. Class side only; the
# field weights stay at 1/F. The equal-weight fits are pinned elsewhere
# (fixtures and earlier results), so here the equal path is only checked for
# the new fields it returns.

# Small ordinal data with unequal class sizes, generated here so that no
# dataset is added to the package. Fitted lazily (see the testing notes in
# CLAUDE.md: no estimation at file scope).
.mix_cache <- NULL
mix_raw <- function() {
  if (is.null(.mix_cache)) {
    set.seed(11)
    n <- c(150, 60, 30)
    loc <- c(-0.8, 0.3, 1.2)
    cls <- rep(seq_along(n), n)
    U <- sapply(1:6, function(j) {
      eta <- loc[cls] + rnorm(length(cls), 0, 0.6)
      return(as.integer(cut(eta, c(-Inf, -0.5, 0.3, 1, Inf))))
    })
    colnames(U) <- paste0("Item", 1:6)
    .mix_cache <<- U
  }
  return(.mix_cache)
}
mix_data <- function(type = "ordinal") {
  return(suppressMessages(dataFormat(mix_raw(), response.type = type)))
}
qfit <- function(...) suppressMessages(suppressWarnings(Biclustering(...)))

test_that("ordinal Biclustering: equal is the default and returns 1/C", {
  dat <- mix_data()
  eq <- qfit(dat, ncls = 3, nfld = 2, method = "R", nstart = 0)
  expect_equal(eq$mixing, "equal")
  expect_equal(unname(eq$ClassWeight), rep(1 / 3, 3))
  expect_equal(eq$LogPrior, log(eq$ClassWeight))
  expect_no_match(paste(capture.output(print(eq)), collapse = "\n"), "Weight \\(estimated\\)")
  expect_error(qfit(dat, ncls = 3, nfld = 2, mixing = "free"), "should be one of")
})

test_that("ordinal Ranklustering mixing = 'estimate' works with isotonic and GTM", {
  dat <- mix_data()
  for (est in c("isotonic", "GTM")) {
    eq <- qfit(dat, ncls = 3, nfld = 2, method = "R", estimation = est, nstart = 0)
    es <- qfit(dat, ncls = 3, nfld = 2, method = "R", estimation = est, nstart = 0, mixing = "estimate")
    expect_equal(es$mixing, "estimate")
    expect_equal(sum(es$ClassWeight), 1)
    expect_true(all(es$ClassWeight > 0))
    expect_equal(es$LogPrior, log(es$ClassWeight))
    # C - 1 weights charged to the test df and to the variational penalty
    expect_equal(es$TestFitIndices$model_df, eq$TestFitIndices$model_df - 2)
    expect_output(print(es), "Rank Weight \\(estimated\\)")
    # The returned posterior is the E-step under the estimated weights, so
    # predict() on the same data reproduces it
    p <- predict(es, dat)
    expect_equal(unname(as.matrix(p$Membership)), unname(es$ClassMembership), tolerance = 1e-10)
    # At the fixed point the weights are the mean posterior
    expect_equal(unname(es$ClassWeight), unname(colMeans(es$ClassMembership)), tolerance = 0.02)
  }
})

test_that("unequal classes get unequal weights and the isotonic order holds", {
  dat <- mix_data()
  es <- qfit(dat, ncls = 3, nfld = 2, method = "R", nstart = 0, mixing = "estimate")
  # the generating sizes are 150 : 60 : 30
  expect_gt(es$ClassWeight[1], es$ClassWeight[3])
  expect_true(es$SOACflg || es$WOACflg)
  expect_true(is.finite(es$ICL_exact) || is.na(es$ICL_exact_se))
})

test_that("plain ordinal Biclustering and mic carry the weights along", {
  dat <- mix_data()
  es <- qfit(dat, ncls = 3, nfld = 2, method = "B", nstart = 0, mic = TRUE, mixing = "estimate")
  expect_equal(sum(es$ClassWeight), 1)
  # mic reorders the classes; the weights must follow them
  expect_equal(unname(es$ClassWeight), unname(colMeans(es$ClassMembership)), tolerance = 0.02)
})

test_that("with conf_class the estimated weights are the given class shares", {
  dat <- mix_data()
  cc <- rep(1:3, c(120, 80, 40))
  es <- qfit(dat, ncls = 3, nfld = 2, method = "R", conf_class = cc, mixing = "estimate")
  expect_equal(unname(es$ClassWeight), c(120, 80, 40) / 240)
})

test_that("nominal and rated Biclustering accept mixing = 'estimate'", {
  nom <- mix_data("nominal")
  es <- qfit(nom, ncls = 3, nfld = 2, mixing = "estimate")
  eq <- qfit(nom, ncls = 3, nfld = 2)
  expect_s3_class(es, "nominalBiclustering")
  expect_equal(sum(es$ClassWeight), 1)
  expect_equal(eq$mixing, "equal")
  # two more parameters in the information criteria
  expect_equal(es$TestFitIndices$AIC - (-2 * es$elbo), eq$TestFitIndices$AIC - (-2 * eq$elbo) + 4)
  expect_output(print(es), "Class Weight \\(estimated\\)")
  p <- predict(es, nom)
  expect_equal(unname(as.matrix(p$Membership)), unname(es$ClassMembership), tolerance = 1e-10)
})

test_that("rated Biclustering sorts the weights with the classes", {
  skip_on_cran()
  es <- qfit(J21S300, ncls = 3, nfld = 3, method = "R", mixing = "estimate")
  expect_s3_class(es, "ratedBiclustering")
  expect_equal(es$mixing, "estimate")
  expect_equal(sum(es$ClassWeight), 1)
  expect_equal(unname(es$ClassWeight), unname(colMeans(es$ClassMembership)), tolerance = 0.01)
  expect_output(print(es), "Rank Weight \\(estimated\\)")
})

# equal categories on every item, as M2() on a biclustering requires
m2_mix_data <- function() {
  set.seed(3)
  field <- rep(1:2, length.out = 6)
  cls <- sample.int(3, 400, TRUE, prob = c(0.6, 0.3, 0.1))
  U <- sapply(1:6, function(j) {
    loc <- (cls - 2) * ifelse(field[j] == 1, 1.2, -1.2)
    return(as.integer(cut(loc + rlogis(400), c(-Inf, -0.6, 0.6, Inf))))
  })
  colnames(U) <- paste0("Item", 1:6)
  return(suppressMessages(dataFormat(U, response.type = "ordinal")))
}

test_that("M2() counts the estimated class weights (2.4.0)", {
  es <- qfit(m2_mix_data(), ncls = 3, nfld = 2, method = "B", init = "quantile", mixing = "estimate")
  m_es <- M2(es, verbose = FALSE)
  # 2 fields x 2 free categories x 3 classes, plus 2 free weights
  expect_equal(m_es$n_param, 14)
  expect_true(is.finite(m_es$M2) && m_es$df > 0)
  # analytic Jacobian (profiles and weights) against a numerical one
  field_hat <- as.vector(es$FieldEstimated)
  ncat <- rep(3, 6)
  idx <- m2_margin_index(ncat)
  FRP <- unclass(es$FRP)
  expand <- function(F) {
    P <- array(0, c(6, 3, 3))
    for (j in 1:6) P[j, , ] <- F[field_hat[j], , ]
    return(P)
  }
  w <- as.vector(es$ClassWeight)
  marg <- function(F, ww) m2_pi(m2_class_products(expand(F), idx), ww)
  A <- m2_class_products(expand(FRP), idx)
  D <- m2_delta_shared(expand(FRP), idx, ncat, field_hat, w = w, A = A)
  h <- 1e-6
  num <- NULL
  for (f in 1:2) for (qq in 1:2) for (cc in 1:3) {
    Fp <- FRP
    Fp[f, cc, qq] <- Fp[f, cc, qq] + h
    Fm <- FRP
    Fm[f, cc, qq] <- Fm[f, cc, qq] - h
    num <- cbind(num, (marg(Fp, w) - marg(Fm, w)) / (2 * h))
  }
  for (cc in 1:2) {
    wp <- w
    wp[c(cc, 3)] <- wp[c(cc, 3)] + c(h, -h)
    wm <- w
    wm[c(cc, 3)] <- wm[c(cc, 3)] - c(h, -h)
    num <- cbind(num, (marg(FRP, wp) - marg(FRP, wm)) / (2 * h))
  }
  expect_equal(unname(D), unname(num), tolerance = 1e-7)
})

test_that("add_M2() works on an estimated-weight fit and equal M2 is unchanged in form", {
  skip_on_cran()
  dat <- m2_mix_data()
  eq <- qfit(dat, ncls = 3, nfld = 2, method = "B", nstart = 0)
  es <- qfit(dat, ncls = 3, nfld = 2, method = "B", nstart = 0, mixing = "estimate")
  expect_equal(M2(es, verbose = FALSE)$n_param, M2(eq, verbose = FALSE)$n_param + 2)
  fit <- add_M2(es, verbose = FALSE)
  expect_false(is.null(fit$TestFitIndicesM2))
})
