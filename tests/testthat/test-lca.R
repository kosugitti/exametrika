library(exametrika)

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
tmp <- dataFormat(J15S500, na = -99)
model <- LCA(tmp, ncls = 5)

### Tests
test_that("LCA Test Info", {
  expect <- test[14:29, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- model$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LCA Class Info", {
  ## TRP
  expect <- class[1, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$TRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- class[2, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$LCD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- class[3, 2:6] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$CMD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("LCA Item Info", {
  ## IRP
  expect <- items[, 6:10] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## FitIndex
  expect <- items[, c(15, 11, 12, 16, 13, 17, 14, 18:26)] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$ItemFitIndices |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LCA Students", {
  ## Membership
  expect <- student[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

### S3 dispatch -------------------------------------------------------------

test_that("LCA dispatches on response type", {
  expect_true(inherits(model, "LCA"))
  # raw input and a pre-formatted object take the same route
  expect_equal(LCA(J15S500, ncls = 5)$log_lik, model$log_lik)
  # every response type now has a method, each with its own return class
  expect_true(inherits(LCA(J21S300, ncls = 2), "ratedLCA"))
  expect_true(inherits(LCA(dataFormat(J20S600, response.type = "nominal"), ncls = 2), "nominalLCA"))
})

### Nominal LCA -------------------------------------------------------------

nominal_data <- dataFormat(J20S600, response.type = "nominal")
nominal_model <- LCA(nominal_data, ncls = 3)

test_that("nominal LCA returns its own class and converges", {
  expect_s3_class(nominal_model, "exametrika")
  expect_true(inherits(nominal_model, "nominalLCA"))
  expect_true(nominal_model$converge)
  expect_equal(nominal_model$n_class, 3)
})

test_that("nominal LCA category profiles are proper distributions", {
  probs <- nominal_model$ICRP[, paste0("class", 1:3)]
  expect_true(all(probs >= 0))
  # every (item, class) column of category probabilities sums to 1
  for (cl in paste0("class", 1:3)) {
    sums <- tapply(nominal_model$ICRP[[cl]], nominal_model$ICRP$ItemLabel, sum)
    expect_equal(as.numeric(sums), rep(1, ncol(nominal_data$Q)), tolerance = 1e-8)
  }
  # one row per (item, category), so ragged category counts stay aligned
  expect_equal(nrow(nominal_model$ICRP), sum(nominal_data$categories))
})

test_that("nominal LCA reports information criteria only", {
  fit <- nominal_model$TestFitIndices
  expect_true(is.na(fit$bench_log_like))
  expect_true(all(is.na(c(fit$model_Chi_sq, fit$model_df, fit$CFI, fit$RMSEA))))
  expect_true(all(is.finite(c(fit$AIC, fit$BIC, fit$CAIC))))
  # information criteria follow the -2 log L + k penalty convention here,
  # not the chi-square based one used on the binary path
  nparam <- 3 * sum(nominal_data$categories - 1)
  expect_equal(fit$AIC, -2 * nominal_model$log_lik + 2 * nparam)
  expect_equal(fit$BIC, -2 * nominal_model$log_lik + nparam * log(nominal_model$nobs))
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
  db <- dataFormat(J15S500)
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
  expect_output(print(nominal_model), "Item Category Reference Profile")
  expect_output(print(nominal_model), "Number of Latent class")
})

test_that("nominal LCA plots", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot(nominal_model, type = "ICRP", items = 1:2, nc = 1, nr = 2))
  expect_silent(plot(nominal_model, type = "LCD"))
  expect_silent(plot(nominal_model, type = "CMP", students = 1:2, nc = 1, nr = 2))
  # types that need a correct-response rate are not defined without a key
  expect_error(plot(nominal_model, type = "IRP"), "does not correspond")
})

test_that("nominal data given to LRA is handed to LCA", {
  expect_message(m <- LRA(nominal_data, nrank = 4), "Latent ranks require ordered")
  expect_true(inherits(m, "nominalLCA"))
  # nrank is the LRA spelling of ncls and must not be dropped
  expect_equal(m$n_class, 4)
})

### Rated LCA ---------------------------------------------------------------

rated_model <- LCA(J21S300, ncls = 3)

test_that("rated LCA estimates through the nominal engine", {
  expect_true(inherits(rated_model, "ratedLCA"))
  expect_true(rated_model$converge)
  # the estimation is the nominal one, so the category profiles must match
  nom <- dataFormat(J21S300)
  nom$response.type <- "nominal"
  n <- LCA(nom, ncls = 3)
  expect_equal(rated_model$ICRP[, paste0("class", 1:3)], n$ICRP[, paste0("class", 1:3)])
  expect_equal(rated_model$log_lik_nominal, n$log_lik)
})

test_that("rated LCA IRP is the keyed category probability", {
  dat <- dataFormat(J21S300)
  ncat <- as.vector(dat$categories)
  offset <- c(0, cumsum(ncat)[-length(ncat)])
  keyed <- as.matrix(rated_model$ICRP[offset + dat$CA, paste0("class", 1:3)])
  expect_equal(unname(rated_model$IRP), unname(keyed))
  expect_true(all(rated_model$IRP >= 0 & rated_model$IRP <= 1))
  # TRP is the weighted item sum of the IRP
  expect_equal(rated_model$TRP, as.vector(t(rated_model$IRP) %*% dat$w))
})

test_that("rated LCA reports both layers of fit", {
  # binary layer keeps the chi-square based indices
  expect_true(all(is.finite(unlist(rated_model$TestFitIndices[c(
    "model_Chi_sq", "model_df", "CFI", "RMSEA", "AIC"
  )]))))
  # nominal layer has information criteria only
  expect_true(is.na(rated_model$TestFitIndicesNominal$model_Chi_sq))
  expect_true(is.finite(rated_model$TestFitIndicesNominal$BIC))
})

test_that("rated LCA does not sort classes by correct rate", {
  # Biclustering.rated sorts; LCA must not, because its classes are unordered.
  # The seedless EM is deterministic, so an unsorted TRP is evidence enough.
  expect_false(identical(rated_model$TRP, sort(rated_model$TRP)))
})

test_that("rated LCA prints and plots", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_output(print(rated_model), "Binary layer")
  expect_output(print(rated_model), "Nominal layer")
  for (ty in c("IRP", "TRP", "ICRP", "LCD", "CMP")) {
    expect_silent(plot(rated_model, type = ty, items = 1:2, students = 1:2, nc = 1, nr = 2))
  }
})
