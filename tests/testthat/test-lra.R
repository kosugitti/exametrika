library(exametrika)

### GOALS - Mathematica reference data
test <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter06LRA_GTMmic0_Test.csv"),
  check.names = FALSE
)
class <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter06LRA_GTMmic0_Rank.csv"),
  check.names = FALSE
)
items <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter06LRA_GTMmic0_Item.csv"),
  check.names = FALSE
)
student <- read.csv(
  test_path("fixtures", "mathematica_reference", "Chapter06LRA_GTMmic0_Student.csv"),
  check.names = FALSE
)


### Setup
# Mathematica reference fixtures are for the GTM method (mic = 0);
# pin method = "GTM" so these keep validating GTM after the default
# changed to "isotonic".
tmp <- dataFormat(J15S500, na = -99)
model <- LRA(tmp, nrank = 6, method = "GTM")

### Tests
test_that("LRA Test Fit", {
  expect <- test[15:30, 2] |>
    unlist() |>
    unname() |>
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- model$TestFitIndices |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Class Info", {
  ## TRP
  expect <- class[1, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$TRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- class[2, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$LRD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- class[3, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$RMD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Item Info", {
  ## IRP
  expect <- items[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## IRP index
  expect <- items[, 12:17] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRPIndex |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## FitIndex
  expect <- items[, c(22, 18, 19, 23, 20, 24, 21, 25:33)] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$ItemFitIndices |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Student Info", {
  ## Membership probabilities
  expect <- student[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 1:6] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Latent Rank Estimate
  expect <- student[, 12] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 7] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## Rank-Up/Down Odds
  expect <- student[, 13:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 8:9] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})


test_that("LRA Students", {
  ## Membership
  expect <- student[, 6:12] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 1:7] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## RUO/RDO
  expect <- student[, 13:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 8:9] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})

### Isotonic method (default) -------------------------------------------------
# No Mathematica reference exists for the order-restricted method, so these are
# structural / internal-consistency checks on the new default path.
model_iso <- LRA(tmp, nrank = 6, method = "isotonic")

test_that("isotonic is the default method", {
  model_def <- LRA(tmp, nrank = 6)
  expect_equal(model_def$method, "isotonic")
  expect_equal(model_def$IRP, model_iso$IRP)
})

test_that("isotonic IRP is monotone non-decreasing across ranks", {
  viol <- apply(model_iso$IRP, 1, function(r) any(diff(r) < -1e-9))
  expect_false(any(viol))
})

test_that("isotonic shape-restricted df is 1..nrank per item", {
  blocks <- apply(model_iso$IRP, 1, function(x) length(unique(round(x, 10))))
  expect_true(all(blocks >= 1 & blocks <= 6))
})

test_that("isotonic returns finite fit indices and converges", {
  expect_true(model_iso$converge)
  expect_true(is.finite(model_iso$TestFitIndices$CFI))
  expect_true(is.finite(model_iso$TestFitIndices$RMSEA))
})

test_that("isotonic attains higher log-likelihood than GTM (same data)", {
  expect_gt(model_iso$log_lik, model$log_lik)
})
