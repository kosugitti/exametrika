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
tmp <- dataFormat(J35S515)
Bic <- Biclustering(tmp, ncls = 6, nfld = 5, method = "R", mic = TRUE)

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
