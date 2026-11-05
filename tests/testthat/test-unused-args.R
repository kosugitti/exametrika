# Unknown arguments must not vanish into `...`: the terminal method of each
# model function warns about what it did not use.

test_that("a misspelt argument to LCA() is reported, a correct call is silent", {
  expect_warning(
    LCA(J15S500, ncls = 3, foo = 1, verbose = FALSE),
    "LCA\\(\\): unknown argument ignored: `foo`"
  )
  expect_no_warning(LCA(J15S500, ncls = 3, verbose = FALSE))
})

test_that("dataType= (for response.type=) is caught at Biclustering()", {
  expect_warning(
    Biclustering(J15S500, ncls = 3, nfld = 2, dataType = "rated", verbose = FALSE),
    "Biclustering\\(\\): unknown argument ignored: `dataType`"
  )
  expect_no_warning(Biclustering(J15S500, ncls = 3, nfld = 2, verbose = FALSE))
})

test_that("several unknown arguments and unnamed extras are listed together", {
  expect_warning(
    LRA(J15S500, nrank = 3, foo = 1, bar = 2, verbose = FALSE),
    "LRA\\(\\): unknown arguments ignored: `foo`, `bar`"
  )
})

test_that("the warning reaches the rated and ordinal methods through dispatch", {
  skip_on_cran()
  expect_warning(
    LRA(J5S1000, nrank = 3, foo = 1, verbose = FALSE),
    "LRA\\(\\): unknown argument ignored: `foo`"
  )
  expect_warning(
    LCA(J21S300, ncls = 3, foo = 1, verbose = FALSE),
    "LCA\\(\\): unknown argument ignored: `foo`"
  )
  expect_warning(
    Biclustering(J5S1000, ncls = 3, nfld = 2, method = "R", foo = 1, verbose = FALSE),
    "Biclustering\\(\\): unknown argument ignored: `foo`"
  )
})

test_that("the deprecated max_iter of Biclustering_IRM is not reported as unknown", {
  skip_on_cran()
  w <- character()
  withCallingHandlers(
    Biclustering_IRM(J15S500, max_iter = 5, verbose = FALSE),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("max_iter. is deprecated", w)))
  expect_false(any(grepl("unknown argument", w)))
})

test_that("warn_unused_dots is silent for an empty list and reports unnamed extras", {
  expect_no_warning(warn_unused_dots(list(), "LCA"))
  expect_warning(warn_unused_dots(list(1, 2), "LCA"), "2 unnamed arguments")
  expect_warning(warn_unused_dots(list(a = 1, 2), "LCA"), "`a`, 1 unnamed argument")
})

test_that("nominal Biclustering takes method = 'B' and refuses Ranklustering by name", {
  skip_on_cran()
  expect_no_warning(fit <- Biclustering(J20S600, ncls = 3, nfld = 2, method = "B", verbose = FALSE))
  expect_s3_class(fit, "nominalBiclustering")
  expect_error(
    Biclustering(J20S600, ncls = 3, nfld = 2, method = "R", verbose = FALSE),
    "Nominal data have no rank order"
  )
})
