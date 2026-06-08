# Tests for forwarding of graphical parameters supplied via ... to the
# underlying base R plotting functions (R Journal reviewer request, v1.14.0).

test_that("merge_plot_dots lets user dots override and extend defaults", {
  # user value overrides a default
  expect_equal(merge_plot_dots(list(pch = 1, type = "b"), list(pch = 16))$pch, 16)
  # user value is added when not present in defaults
  expect_equal(merge_plot_dots(list(type = "b"), list(las = 2))$las, 2)
  # defaults preserved
  expect_equal(merge_plot_dots(list(type = "b"), list(las = 2))$type, "b")
  # empty dots leaves defaults untouched
  expect_identical(merge_plot_dots(list(type = "b"), list()), list(type = "b"))
})

test_that("call_plot forwards merged arguments to the target function", {
  spy <- function(...) list(...)
  res <- call_plot(spy, list(side = 1, at = 1:3), list(las = 2, pch = 16))
  expect_equal(res$side, 1)
  expect_equal(res$at, 1:3)
  expect_equal(res$las, 2) # graphical parameter reaches the call
  expect_equal(res$pch, 16)
})

test_that("call_plot lets dots override a default passed by the package", {
  spy <- function(...) list(...)
  res <- call_plot(spy, list(xlab = "default", main = "m"), list(xlab = "user"))
  expect_equal(res$xlab, "user")
  expect_equal(res$main, "m")
})

test_that("draw_curve forwards graphical parameters to plot and lines", {
  captured <- new.env()
  captured$plot_args <- NULL
  captured$line_args <- NULL
  local_mocked_bindings(
    call_plot = function(.fun, defaults, dots = list()) {
      merged <- merge_plot_dots(defaults, dots)
      if (identical(.fun, graphics::lines)) {
        captured$line_args <- merged
      } else {
        captured$plot_args <- merged
      }
      invisible(NULL)
    }
  )
  draw_curve(function(theta) theta, from = -1, to = 1, dots = list(las = 2))
  expect_equal(captured$plot_args$las, 2)
  draw_curve(function(theta) theta, from = -1, to = 1, add = TRUE, dots = list(pch = 16))
  expect_equal(captured$line_args$pch, 16)
})

test_that("plot.exametrika accepts the reviewer's example without error", {
  skip_on_cran()
  pdf(tempfile(fileext = ".pdf"))
  on.exit(dev.off())
  res_lra <- LRA(J15S500, nrank = 4)
  # exact call from the R Journal review
  expect_no_error(
    plot(res_lra, type = "IRP", items = 1:4, nc = 2, nr = 2, las = 2, pch = 16)
  )
})

test_that("graphical parameters propagate across LRA / IRT / GRM plot types", {
  skip_on_cran()
  pdf(tempfile(fileext = ".pdf"))
  on.exit(dev.off())

  res_lra <- LRA(J15S500, nrank = 4)
  expect_no_error(plot(res_lra, type = "IRP", items = 1:2, las = 2, pch = 16))
  expect_no_error(plot(res_lra, type = "TRP", las = 2))
  expect_no_error(plot(res_lra, type = "LRD", cex.axis = 1.2))
  expect_no_error(plot(res_lra, type = "RMP", students = 1:2, pch = 17))

  res_irt <- IRT(J15S500)
  expect_no_error(plot(res_irt, type = "IRF", items = 1:3, pch = 16, las = 2))
  expect_no_error(plot(res_irt, type = "IRF", items = 1:3, overlay = TRUE, lwd = 3))
  expect_no_error(plot(res_irt, type = "TRF", las = 2))
  expect_no_error(plot(res_irt, type = "TIF", col = "blue"))

  res_grm <- GRM(J5S1000)
  expect_no_error(plot(res_grm, type = "IRF", items = 1:2, las = 2))
  expect_no_error(plot(res_grm, type = "TIF", lwd = 2))
})

test_that("graphical parameters propagate across Biclustering plot types", {
  skip_on_cran()
  pdf(tempfile(fileext = ".pdf"))
  on.exit(dev.off())

  bic <- Biclustering(J35S515, nfld = 5, ncls = 4, method = "B", verbose = FALSE)
  expect_no_error(plot(bic, type = "Array", las = 2))
  expect_no_error(plot(bic, type = "FRP", pch = 16))
  expect_no_error(plot(bic, type = "CRV", las = 2, pch = 15))
  expect_no_error(plot(bic, type = "TRP", las = 2))
})
