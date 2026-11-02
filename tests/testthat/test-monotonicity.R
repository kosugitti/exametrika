# Tests for ManifestMonotonicity() (rest-score regression).
#
# Reference values come from mokken::check.monotonicity() (mokken 3.1.2),
# computed by develop/monotonicity_reference_mokken.R on the data sets of
# make_monotonicity_cases() in helper-scalability.R. mokken is not a
# dependency; the values are frozen in a fixture.

mono_ref <- function() {
  return(utils::read.csv(test_path("fixtures", "monotonicity_mokken_reference.csv")))
}

vi_stats <- c(
  "n_active", "n_violation", "vi_per_active", "max_violation",
  "sum_violation", "sum_per_active", "z_max", "group_low",
  "group_high", "n_significant"
)

test_that("ManifestMonotonicity agrees with mokken::check.monotonicity", {
  ref <- mono_ref()
  cases <- suppressMessages(make_monotonicity_cases())
  for (nm in names(cases)) {
    res <- suppressMessages(
      ManifestMonotonicity(cases[[nm]]$X, minsize = cases[[nm]]$minsize)
    )
    for (j in seq_along(res$violations)) {
      r <- ref[ref$case == nm & ref$item == j & ref$stat %in% vi_stats, ]
      got <- res$violations[[j]][cbind(r$row, match(r$stat, vi_stats))]
      expect_equal(got, r$value,
        tolerance = 1e-10,
        label = paste(nm, "item", j, "violations")
      )

      g <- ref[ref$case == nm & ref$item == j & !(ref$stat %in% vi_stats), ]
      tab <- res$groups[[j]]
      pcols <- grep("^P\\(X", names(tab))
      gn <- g[g$stat == "N", ]
      expect_equal(tab$N[gn$row], gn$value, label = paste(nm, "item", j, "N"))
      gp <- g[g$stat != "N", ]
      k <- as.integer(sub("^P", "", gp$stat))
      expect_equal(as.matrix(tab[, pcols])[cbind(gp$row, k)], gp$value,
        tolerance = 1e-10, label = paste(nm, "item", j, "P")
      )
    }
  }
})

test_that("a single-peaked item is flagged and monotone items are not", {
  cases <- suppressMessages(make_monotonicity_cases())
  res <- suppressMessages(ManifestMonotonicity(cases$peaked$X))
  s <- res$summary
  expect_gt(s$n_significant[1], 0)
  expect_equal(sum(s$n_violation[-1] > 0 & s$n_significant[-1] > 0), 0)
})

test_that("missing responses are dropped listwise with a message", {
  cases <- suppressMessages(make_monotonicity_cases())
  X <- cases$monotone$X
  X[1:7, 2] <- NA
  dat <- suppressMessages(dataFormat(X))
  expect_message(res <- ManifestMonotonicity(dat), "7 respondents")
  expect_equal(res$n_dropped, 7)
  expect_equal(sum(res$groups[[1]]$N), nrow(X) - 7)
})

test_that("default minsize follows the mokken rule", {
  expect_equal(mm_default_minsize(100), 50)
  expect_equal(mm_default_minsize(200), 66)
  expect_equal(mm_default_minsize(400), 80)
  expect_equal(mm_default_minsize(1000), 100)
})
