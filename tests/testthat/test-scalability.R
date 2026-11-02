# Tests for Scalability() (Loevinger / Molenaar H coefficients)
#
# Reference values come from mokken::coefH() (mokken 3.1.2), computed by
# develop/scalability_reference_mokken.R on the data sets built by
# make_scalability_cases() in helper-scalability.R. mokken is not a
# dependency; the values are frozen in a fixture.

ref_path <- test_path("fixtures", "scalability_mokken_reference.csv")

get_ref <- function(ref, case, type) {
  return(ref[ref$case == case & ref$type == type, , drop = FALSE])
}

test_that("Scalability agrees with mokken::coefH on complete data", {
  ref <- utils::read.csv(ref_path)
  cases <- suppressMessages(make_scalability_cases())
  for (nm in names(cases)) {
    sc <- suppressMessages(Scalability(cases[[nm]]))

    expect_equal(sc$H, get_ref(ref, nm, "H")$value,
      tolerance = 1e-10,
      label = paste(nm, "H")
    )

    r_hj <- get_ref(ref, nm, "Hj")
    expect_equal(unname(sc$Hj[r_hj$i]), r_hj$value,
      tolerance = 1e-10,
      label = paste(nm, "Hj")
    )

    r_hij <- get_ref(ref, nm, "Hij")
    expect_equal(unname(sc$Hij[cbind(r_hij$i, r_hij$j)]), r_hij$value,
      tolerance = 1e-10, label = paste(nm, "Hij")
    )
  }
})

test_that("Scalability returns a symmetric Hij with an NA diagonal", {
  cases <- suppressMessages(make_scalability_cases())
  sc <- suppressMessages(Scalability(cases$ordinal_reversed))
  expect_s3_class(sc, "Scalability")
  expect_true(all(is.na(diag(sc$Hij))))
  expect_equal(sc$Hij, t(sc$Hij))
  expect_named(sc$Hj, sc$ItemLabel)
})

test_that("a reverse-keyed item gets negative coefficients", {
  cases <- suppressMessages(make_scalability_cases())
  sc <- suppressMessages(Scalability(cases$ordinal_reversed))
  expect_lt(sc$Hj[6], 0)
  expect_true(all(sc$Hij[6, -6] < 0))
  expect_true(all(sc$Hj[-6] > 0))

  # Recoding the item restores a positive, larger H
  fixed <- cases$ordinal_reversed
  fixed[, 6] <- 5L - fixed[, 6]
  sc2 <- suppressMessages(Scalability(fixed))
  expect_gt(sc2$Hj[6], 0)
  expect_gt(sc2$H, sc$H)
})

test_that("missing responses are handled pair-wise", {
  cases <- suppressMessages(make_scalability_cases())
  X <- cases$ordinal_reversed
  Xna <- X
  Xna[1:20, 1] <- NA
  sc <- suppressMessages(Scalability(Xna))
  # Pairs without item 1 are unaffected by its missing cells
  full <- suppressMessages(Scalability(X))
  expect_equal(sc$Hij[2, 3], full$Hij[2, 3])
  # Pairs with item 1 equal the complete-case value on the remaining rows
  sub <- suppressMessages(Scalability(X[-(1:20), ]))
  expect_equal(sc$Hij[1, 2], sub$Hij[1, 2])
})

test_that("Scalability rejects nominal data", {
  set.seed(1)
  X <- matrix(sample(1:4, 200, replace = TRUE), 50, 4)
  dat <- suppressMessages(dataFormat(X, response.type = "nominal"))
  expect_error(Scalability(dat), "not applicable to nominal")
})
