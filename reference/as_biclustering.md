# Build a biclustering reference from published field profiles

Assembles the minimal object that
[`equate()`](https://kosugitti.github.io/exametrika/reference/equate.md)
and [`predict()`](https://rdrr.io/r/stats/predict.html) need from a
field reference profile matrix and the field of each item, so that a new
form can be equated, or new respondents scored, from published values
alone, without the data the model was fitted to.

## Usage

``` r
as_biclustering(
  FRP,
  fields,
  category_shift = 0,
  ordered = FALSE,
  msg = c("Class", "Rank")
)
```

## Arguments

- FRP:

  The field reference profiles: a fields x classes matrix of
  correct-response probabilities for binary data, or a fields x classes
  x categories array of category probabilities for polytomous data (each
  row summing to one over the categories).

- fields:

  The field of each item, as an integer vector named by the item labels
  (a factor or a character vector of field numbers is also accepted).

- category_shift:

  Polytomous data only: the amount the response codes must be moved up
  to start at 1 (0 when the codes are already 1, 2, ...; 1 when they are
  0-based). The `CategoryShift` of a fit.

- ordered:

  Whether the profiles are order-restricted across the classes (an
  isotonic Ranklustering). Only the new-field diagnostic of
  [`equate()`](https://kosugitti.github.io/exametrika/reference/equate.md)
  reads it.

- msg:

  "Class" or "Rank": how the latent groups are labelled.

## Value

An object of class `BiclusteringReference`, accepted by
[`equate()`](https://kosugitti.github.io/exametrika/reference/equate.md)
and [`predict()`](https://rdrr.io/r/stats/predict.html).

## Examples

``` r
fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
ref <- as_biclustering(fit$FRP, fit$FieldEstimated)
p <- predict(ref, J15S500$U[1:5, ])
p$Estimate
#> Student1 Student2 Student3 Student4 Student5 
#>        1        2        2        2        1 
```
