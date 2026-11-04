# Equate new items and respondents to a fitted model

Places a new test form on the scale of a fitted model through the items
the two share (the anchor items). The parameters of the fit are held
fixed; the new respondents are assigned to its latent classes or ranks,
and the items the fit has not seen are placed on the same scale. For the
biclustering family
([`Biclustering()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md),
Ranklustering, and the reference objects of
[`as_biclustering()`](https://kosugitti.github.io/exametrika/reference/as_biclustering.md))
a new item is assigned to one of the existing fields, with a diagnostic
that tells when none of them fits. For
[`LCA()`](https://kosugitti.github.io/exametrika/reference/LCA.md) and
[`LRA()`](https://kosugitti.github.io/exametrika/reference/LRA.md)
(binary, nominal and ordinal) the reference profile of each new item is
estimated, under the order restriction of the fit when the fit is
isotonic, or with its filter when the fit is a GTM.

The two unknowns, the membership of the respondents and the parameters
of the new items, are updated alternately until the membership settles.
The first step uses the anchor items alone, so the scale is the fit's
from the start. Anchor items missing from `newdata` are simply left out;
at least one anchor item is required.

## Usage

``` r
equate(fit, newdata, ...)

# S3 method for class 'exametrika'
equate(
  fit,
  newdata,
  na = NULL,
  maxiter = 100,
  tol = 1e-06,
  alpha = 1,
  M = 10000,
  seed = 1,
  ...
)
```

## Arguments

- fit:

  A fitted model:
  [`LCA()`](https://kosugitti.github.io/exametrika/reference/LCA.md)
  (binary, nominal),
  [`LRA()`](https://kosugitti.github.io/exametrika/reference/LRA.md)
  (binary, ordinal),
  [`Biclustering()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
  (binary, nominal, ordinal, rated) or a reference object from
  [`as_biclustering()`](https://kosugitti.github.io/exametrika/reference/as_biclustering.md).
  The rated LCA and LRA, the SOM, the IRM and the network models are not
  supported.

- newdata:

  New responses: a matrix or data frame with the items in columns, or an
  `exametrikaData` object from
  [`dataFormat()`](https://kosugitti.github.io/exametrika/reference/dataFormat.md).
  Column names are required: the items whose names match the fit are the
  anchor items, every other column is a new item. Missing responses
  (`NA`, or the codes given in `na`) are left out of the likelihood.
  Responses must use the coding of the fitted data.

- ...:

  Not used.

- na:

  Values to treat as missing, besides `NA`.

- maxiter:

  Maximum number of alternating cycles.

- tol:

  The cycles stop when no membership probability moves by more than
  this.

- alpha:

  Concentration of the Beta or Dirichlet prior of the new-field
  diagnostic (biclustering) and of the reference profiles of the new
  items (nominal and ordinal one-mode models). Default 1, the flat prior
  of the estimation.

- M:

  Number of particles of the Monte Carlo behind the order restriction in
  the diagnostic of an isotonic fit; see `ICL_exact`.

- seed:

  Seed of that Monte Carlo (the global stream is left untouched).

## Value

An object of class `exametrikaEquate`, a list with

- Membership, Estimate, log_lik:

  As in
  [`predict.exametrika()`](https://kosugitti.github.io/exametrika/reference/predict.exametrika.md):
  the posterior membership of the new respondents, their class or rank,
  and the log-likelihood of each respondent under the model.

- AnchorItems, NewItems:

  The item labels of the two groups.

- FieldMembership, FieldEstimated:

  Biclustering: the posterior field membership of each new item and the
  field with the largest probability.

- NewFieldLogBF:

  Biclustering: the log Bayes factor of "a field of its own" against
  "one of the existing fields" for each new item (see Details). `NA`
  when the Monte Carlo could not resolve the order probability; the
  value reported is then a bound, with a warning.

- AnchorLogBF:

  Biclustering: the same quantity for the anchor items of `newdata`, the
  yardstick for the new items. With many respondents every item departs
  measurably from its field profile, so a new item stands out only when
  it scores well above the anchor items.

- NewItems:

  Biclustering: the three columns above as a data frame.

- IRP:

  Binary LCA / LRA: the item reference profile of each new item (items x
  classes).

- ICRP:

  Nominal LCA / ordinal LRA: the category reference profile of each new
  item, one row per category, in the layout of the fit.

- n_cycle, converge, total_log_lik:

  Cycles run, whether `tol` was met, and the sum of `log_lik`.

## Details

For a biclustering fit the field reference profiles are never changed,
so a new item can only be placed in a field the fit already has. To tell
whether that is appropriate, `NewFieldLogBF` compares, for each new
item, the marginal likelihood of its responses under a field of its own
(the block probabilities integrated out under the prior of the fit, as
in `ICL_exact`; the Beta or Dirichlet with parameter `alpha`, restricted
to the ordered region when the fit is isotonic) with the marginal
likelihood under the existing fields (equal prior weight on each field).
The respondents enter with their hard assignment, as in `ICL_exact`. A
positive value says that the item behaves unlike every field of the fit;
in that case the field it was given is the least bad one, and a new
field should be considered when the fit is revised. Because a field
profile is an average over its items, a single item departs from it more
and more measurably as the respondents grow in number, and even the
fitted items come out positive in a large sample. The same quantity is
therefore computed for the anchor items of `newdata` (`AnchorLogBF`),
and a new item is read against that range rather than against zero.

## Examples

``` r
# \donttest{
# A new form: ten items of the fitted test as anchors, two new items
fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
X <- J15S500$U[1:100, ]
colnames(X)[11:12] <- c("new1", "new2")
eq <- equate(fit, X[, 1:12])
eq$NewItems
#>   ItemLabel Field Membership     logBF
#> 1      new1     2          1 -3.809214
#> 2      new2     2          1 -0.621552
head(eq$Estimate)
#> Student1 Student2 Student3 Student4 Student5 Student6 
#>        1        2        2        3        1        3 
# }
```
