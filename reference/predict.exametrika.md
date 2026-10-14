# Classify new respondents with a fitted model

Assigns new respondents to the latent classes or ranks of a fitted
model. The estimated parameters (and, for biclustering, the field
memberships of the items) are held fixed and one E-step is run on the
new responses, with the uniform class prior the estimation itself uses
(or the rank prior given by `trapezoidal` in the ordinal and rated LRA).
On the data the model was fitted to, the result reproduces the
membership stored in the fit.

Supported fits:
[`LCA()`](https://kosugitti.github.io/exametrika/reference/LCA.md)
(binary, nominal, ordinal, rated),
[`LRA()`](https://kosugitti.github.io/exametrika/reference/LRA.md)
(binary, ordinal, rated) and
[`Biclustering()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
/ Ranklustering (binary, nominal, ordinal, rated). The IRM and network
models are not supported.

## Usage

``` r
# S3 method for class 'exametrika'
predict(object, newdata, na = NULL, ...)
```

## Arguments

- object:

  A fitted model.

- newdata:

  New responses: a matrix or data frame with the items in columns (named
  as in the fitted data; extra columns are ignored, and the item names
  are required unless the column count already matches), or an
  `exametrikaData` object from
  [`dataFormat()`](https://kosugitti.github.io/exametrika/reference/dataFormat.md).
  Missing responses (`NA`, or the codes given in `na`) are allowed and
  simply left out of the likelihood. Responses must use the coding of
  the fitted data.

- na:

  Values to treat as missing, besides `NA`.

- ...:

  Not used.

## Value

An object of class `exametrikaPrediction`, a list with

- Membership:

  Matrix of posterior membership probabilities (respondents x classes or
  ranks).

- Estimate:

  The class or rank with the largest membership.

- log_lik:

  Log-likelihood of each respondent's responses under the model (the
  class or rank mixed out).

- model:

  The class of the fitted object.

- msg:

  "Class" or "Rank".

A response with a category the fitted data never used for that item
cannot be scored by the model; it is treated as missing, with a warning.

## Examples

``` r
# \donttest{
fit <- LCA(J15S500, ncls = 5)
pred <- predict(fit, J15S500$U[1:10, ])
pred$Estimate
#>  Student1  Student2  Student3  Student4  Student5  Student6  Student7  Student8 
#>         1         3         3         4         1         4         5         4 
#>  Student9 Student10 
#>         5         2 
# }
```
