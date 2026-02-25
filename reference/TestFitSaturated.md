# Model Fit Functions for saturated model

A general function that returns the model fit indices.

## Usage

``` r
TestFitSaturated(U, Z, ell_A, nparam)
```

## Arguments

- U:

  U is either a data class of exametrika, or raw data. When raw data is
  given, it is converted to the exametrika class with the
  [dataFormat](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  function.

- Z:

  Z is a missing indicator matrix of the type matrix or data.frame

- ell_A:

  log likelihood of this model

- nparam:

  number of parameters for this model

## Value

- model_log_like:

  log likelihood of analysis model

- bench_log_like:

  log likelihood of benchmark model

- null_log_like:

  log likelihood of null model

- model_Chi_sq:

  Chi-Square statistics for analysis model

- null_Chi_sq:

  Chi-Square statistics for null model

- model_df:

  degrees of freedom of analysis model

- null_df:

  degrees of freedom of null model

- NFI:

  Normed Fit Index. Lager values closer to 1.0 indicate a better fit.

- RFI:

  Relative Fit Index. Lager values closer to 1.0 indicate a better fit.

- IFI:

  Incremental Fit Index. Lager values closer to 1.0 indicate a better
  fit.

- TLI:

  Tucker-Lewis Index. Lager values closer to 1.0 indicate a better fit.

- CFI:

  Comparative Fit Index. Lager values closer to 1.0 indicate a better
  fit.

- RMSEA:

  Root Mean Square Error of Approximation. Smaller values closer to 0.0
  indicate a better fit.

- AIC:

  Akaike Information Criterion. A lower value indicates a better fit.

- CAIC:

  Consistent AIC.A lower value indicates a better fit.

- BIC:

  Bayesian Information Criterion. A lower value indicates a better fit.
