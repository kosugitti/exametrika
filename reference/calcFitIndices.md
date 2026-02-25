# calc Fit Indices

A general function that returns the model fit indices.

## Usage

``` r
calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)
```

## Arguments

- chi_A:

  chi-squares for this model

- chi_B:

  chi-squares for compared model

- df_A:

  degrees of freedom for this model

- df_B:

  degrees of freedom for compared model

- nobs:

  number of observations for Information criteria

## Value

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
