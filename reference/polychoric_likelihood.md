# Calculate Polychoric Correlation Likelihood

Calculates the negative log-likelihood for estimating polychoric
correlation from a contingency table of two ordinal variables.

## Usage

``` r
polychoric_likelihood(rho, mat)
```

## Arguments

- rho:

  Numeric value between -1 and 1, the correlation coefficient

- mat:

  A contingency table matrix for two ordinal variables

## Value

The negative log-likelihood value for the given correlation coefficient

## Details

The function estimates thresholds from the marginal distributions and
calculates the expected probabilities based on a bivariate normal
distribution. It then computes the log-likelihood by comparing observed
and expected frequencies.
