# bivariate normal CDF

Calculates the cumulative distribution function (CDF) of a bivariate
normal distribution. This function computes P(X \<= a, Y \<= b) where X
and Y follow a bivariate normal distribution with correlation
coefficient rho.

## Usage

``` r
qBiNormal(a, b, rho)
```

## Arguments

- a:

  Numeric value, the upper limit for the first variable.

- b:

  Numeric value, the upper limit for the second variable.

- rho:

  Numeric value between -1 and 1, the correlation coefficient.

## Value

The probability P(X \<= a, Y \<= b), a value between 0 and 1.

## Details

The implementation uses numerical integration with Gauss-Legendre
quadrature for accurate computation. Special cases for infinite bounds
are handled separately.
