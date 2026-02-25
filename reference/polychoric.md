# Polychoric Correlation

Calculate the polychoric correlation coefficient between two polytomous
(categorical ordinal) variables. Polychoric correlation estimates the
correlation between two theorized normally distributed continuous latent
variables from two observed ordinal variables.

## Usage

``` r
polychoric(x, y)
```

## Arguments

- x:

  A polytomous vector (categorical ordinal variable)

- y:

  A polytomous vector (categorical ordinal variable)

## Value

The polychoric correlation coefficient between x and y

## Details

This function handles missing values (coded as -1 or NA) using pairwise
deletion. The estimation uses maximum likelihood approach with Brent's
method for optimization.

## Examples

``` r
# Example with simulated data
set.seed(123)
x <- sample(1:5, 100, replace = TRUE)
y <- sample(1:4, 100, replace = TRUE)
polychoric(x, y)
#> [1] -0.1981319
```
