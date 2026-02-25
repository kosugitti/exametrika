# Polyserial Correlation

Calculates the polyserial correlation coefficient between a continuous
variable and an ordinal variable.

## Usage

``` r
polyserial(x, y)
```

## Arguments

- x:

  A numeric vector representing the continuous variable.

- y:

  A numeric vector representing the ordinal variable (must be integer
  values).

## Value

A numeric value representing the estimated polyserial correlation
coefficient.

## Details

This function implements Olsson et al.'s ad hoc method for estimating
the polyserial correlation coefficient. The method assumes that the
continuous variable follows a normal distribution and that the ordinal
variable is derived from an underlying continuous normal variable
through thresholds.

## References

U.Olsson, F.Drasgow, and N.Dorans (1982). The polyserial correlation
coefficient. Psychometrika, 47,337-347.

## Examples

``` r
n <- 300
x <- rnorm(n)
y <- sample(1:5, size = n, replace = TRUE)
polyserial(x, y)
#> [1] 0.100766
```
