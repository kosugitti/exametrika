# Polychoric Correlation Matrix

Polychoric Correlation Matrix

## Usage

``` r
PolychoricCorrelationMatrix(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
PolychoricCorrelationMatrix(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'ordinal'
PolychoricCorrelationMatrix(U, na = NULL, Z = NULL, w = NULL)
```

## Arguments

- U:

  Either an object of class "exametrika" or raw data. When raw data is
  given, it is converted to the exametrika class with the
  [`dataFormat`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  function.

- na:

  Values to be treated as missing values.

- Z:

  Missing indicator matrix of type matrix or data.frame. Values of 1
  indicate observed responses, while 0 indicates missing data.

- w:

  Item weight vector specifying the relative importance of each item.

## Value

A matrix of polychoric correlations with exametrika class. Each element
(i,j) represents the polychoric correlation between items i and j. The
matrix is symmetric with ones on the diagonal.

## Examples

``` r
# \donttest{
# example code
PolychoricCorrelationMatrix(J5S1000)
#>           V1        V2        V3        V4        V5
#> V1 1.0000000 0.2456615 0.2452778 0.2906535 0.2766477
#> V2 0.2456615 1.0000000 0.2557498 0.4055238 0.2831193
#> V3 0.2452778 0.2557498 1.0000000 0.3237882 0.2087903
#> V4 0.2906535 0.4055238 0.3237882 1.0000000 0.2942859
#> V5 0.2766477 0.2831193 0.2087903 0.2942859 1.0000000
# }
```
