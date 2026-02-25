# Alpha Coefficient

This function computes Tau-Equivalent Measurement, also known as
Cronbach's alpha coefficient, for a given data set.

## Usage

``` r
AlphaCoefficient(x, na = NULL, Z = NULL, w = NULL)
```

## Arguments

- x:

  This should be a data matrix or a Covariance/Phi/Tetrachoric matrix.

- na:

  This parameter identifies the numbers or characters that should be
  treated as missing values when 'x' is a data matrix.

- Z:

  This parameter represents a missing indicator matrix. It is only
  needed if 'x' is a data matrix.

- w:

  This parameter is an item weight vector. It is only required if 'x' is
  a data matrix.

## Value

For a correlation/covariance matrix input, returns a single numeric
value representing the alpha coefficient. For a data matrix input,
returns a list with three components:

- AlphaCov:

  Alpha coefficient calculated from covariance matrix

- AlphaPhi:

  Alpha coefficient calculated from phi coefficient matrix

- AlphaTetrachoric:

  Alpha coefficient calculated from tetrachoric correlation matrix

## References

Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
a test. Psychometrika, 16,297–334.
