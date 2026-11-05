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
value representing the alpha coefficient. For binary data, returns a
list with three components:

- AlphaCov:

  Alpha coefficient calculated from covariance matrix

- AlphaPhi:

  Alpha coefficient calculated from phi coefficient matrix

- AlphaTetrachoric:

  Alpha coefficient calculated from tetrachoric correlation matrix

For ordinal data, returns a list with `AlphaCov` (from the covariance
matrix of the category scores) and `AlphaPolychoric` (from the
polychoric correlation matrix, the ordinal alpha of Zumbo et al., 2007).
Rated and nominal data are not supported.

## References

Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
a test. Psychometrika, 16,297-334.

Zumbo, B. D., Gadermann, A. M., & Zeisser, C. (2007). Ordinal versions
of coefficients alpha and theta for Likert rating scales. Journal of
Modern Applied Statistical Methods, 6(1), 21–29.
