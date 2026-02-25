# Omega Coefficient

This function computes Tau-Congeneric Measurement, also known as
McDonald's tau coefficient, for a given data set.

## Usage

``` r
OmegaCoefficient(x, na = NULL, Z = NULL, w = NULL)
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
value representing the omega coefficient. For a data matrix input,
returns a list with three components:

- OmegaCov:

  Omega coefficient calculated from covariance matrix

- OmegaPhi:

  Omega coefficient calculated from phi coefficient matrix

- OmegaTetrachoric:

  Omega coefficient calculated from tetrachoric correlation matrix

## References

McDonald, R. P. (1999). Test theory: A unified treatment. Erlbaum.
