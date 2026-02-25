# Alpha Coefficient if Item removed

This function returns the alpha coefficient when the specified item is
excluded.

## Usage

``` r
AlphaIfDel(x, delItem = NULL, na = NULL, Z = NULL, w = NULL)
```

## Arguments

- x:

  This should be a data matrix or a Covariance/Phi/Tetrachoric matrix.

- delItem:

  Specify the item to be deleted. If NULL, calculations are performed
  for all cases.

- na:

  This parameter identifies the numbers or characters that should be
  treated as missing values when 'x' is a data matrix.

- Z:

  This parameter represents a missing indicator matrix. It is only
  needed if 'x' is a data matrix.

- w:

  This parameter is an item weight vector. It is only required if 'x' is
  a data matrix.
