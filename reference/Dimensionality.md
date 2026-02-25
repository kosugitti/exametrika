# Dimensionality

The dimensionality is the number of components the test is measuring.

## Usage

``` r
Dimensionality(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
Dimensionality(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
Dimensionality(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'rated'
Dimensionality(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'ordinal'
Dimensionality(U, na = NULL, Z = NULL, w = NULL)
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

Returns a list of class c("exametrika", "Dimensionality") containing:

- Component:

  Sequence of component numbers

- Eigenvalue:

  Eigenvalues of the tetrachoric correlation matrix

- PerOfVar:

  Percentage of variance explained by each component

- CumOfPer:

  Cumulative percentage of variance explained
