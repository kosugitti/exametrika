# Item-Total Biserial Correlation

The Item-Total Biserial Correlation computes the biserial correlation
between each item and the total score. This function is applicable only
to binary response data.

This correlation provides a measure of item discrimination, indicating
how well each item distinguishes between high and low performing
examinees.

## Usage

``` r
ITBiserial(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
ITBiserial(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
ITBiserial(U, na = NULL, Z = NULL, w = NULL)
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

A numeric vector of item-total biserial correlations. Values range from
-1 to 1, where:

- Values near 1: Strong positive discrimination

- Values near 0: No discrimination

- Negative values: Potential item problems

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

The biserial correlation is generally preferred over the point-biserial
correlation when the dichotomization is artificial (i.e., when the
underlying trait is continuous).

## Examples

``` r
# using sample dataset
ITBiserial(J15S500)
#>  [1] 0.4830380 0.4985034 0.4130636 0.6332807 0.4444282 0.5178833 0.5907845
#>  [8] 0.4938693 0.2866633 0.3933372 0.6278208 0.6513541 0.5675060 0.6171171
#> [15] 0.5104599
```
