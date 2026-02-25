# Correct Response Rate

The correct response rate (CRR) is one of the most basic and important
statistics for item analysis. This is an index of item difficulty and a
measure of how many students out of those who tried an item correctly
responded to it. This function is applicable only to binary response
data.

The CRR for each item is calculated as: \$\$p_j = \frac{\sum\_{i=1}^n
z\_{ij}u\_{ij}}{\sum\_{i=1}^n z\_{ij}}\$\$ where \\z\_{ij}\\ is the
missing indicator and \\u\_{ij}\\ is the response.

## Usage

``` r
crr(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
crr(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
crr(U, na = NULL, Z = NULL, w = NULL)
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

A numeric vector of weighted correct response rates for each item.
Values range from 0 to 1, where higher values indicate easier items
(more students answered correctly).

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## Examples

``` r
# using sample datasaet
crr(J15S500)
#>         [,1]
#> Item01 0.746
#> Item02 0.754
#> Item03 0.726
#> Item04 0.776
#> Item05 0.804
#> Item06 0.864
#> Item07 0.716
#> Item08 0.588
#> Item09 0.364
#> Item10 0.662
#> Item11 0.286
#> Item12 0.274
#> Item13 0.634
#> Item14 0.764
#> Item15 0.706
```
