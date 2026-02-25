# Student Percentile Ranks

The percentile function calculates each student's relative standing in
the group, expressed as a percentile rank (1-100). This function is
applicable only to binary response data.

The percentile rank indicates the percentage of scores in the
distribution that fall below a given score. For example, a percentile
rank of 75 means the student performed better than 75% of the group.

## Usage

``` r
percentile(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
percentile(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
percentile(U, na = NULL, Z = NULL, w = NULL)
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

A numeric vector of percentile ranks (1-100) for each student, where:

- 100: Highest performing student(s)

- 50: Median performance

- 1: Lowest performing student(s)

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

Percentile ranks are calculated using the empirical cumulative
distribution function of standardized scores. Tied scores receive the
same percentile rank. The values are rounded up to the nearest integer
to provide ranks from 1 to 100.

## Examples

``` r
# using sample dataset
percentile(J5S10)
#>  [1]  70  90  10  90  50  20  70  50  50 100
```
