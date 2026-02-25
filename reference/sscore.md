# Standardized Score

The standardized score (z-score) indicates how far a student's
performance deviates from the mean in units of standard deviation. This
function is applicable only to binary response data.

The score is calculated by standardizing the passage rates: \$\$Z_i =
\frac{r_i - \bar{r}}{\sigma_r}\$\$ where:

- \\r_i\\ is student i's passage rate

- \\\bar{r}\\ is the mean passage rate

- \\\sigma_r\\ is the standard deviation of passage rates

## Usage

``` r
sscore(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
sscore(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
sscore(U, na = NULL, Z = NULL, w = NULL)
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

A numeric vector of standardized scores for each student. The scores
follow a standard normal distribution with:

- Mean = 0

- Standard deviation = 1

- Approximately 68% of scores between -1 and 1

- Approximately 95% of scores between -2 and 2

- Approximately 99% of scores between -3 and 3

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

The standardization allows for comparing student performance across
different tests or groups. A positive score indicates above-average
performance, while a negative score indicates below-average performance.

## Examples

``` r
# using sample dataset
sscore(J5S10)
#>             [,1]
#>  [1,]  0.2656845
#>  [2,]  0.9298956
#>  [3,] -1.7269490
#>  [4,]  0.9298956
#>  [5,] -0.3985267
#>  [6,] -1.0627379
#>  [7,]  0.2656845
#>  [8,] -0.3985267
#>  [9,] -0.3985267
#> [10,]  1.5941068
```
