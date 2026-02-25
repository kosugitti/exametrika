# Generate Score Report for Non-Binary Test Data

Calculates comprehensive descriptive statistics for a test, including
measures of central tendency, variability, distribution shape, and
reliability.

## Usage

``` r
ScoreReport(U, na = NULL, Z = NULL, w = NULL)
```

## Arguments

- U:

  Either an object of class "exametrika" or raw data. When raw data is
  given, it is converted to the exametrika class with the
  [`dataFormat`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  function.

- na:

  Values to be treated as missing values

- Z:

  Missing indicator matrix of type matrix or data.frame. Values of 1
  indicate observed responses, while 0 indicates missing data.

- w:

  Item weight vector specifying the relative importance of each item

## Value

An object of class "exametrika" and "TestStatistics" containing:

- TestLength:

  Number of items included in the test

- SampleSize:

  Number of examinees (rows) in the dataset

- Mean:

  Average score across all examinees

- Median:

  Median score

- SD:

  Standard deviation of test scores

- Variance:

  Variance of test scores

- Skewness:

  Skewness of the score distribution (measure of asymmetry)

- Kurtosis:

  Kurtosis of the score distribution (measure of tail extremity)

- Min:

  Minimum score obtained

- Max:

  Maximum score obtained

- Range:

  Difference between maximum and minimum scores

- Alpha:

  Cronbach's alpha coefficient, a measure of internal consistency
  reliability

## Details

This function is intended for non-binary (ordinal or rated) response
data. It calculates descriptive statistics for the overall test
performance. If binary data is provided, an error message will be
displayed.

## Examples

``` r
# \donttest{
# Generate score report for sample ordinal data
ScoreReport(J15S3810)
#> Test Statistics
#>                   value
#> SampleSize 3810.0000000
#> TestLength   15.0000000
#> Median       15.0000000
#> Max          27.0000000
#> Min           0.0000000
#> Range        27.0000000
#> Mean         14.9939633
#> SD            4.5139980
#> Skewness      0.3003000
#> Kurtosis     -0.4333525
#> Alpha         0.4496500

# Example with rated data
ScoreReport(J35S5000)
#> Test Statistics
#>                    value
#> SampleSize 5000.00000000
#> TestLength   35.00000000
#> Median       17.00000000
#> Max          35.00000000
#> Min           2.00000000
#> Range        33.00000000
#> Mean         16.91100000
#> SD            4.97699036
#> Skewness      0.31342138
#> Kurtosis     -0.07429596
#> Alpha         0.70437957
# }
```
