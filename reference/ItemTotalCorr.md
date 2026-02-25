# Item-Total Correlation

Item-Total correlation (ITC) is a Pearson's correlation of an item with
the Number-Right Score (NRS) or total score. This function is applicable
only to binary response data.

The ITC is a measure of item discrimination, indicating how well an item
distinguishes between high and low performing examinees.

## Usage

``` r
ItemTotalCorr(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
ItemTotalCorr(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
ItemTotalCorr(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'ordinal'
ItemTotalCorr(U, na = NULL, Z = NULL, w = NULL)
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

A numeric vector of item-total correlations. Values typically range from
-1 to 1, where:

- Values near 1: Strong positive discrimination

- Values near 0: No discrimination

- Negative values: Potential item problems (lower ability students
  performing better than higher ability students)

## Details

The correlation is calculated between:

- Each item's responses (0 or 1)

- The total test score (sum of correct responses)

Higher positive correlations indicate items that better discriminate
between high and low ability examinees.

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

Values below 0.2 might indicate problematic items that should be
reviewed. Values above 0.3 are generally considered acceptable.

## Examples

``` r
# using sample dataset
ItemTotalCorr(J15S500)
#>             [,1]
#> Item01 0.3746481
#> Item02 0.3932046
#> Item03 0.3213003
#> Item04 0.5028239
#> Item05 0.3290539
#> Item06 0.3768598
#> Item07 0.4830672
#> Item08 0.4052152
#> Item09 0.2248203
#> Item10 0.3144456
#> Item11 0.4550809
#> Item12 0.4677839
#> Item13 0.4712502
#> Item14 0.4848413
#> Item15 0.4127747
```
