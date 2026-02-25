# Item Odds

Item Odds are defined as the ratio of Correct Response Rate to Incorrect
Response Rate: \$\$O_j = \frac{p_j}{1-p_j}\$\$ where \\p_j\\ is the
correct response rate for item j. This function is applicable only to
binary response data.

The odds value represents how many times more likely a correct response
is compared to an incorrect response. For example, an odds of 2 means
students are twice as likely to answer correctly as incorrectly.

## Usage

``` r
ItemOdds(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
ItemOdds(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
ItemOdds(U, na = NULL, Z = NULL, w = NULL)
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

A numeric vector of odds values for each item. Values range from 0 to
infinity, where:

- odds \> 1: correct response more likely than incorrect

- odds = 1: equally likely

- odds \< 1: incorrect response more likely than correct

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## Examples

``` r
# using sample dataset
ItemOdds(J5S10)
#>             [,1]
#> Item01 1.5000000
#> Item02 0.6666667
#> Item03 9.0000000
#> Item04 0.4285714
#> Item05 0.6666667
```
