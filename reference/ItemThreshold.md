# Item Threshold

Item threshold is a measure of difficulty based on a standard normal
distribution. This function is applicable only to binary response data.

The threshold is calculated as: \$\$\tau_j = \Phi^{-1}(1-p_j)\$\$ where
\\\Phi^{-1}\\ is the inverse standard normal distribution function and
\\p_j\\ is the correct response rate for item j.

Higher threshold values indicate more difficult items, as they represent
the point on the standard normal scale above which examinees tend to
answer incorrectly.

## Usage

``` r
ItemThreshold(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
ItemThreshold(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'ordinal'
ItemThreshold(U, na = NULL, Z = NULL, w = NULL)
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

A numeric vector of threshold values for each item on the standard
normal scale. Typical values range from about -3 to 3, where:

- Positive values indicate difficult items

- Zero indicates items of medium difficulty (50% correct)

- Negative values indicate easy items

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## Examples

``` r
# using sample dataset
ItemThreshold(J5S10)
#>              [,1]
#> Item01 -0.2533471
#> Item02  0.2533471
#> Item03 -1.2815516
#> Item04  0.5244005
#> Item05  0.2533471
```
