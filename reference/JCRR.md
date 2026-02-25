# Joint Correct Response Rate

The joint correct response rate (JCRR) is the rate of students who
passed both items. This function is applicable only to binary response
data. For non-binary data, it will automatically redirect to the JSR
function with an appropriate message.

## Usage

``` r
JCRR(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
JCRR(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
JCRR(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'nominal'
JCRR(U, na = NULL, Z = NULL, w = NULL)
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

A matrix of joint correct response rates with exametrika class. Each
element (i,j) represents the proportion of students who correctly
answered both items i and j.

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## Examples

``` r
# example code
# Calculate JCRR using sample dataset J5S10
JCRR(J5S10)
#>        Item01 Item02 Item03 Item04 Item05
#> Item01    0.6    0.3    0.6    0.3    0.2
#> Item02    0.3    0.4    0.4    0.2    0.2
#> Item03    0.6    0.4    0.9    0.3    0.4
#> Item04    0.3    0.2    0.3    0.3    0.2
#> Item05    0.2    0.2    0.4    0.2    0.4
```
