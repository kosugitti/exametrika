# Conditional Correct Response Rate

The conditional correct response rate (CCRR) represents the ratio of the
students who passed Item C (consequent item) to those who passed Item A
(antecedent item). This function is applicable only to binary response
data.

## Usage

``` r
CCRR(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
CCRR(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
CCRR(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'nominal'
CCRR(U, na = NULL, Z = NULL, w = NULL)
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

A matrix of conditional correct response rates with exametrika class.
Each element (i,j) represents the probability of correctly answering
item j given that item i was answered correctly.

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## Examples

``` r
# example code
# Calculate CCRR using sample dataset J5S10
CCRR(J5S10)
#>        Item01 Item02 Item03 Item04 Item05
#> Item01  1.000  0.500      1  0.500  0.333
#> Item02  0.750  1.000      1  0.500  0.500
#> Item03  0.667  0.444      1  0.333  0.444
#> Item04  1.000  0.667      1  1.000  0.667
#> Item05  0.500  0.500      1  0.500  1.000
```
