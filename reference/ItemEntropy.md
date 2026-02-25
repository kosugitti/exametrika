# Item Entropy

The item entropy is an indicator of the variability or randomness of the
responses. This function is applicable only to binary response data.

The entropy value represents the uncertainty or information content of
the response pattern for each item, measured in bits. Maximum entropy (1
bit) occurs when correct and incorrect responses are equally likely (p =
0.5).

## Usage

``` r
ItemEntropy(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
ItemEntropy(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
ItemEntropy(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'ordinal'
ItemEntropy(U, na = NULL, Z = NULL, w = NULL)
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

A numeric vector of entropy values for each item, measured in bits.
Values range from 0 to 1, where:

- 1: maximum uncertainty (p = 0.5)

- 0: complete certainty (p = 0 or 1)

- Values near 1 indicate items with balanced response patterns

- Values near 0 indicate items with extreme response patterns

## Details

The item entropy is calculated as: \$\$e_j =
-p_j\log_2p_j-(1-p_j)\log_2(1-p_j)\$\$ where \\p_j\\ is the correct
response rate for item j.

The entropy value has the following properties:

- Maximum value of 1 bit when p = 0.5 (most uncertainty)

- Minimum value of 0 bits when p = 0 or 1 (no uncertainty)

- Higher values indicate more balanced response patterns

- Lower values indicate more predictable response patterns

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## Examples

``` r
# using sample dataset
ItemEntropy(J5S10)
#>             [,1]
#> Item01 0.9709506
#> Item02 0.9709506
#> Item03 0.4689956
#> Item04 0.8812909
#> Item05 0.9709506
```
