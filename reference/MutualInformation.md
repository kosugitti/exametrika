# Mutual Information

Mutual Information is a measure that represents the degree of
interdependence between two items. This function is applicable to both
binary and polytomous response data. The measure is calculated using the
joint probability distribution of responses between item pairs and their
marginal probabilities.

## Usage

``` r
MutualInformation(U, na = NULL, Z = NULL, w = NULL, base = 2)

# Default S3 method
MutualInformation(U, na = NULL, Z = NULL, w = NULL, base = 2)

# S3 method for class 'binary'
MutualInformation(U, na = NULL, Z = NULL, w = NULL, base = 2)

# S3 method for class 'ordinal'
MutualInformation(U, na = NULL, Z = NULL, w = NULL, base = 2)
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

- base:

  The base for the logarithm. Default is 2. For polytomous data, you can
  use "V" to set the base to min(rows, columns), "e" for natural
  logarithm (base e), or any other number to use that specific base.

## Value

A matrix of mutual information values with exametrika class. Each
element (i,j) represents the mutual information between items i and j,
measured in bits. Higher values indicate stronger interdependence
between items.

## Details

For binary data, the following formula is used: \$\$ MI\_{jk} = p\_{00}
\log_2 \frac{p\_{00}}{(1-p_j)(1-p_k)} + p\_{01} \log_2
\frac{p\_{01}}{(1-p_j)p_k} + p\_{10} \log_2 \frac{p\_{10}}{p_j(1-p_k)} +
p\_{11} \log_2 \frac{p\_{11}}{p_jp_k} \$\$ Where:

- \\p\_{00}\\ is the joint probability of incorrect responses to both
  items j and k

- \\p\_{01}\\ is the joint probability of incorrect response to item j
  and correct to item k

- \\p\_{10}\\ is the joint probability of correct response to item j and
  incorrect to item k

- \\p\_{11}\\ is the joint probability of correct responses to both
  items j and k

For polytomous data, the following formula is used: \$\$MI\_{jk} =
\sum\_{j=1}^{C_j}\sum\_{k=1}^{C_k}p\_{jk}\log
\frac{p\_{jk}}{p\_{j.}p\_{.k}}\$\$

The base of the logarithm can be the number of rows, number of columns,
min(rows, columns), base-10 logarithm, natural logarithm (e), etc.

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## Examples

``` r
# example code
# Calculate Mutual Information using sample dataset J15S500
MutualInformation(J15S500)
#>          Item01   Item02   Item03   Item04  Item05   Item06  Item07   Item08
#> Item01 8.18e-01 3.65e-03 3.12e-06 0.003643 0.01330 0.011319 0.00352 0.011631
#> Item02 3.65e-03 8.05e-01 6.54e-03 0.025339 0.00450 0.008408 0.00751 0.004412
#> Item03 3.12e-06 6.54e-03 8.47e-01 0.014072 0.00454 0.006334 0.01368 0.001839
#> Item04 3.64e-03 2.53e-02 1.41e-02 0.767404 0.00646 0.012223 0.04463 0.012990
#> Item05 1.33e-02 4.50e-03 4.54e-03 0.006457 0.71386 0.001994 0.00726 0.001604
#> Item06 1.13e-02 8.41e-03 6.33e-03 0.012223 0.00199 0.573667 0.01797 0.016802
#> Item07 3.52e-03 7.51e-03 1.37e-02 0.044626 0.00726 0.017970 0.86084 0.017757
#> Item08 1.16e-02 4.41e-03 1.84e-03 0.012990 0.00160 0.016802 0.01776 0.977539
#> Item09 2.59e-03 3.49e-06 1.09e-06 0.000225 0.00146 0.000531 0.00136 0.000209
#> Item10 1.12e-03 2.02e-03 1.41e-03 0.002754 0.00191 0.003855 0.01051 0.000604
#> Item11 1.20e-02 1.65e-02 2.78e-03 0.023124 0.00463 0.002486 0.02052 0.021517
#> Item12 7.70e-03 1.14e-02 3.20e-03 0.042072 0.00453 0.004170 0.02200 0.011050
#> Item13 8.55e-03 6.57e-03 7.26e-03 0.027819 0.01532 0.006766 0.01357 0.012440
#> Item14 1.36e-02 1.59e-02 5.66e-04 0.020775 0.00756 0.020448 0.01178 0.007050
#> Item15 6.66e-03 8.53e-03 9.58e-04 0.027525 0.00329 0.021479 0.01540 0.004058
#>          Item09   Item10   Item11  Item12  Item13   Item14   Item15
#> Item01 2.59e-03 0.001119 0.011986 0.00770 0.00855 0.013613 0.006661
#> Item02 3.49e-06 0.002021 0.016462 0.01139 0.00657 0.015904 0.008525
#> Item03 1.09e-06 0.001415 0.002775 0.00320 0.00726 0.000566 0.000958
#> Item04 2.25e-04 0.002754 0.023124 0.04207 0.02782 0.020775 0.027525
#> Item05 1.46e-03 0.001914 0.004630 0.00453 0.01532 0.007563 0.003286
#> Item06 5.31e-04 0.003855 0.002486 0.00417 0.00677 0.020448 0.021479
#> Item07 1.36e-03 0.010505 0.020521 0.02200 0.01357 0.011781 0.015395
#> Item08 2.09e-04 0.000604 0.021517 0.01105 0.01244 0.007050 0.004058
#> Item09 9.46e-01 0.003098 0.000946 0.01598 0.00170 0.004467 0.000133
#> Item10 3.10e-03 0.922892 0.005627 0.00121 0.00950 0.013835 0.001738
#> Item11 9.46e-04 0.005627 0.863498 0.03838 0.00430 0.038606 0.007099
#> Item12 1.60e-02 0.001214 0.038381 0.84715 0.01292 0.027044 0.009269
#> Item13 1.70e-03 0.009501 0.004301 0.01292 0.94755 0.026346 0.023977
#> Item14 4.47e-03 0.013835 0.038606 0.02704 0.02635 0.788325 0.015130
#> Item15 1.33e-04 0.001738 0.007099 0.00927 0.02398 0.015130 0.873832
```
