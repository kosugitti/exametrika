# Simple Item Statistics

This function calculates statistics for each item, with different
metrics available depending on the data type (binary, ordinal, or
rated).

## Usage

``` r
ItemStatistics(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
ItemStatistics(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
ItemStatistics(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'ordinal'
ItemStatistics(U, na = NULL, Z = NULL, w = NULL)
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

For binary data:

- ItemLabel:

  Label identifying each item

- NR:

  Number of Respondents for each item

- CRR:

  Correct Response Rate denoted as \$p_j\$.

- ODDs:

  Item Odds is the ratio of the correct response rate to the incorrect
  response rate. Defined as \\o_j = \frac{p_j}{1-p_j}\\

- Threshold:

  Item Threshold is a measure of difficulty based on a standard normal
  distribution.

- Entropy:

  Item Entropy is an indicator of the variability or randomness of the
  responses. Defined as \\e_j=-p_j \log_2 p_j - (1-p_j)\log_2(1-p_j)\\

- ITCrr:

  Item-total Correlation is a Pearson's correlation of an item with the
  Number-Right score.

For ordinal polytomous data:

- ItemLabel:

  Label identifying each item

- NR:

  Number of Respondents for each item

- Threshold:

  Matrix of threshold values for each item's category boundaries, based
  on a standard normal distribution. For an item with K categories,
  there are K-1 thresholds.

- Entropy:

  Item Entropy calculated using the category probabilities. Unlike
  binary data, this is calculated using the formula \\e_j =
  -\sum\_{k=1}^{K_j} p\_{jk} \log\_{K_j} p\_{jk}\\, where \\K_j\\ is the
  number of categories for item j.

- ITCrr:

  Item-total Correlation calculated using polyserial correlation, which
  accounts for the ordinal nature of the item responses and the
  continuous total score.

## Note

For rated data, the function processes the data as binary, with each
response being compared to the correct answer to determine correctness.

## Examples

``` r
# using sample dataset(binary)
ItemStatistics(J15S500)
#> Item Statistics
#>    ItemLabel  NR   CRR  ODDs Threshold Entropy ITCrr
#> 1     Item01 500 0.746 2.937    -0.662   0.818 0.375
#> 2     Item02 500 0.754 3.065    -0.687   0.805 0.393
#> 3     Item03 500 0.726 2.650    -0.601   0.847 0.321
#> 4     Item04 500 0.776 3.464    -0.759   0.767 0.503
#> 5     Item05 500 0.804 4.102    -0.856   0.714 0.329
#> 6     Item06 500 0.864 6.353    -1.098   0.574 0.377
#> 7     Item07 500 0.716 2.521    -0.571   0.861 0.483
#> 8     Item08 500 0.588 1.427    -0.222   0.978 0.405
#> 9     Item09 500 0.364 0.572     0.348   0.946 0.225
#> 10    Item10 500 0.662 1.959    -0.418   0.923 0.314
#> 11    Item11 500 0.286 0.401     0.565   0.863 0.455
#> 12    Item12 500 0.274 0.377     0.601   0.847 0.468
#> 13    Item13 500 0.634 1.732    -0.342   0.948 0.471
#> 14    Item14 500 0.764 3.237    -0.719   0.788 0.485
#> 15    Item15 500 0.706 2.401    -0.542   0.874 0.413
```
