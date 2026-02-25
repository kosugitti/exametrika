# Item Lift

The lift is a commonly used index in a POS data analysis. The item lift
of Item k to Item j is defined as follow: \\ l\_{jk} = \frac{p\_{k\mid
j}}{p_k} \\ This function is applicable only to binary response data.

## Usage

``` r
ItemLift(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
ItemLift(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
ItemLift(U, na = NULL, Z = NULL, w = NULL)
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

A matrix of item lift values with exametrika class. Each element (j,k)
represents the lift value of item k given item j, which indicates how
much more likely item k is to be correct given that item j was answered
correctly.

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## References

Brin, S., Motwani, R., Ullman, J., & Tsur, S. (1997). Dynamic itemset
counting and implication rules for market basket data. In Proceedings of
ACM SIGMOD International Conference on Management of Data (pp. 255–264).
https://dl.acm.org/doi/10.1145/253262.253325

## Examples

``` r
# example code
# Calculate ItemLift using sample dataset J5S10
ItemLift(J5S10)
#>        Item01 Item02 Item03 Item04 Item05
#> Item01  1.667   1.25   1.11   1.67  0.833
#> Item02  1.250   2.50   1.11   1.67  1.250
#> Item03  1.111   1.11   1.11   1.11  1.111
#> Item04  1.667   1.67   1.11   3.33  1.667
#> Item05  0.833   1.25   1.11   1.67  2.500
```
