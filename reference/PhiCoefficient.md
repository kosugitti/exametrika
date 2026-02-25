# Phi-Coefficient

The phi coefficient is the Pearson's product moment correlation
coefficient between two binary items. This function is applicable only
to binary response data. The coefficient ranges from -1 to 1, where 1
indicates perfect positive correlation, -1 indicates perfect negative
correlation, and 0 indicates no correlation.

## Usage

``` r
PhiCoefficient(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
PhiCoefficient(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
PhiCoefficient(U, na = NULL, Z = NULL, w = NULL)
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

A matrix of phi coefficients with exametrika class. Each element (i,j)
represents the phi coefficient between items i and j. The matrix is
symmetric with ones on the diagonal.

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## Examples

``` r
# example code
# Calculate Phi-Coefficient using sample dataset J15S500
PhiCoefficient(J15S500)
#>         Item01  Item02   Item03 Item04  Item05  Item06 Item07  Item08   Item09
#> Item01 1.00000  0.0721  0.00208 0.0722  0.1401  0.1304 0.0706  0.1277  0.05947
#> Item02 0.07210  1.0000  0.09681 0.1943  0.0806  0.1121 0.1037  0.0785 -0.00220
#> Item03 0.00208  0.0968  1.00000 0.1432  0.0807  0.0964 0.1401  0.0506 -0.00123
#> Item04 0.07220  0.1943  0.14318 1.0000  0.0973  0.1367 0.2574  0.1350  0.01763
#> Item05 0.14014  0.0806  0.08074 0.0973  1.0000  0.0540 0.1024  0.0473 -0.04532
#> Item06 0.13039  0.1121  0.09639 0.1367  0.0540  1.0000 0.1642  0.1539 -0.02726
#> Item07 0.07063  0.1037  0.14013 0.2574  0.1024  0.1642 1.0000  0.1577  0.04321
#> Item08 0.12766  0.0785  0.05062 0.1350  0.0473  0.1539 0.1577  1.0000 -0.01703
#> Item09 0.05947 -0.0022 -0.00123 0.0176 -0.0453 -0.0273 0.0432 -0.0170  1.00000
#> Item10 0.03957  0.0533  0.04450 0.0623  0.0519  0.0742 0.1219  0.0290 -0.06577
#> Item11 0.12528  0.1457  0.06135 0.1702  0.0784  0.0574 0.1630  0.1701  0.03632
#> Item12 0.10093  0.1218  0.06573 0.2225  0.0774  0.0737 0.1681  0.1225  0.15035
#> Item13 0.10986  0.0962  0.10108 0.1992  0.1478  0.0983 0.1384  0.1316  0.04843
#> Item14 0.14097  0.1528  0.02818 0.1759  0.1053  0.1780 0.1304  0.0994  0.07784
#> Item15 0.09744  0.1105  0.03663 0.2008  0.0684  0.1794 0.1485  0.0752 -0.01361
#>         Item10 Item11 Item12 Item13 Item14  Item15
#> Item01  0.0396 0.1253 0.1009 0.1099 0.1410  0.0974
#> Item02  0.0533 0.1457 0.1218 0.0962 0.1528  0.1105
#> Item03  0.0445 0.0613 0.0657 0.1011 0.0282  0.0366
#> Item04  0.0623 0.1702 0.2225 0.1992 0.1759  0.2008
#> Item05  0.0519 0.0784 0.0774 0.1478 0.1053  0.0684
#> Item06  0.0742 0.0574 0.0737 0.0983 0.1780  0.1794
#> Item07  0.1219 0.1630 0.1681 0.1384 0.1304  0.1485
#> Item08  0.0290 0.1701 0.1225 0.1316 0.0994  0.0752
#> Item09 -0.0658 0.0363 0.1503 0.0484 0.0778 -0.0136
#> Item10  1.0000 0.0873 0.0408 0.1154 0.1406  0.0493
#> Item11  0.0873 1.0000 0.2364 0.0766 0.2163  0.0976
#> Item12  0.0408 0.2364 1.0000 0.1316 0.1830  0.1110
#> Item13  0.1154 0.0766 0.1316 1.0000 0.1937  0.1841
#> Item14  0.1406 0.2163 0.1830 0.1937 1.0000  0.1479
#> Item15  0.0493 0.0976 0.1110 0.1841 0.1479  1.0000
```
