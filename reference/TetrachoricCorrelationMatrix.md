# Tetrachoric Correlation Matrix

Calculates the matrix of tetrachoric correlations between all pairs of
items. Tetrachoric Correlation is superior to the phi coefficient as a
measure of the relation of an item pair. This function is applicable
only to binary response data.

## Usage

``` r
TetrachoricCorrelationMatrix(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
TetrachoricCorrelationMatrix(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
TetrachoricCorrelationMatrix(U, na = NULL, Z = NULL, w = NULL)
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

A matrix of tetrachoric correlations with exametrika class. Each element
(i,j) represents the tetrachoric correlation between items i and j. The
matrix is symmetric with ones on the diagonal.

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

## Examples

``` r
# \donttest{
# example code
TetrachoricCorrelationMatrix(J15S500)
#>         Item01   Item02   Item03 Item04  Item05  Item06 Item07  Item08   Item09
#> Item01 1.00000  0.12990  0.00378 0.1322  0.2540  0.2544 0.1246  0.2149  0.10468
#> Item02 0.12990  1.00000  0.17081 0.3373  0.1513  0.2219 0.1816  0.1342 -0.00385
#> Item03 0.00378  0.17081  1.00000 0.2516  0.1493  0.1907 0.2386  0.0852 -0.00211
#> Item04 0.13217  0.33732  0.25155 1.0000  0.1833  0.2687 0.4291  0.2325  0.03161
#> Item05 0.25397  0.15131  0.14930 0.1833  1.0000  0.1154 0.1867  0.0852 -0.08231
#> Item06 0.25443  0.22191  0.19073 0.2687  0.1154  1.0000 0.3121  0.2957 -0.05430
#> Item07 0.12457  0.18162  0.23858 0.4291  0.1867  0.3121 1.0000  0.2589  0.07417
#> Item08 0.21492  0.13418  0.08517 0.2325  0.0852  0.2957 0.2589  1.0000 -0.02755
#> Item09 0.10468 -0.00385 -0.00211 0.0316 -0.0823 -0.0543 0.0742 -0.0276  1.00000
#> Item10 0.06887  0.09292  0.07633 0.1103  0.0948  0.1460 0.2040  0.0473 -0.10802
#> Item11 0.23607  0.27919  0.11117 0.3397  0.1552  0.1248 0.3017  0.2887  0.06143
#> Item12 0.19055  0.23396  0.12039 0.4670  0.1549  0.1640 0.3154  0.2095  0.24969
#> Item13 0.18651  0.16483  0.16982 0.3373  0.2606  0.1909 0.2292  0.2101  0.07976
#> Item14 0.24813  0.26860  0.05143 0.3094  0.1961  0.3387 0.2276  0.1705  0.13977
#> Item15 0.16949  0.19225  0.06418 0.3424  0.1261  0.3381 0.2500  0.1247 -0.02301
#>         Item10 Item11 Item12 Item13 Item14  Item15
#> Item01  0.0689 0.2361 0.1906 0.1865 0.2481  0.1695
#> Item02  0.0929 0.2792 0.2340 0.1648 0.2686  0.1922
#> Item03  0.0763 0.1112 0.1204 0.1698 0.0514  0.0642
#> Item04  0.1103 0.3397 0.4670 0.3373 0.3094  0.3424
#> Item05  0.0948 0.1552 0.1549 0.2606 0.1961  0.1261
#> Item06  0.1460 0.1248 0.1640 0.1909 0.3387  0.3381
#> Item07  0.2040 0.3017 0.3154 0.2292 0.2276  0.2500
#> Item08  0.0473 0.2887 0.2095 0.2101 0.1705  0.1247
#> Item09 -0.1080 0.0614 0.2497 0.0798 0.1398 -0.0230
#> Item10  1.0000 0.1526 0.0713 0.1878 0.2408  0.0835
#> Item11  0.1526 1.0000 0.3881 0.1318 0.4345  0.1758
#> Item12  0.0713 0.3881 1.0000 0.2302 0.3667  0.2028
#> Item13  0.1878 0.1318 0.2302 1.0000 0.3255  0.2999
#> Item14  0.2408 0.4345 0.3667 0.3255 1.0000  0.2555
#> Item15  0.0835 0.1758 0.2028 0.2999 0.2555  1.0000
# }
```
