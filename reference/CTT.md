# Classical Test Theory

This function calculates the overall alpha and omega coefficients for
the given data matrix. It also computes the alpha coefficient for each
item, assuming that item is excluded.

## Usage

``` r
CTT(U, na = NULL, Z = NULL, w = NULL)
```

## Arguments

- U:

  U is a data matrix of the type matrix or data.frame.

- na:

  na argument specifies the numbers or characters to be treated as
  missing values.

- Z:

  Z is a missing indicator matrix of the type matrix or data.frame

- w:

  w is item weight vector

## Value

Returns a list of class c("exametrika", "CTT") containing two data
frames:

- Reliability:

  A data frame with overall reliability coefficients (Alpha and Omega)
  calculated using different correlation matrices (Covariance, Phi, and
  Tetrachoric)

- ReliabilityExcludingItem:

  A data frame showing alpha coefficients when each item is excluded,
  calculated using different correlation matrices

## Examples

``` r
# \donttest{
# using sample dataset
CTT(J15S500)
#> Realiability
#>                 name value
#> 1  Alpha(Covariance) 0.625
#> 2         Alpha(Phi) 0.630
#> 3 Alpha(Tetrachoric) 0.771
#> 4  Omega(Covariance) 0.632
#> 5         Omega(Phi) 0.637
#> 6 Omega(Tetrachoric) 0.779
#> 
#> Reliability Excluding Item
#>    IfDeleted Alpha.Covariance Alpha.Phi Alpha.Tetrachoric
#> 1     Item01            0.613     0.618             0.762
#> 2     Item02            0.609     0.615             0.759
#> 3     Item03            0.622     0.628             0.770
#> 4     Item04            0.590     0.595             0.742
#> 5     Item05            0.617     0.624             0.766
#> 6     Item06            0.608     0.613             0.754
#> 7     Item07            0.594     0.600             0.748
#> 8     Item08            0.611     0.616             0.762
#> 9     Item09            0.642     0.645             0.785
#> 10    Item10            0.626     0.630             0.773
#> 11    Item11            0.599     0.606             0.751
#> 12    Item12            0.597     0.603             0.748
#> 13    Item13            0.597     0.604             0.753
#> 14    Item14            0.593     0.598             0.745
#> 15    Item15            0.607     0.612             0.759
# }
```
