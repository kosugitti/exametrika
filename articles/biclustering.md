# Biclustering and Ranklustering

> **Note**: Some computationally intensive examples below are shown with
> `eval=FALSE` to keep CRAN build times short. For full rendered output,
> see the [pkgdown
> site](https://kosugitti.github.io/exametrika/articles/biclustering.html).

``` r
library(exametrika)
```

## Biclustering and Ranklustering

Biclustering and Ranklustering simultaneously cluster items into fields
and examinees into classes/ranks. The difference is specified via the
`method` option:

- `method = "B"`: Biclustering (no filtering matrix)
- `method = "R"`: Ranklustering (with filtering matrix for ordered
  ranks)

### Biclustering

``` r
Biclustering(J35S515, nfld = 5, ncls = 6, method = "B")
#> Biclustering Analysis
#> 
#> Biclustering Reference Matrix Profile
#>        Class1 Class2 Class3 Class4 Class5 Class6
#> Field1 0.6236 0.8636 0.8718  0.898  0.952  1.000
#> Field2 0.0627 0.3332 0.4255  0.919  0.990  1.000
#> Field3 0.2008 0.5431 0.2281  0.475  0.706  1.000
#> Field4 0.0495 0.2455 0.0782  0.233  0.648  0.983
#> Field5 0.0225 0.0545 0.0284  0.043  0.160  0.983
#> 
#> Field Reference Profile Indices
#>        Alpha     A Beta     B Gamma       C
#> Field1     1 0.240    1 0.624   0.0  0.0000
#> Field2     3 0.493    3 0.426   0.0  0.0000
#> Field3     1 0.342    4 0.475   0.2 -0.3149
#> Field4     4 0.415    5 0.648   0.2 -0.1673
#> Field5     5 0.823    5 0.160   0.2 -0.0261
#> 
#>                               Class 1 Class 2 Class 3 Class 4 Class 5 Class 6
#> Test Reference Profile          4.431  11.894   8.598  16.002  23.326  34.713
#> Latent Class Ditribution      157.000  64.000  82.000 106.000  89.000  17.000
#> Class Membership Distribution 146.105  73.232  85.753 106.414  86.529  16.968
#> 
#> Field Membership Profile
#>          CRR   LFE Field1 Field2 Field3 Field4 Field5
#> Item01 0.850 1.000  1.000  0.000  0.000  0.000  0.000
#> Item31 0.812 1.000  1.000  0.000  0.000  0.000  0.000
#> Item32 0.808 1.000  1.000  0.000  0.000  0.000  0.000
#> Item21 0.616 2.000  0.000  1.000  0.000  0.000  0.000
#> Item23 0.600 2.000  0.000  1.000  0.000  0.000  0.000
#> Item22 0.586 2.000  0.000  1.000  0.000  0.000  0.000
#> Item24 0.567 2.000  0.000  1.000  0.000  0.000  0.000
#> Item25 0.491 2.000  0.000  1.000  0.000  0.000  0.000
#> Item11 0.476 2.000  0.000  1.000  0.000  0.000  0.000
#> Item26 0.452 2.000  0.000  1.000  0.000  0.000  0.000
#> Item27 0.414 2.000  0.000  1.000  0.000  0.000  0.000
#> Item07 0.573 3.000  0.000  0.000  1.000  0.000  0.000
#> Item03 0.458 3.000  0.000  0.000  1.000  0.000  0.000
#> Item33 0.437 3.000  0.000  0.000  1.000  0.000  0.000
#> Item02 0.392 3.000  0.000  0.000  1.000  0.000  0.000
#> Item09 0.390 3.000  0.000  0.000  1.000  0.000  0.000
#> Item10 0.353 3.000  0.000  0.000  1.000  0.000  0.000
#> Item08 0.350 3.000  0.000  0.000  1.000  0.000  0.000
#> Item12 0.340 4.000  0.000  0.000  0.000  1.000  0.000
#> Item04 0.303 4.000  0.000  0.000  0.000  1.000  0.000
#> Item17 0.276 4.000  0.000  0.000  0.000  1.000  0.000
#> Item05 0.250 4.000  0.000  0.000  0.000  1.000  0.000
#> Item13 0.237 4.000  0.000  0.000  0.000  1.000  0.000
#> Item34 0.229 4.000  0.000  0.000  0.000  1.000  0.000
#> Item29 0.227 4.000  0.000  0.000  0.000  1.000  0.000
#> Item28 0.221 4.000  0.000  0.000  0.000  1.000  0.000
#> Item06 0.216 4.000  0.000  0.000  0.000  1.000  0.000
#> Item16 0.216 4.000  0.000  0.000  0.000  1.000  0.000
#> Item35 0.155 5.000  0.000  0.000  0.000  0.000  1.000
#> Item14 0.126 5.000  0.000  0.000  0.000  0.000  1.000
#> Item15 0.087 5.000  0.000  0.000  0.000  0.000  1.000
#> Item30 0.085 5.000  0.000  0.000  0.000  0.000  1.000
#> Item20 0.054 5.000  0.000  0.000  0.000  0.000  1.000
#> Item19 0.052 5.000  0.000  0.000  0.000  0.000  1.000
#> Item18 0.049 5.000  0.000  0.000  0.000  0.000  1.000
#> Latent Field Distribution
#>            Field 1 Field 2 Field 3 Field 4 Field 5
#> N of Items       3       8       7      10       7
#> 
#> Model Fit Indices
#> Number of Latent Class : 6
#> Number of Latent Field: 5
#> Number of EM cycle: 33 
#>                    value
#> model_log_like -6884.582
#> bench_log_like -5891.314
#> null_log_like  -9862.114
#> model_Chi_sq    1986.535
#> null_Chi_sq     7941.601
#> model_df        1160.000
#> null_df         1155.000
#> NFI                0.750
#> RFI                0.751
#> IFI                0.878
#> TLI                0.879
#> CFI                0.878
#> RMSEA              0.037
#> AIC             -333.465
#> CAIC           -6416.699
#> BIC            -5256.699
```

### Ranklustering

``` r
result.Ranklustering <- Biclustering(J35S515, nfld = 5, ncls = 6, method = "R")
```

``` r
plot(result.Ranklustering, type = "Array")
```

![](biclustering_files/figure-html/plot-ranklustering-1.png)

``` r
plot(result.Ranklustering, type = "FRP", nc = 2, nr = 3)
```

![](biclustering_files/figure-html/plot-ranklustering-2.png)

``` r
plot(result.Ranklustering, type = "RRV")
```

![](biclustering_files/figure-html/plot-ranklustering-3.png)

``` r
plot(result.Ranklustering, type = "RMP", students = 1:9, nc = 3, nr = 3)
```

![](biclustering_files/figure-html/plot-ranklustering-4.png)

``` r
plot(result.Ranklustering, type = "LRD")
```

![](biclustering_files/figure-html/plot-ranklustering-5.png)

## Finding Optimal Number of Classes and Fields

### Grid Search

[`GridSearch()`](https://kosugitti.github.io/exametrika/reference/GridSearch.md)
systematically evaluates multiple parameter combinations and selects the
best-fitting model:

``` r
result <- GridSearch(J35S515, method = "R", max_ncls = 10, max_nfld = 10, index = "BIC")
result$optimal_ncls
result$optimal_nfld
result$optimal_result
```

### Infinite Relational Model (IRM)

The IRM uses the Chinese Restaurant Process to automatically determine
the optimal number of fields and classes:

``` r
result.IRM <- Biclustering_IRM(J35S515, gamma_c = 1, gamma_f = 1, verbose = TRUE)
plot(result.IRM, type = "Array")
plot(result.IRM, type = "FRP", nc = 3)
plot(result.IRM, type = "TRP")
```

## Biclustering for Polytomous Data

### Ordinal Data

``` r
result.B.ord <- Biclustering(J35S500, ncls = 5, nfld = 5, method = "R")
result.B.ord
plot(result.B.ord, type = "Array")
```

FRP (Field Reference Profile) shows the expected score per field across
latent ranks:

``` r
plot(result.B.ord, type = "FRP", nc = 3, nr = 2)
```

FCRP (Field Category Response Profile) shows category probabilities
across ranks. The `style` parameter can be `"line"` or `"bar"`:

``` r
plot(result.B.ord, type = "FCRP", nc = 3, nr = 2)
plot(result.B.ord, type = "FCRP", style = "bar", nc = 3, nr = 2)
```

FCBR (Field Cumulative Boundary Reference) shows cumulative boundary
probabilities (ordinal only):

``` r
plot(result.B.ord, type = "FCBR", nc = 3, nr = 2)
```

ScoreField and RRV plots:

``` r
plot(result.B.ord, type = "ScoreField")
plot(result.B.ord, type = "RRV")
```

### Nominal Data

``` r
result.B.nom <- Biclustering(J20S600, ncls = 5, nfld = 4)
result.B.nom
plot(result.B.nom, type = "Array")
```

Nominal Biclustering supports FRP, FCRP, ScoreField, and RRV (but not
FCBR):

``` r
plot(result.B.nom, type = "FRP", nc = 2, nr = 2)
plot(result.B.nom, type = "FCRP", nc = 2, nr = 2)
plot(result.B.nom, type = "FCRP", style = "bar", nc = 2, nr = 2)
plot(result.B.nom, type = "ScoreField")
plot(result.B.nom, type = "RRV")
```

## Reference

Shojima, K. (2022). *Test Data Engineering*. Springer.
