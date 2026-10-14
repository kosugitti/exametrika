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
#> Field1 0.6211 0.8643 0.8708 0.8975  0.951  1.000
#> Field2 0.0615 0.3375 0.4122 0.9172  0.990  1.000
#> Field3 0.2012 0.5444 0.2269 0.4721  0.705  1.000
#> Field4 0.0498 0.2471 0.0765 0.2301  0.647  0.983
#> Field5 0.0227 0.0547 0.0276 0.0429  0.160  0.983
#> 
#> Field Reference Profile Indices
#>        Alpha     A Beta     B Gamma       C
#> Field1     1 0.243    1 0.621   0.0  0.0000
#> Field2     3 0.505    3 0.412   0.0  0.0000
#> Field3     1 0.343    4 0.472   0.2 -0.3175
#> Field4     4 0.417    5 0.647   0.2 -0.1706
#> Field5     5 0.824    5 0.160   0.2 -0.0271
#> 
#>                               Class 1 Class 2 Class 3 Class 4 Class 5 Class 6
#> Test Reference Profile          4.420  11.958   8.458  15.936  23.296  34.713
#> Latent Class Distribution     154.000  62.000  86.000 106.000  90.000  17.000
#> Class Membership Distribution 142.719  72.744  87.731 106.916  87.920  16.971
#> 
#> Field Membership Profile
#>          CRR LFE Field1 Field2 Field3 Field4 Field5
#> Item01 0.850   1  1.000  0.000  0.000  0.000  0.000
#> Item31 0.812   1  1.000  0.000  0.000  0.000  0.000
#> Item32 0.808   1  1.000  0.000  0.000  0.000  0.000
#> Item21 0.616   2  0.000  1.000  0.000  0.000  0.000
#> Item23 0.600   2  0.000  1.000  0.000  0.000  0.000
#> Item22 0.586   2  0.000  1.000  0.000  0.000  0.000
#> Item24 0.567   2  0.000  1.000  0.000  0.000  0.000
#> Item25 0.491   2  0.000  1.000  0.000  0.000  0.000
#> Item11 0.476   2  0.000  1.000  0.000  0.000  0.000
#> Item26 0.452   2  0.000  1.000  0.000  0.000  0.000
#> Item27 0.414   2  0.000  1.000  0.000  0.000  0.000
#> Item07 0.573   3  0.000  0.000  1.000  0.000  0.000
#> Item03 0.458   3  0.000  0.000  1.000  0.000  0.000
#> Item33 0.437   3  0.000  0.000  1.000  0.000  0.000
#> Item02 0.392   3  0.000  0.000  1.000  0.000  0.000
#> Item09 0.390   3  0.000  0.000  1.000  0.000  0.000
#> Item10 0.353   3  0.000  0.000  1.000  0.000  0.000
#> Item08 0.350   3  0.000  0.000  1.000  0.000  0.000
#> Item12 0.340   4  0.000  0.000  0.000  1.000  0.000
#> Item04 0.303   4  0.000  0.000  0.000  1.000  0.000
#> Item17 0.276   4  0.000  0.000  0.000  1.000  0.000
#> Item05 0.250   4  0.000  0.000  0.000  1.000  0.000
#> Item13 0.237   4  0.000  0.000  0.000  1.000  0.000
#> Item34 0.229   4  0.000  0.000  0.000  1.000  0.000
#> Item29 0.227   4  0.000  0.000  0.000  1.000  0.000
#> Item28 0.221   4  0.000  0.000  0.000  1.000  0.000
#> Item06 0.216   4  0.000  0.000  0.000  1.000  0.000
#> Item16 0.216   4  0.000  0.000  0.000  1.000  0.000
#> Item35 0.155   5  0.000  0.000  0.000  0.000  1.000
#> Item14 0.126   5  0.000  0.000  0.000  0.000  1.000
#> Item15 0.087   5  0.000  0.000  0.000  0.000  1.000
#> Item30 0.085   5  0.000  0.000  0.000  0.000  1.000
#> Item20 0.054   5  0.000  0.000  0.000  0.000  1.000
#> Item19 0.052   5  0.000  0.000  0.000  0.000  1.000
#> Item18 0.049   5  0.000  0.000  0.000  0.000  1.000
#> Latent Field Distribution
#>            Field 1 Field 2 Field 3 Field 4 Field 5
#> N of Items       3       8       7      10       7
#> 
#> Model Fit Indices
#> Number of Latent Class : 6
#> Number of Latent Field: 5
#> Number of EM cycle: 35 
#>                    value
#> model_log_like -6884.961
#> bench_log_like -5891.314
#> null_log_like  -9862.114
#> model_Chi_sq    1987.294
#> null_Chi_sq     7941.601
#> model_df        1160.000
#> null_df         1155.000
#> NFI                0.750
#> RFI                0.751
#> IFI                0.878
#> TLI                0.879
#> CFI                0.878
#> RMSEA              0.037
#> AIC             -332.706
#> CAIC           -6415.940
#> BIC            -5255.940
#> 
#> Variational Fit
#>                      value
#> ELBO             -7703.364
#> log_lik_marginal -7647.034
#> entropy_class      160.682
#> entropy_field        0.001
#> ICL              15759.940
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

### Rated Data (Multiple-Choice with Correct Answers)

Rated data has both multiple response categories and correct answers.
[`Biclustering.rated()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
internally runs nominal Biclustering, then sorts classes by correct
response rate:

``` r

result.B.rated <- Biclustering(J21S300, ncls = 5, nfld = 3, method = "R", maxiter = 300)
result.B.rated
#> Ranklustering Analysis (Rated) [MIC]
#> 
#> Ranklustering Reference Matrix Profile (Nominal)
#> For category 1 
#>         Rank 1 Rank 2 Rank 3 Rank 4  Rank 5
#> Field 1  0.106  0.348 0.2844 0.6616 0.70854
#> Field 2  0.257  0.243 0.1832 0.0718 0.07513
#> Field 3  0.267  0.221 0.0966 0.1617 0.00454
#> For category 2 
#>         Rank 1 Rank 2 Rank 3 Rank 4 Rank 5
#> Field 1  0.283  0.257  0.228 0.1223 0.0973
#> Field 2  0.118  0.290  0.416 0.7988 0.7767
#> Field 3  0.308  0.216  0.129 0.0695 0.0600
#> For category 3 
#>         Rank 1 Rank 2 Rank 3 Rank 4 Rank 5
#> Field 1  0.315  0.183  0.236 0.1163 0.1030
#> Field 2  0.300  0.258  0.190 0.0666 0.0775
#> Field 3  0.121  0.409  0.557 0.7034 0.8295
#> For category 4 
#>         Rank 1 Rank 2 Rank 3 Rank 4 Rank 5
#> Field 1  0.297  0.212  0.252 0.0998 0.0911
#> Field 2  0.325  0.210  0.211 0.0628 0.0707
#> Field 3  0.304  0.154  0.218 0.0654 0.1060
#> 
#> Field Reference Profile (Binary)
#>         Rank 1 Rank 2 Rank 3 Rank 4 Rank 5
#> Field 1  0.102  0.387  0.252  0.658  0.702
#> Field 2  0.115  0.275  0.424  0.798  0.775
#> Field 3  0.121  0.389  0.574  0.655  0.835
#> 
#> Field Reference Profile Indices
#>        Alpha     A Beta     B Gamma       C
#> Field1     3 0.407    2 0.387  0.25 -0.1347
#> Field2     3 0.374    3 0.424  0.25 -0.0235
#> Field3     1 0.268    3 0.574  0.00  0.0000
#> 
#>                              Rank 1 Rank 2 Rank 3 Rank 4 Rank 5
#> Test Reference Profile        2.370  7.353  8.746 14.783 16.183
#> Latent Rank Distribution     73.000 51.000 59.000 46.000 71.000
#> Rank Membership Distribution 69.314 57.806 59.078 57.798 56.005
#> 
#> Field Membership Profile
#>          CRR LFE Field1 Field2 Field3
#> Item01 0.440   1  1.000  0.000  0.000
#> Item02 0.430   1  1.000  0.000  0.000
#> Item07 0.417   1  1.000  0.000  0.000
#> Item06 0.407   1  1.000  0.000  0.000
#> Item03 0.400   1  1.000  0.000  0.000
#> Item04 0.380   1  1.000  0.000  0.000
#> Item05 0.377   1  1.000  0.000  0.000
#> Item09 0.510   2  0.000  1.000  0.000
#> Item13 0.473   2  0.000  1.000  0.000
#> Item12 0.470   2  0.000  1.000  0.000
#> Item14 0.463   2  0.000  1.000  0.000
#> Item11 0.460   2  0.000  1.000  0.000
#> Item08 0.450   2  0.000  1.000  0.000
#> Item10 0.420   2  0.000  1.000  0.000
#> Item18 0.530   3  0.000  0.000  1.000
#> Item21 0.527   3  0.000  0.000  1.000
#> Item16 0.520   3  0.000  0.000  1.000
#> Item15 0.510   3  0.000  0.000  1.000
#> Item19 0.500   3  0.000  0.000  1.000
#> Item20 0.497   3  0.000  0.000  1.000
#> Item17 0.463   3  0.000  0.000  1.000
#> Latent Field Distribution
#>            Field 1 Field 2 Field 3
#> N of Items       7       7       7
#> 
#> Model Fit Indices (Binary)
#> Number of Latent Rank : 5
#> Number of Latent Field: 3
#> Number of EM cycle: 142 
#>                    value
#> model_log_like -3294.183
#> bench_log_like -3079.142
#> null_log_like  -4318.046
#> model_Chi_sq     430.082
#> null_Chi_sq     2477.806
#> model_df         336.000
#> null_df          420.000
#> NFI                0.826
#> RFI                0.783
#> IFI                0.956
#> TLI                0.943
#> CFI                0.954
#> RMSEA              0.031
#> AIC             -241.918
#> CAIC           -1822.389
#> BIC            -1486.389
#> 
#> Model Fit Indices (Nominal)
#>                    value
#> model_log_like -7478.122
#> AIC            15046.245
#> CAIC           15257.915
#> BIC            15212.915
#> 
#> Variational Fit (Nominal layer)
#>                      value
#> ELBO             -7478.122
#> log_lik_marginal -7455.051
#> entropy_class      154.843
#> entropy_field        0.000
#> ICL              15525.792
#> Weakly Ordinal Alignment Condition is Satisfied.
plot(result.B.rated, type = "Array")
```

![](biclustering_files/figure-html/bic-rated-1.png)

``` r

plot(result.B.rated, type = "FRP", nc = 3, nr = 1)
```

![](biclustering_files/figure-html/bic-rated-2.png)

Two layers of fit indices are reported: binary (with CFI/RMSEA) and
nominal (AIC/BIC/CAIC only). Access them via `result$TestFitIndices` and
`result$TestFitIndices_nominal`.

### Rated IRM

The IRM also supports rated data:

``` r

result.IRM.rated <- Biclustering_IRM(J21S300, gamma_c = 1, gamma_f = 1, verbose = FALSE)
plot(result.IRM.rated, type = "Array")
```

![](biclustering_files/figure-html/irm-rated-1.png)

``` r

plot(result.IRM.rated, type = "FRP", nc = 3, nr = 1)
```

![](biclustering_files/figure-html/irm-rated-2.png)

## Distractor Analysis

[`DistractorAnalysis()`](https://kosugitti.github.io/exametrika/reference/DistractorAnalysis.md)
examines how examinees in each rank/class respond to each item’s
categories. It computes observed frequencies, proportions, chi-square
statistics, p-values, and Cramer’s V (effect size) for each item-by-rank
cell.

``` r

result.B.rated <- Biclustering(J21S300, ncls = 5, nfld = 3, method = "R", maxiter = 300)
da <- DistractorAnalysis(result.B.rated)

# Full output (grouped by field for Biclustering)
print(da)

# Filter by items and/or ranks
print(da, items = 1:7, ranks = c(1, 5))

# Plot distractor bar charts
plot(da, items = 1:6, nc = 3, nr = 2)
```

[`DistractorAnalysis()`](https://kosugitti.github.io/exametrika/reference/DistractorAnalysis.md)
also works with
[`LRA()`](https://kosugitti.github.io/exametrika/reference/LRA.md)
results for rated data:

``` r

result.LRA.rated <- LRA(J21S300, nrank = 5, mic = TRUE)
da_lra <- DistractorAnalysis(result.LRA.rated)
print(da_lra, items = 1:3)
plot(da_lra, items = 1:6, nc = 3, nr = 2)
```

## Reference

Shojima, K. (2022). *Test Data Engineering*. Springer.
