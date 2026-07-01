# Distractor Analysis

Performs distractor analysis for rated (multiple-choice) models. For
each item and rank/class, computes observed category frequency tables,
chi-square tests against chance level (uniform distribution), and
Cramer's V as an effect size measure.

This function works with results from
[`LRA`](https://kosugitti.github.io/exametrika/reference/LRA.md) (rated
data) and
[`Biclustering`](https://kosugitti.github.io/exametrika/reference/Biclustering.md)
/
[`Biclustering_IRM`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md)
(rated data).

## Usage

``` r
DistractorAnalysis(x, ...)

# S3 method for class 'LRArated'
DistractorAnalysis(x, ...)

# S3 method for class 'ratedBiclustering'
DistractorAnalysis(x, ...)

# S3 method for class 'DistractorAnalysis'
print(x, items = NULL, ranks = NULL, digits = 4, ...)

# S3 method for class 'DistractorAnalysis'
plot(x, type = "Distractor", items = NULL, ranks = NULL, nc = 1, nr = 1, ...)
```

## Arguments

- x:

  A result object from a rated model (class `LRArated` or
  `ratedBiclustering`).

- ...:

  Additional arguments (currently unused).

- items:

  Integer vector of item indices to display. NULL for all items.

- ranks:

  Integer vector of rank/class indices to display. NULL for all ranks.

- digits:

  Number of digits for rounding. Default is 4.

- type:

  Plot type. Currently only "Distractor" is supported.

- nc:

  Number of columns in the plot grid.

- nr:

  Number of rows in the plot grid.

## Value

An object of class `c("exametrika", "DistractorAnalysis")` containing:

- freq_table:

  List of frequency matrices (nrank x maxQ), one per item.

- prop_table:

  List of proportion matrices (nrank x maxQ), one per item.

- chisq_table:

  Matrix (nitems x nrank) of chi-square statistics.

- pvalue_table:

  Matrix (nitems x nrank) of p-values.

- cramersv_table:

  Matrix (nitems x nrank) of Cramer's V effect sizes.

- CA:

  Correct answer vector.

- n_rank:

  Number of ranks/classes.

- maxQ:

  Number of response categories.

- nitems:

  Number of items.

- ItemLabel:

  Item label vector.

- n_field:

  Number of fields (Biclustering only).

- FieldEstimated:

  Field assignment vector (Biclustering only).

- field_items:

  List of item indices per field (Biclustering only).

## Examples

``` r
# \donttest{
# LRA.rated example
result_lra <- LRA(J21S300, nrank = 5, mic = TRUE)
da <- DistractorAnalysis(result_lra)
print(da)
#> Distractor Analysis
#> 
#> 
#> Cramer's V
#>         Rank1  Rank2  Rank3  Rank4  Rank5
#> Item01 0.2838 0.2695 0.2773 0.2688 0.7160
#> Item02 0.2459 0.2091 0.3360 0.4620 0.6110
#> Item03 0.1592 0.2874 0.2133 0.4765 0.6449
#> Item04 0.1592 0.2091 0.2355 0.4355 0.6684
#> Item05 0.2937 0.2010 0.2255 0.2315 0.5920
#> Item06 0.2340 0.3006 0.3738 0.2351 0.7550
#> Item07 0.1570 0.2546 0.2060 0.3988 0.7160
#> Item08 0.2601 0.1645 0.2466 0.5737 0.7213
#> Item09 0.1900 0.0841 0.2527 0.5955 0.8936
#> Item10 0.2761 0.1837 0.1448 0.4144 0.7515
#> Item11 0.1266 0.0773 0.1623 0.7011 0.6824
#> Item12 0.2148 0.2874 0.3303 0.6931 0.7346
#> Item13 0.2695 0.2118 0.1885 0.4583 0.7824
#> Item14 0.2340 0.2010 0.3888 0.4974 0.8060
#> Item15 0.3080 0.4488 0.3405 0.5510 0.6831
#> Item16 0.2045 0.3220 0.3232 0.5109 0.8617
#> Item17 0.2900 0.4046 0.3371 0.4315 0.7531
#> Item18 0.3401 0.2342 0.3472 0.8128 0.8081
#> Item19 0.1937 0.2064 0.2934 0.7161 0.6460
#> Item20 0.1499 0.1982 0.3303 0.5281 0.7525
#> Item21 0.2517 0.3272 0.4406 0.6096 0.6845
#> 
#> P-values
#>            Rank1     Rank2     Rank3     Rank4     Rank5
#> Item01 2.055e-03 1.363e-02 3.487e-03 6.909e-03 7.753e-25
#> Item02 1.138e-02 9.252e-02 1.711e-04 8.028e-08 4.282e-18
#> Item03 2.002e-01 6.909e-03 4.497e-02 2.636e-08 3.716e-20
#> Item04 2.002e-01 9.252e-02 2.022e-02 5.609e-07 1.201e-21
#> Item05 1.254e-03 1.146e-01 2.929e-02 2.929e-02 5.372e-17
#> Item06 1.843e-02 4.058e-03 1.759e-05 2.572e-02 1.287e-27
#> Item07 2.116e-01 2.301e-02 5.734e-02 6.758e-06 7.753e-25
#> Item08 6.197e-03 2.637e-01 1.308e-02 5.967e-12 3.326e-25
#> Item09 8.555e-02 7.914e-01 1.019e-02 7.258e-13 1.041e-38
#> Item10 2.973e-03 1.748e-01 2.943e-01 2.400e-06 2.303e-27
#> Item11 4.018e-01 8.308e-01 1.984e-01 8.617e-18 1.452e-22
#> Item12 3.770e-02 6.909e-03 2.364e-04 2.157e-17 3.803e-26
#> Item13 4.040e-03 8.611e-02 9.840e-02 1.060e-07 1.157e-29
#> Item14 1.843e-02 1.146e-01 6.602e-06 4.947e-09 1.763e-31
#> Item15 5.957e-04 1.665e-06 1.321e-04 4.892e-11 1.307e-22
#> Item16 5.369e-02 1.619e-03 3.482e-04 1.618e-09 5.433e-36
#> Item17 1.510e-03 2.426e-05 1.604e-04 7.401e-07 1.768e-27
#> Item18 9.734e-05 4.476e-02 8.952e-05 6.685e-24 1.217e-31
#> Item19 7.620e-02 9.939e-02 1.625e-03 1.475e-18 3.172e-20
#> Item20 2.493e-01 1.231e-01 2.364e-04 3.727e-10 1.965e-27
#> Item21 8.927e-03 1.285e-03 1.666e-07 1.779e-13 1.058e-22
#> 
#> Category Proportions by Rank 
#> 
#>   Item01 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.0492 0.3115 0.3770 0.2623
#> Rank2 0.4082 0.1020 0.3061 0.1837
#> Rank3 0.4237 0.2542 0.0847 0.2373
#> Rank4 0.4464 0.1429 0.2143 0.1964
#> Rank5 0.7867 0.0800 0.0800 0.0533
#> 
#>   Item02 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1148 0.1803 0.3279 0.3770
#> Rank2 0.1429 0.3673 0.3061 0.1837
#> Rank3 0.4915 0.2034 0.2034 0.1017
#> Rank4 0.5893 0.0714 0.1786 0.1607
#> Rank5 0.7067 0.0800 0.0800 0.1333
#> 
#>   Item03 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1311 0.2951 0.2787 0.2951
#> Rank2 0.0408 0.3673 0.2857 0.3061
#> Rank3 0.3729 0.1525 0.1695 0.3051
#> Rank4 0.5893 0.2143 0.0357 0.1607
#> Rank5 0.7333 0.0800 0.1067 0.0800
#> 
#>   Item04 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1311 0.2951 0.2787 0.2951
#> Rank2 0.1429 0.3673 0.3061 0.1837
#> Rank3 0.1864 0.1695 0.2203 0.4237
#> Rank4 0.5714 0.1250 0.1964 0.1071
#> Rank5 0.7467 0.1467 0.0400 0.0667
#> 
#>   Item05 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.0492 0.3115 0.2459 0.3934
#> Rank2 0.3673 0.1224 0.2449 0.2653
#> Rank3 0.3220 0.2712 0.3220 0.0847
#> Rank4 0.3750 0.3036 0.1071 0.2143
#> Rank5 0.6933 0.1067 0.1200 0.0800
#> 
#>   Item06 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1148 0.3934 0.2787 0.2131
#> Rank2 0.1020 0.1429 0.4082 0.3469
#> Rank3 0.4576 0.3220 0.0169 0.2034
#> Rank4 0.3929 0.1071 0.2679 0.2321
#> Rank5 0.8133 0.0667 0.1067 0.0133
#> 
#>   Item07 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1475 0.2295 0.3115 0.3115
#> Rank2 0.1633 0.3878 0.1224 0.3265
#> Rank3 0.3390 0.3390 0.1525 0.1695
#> Rank4 0.5179 0.1071 0.2857 0.0893
#> Rank5 0.7867 0.0800 0.0800 0.0533
#> 
#>   Item08 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1639 0.1148 0.3770 0.3443
#> Rank2 0.2041 0.2449 0.3673 0.1837
#> Rank3 0.3729 0.3390 0.1356 0.1525
#> Rank4 0.0714 0.6607 0.2321 0.0357
#> Rank5 0.0533 0.7867 0.0267 0.1333
#> 
#>   Item09 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2459 0.1475 0.3770 0.2295
#> Rank2 0.2041 0.2449 0.2449 0.3061
#> Rank3 0.2203 0.4068 0.2712 0.1017
#> Rank4 0.1071 0.6964 0.1071 0.0893
#> Rank5 0.0267 0.9200 0.0400 0.0133
#> 
#>   Item10 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.4098 0.0820 0.2131 0.2951
#> Rank2 0.1224 0.3061 0.2449 0.3265
#> Rank3 0.3051 0.2373 0.3051 0.1525
#> Rank4 0.1964 0.5536 0.0893 0.1607
#> Rank5 0.0533 0.8133 0.0533 0.0800
#> 
#>   Item11 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1803 0.2131 0.2951 0.3115
#> Rank2 0.2245 0.2449 0.3061 0.2245
#> Rank3 0.1525 0.2203 0.2881 0.3390
#> Rank4 0.0536 0.7679 0.0179 0.1607
#> Rank5 0.1067 0.7600 0.0933 0.0400
#> 
#>   Item12 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2131 0.1148 0.3279 0.3443
#> Rank2 0.2857 0.0408 0.3061 0.3673
#> Rank3 0.1186 0.4915 0.1864 0.2034
#> Rank4 0.1071 0.7679 0.0893 0.0357
#> Rank5 0.0933 0.8000 0.0400 0.0667
#> 
#>   Item13 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.3934 0.0984 0.1803 0.3279
#> Rank2 0.1837 0.4082 0.2041 0.2041
#> Rank3 0.2881 0.3559 0.2203 0.1356
#> Rank4 0.1429 0.5893 0.0893 0.1786
#> Rank5 0.0000 0.8267 0.1600 0.0133
#> 
#>   Item14 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.3115 0.0820 0.2623 0.3443
#> Rank2 0.3673 0.1224 0.2449 0.2653
#> Rank3 0.0508 0.5085 0.1695 0.2712
#> Rank4 0.0893 0.6071 0.0714 0.2321
#> Rank5 0.0667 0.8533 0.0667 0.0133
#> 
#>   Item15 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2459 0.2459 0.0656 0.4426
#> Rank2 0.0612 0.1429 0.5714 0.2245
#> Rank3 0.1017 0.2373 0.4915 0.1695
#> Rank4 0.2857 0.0714 0.6250 0.0179
#> Rank5 0.0800 0.0400 0.7600 0.1200
#> 
#>   Item16 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.3115 0.3115 0.0984 0.2787
#> Rank2 0.0816 0.1429 0.3878 0.3878
#> Rank3 0.1695 0.1525 0.4915 0.1864
#> Rank4 0.0536 0.1786 0.6250 0.1429
#> Rank5 0.0933 0.0000 0.8933 0.0133
#> 
#>   Item17 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1311 0.4262 0.1311 0.3115
#> Rank2 0.5510 0.1429 0.1837 0.1224
#> Rank3 0.2203 0.1864 0.4915 0.1017
#> Rank4 0.1607 0.1607 0.5714 0.1071
#> Rank5 0.0267 0.0667 0.8133 0.0933
#> 
#>   Item18 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2295 0.2951 0.0328 0.4426
#> Rank2 0.3878 0.1633 0.3061 0.1429
#> Rank3 0.1356 0.1695 0.5085 0.1864
#> Rank4 0.0893 0.0000 0.8571 0.0536
#> Rank5 0.0000 0.0533 0.8533 0.0933
#> 
#>   Item19 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2787 0.3443 0.1148 0.2623
#> Rank2 0.1224 0.2857 0.3673 0.2245
#> Rank3 0.2203 0.0847 0.4407 0.2542
#> Rank4 0.0536 0.0536 0.7857 0.1071
#> Rank5 0.0667 0.0800 0.7333 0.1200
#> 
#>   Item20 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2295 0.3443 0.1639 0.2623
#> Rank2 0.2857 0.3061 0.3061 0.1020
#> Rank3 0.0678 0.1864 0.4576 0.2881
#> Rank4 0.1429 0.0714 0.6429 0.1429
#> Rank5 0.0533 0.0933 0.8133 0.0400
#> 
#>   Item21 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.3115 0.3443 0.0656 0.2787
#> Rank2 0.1224 0.2041 0.4898 0.1837
#> Rank3 0.1864 0.1017 0.5763 0.1356
#> Rank4 0.0179 0.1786 0.6964 0.1071
#> Rank5 0.0933 0.0267 0.7600 0.1200
print(da, items = 1:3)
#> Distractor Analysis
#> 
#> 
#> Cramer's V
#>         Rank1  Rank2  Rank3  Rank4  Rank5
#> Item01 0.2838 0.2695 0.2773 0.2688 0.7160
#> Item02 0.2459 0.2091 0.3360 0.4620 0.6110
#> Item03 0.1592 0.2874 0.2133 0.4765 0.6449
#> 
#> P-values
#>           Rank1    Rank2     Rank3     Rank4     Rank5
#> Item01 0.002055 0.013630 0.0034870 6.909e-03 7.753e-25
#> Item02 0.011380 0.092520 0.0001711 8.028e-08 4.282e-18
#> Item03 0.200200 0.006909 0.0449700 2.636e-08 3.716e-20
#> 
#> Category Proportions by Rank 
#> 
#>   Item01 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.0492 0.3115 0.3770 0.2623
#> Rank2 0.4082 0.1020 0.3061 0.1837
#> Rank3 0.4237 0.2542 0.0847 0.2373
#> Rank4 0.4464 0.1429 0.2143 0.1964
#> Rank5 0.7867 0.0800 0.0800 0.0533
#> 
#>   Item02 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1148 0.1803 0.3279 0.3770
#> Rank2 0.1429 0.3673 0.3061 0.1837
#> Rank3 0.4915 0.2034 0.2034 0.1017
#> Rank4 0.5893 0.0714 0.1786 0.1607
#> Rank5 0.7067 0.0800 0.0800 0.1333
#> 
#>   Item03 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1311 0.2951 0.2787 0.2951
#> Rank2 0.0408 0.3673 0.2857 0.3061
#> Rank3 0.3729 0.1525 0.1695 0.3051
#> Rank4 0.5893 0.2143 0.0357 0.1607
#> Rank5 0.7333 0.0800 0.1067 0.0800
print(da, ranks = c(1, 5))
#> Distractor Analysis
#> 
#> 
#> Cramer's V
#>         Rank1  Rank5
#> Item01 0.2838 0.7160
#> Item02 0.2459 0.6110
#> Item03 0.1592 0.6449
#> Item04 0.1592 0.6684
#> Item05 0.2937 0.5920
#> Item06 0.2340 0.7550
#> Item07 0.1570 0.7160
#> Item08 0.2601 0.7213
#> Item09 0.1900 0.8936
#> Item10 0.2761 0.7515
#> Item11 0.1266 0.6824
#> Item12 0.2148 0.7346
#> Item13 0.2695 0.7824
#> Item14 0.2340 0.8060
#> Item15 0.3080 0.6831
#> Item16 0.2045 0.8617
#> Item17 0.2900 0.7531
#> Item18 0.3401 0.8081
#> Item19 0.1937 0.6460
#> Item20 0.1499 0.7525
#> Item21 0.2517 0.6845
#> 
#> P-values
#>            Rank1     Rank5
#> Item01 2.055e-03 7.753e-25
#> Item02 1.138e-02 4.282e-18
#> Item03 2.002e-01 3.716e-20
#> Item04 2.002e-01 1.201e-21
#> Item05 1.254e-03 5.372e-17
#> Item06 1.843e-02 1.287e-27
#> Item07 2.116e-01 7.753e-25
#> Item08 6.197e-03 3.326e-25
#> Item09 8.555e-02 1.041e-38
#> Item10 2.973e-03 2.303e-27
#> Item11 4.018e-01 1.452e-22
#> Item12 3.770e-02 3.803e-26
#> Item13 4.040e-03 1.157e-29
#> Item14 1.843e-02 1.763e-31
#> Item15 5.957e-04 1.307e-22
#> Item16 5.369e-02 5.433e-36
#> Item17 1.510e-03 1.768e-27
#> Item18 9.734e-05 1.217e-31
#> Item19 7.620e-02 3.172e-20
#> Item20 2.493e-01 1.965e-27
#> Item21 8.927e-03 1.058e-22
#> 
#> Category Proportions by Rank 
#> 
#>   Item01 (CA = Cat1)
#>         Cat1   Cat2  Cat3   Cat4
#> Rank1 0.0492 0.3115 0.377 0.2623
#> Rank5 0.7867 0.0800 0.080 0.0533
#> 
#>   Item02 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1148 0.1803 0.3279 0.3770
#> Rank5 0.7067 0.0800 0.0800 0.1333
#> 
#>   Item03 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1311 0.2951 0.2787 0.2951
#> Rank5 0.7333 0.0800 0.1067 0.0800
#> 
#>   Item04 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1311 0.2951 0.2787 0.2951
#> Rank5 0.7467 0.1467 0.0400 0.0667
#> 
#>   Item05 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.0492 0.3115 0.2459 0.3934
#> Rank5 0.6933 0.1067 0.1200 0.0800
#> 
#>   Item06 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1148 0.3934 0.2787 0.2131
#> Rank5 0.8133 0.0667 0.1067 0.0133
#> 
#>   Item07 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1475 0.2295 0.3115 0.3115
#> Rank5 0.7867 0.0800 0.0800 0.0533
#> 
#>   Item08 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1639 0.1148 0.3770 0.3443
#> Rank5 0.0533 0.7867 0.0267 0.1333
#> 
#>   Item09 (CA = Cat2)
#>         Cat1   Cat2  Cat3   Cat4
#> Rank1 0.2459 0.1475 0.377 0.2295
#> Rank5 0.0267 0.9200 0.040 0.0133
#> 
#>   Item10 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.4098 0.0820 0.2131 0.2951
#> Rank5 0.0533 0.8133 0.0533 0.0800
#> 
#>   Item11 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1803 0.2131 0.2951 0.3115
#> Rank5 0.1067 0.7600 0.0933 0.0400
#> 
#>   Item12 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2131 0.1148 0.3279 0.3443
#> Rank5 0.0933 0.8000 0.0400 0.0667
#> 
#>   Item13 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.3934 0.0984 0.1803 0.3279
#> Rank5 0.0000 0.8267 0.1600 0.0133
#> 
#>   Item14 (CA = Cat2)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.3115 0.0820 0.2623 0.3443
#> Rank5 0.0667 0.8533 0.0667 0.0133
#> 
#>   Item15 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2459 0.2459 0.0656 0.4426
#> Rank5 0.0800 0.0400 0.7600 0.1200
#> 
#>   Item16 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.3115 0.3115 0.0984 0.2787
#> Rank5 0.0933 0.0000 0.8933 0.0133
#> 
#>   Item17 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1311 0.4262 0.1311 0.3115
#> Rank5 0.0267 0.0667 0.8133 0.0933
#> 
#>   Item18 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2295 0.2951 0.0328 0.4426
#> Rank5 0.0000 0.0533 0.8533 0.0933
#> 
#>   Item19 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2787 0.3443 0.1148 0.2623
#> Rank5 0.0667 0.0800 0.7333 0.1200
#> 
#>   Item20 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.2295 0.3443 0.1639 0.2623
#> Rank5 0.0533 0.0933 0.8133 0.0400
#> 
#>   Item21 (CA = Cat3)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.3115 0.3443 0.0656 0.2787
#> Rank5 0.0933 0.0267 0.7600 0.1200
plot(da)





















plot(da, items = 1:6, nc = 3, nr = 2)


# Biclustering.rated example
result_bic <- Biclustering(J21S300, ncls = 5, nfld = 3, method = "R")
#> 
#> iter 1 log_lik -8031.89                                                         
#> 
#> iter 2 log_lik -7319.77                                                         
#> 
#> iter 3 log_lik -7120.63                                                         
#> 
#> iter 4 log_lik -7080.17                                                         
#> 
#> iter 5 log_lik -7065.77                                                         
#> 
#> iter 6 log_lik -7058.29                                                         
#> 
#> iter 7 log_lik -7053.48                                                         
#> 
#> iter 8 log_lik -7050.05                                                         
#> 
#> iter 9 log_lik -7047.52                                                         
#> 
#> iter 10 log_lik -7045.6                                                         
#> 
#> iter 11 log_lik -7044.08                                                        
#> 
#> iter 12 log_lik -7042.8                                                         
#> 
#> iter 13 log_lik -7041.64                                                        
#> 
#> iter 14 log_lik -7040.54                                                        
#> 
#> iter 15 log_lik -7039.45                                                        
#> 
#> iter 16 log_lik -7038.38                                                        
#> 
#> iter 17 log_lik -7037.33                                                        
#> 
#> iter 18 log_lik -7036.3                                                         
#> 
#> iter 19 log_lik -7035.32                                                        
#> 
#> iter 20 log_lik -7034.39                                                        
#> 
#> iter 21 log_lik -7033.54                                                        
#> 
#> iter 22 log_lik -7032.76                                                        
#> 
#> iter 23 log_lik -7032.06                                                        
#> 
#> iter 24 log_lik -7031.44                                                        
#> 
#> iter 25 log_lik -7030.88                                                        
#> 
#> iter 26 log_lik -7030.39                                                        
#> 
#> iter 27 log_lik -7029.96                                                        
#> 
#> iter 28 log_lik -7029.58                                                        
#> 
#> iter 29 log_lik -7029.24                                                        
#> 
#> iter 30 log_lik -7028.94                                                        
#> 
#> iter 31 log_lik -7028.68                                                        
#> 
#> iter 32 log_lik -7028.44                                                        
#> 
#> iter 33 log_lik -7028.23                                                        
#> 
#> iter 34 log_lik -7028.03                                                        
#> 
#> iter 35 log_lik -7027.86                                                        
#> 
#> iter 36 log_lik -7027.69                                                        
#> 
#> iter 37 log_lik -7027.54                                                        
#> 
#> iter 38 log_lik -7027.4                                                         
#> 
#> iter 39 log_lik -7027.26                                                        
#> 
#> iter 40 log_lik -7027.12                                                        
#> 
#> iter 41 log_lik -7026.97                                                        
#> 
#> iter 42 log_lik -7026.83                                                        
#> 
#> iter 43 log_lik -7026.68                                                        
#> 
#> iter 44 log_lik -7026.53                                                        
#> 
#> iter 45 log_lik -7026.36                                                        
#> 
#> iter 46 log_lik -7026.19                                                        
#> 
#> iter 47 log_lik -7026.01                                                        
#> 
#> iter 48 log_lik -7025.82                                                        
#> 
#> iter 49 log_lik -7025.61                                                        
#> 
#> iter 50 log_lik -7025.4                                                         
#> 
#> iter 51 log_lik -7025.17                                                        
#> 
#> iter 52 log_lik -7024.93                                                        
#> 
#> iter 53 log_lik -7024.69                                                        
#> 
#> iter 54 log_lik -7024.43                                                        
#> 
#> iter 55 log_lik -7024.16                                                        
#> 
#> iter 56 log_lik -7023.89                                                        
#> 
#> iter 57 log_lik -7023.61                                                        
#> 
#> iter 58 log_lik -7023.32                                                        
#> 
#> iter 59 log_lik -7023.04                                                        
#> 
#> iter 60 log_lik -7022.74                                                        
#> 
#> iter 61 log_lik -7022.45                                                        
#> 
#> iter 62 log_lik -7022.16                                                        
#> 
#> iter 63 log_lik -7021.87                                                        
#> 
#> iter 64 log_lik -7021.58                                                        
#> 
#> iter 65 log_lik -7021.3                                                         
#> 
#> iter 66 log_lik -7021.02                                                        
#> 
#> iter 67 log_lik -7020.74                                                        
#> 
#> iter 68 log_lik -7020.48                                                        
#> 
#> iter 69 log_lik -7020.21                                                        
#> 
#> iter 70 log_lik -7019.96                                                        
#> 
#> iter 71 log_lik -7019.71                                                        
#> 
#> iter 72 log_lik -7019.47                                                        
#> 
#> iter 73 log_lik -7019.24                                                        
#> 
#> iter 74 log_lik -7019.02                                                        
#> 
#> iter 75 log_lik -7018.8                                                         
#> 
#> iter 76 log_lik -7018.59                                                        
#> 
#> iter 77 log_lik -7018.39                                                        
#> 
#> iter 78 log_lik -7018.19                                                        
#> 
#> iter 79 log_lik -7018.01                                                        
#> 
#> iter 80 log_lik -7017.83                                                        
#> 
#> iter 81 log_lik -7017.65                                                        
#> 
#> iter 82 log_lik -7017.49                                                        
#> 
#> iter 83 log_lik -7017.33                                                        
#> 
#> iter 84 log_lik -7017.18                                                        
#> 
#> iter 85 log_lik -7017.03                                                        
#> 
#> iter 86 log_lik -7016.89                                                        
#> 
#> iter 87 log_lik -7016.75                                                        
#> 
#> iter 88 log_lik -7016.62                                                        
#> 
#> iter 89 log_lik -7016.5                                                         
#> 
#> iter 90 log_lik -7016.38                                                        
#> 
#> iter 91 log_lik -7016.27                                                        
#> 
#> iter 92 log_lik -7016.16                                                        
#> 
#> iter 93 log_lik -7016.06                                                        
#> 
#> iter 94 log_lik -7015.96                                                        
#> 
#> iter 95 log_lik -7015.86                                                        
#> 
#> iter 96 log_lik -7015.77                                                        
#> 
#> iter 97 log_lik -7015.68                                                        
#> 
#> iter 98 log_lik -7015.6                                                         
#> 
#> iter 99 log_lik -7015.52                                                        
#> 
#> iter 100 log_lik -7015.44                                                       
#> 
#> Reached the maximum number of iterations (100).
#> Warning: Algorithm may not have converged. Interpret results with caution.
#> 
#> iter 101 log_lik -7015.36                                                       
#> 
#> Weakly ordinal alignment condition was satisfied.
da_bic <- DistractorAnalysis(result_bic)
print(da_bic, items = 1:7)
#> Distractor Analysis
#> 
#> Field Grouping:
#>   Field 1: Item01, Item02, Item03, Item04, Item05, Item06, Item07
#>   Field 2: Item08, Item09, Item10, Item11, Item12, Item13, Item14
#>   Field 3: Item15, Item16, Item17, Item18, Item19, Item20, Item21
#> 
#> 
#> --- Field 1 ---
#> 
#> Cramer's V
#>         Rank1  Rank2  Rank3  Rank4  Rank5
#> Item01 0.2316 0.2401 0.2151 0.6824 0.5129
#> Item02 0.2199 0.3004 0.0599 0.5089 0.6470
#> Item03 0.2075 0.1273 0.2403 0.6255 0.4946
#> Item04 0.2222 0.1164 0.1775 0.4938 0.6437
#> Item05 0.2706 0.2437 0.1293 0.4055 0.5550
#> Item06 0.2010 0.1663 0.1325 0.5752 0.6824
#> Item07 0.1665 0.2149 0.2151 0.5697 0.7015
#> 
#> P-values
#>           Rank1    Rank2   Rank3     Rank4     Rank5
#> Item01 0.009647 0.023230 0.04786 7.219e-14 4.115e-12
#> Item02 0.016210 0.001912 0.89320 8.502e-08 3.304e-19
#> Item03 0.027130 0.444900 0.01964 1.122e-11 2.854e-11
#> Item04 0.014620 0.524800 0.14560 2.346e-07 5.160e-19
#> Item05 0.001375 0.020340 0.41380 4.673e-05 3.696e-14
#> Item06 0.035020 0.206700 0.39160 6.724e-10 2.313e-21
#> Item07 0.116500 0.054600 0.04786 1.029e-09 1.421e-22
#> 
#> Category Proportions by Rank 
#> 
#>   Item01 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.0986 0.2535 0.3803 0.2676
#> Rank2 0.4182 0.2545 0.1636 0.1636
#> Rank3 0.3860 0.1404 0.1930 0.2807
#> Rank4 0.7609 0.0652 0.1087 0.0652
#> Rank5 0.6338 0.1408 0.1268 0.0986
#> 
#>   Item02 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1127 0.2113 0.3521 0.3239
#> Rank2 0.4727 0.2000 0.1818 0.1455
#> Rank3 0.2456 0.2632 0.2807 0.2105
#> Rank4 0.6304 0.1522 0.1087 0.1087
#> Rank5 0.7324 0.0423 0.0986 0.1268
#> 
#>   Item03 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.0986 0.3239 0.3099 0.2676
#> Rank2 0.3273 0.2727 0.1818 0.2182
#> Rank3 0.3158 0.1579 0.1404 0.3860
#> Rank4 0.7174 0.1304 0.0870 0.0652
#> Rank5 0.6197 0.1408 0.0986 0.1408
#> 
#>   Item04 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.0845 0.3239 0.2958 0.2958
#> Rank2 0.2727 0.2909 0.1636 0.2727
#> Rank3 0.2281 0.1404 0.3509 0.2807
#> Rank4 0.6087 0.2174 0.0870 0.0870
#> Rank5 0.7324 0.0986 0.0704 0.0986
#> 
#>   Item05 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.0563 0.2676 0.3099 0.3662
#> Rank2 0.4182 0.2182 0.2364 0.1273
#> Rank3 0.2456 0.3333 0.1754 0.2456
#> Rank4 0.5435 0.0870 0.2174 0.1522
#> Rank5 0.6620 0.1690 0.0845 0.0845
#> 
#>   Item06 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1127 0.3380 0.3099 0.2394
#> Rank2 0.3455 0.2727 0.1455 0.2364
#> Rank3 0.1754 0.2281 0.2632 0.3333
#> Rank4 0.6739 0.1087 0.1739 0.0435
#> Rank5 0.7606 0.0563 0.1127 0.0704
#> 
#>   Item07 (CA = Cat1)
#>         Cat1   Cat2   Cat3   Cat4
#> Rank1 0.1268 0.2817 0.2817 0.3099
#> Rank2 0.3636 0.2909 0.1091 0.2364
#> Rank3 0.1754 0.3509 0.3333 0.1404
#> Rank4 0.6739 0.1087 0.0652 0.1522
#> Rank5 0.7746 0.0563 0.1127 0.0563
plot(da_bic, items = 1:6, nc = 3, nr = 2)

# }
```
