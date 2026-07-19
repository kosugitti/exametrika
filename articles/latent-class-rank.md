# Latent Class and Rank Analysis

> **Note**: Some computationally intensive examples below are shown with
> `eval=FALSE` to keep CRAN build times short. For full rendered output,
> see the [pkgdown
> site](https://kosugitti.github.io/exametrika/articles/latent-class-rank.html).

``` r

library(exametrika)
```

## Latent Class Analysis (LCA)

LCA classifies examinees into unordered latent classes. Specify the
dataset and the number of classes.

``` r

LCA(J15S500, ncls = 5)
#> 
#> Item Reference Profile
#>          IRP1   IRP2    IRP3  IRP4  IRP5
#> Item01 0.5185 0.6996 0.76358 0.856 0.860
#> Item02 0.5529 0.6276 0.81161 0.888 0.855
#> Item03 0.7959 0.3205 0.93735 0.706 0.849
#> Item04 0.5069 0.5814 0.86940 0.873 1.000
#> Item05 0.6154 0.7523 0.94673 0.789 0.886
#> Item06 0.6840 0.7501 0.94822 1.000 0.907
#> Item07 0.4832 0.4395 0.83377 0.874 0.900
#> Item08 0.3767 0.3982 0.62563 0.912 0.590
#> Item09 0.3107 0.3980 0.26616 0.165 0.673
#> Item10 0.5290 0.5341 0.76134 0.677 0.781
#> Item11 0.1007 0.0497 0.00132 0.621 0.623
#> Item12 0.0355 0.1673 0.15911 0.296 0.673
#> Item13 0.2048 0.5490 0.89445 0.672 0.784
#> Item14 0.3508 0.7384 0.77159 0.904 1.000
#> Item15 0.3883 0.6077 0.82517 0.838 0.823
#> 
#> Test Profile
#>                               Class 1 Class 2 Class 3 Class 4 Class 5
#> Test Reference Profile          6.453   7.613  10.415  11.072  12.205
#> Latent Class Ditribution       87.000  97.000 125.000  91.000 100.000
#> Class Membership Distribution  90.372  97.105 105.238 102.800 104.484
#> 
#> Item Fit Indices
#>        model_log_like bench_log_like null_log_like model_Chi_sq null_Chi_sq
#> Item01       -264.179       -240.190      -283.343       47.978      86.307
#> Item02       -256.363       -235.436      -278.949       41.853      87.025
#> Item03       -237.888       -260.906      -293.598      -46.037      65.383
#> Item04       -208.536       -192.072      -265.962       32.928     147.780
#> Item05       -226.447       -206.537      -247.403       39.819      81.732
#> Item06       -164.762       -153.940      -198.817       21.644      89.755
#> Item07       -249.377       -228.379      -298.345       41.997     139.933
#> Item08       -295.967       -293.225      -338.789        5.483      91.127
#> Item09       -294.250       -300.492      -327.842      -12.484      54.700
#> Item10       -306.985       -288.198      -319.850       37.574      63.303
#> Item11       -187.202       -224.085      -299.265      -73.767     150.360
#> Item12       -232.307       -214.797      -293.598       35.020     157.603
#> Item13       -267.647       -262.031      -328.396       11.232     132.730
#> Item14       -203.468       -204.953      -273.212       -2.969     136.519
#> Item15       -268.616       -254.764      -302.847       27.705      96.166
#>        model_df null_df   NFI   RFI   IFI   TLI   CFI RMSEA     AIC     CAIC
#> Item01        9      13 0.444 0.197 0.496 0.232 0.468 0.093  29.978  -16.954
#> Item02        9      13 0.519 0.305 0.579 0.359 0.556 0.086  23.853  -23.079
#> Item03        9      13 1.000 1.000 1.000 1.000 1.000 0.000 -64.037 -110.969
#> Item04        9      13 0.777 0.678 0.828 0.744 0.822 0.073  14.928  -32.004
#> Item05        9      13 0.513 0.296 0.576 0.352 0.552 0.083  21.819  -25.112
#> Item06        9      13 0.759 0.652 0.843 0.762 0.835 0.053   3.644  -43.287
#> Item07        9      13 0.700 0.566 0.748 0.625 0.740 0.086  23.997  -22.934
#> Item08        9      13 0.940 0.913 1.000 1.000 1.000 0.000 -12.517  -59.448
#> Item09        9      13 1.000 1.000 1.000 1.000 1.000 0.000 -30.484  -77.415
#> Item10        9      13 0.406 0.143 0.474 0.179 0.432 0.080  19.574  -27.357
#> Item11        9      13 1.000 1.000 1.000 1.000 1.000 0.000 -91.767 -138.698
#> Item12        9      13 0.778 0.679 0.825 0.740 0.820 0.076  17.020  -29.912
#> Item13        9      13 0.915 0.878 0.982 0.973 0.981 0.022  -6.768  -53.699
#> Item14        9      13 1.000 1.000 1.000 1.000 1.000 0.000 -20.969  -67.901
#> Item15        9      13 0.712 0.584 0.785 0.675 0.775 0.065   9.705  -37.226
#>             BIC
#> Item01   -7.954
#> Item02  -14.079
#> Item03 -101.969
#> Item04  -23.004
#> Item05  -16.112
#> Item06  -34.287
#> Item07  -13.934
#> Item08  -50.448
#> Item09  -68.415
#> Item10  -18.357
#> Item11 -129.698
#> Item12  -20.912
#> Item13  -44.699
#> Item14  -58.901
#> Item15  -28.226
#> 
#> Model Fit Indices
#> Number of Latent class: 5
#> Number of EM cycle: 73 
#>                    value
#> model_log_like -3663.994
#> bench_log_like -3560.005
#> null_log_like  -4350.217
#> model_Chi_sq     207.977
#> null_Chi_sq     1580.424
#> model_df         135.000
#> null_df          195.000
#> NFI                0.868
#> RFI                0.810
#> IFI                0.950
#> TLI                0.924
#> CFI                0.947
#> RMSEA              0.033
#> AIC              -62.023
#> CAIC            -765.995
#> BIC             -630.995
```

The Class Membership Matrix indicates which latent class each examinee
belongs to:

``` r

result.LCA <- LCA(J15S500, ncls = 5)
head(result.LCA$Students)
#>            Membership 1 Membership 2 Membership 3 Membership 4 Membership 5
#> Student001 0.7839477684  0.171152798  0.004141844 4.075759e-02 3.744590e-12
#> Student002 0.0347378747  0.051502214  0.836022799 7.773694e-02 1.698776e-07
#> Student003 0.0146307878  0.105488644  0.801853496 3.343026e-02 4.459682e-02
#> Student004 0.0017251650  0.023436459  0.329648386 3.656488e-01 2.795412e-01
#> Student005 0.2133830569  0.784162066  0.001484616 2.492073e-08 9.702355e-04
#> Student006 0.0003846482  0.001141448  0.001288901 8.733869e-01 1.237981e-01
#>            Estimate
#> Student001        1
#> Student002        3
#> Student003        3
#> Student004        4
#> Student005        2
#> Student006        4
```

### LCA Plot Types

- **IRP**: Item Reference Profile
- **CMP**: Class Membership Profile
- **TRP**: Test Reference Profile
- **LCD**: Latent Class Distribution

``` r

plot(result.LCA, type = "IRP", items = 1:6, nc = 2, nr = 3)
```

![](latent-class-rank_files/figure-html/plot-lca-1.png)

``` r

plot(result.LCA, type = "CMP", students = 1:9, nc = 3, nr = 3)
```

![](latent-class-rank_files/figure-html/plot-lca-2.png)

``` r

plot(result.LCA, type = "TRP")
```

![](latent-class-rank_files/figure-html/plot-lca-3.png)

``` r

plot(result.LCA, type = "LCD")
```

![](latent-class-rank_files/figure-html/plot-lca-4.png)

## Latent Rank Analysis (LRA)

LRA is similar to LCA but assumes an ordering among the latent classes
(ranks). Specify the dataset and the number of ranks.

``` r

LRA(J15S500, nrank = 6)
#> estimating method is  isotonic 
#> Item Reference Profile
#>          IRP1   IRP2   IRP3   IRP4  IRP5  IRP6
#> Item01 0.4580 0.7488 0.7488 0.7488 0.839 0.914
#> Item02 0.5603 0.5603 0.8047 0.8047 0.883 0.883
#> Item03 0.5998 0.5998 0.7589 0.7589 0.759 0.866
#> Item04 0.4671 0.4671 0.8950 0.8950 0.895 0.995
#> Item05 0.5590 0.8286 0.8286 0.8286 0.829 0.936
#> Item06 0.6194 0.7695 0.9417 0.9417 0.942 0.942
#> Item07 0.4113 0.4113 0.7427 0.8953 0.895 0.895
#> Item08 0.3483 0.3483 0.6037 0.7309 0.731 0.731
#> Item09 0.3148 0.3148 0.3148 0.3148 0.315 0.620
#> Item10 0.4448 0.6333 0.6935 0.6935 0.725 0.764
#> Item11 0.0819 0.0819 0.0819 0.0966 0.681 0.681
#> Item12 0.0654 0.0654 0.2184 0.2184 0.218 0.862
#> Item13 0.2218 0.4922 0.7525 0.7525 0.752 0.788
#> Item14 0.2919 0.7735 0.7735 0.7735 0.938 1.000
#> Item15 0.3816 0.5214 0.8160 0.8160 0.816 0.845
#> 
#> Item Reference Profile Indices
#>        Alpha     A Beta     B Gamma C
#> Item01     1 0.291    1 0.458     0 0
#> Item02     2 0.244    1 0.560     0 0
#> Item03     2 0.159    1 0.600     0 0
#> Item04     2 0.428    1 0.467     0 0
#> Item05     1 0.270    1 0.559     0 0
#> Item06     2 0.172    1 0.619     0 0
#> Item07     2 0.331    1 0.411     0 0
#> Item08     2 0.255    3 0.604     0 0
#> Item09     5 0.305    6 0.620     0 0
#> Item10     1 0.188    1 0.445     0 0
#> Item11     4 0.584    5 0.681     0 0
#> Item12     5 0.644    3 0.218     0 0
#> Item13     1 0.270    2 0.492     0 0
#> Item14     1 0.482    1 0.292     0 0
#> Item15     2 0.295    2 0.521     0 0
#> 
#> Test Profile
#>                              Rank 1 Rank 2 Rank 3  Rank 4 Rank 5 Rank 6
#> Test Reference Profile        5.825  7.616  9.975  10.269 11.218 12.722
#> Latent Rank Ditribution      75.000 82.000 71.000 105.000 74.000 93.000
#> Rank Membership Distribution 77.737 80.091 86.925  87.038 87.526 80.684
#> 
#> Item Fit Indices
#>        model_log_like bench_log_like null_log_like model_Chi_sq null_Chi_sq
#> Item01       -259.106       -240.190      -283.343       37.833      86.307
#> Item02       -254.739       -235.436      -278.949       38.606      87.025
#> Item03       -282.404       -260.906      -293.598       42.996      65.383
#> Item04       -199.646       -192.072      -265.962       15.148     147.780
#> Item05       -229.021       -206.537      -247.403       44.967      81.732
#> Item06       -170.951       -153.940      -198.817       34.022      89.755
#> Item07       -242.039       -228.379      -298.345       27.320     139.933
#> Item08       -309.029       -293.225      -338.789       31.607      91.127
#> Item09       -314.761       -300.492      -327.842       28.538      54.700
#> Item10       -308.861       -288.198      -319.850       41.326      63.303
#> Item11       -202.284       -224.085      -299.265      -43.602     150.360
#> Item12       -207.715       -214.797      -293.598      -14.163     157.603
#> Item13       -284.652       -262.031      -328.396       45.242     132.730
#> Item14       -203.162       -204.953      -273.212       -3.582     136.519
#> Item15       -266.721       -254.764      -302.847       23.916      96.166
#>        model_df null_df   NFI   RFI   IFI   TLI   CFI RMSEA     AIC     CAIC
#> Item01       10      13 0.562 0.430 0.635 0.506 0.620 0.075  17.833  -34.313
#> Item02       11      13 0.556 0.476 0.637 0.559 0.627 0.071  16.606  -40.755
#> Item03       11      13 0.342 0.223 0.412 0.278 0.389 0.076  20.996  -36.365
#> Item04       11      13 0.897 0.879 0.970 0.964 0.969 0.027  -6.852  -64.212
#> Item05       11      13 0.450 0.350 0.520 0.416 0.506 0.079  22.967  -34.394
#> Item06       11      13 0.621 0.552 0.708 0.646 0.700 0.065  12.022  -45.339
#> Item07       11      13 0.805 0.769 0.873 0.848 0.871 0.055   5.320  -52.041
#> Item08       11      13 0.653 0.590 0.743 0.688 0.736 0.061   9.607  -47.754
#> Item09       12      13 0.478 0.435 0.613 0.570 0.603 0.053   4.538  -58.037
#> Item10        9      13 0.347 0.057 0.405 0.072 0.357 0.085  23.326  -23.606
#> Item11       11      13 1.000 1.000 1.000 1.000 1.000 0.000 -65.602 -122.963
#> Item12       11      13 1.000 1.000 1.000 1.000 1.000 0.000 -36.163  -93.524
#> Item13       10      13 0.659 0.557 0.713 0.617 0.706 0.084  25.242  -26.904
#> Item14       10      13 1.000 1.000 1.000 1.000 1.000 0.000 -23.582  -75.728
#> Item15       10      13 0.751 0.677 0.839 0.782 0.833 0.053   3.916  -48.230
#>             BIC
#> Item01  -24.313
#> Item02  -29.755
#> Item03  -25.365
#> Item04  -53.212
#> Item05  -23.394
#> Item06  -34.339
#> Item07  -41.041
#> Item08  -36.754
#> Item09  -46.037
#> Item10  -14.606
#> Item11 -111.963
#> Item12  -82.524
#> Item13  -16.904
#> Item14  -65.728
#> Item15  -38.230
#> 
#> Model Fit Indices
#> Number of Latent rank: 6
#> Number of EM cycle: 47 
#>                    value
#> model_log_like -3735.091
#> bench_log_like -3560.005
#> null_log_like  -4350.217
#> model_Chi_sq     350.173
#> null_Chi_sq     1580.424
#> model_df         160.000
#> null_df          195.000
#> NFI                0.778
#> RFI                0.730
#> IFI                0.866
#> TLI                0.833
#> CFI                0.863
#> RMSEA              0.049
#> AIC               30.173
#> CAIC            -804.165
#> BIC             -644.165
```

Rank membership probabilities and rank-up/rank-down odds are calculated:

``` r

result.LRA <- LRA(J15S500, nrank = 6)
head(result.LRA$Students)
#>            Membership 1 Membership 2 Membership 3 Membership 4 Membership 5
#> Student001 0.3677662894  0.434091572   0.11058891  0.036872666   0.05068055
#> Student002 0.0197536734  0.082316601   0.58176142  0.281540126   0.03462778
#> Student003 0.0062820540  0.216416748   0.53609302  0.145636332   0.08059171
#> Student004 0.0010014078  0.009064826   0.19851135  0.284268528   0.13509623
#> Student005 0.2584603668  0.721793236   0.01379897  0.003748661   0.00207442
#> Student006 0.0001302681  0.002082945   0.04646690  0.081667731   0.75534209
#>            Membership 6 Estimate Rank-Up Odds Rank-Down Odds
#> Student001 1.359014e-08        2   0.25475940      0.8472090
#> Student002 4.013907e-07        3   0.48394431      0.1414955
#> Student003 1.498014e-02        3   0.27166243      0.4036925
#> Student004 3.720577e-01        6           NA      0.3631056
#> Student005 1.243501e-04        2   0.01911762      0.3580809
#> Student006 1.143101e-01        5   0.15133549      0.1081202
```

``` r

plot(result.LRA, type = "IRP", items = 1:6, nc = 2, nr = 3)
```

![](latent-class-rank_files/figure-html/plot-lra-1.png)

``` r

plot(result.LRA, type = "RMP", students = 1:9, nc = 3, nr = 3)
```

![](latent-class-rank_files/figure-html/plot-lra-2.png)

``` r

plot(result.LRA, type = "TRP")
```

![](latent-class-rank_files/figure-html/plot-lra-3.png)

``` r

plot(result.LRA, type = "LRD")
```

![](latent-class-rank_files/figure-html/plot-lra-4.png)

## LRA for Ordinal Data

LRA can also handle ordinal scale data. The `mic` option enforces
monotonic increasing constraints.

``` r

result.LRAord <- LRA(J15S3810, nrank = 3, mic = TRUE)
```

Score-rank relationship visualizations:

``` r

plot(result.LRAord, type = "ScoreFreq")
plot(result.LRAord, type = "ScoreRank")
```

Item-rank relationship plots:

- **ICBR**: Item Category Boundary Reference – cumulative probability
  curves for each category threshold
- **ICRP**: Item Category Response Profile – probability of each
  response category across ranks

``` r

plot(result.LRAord, type = "ICBR", items = 1:4, nc = 2, nr = 2)
plot(result.LRAord, type = "ICRP", items = 1:4, nc = 2, nr = 2)
```

Rank membership profiles for individual examinees:

``` r

plot(result.LRAord, type = "RMP", students = 1:9, nc = 3, nr = 3)
```

## LRA for Rated/Nominal Data

For multiple-choice tests (nominal scale), LRA can analyze response
patterns including distractor choices.

``` r

result.LRArated <- LRA(J35S5000, nrank = 10, mic = TRUE)
```

``` r

plot(result.LRArated, type = "ScoreFreq")
plot(result.LRArated, type = "ScoreRank")
```

``` r

plot(result.LRArated, type = "ICRP", items = 1:4, nc = 2, nr = 2)
```

## Reference

Shojima, K. (2022). *Test Data Engineering*. Springer.
