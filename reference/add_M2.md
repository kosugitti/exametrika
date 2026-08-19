# Attach the margin-based fit indices to a fitted model

Computes [`M2`](https://kosugitti.github.io/exametrika/reference/M2.md)
for the model and for the independence baseline, and returns the fitted
object with a `TestFitIndicesM2` component added. The print method then
shows the response-pattern indices and the margin-based ones side by
side.

This is a separate step rather than part of the fit because it is
expensive: the cost is the Cholesky factorisation of a dense matrix
whose size grows with the square of the item count (see
[`M2`](https://kosugitti.github.io/exametrika/reference/M2.md)).

## Usage

``` r
add_M2(x, ...)

# Default S3 method
add_M2(x, ...)

# S3 method for class 'nominalLCA'
add_M2(x, verbose = TRUE, gc = TRUE, ...)

# S3 method for class 'ratedLCA'
add_M2(x, verbose = TRUE, gc = TRUE, ...)

# S3 method for class 'LRAordinal'
add_M2(x, verbose = TRUE, gc = TRUE, ...)

# S3 method for class 'ordinalBiclustering'
add_M2(x, verbose = TRUE, gc = TRUE, ...)

# S3 method for class 'nominalBiclustering'
add_M2(x, verbose = TRUE, gc = TRUE, ...)
```

## Arguments

- x:

  A fitted model object of class "exametrika".

- ...:

  Additional arguments passed to methods.

- verbose:

  Logical; if TRUE (default), reports the size of the margin covariance
  matrix before computing it when that matrix is large.

- gc:

  Logical; if TRUE (default), releases the workspace before returning.
  See [`M2`](https://kosugitti.github.io/exametrika/reference/M2.md).

## Value

The fitted object with `TestFitIndicesM2` added.

## Examples

``` r
# \donttest{
dat <- dataFormat(J20S600, response.type = "nominal")
fit <- LCA(dat, ncls = 3)
fit <- add_M2(fit)
fit
#> 
#> Item Category Reference Profile
#>         ItemLabel CategoryLabel class1 class2 class3
#> Item011    Item01   Item01-Cat1 0.1224 0.6185 0.1744
#> Item012    Item01   Item01-Cat2 0.3765 0.1225 0.1204
#> Item013    Item01   Item01-Cat3 0.3737 0.1219 0.0786
#> Item014    Item01   Item01-Cat4 0.1274 0.1370 0.6266
#> Item021    Item02   Item02-Cat1 0.3800 0.0994 0.1650
#> Item022    Item02   Item02-Cat2 0.3115 0.1956 0.1918
#> Item023    Item02   Item02-Cat3 0.1514 0.1314 0.5053
#> Item024    Item02   Item02-Cat4 0.1571 0.5735 0.1379
#> Item031    Item03   Item03-Cat1 0.3381 0.1156 0.2032
#> Item032    Item03   Item03-Cat2 0.4092 0.1642 0.1931
#> Item033    Item03   Item03-Cat3 0.1237 0.1657 0.4735
#> Item034    Item03   Item03-Cat4 0.1290 0.5545 0.1301
#> Item041    Item04   Item04-Cat1 0.1211 0.5065 0.1716
#> Item042    Item04   Item04-Cat2 0.3318 0.1673 0.1799
#> Item043    Item04   Item04-Cat3 0.4487 0.1551 0.0992
#> Item044    Item04   Item04-Cat4 0.0984 0.1712 0.5493
#> Item051    Item05   Item05-Cat1 0.1855 0.5108 0.1682
#> Item052    Item05   Item05-Cat2 0.3542 0.1648 0.1402
#> Item053    Item05   Item05-Cat3 0.3267 0.1813 0.1427
#> Item054    Item05   Item05-Cat4 0.1336 0.1431 0.5489
#> Item061    Item06   Item06-Cat1 0.1442 0.1599 0.5766
#> Item062    Item06   Item06-Cat2 0.1398 0.5091 0.1746
#> Item063    Item06   Item06-Cat3 0.3741 0.1977 0.0958
#> Item064    Item06   Item06-Cat4 0.3418 0.1333 0.1530
#> Item071    Item07   Item07-Cat1 0.3482 0.1560 0.1006
#> Item072    Item07   Item07-Cat2 0.1424 0.1476 0.5518
#> Item073    Item07   Item07-Cat3 0.1064 0.5515 0.1658
#> Item074    Item07   Item07-Cat4 0.4031 0.1448 0.1818
#> Item081    Item08   Item08-Cat1 0.3454 0.1716 0.0981
#> Item082    Item08   Item08-Cat2 0.1343 0.1639 0.4648
#> Item083    Item08   Item08-Cat3 0.1596 0.4881 0.1843
#> Item084    Item08   Item08-Cat4 0.3607 0.1764 0.2528
#> Item091    Item09   Item09-Cat1 0.1373 0.1568 0.4843
#> Item092    Item09   Item09-Cat2 0.1245 0.5271 0.2100
#> Item093    Item09   Item09-Cat3 0.3670 0.1285 0.1661
#> Item094    Item09   Item09-Cat4 0.3712 0.1876 0.1395
#> Item101    Item10   Item10-Cat1 0.1627 0.5731 0.1487
#> Item102    Item10   Item10-Cat2 0.3565 0.1580 0.1603
#> Item103    Item10   Item10-Cat3 0.3442 0.1478 0.1140
#> Item104    Item10   Item10-Cat4 0.1366 0.1211 0.5770
#> Item111    Item11   Item11-Cat1 0.3999 0.1367 0.1177
#> Item112    Item11   Item11-Cat2 0.3330 0.1603 0.1231
#> Item113    Item11   Item11-Cat3 0.1382 0.1400 0.6270
#> Item114    Item11   Item11-Cat4 0.1289 0.5630 0.1322
#> Item121    Item12   Item12-Cat1 0.1297 0.5605 0.1345
#> Item122    Item12   Item12-Cat2 0.4116 0.1751 0.1864
#> Item123    Item12   Item12-Cat3 0.3166 0.1343 0.1696
#> Item124    Item12   Item12-Cat4 0.1422 0.1301 0.5095
#> Item131    Item13   Item13-Cat1 0.1559 0.1232 0.4688
#> Item132    Item13   Item13-Cat2 0.1218 0.5657 0.1553
#> Item133    Item13   Item13-Cat3 0.3668 0.1443 0.2327
#> Item134    Item13   Item13-Cat4 0.3556 0.1667 0.1432
#> Item141    Item14   Item14-Cat1 0.3561 0.1350 0.1559
#> Item142    Item14   Item14-Cat2 0.1076 0.1307 0.5308
#> Item143    Item14   Item14-Cat3 0.1619 0.6057 0.1222
#> Item144    Item14   Item14-Cat4 0.3744 0.1287 0.1910
#> Item151    Item15   Item15-Cat1 0.1999 0.1604 0.5822
#> Item152    Item15   Item15-Cat2 0.1571 0.5870 0.1307
#> Item153    Item15   Item15-Cat3 0.3352 0.1222 0.1220
#> Item154    Item15   Item15-Cat4 0.3078 0.1304 0.1651
#> Item161    Item16   Item16-Cat1 0.3323 0.1454 0.1439
#> Item162    Item16   Item16-Cat2 0.3863 0.1693 0.1669
#> Item163    Item16   Item16-Cat3 0.1282 0.1440 0.5236
#> Item164    Item16   Item16-Cat4 0.1532 0.5413 0.1657
#> Item171    Item17   Item17-Cat1 0.1177 0.2133 0.5100
#> Item172    Item17   Item17-Cat2 0.1881 0.4860 0.1663
#> Item173    Item17   Item17-Cat3 0.3380 0.1469 0.1978
#> Item174    Item17   Item17-Cat4 0.3563 0.1538 0.1259
#> Item181    Item18   Item18-Cat1 0.3063 0.1712 0.1672
#> Item182    Item18   Item18-Cat2 0.3246 0.1632 0.1004
#> Item183    Item18   Item18-Cat3 0.1867 0.1599 0.6146
#> Item184    Item18   Item18-Cat4 0.1823 0.5058 0.1178
#> Item191    Item19   Item19-Cat1 0.3312 0.1882 0.2026
#> Item192    Item19   Item19-Cat2 0.1461 0.1679 0.5295
#> Item193    Item19   Item19-Cat3 0.1644 0.5205 0.0943
#> Item194    Item19   Item19-Cat4 0.3583 0.1234 0.1737
#> Item201    Item20   Item20-Cat1 0.3515 0.1609 0.1799
#> Item202    Item20   Item20-Cat2 0.1452 0.1652 0.5222
#> Item203    Item20   Item20-Cat3 0.0903 0.4681 0.1353
#> Item204    Item20   Item20-Cat4 0.4129 0.2057 0.1626
#> 
#> Test Profile
#>                               Class 1 Class 2 Class 3
#> Latent Class Ditribution      233.000 245.000 122.000
#> Class Membership Distribution 231.324 244.059 124.617
#> 
#> Model Fit Indices
#> Number of Latent class: 3
#> Number of EM cycle: 16 
#> 
#> Response-pattern based
#>                    value
#> model_log_like -15223.56
#> bench_log_like        NA
#> null_log_like  -16424.04
#> model_Chi_sq          NA
#> null_Chi_sq           NA
#> model_df              NA
#> null_df               NA
#> NFI                   NA
#> RFI                   NA
#> IFI                   NA
#> TLI                   NA
#> CFI                   NA
#> RMSEA                 NA
#> AIC             30807.12
#> CAIC            31778.56
#> BIC             31598.56
#> 
#> Margin based (M2)
#>             value
#> M2       3510.944
#> df       1591.000
#> p           0.000
#> M2_null  9282.443
#> df_null  1710.000
#> n_margin 1770.000
#> NFI         0.622
#> RFI         0.593
#> IFI         0.750
#> TLI         0.727
#> CFI         0.746
#> RMSEA       0.045
print(fit, fit_indices = "margin")
#> 
#> Item Category Reference Profile
#>         ItemLabel CategoryLabel class1 class2 class3
#> Item011    Item01   Item01-Cat1 0.1224 0.6185 0.1744
#> Item012    Item01   Item01-Cat2 0.3765 0.1225 0.1204
#> Item013    Item01   Item01-Cat3 0.3737 0.1219 0.0786
#> Item014    Item01   Item01-Cat4 0.1274 0.1370 0.6266
#> Item021    Item02   Item02-Cat1 0.3800 0.0994 0.1650
#> Item022    Item02   Item02-Cat2 0.3115 0.1956 0.1918
#> Item023    Item02   Item02-Cat3 0.1514 0.1314 0.5053
#> Item024    Item02   Item02-Cat4 0.1571 0.5735 0.1379
#> Item031    Item03   Item03-Cat1 0.3381 0.1156 0.2032
#> Item032    Item03   Item03-Cat2 0.4092 0.1642 0.1931
#> Item033    Item03   Item03-Cat3 0.1237 0.1657 0.4735
#> Item034    Item03   Item03-Cat4 0.1290 0.5545 0.1301
#> Item041    Item04   Item04-Cat1 0.1211 0.5065 0.1716
#> Item042    Item04   Item04-Cat2 0.3318 0.1673 0.1799
#> Item043    Item04   Item04-Cat3 0.4487 0.1551 0.0992
#> Item044    Item04   Item04-Cat4 0.0984 0.1712 0.5493
#> Item051    Item05   Item05-Cat1 0.1855 0.5108 0.1682
#> Item052    Item05   Item05-Cat2 0.3542 0.1648 0.1402
#> Item053    Item05   Item05-Cat3 0.3267 0.1813 0.1427
#> Item054    Item05   Item05-Cat4 0.1336 0.1431 0.5489
#> Item061    Item06   Item06-Cat1 0.1442 0.1599 0.5766
#> Item062    Item06   Item06-Cat2 0.1398 0.5091 0.1746
#> Item063    Item06   Item06-Cat3 0.3741 0.1977 0.0958
#> Item064    Item06   Item06-Cat4 0.3418 0.1333 0.1530
#> Item071    Item07   Item07-Cat1 0.3482 0.1560 0.1006
#> Item072    Item07   Item07-Cat2 0.1424 0.1476 0.5518
#> Item073    Item07   Item07-Cat3 0.1064 0.5515 0.1658
#> Item074    Item07   Item07-Cat4 0.4031 0.1448 0.1818
#> Item081    Item08   Item08-Cat1 0.3454 0.1716 0.0981
#> Item082    Item08   Item08-Cat2 0.1343 0.1639 0.4648
#> Item083    Item08   Item08-Cat3 0.1596 0.4881 0.1843
#> Item084    Item08   Item08-Cat4 0.3607 0.1764 0.2528
#> Item091    Item09   Item09-Cat1 0.1373 0.1568 0.4843
#> Item092    Item09   Item09-Cat2 0.1245 0.5271 0.2100
#> Item093    Item09   Item09-Cat3 0.3670 0.1285 0.1661
#> Item094    Item09   Item09-Cat4 0.3712 0.1876 0.1395
#> Item101    Item10   Item10-Cat1 0.1627 0.5731 0.1487
#> Item102    Item10   Item10-Cat2 0.3565 0.1580 0.1603
#> Item103    Item10   Item10-Cat3 0.3442 0.1478 0.1140
#> Item104    Item10   Item10-Cat4 0.1366 0.1211 0.5770
#> Item111    Item11   Item11-Cat1 0.3999 0.1367 0.1177
#> Item112    Item11   Item11-Cat2 0.3330 0.1603 0.1231
#> Item113    Item11   Item11-Cat3 0.1382 0.1400 0.6270
#> Item114    Item11   Item11-Cat4 0.1289 0.5630 0.1322
#> Item121    Item12   Item12-Cat1 0.1297 0.5605 0.1345
#> Item122    Item12   Item12-Cat2 0.4116 0.1751 0.1864
#> Item123    Item12   Item12-Cat3 0.3166 0.1343 0.1696
#> Item124    Item12   Item12-Cat4 0.1422 0.1301 0.5095
#> Item131    Item13   Item13-Cat1 0.1559 0.1232 0.4688
#> Item132    Item13   Item13-Cat2 0.1218 0.5657 0.1553
#> Item133    Item13   Item13-Cat3 0.3668 0.1443 0.2327
#> Item134    Item13   Item13-Cat4 0.3556 0.1667 0.1432
#> Item141    Item14   Item14-Cat1 0.3561 0.1350 0.1559
#> Item142    Item14   Item14-Cat2 0.1076 0.1307 0.5308
#> Item143    Item14   Item14-Cat3 0.1619 0.6057 0.1222
#> Item144    Item14   Item14-Cat4 0.3744 0.1287 0.1910
#> Item151    Item15   Item15-Cat1 0.1999 0.1604 0.5822
#> Item152    Item15   Item15-Cat2 0.1571 0.5870 0.1307
#> Item153    Item15   Item15-Cat3 0.3352 0.1222 0.1220
#> Item154    Item15   Item15-Cat4 0.3078 0.1304 0.1651
#> Item161    Item16   Item16-Cat1 0.3323 0.1454 0.1439
#> Item162    Item16   Item16-Cat2 0.3863 0.1693 0.1669
#> Item163    Item16   Item16-Cat3 0.1282 0.1440 0.5236
#> Item164    Item16   Item16-Cat4 0.1532 0.5413 0.1657
#> Item171    Item17   Item17-Cat1 0.1177 0.2133 0.5100
#> Item172    Item17   Item17-Cat2 0.1881 0.4860 0.1663
#> Item173    Item17   Item17-Cat3 0.3380 0.1469 0.1978
#> Item174    Item17   Item17-Cat4 0.3563 0.1538 0.1259
#> Item181    Item18   Item18-Cat1 0.3063 0.1712 0.1672
#> Item182    Item18   Item18-Cat2 0.3246 0.1632 0.1004
#> Item183    Item18   Item18-Cat3 0.1867 0.1599 0.6146
#> Item184    Item18   Item18-Cat4 0.1823 0.5058 0.1178
#> Item191    Item19   Item19-Cat1 0.3312 0.1882 0.2026
#> Item192    Item19   Item19-Cat2 0.1461 0.1679 0.5295
#> Item193    Item19   Item19-Cat3 0.1644 0.5205 0.0943
#> Item194    Item19   Item19-Cat4 0.3583 0.1234 0.1737
#> Item201    Item20   Item20-Cat1 0.3515 0.1609 0.1799
#> Item202    Item20   Item20-Cat2 0.1452 0.1652 0.5222
#> Item203    Item20   Item20-Cat3 0.0903 0.4681 0.1353
#> Item204    Item20   Item20-Cat4 0.4129 0.2057 0.1626
#> 
#> Test Profile
#>                               Class 1 Class 2 Class 3
#> Latent Class Ditribution      233.000 245.000 122.000
#> Class Membership Distribution 231.324 244.059 124.617
#> 
#> Model Fit Indices
#> Number of Latent class: 3
#> Number of EM cycle: 16 
#> 
#> Margin based (M2)
#>             value
#> M2       3510.944
#> df       1591.000
#> p           0.000
#> M2_null  9282.443
#> df_null  1710.000
#> n_margin 1770.000
#> NFI         0.622
#> RFI         0.593
#> IFI         0.750
#> TLI         0.727
#> CFI         0.746
#> RMSEA       0.045
# }
```
