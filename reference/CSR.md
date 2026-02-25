# Conditional Selection Rate

Calculate the Conditional Selection Rate (CSR) for polytomous data. CSR
measures the proportion of respondents who selected a specific category
in item K, given that they selected a particular category in item J.

## Usage

``` r
CSR(U, na = NULL, Z = NULL, w = NULL)
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

A list of Joint Selection Rate matrices for each item pair.

## Details

The function returns a nested list structure CSR, where `CSR[[j]][[k]]`
contains a matrix of conditional probabilities. In this matrix, the
element at row l and column m represents P(K=m\|J=l), which is the
probability of selecting category m for item K, given that category l
was selected for item J.

Mathematically, for each cell (l,m) in the `CSR[[j]][[k]]` matrix:
`CSR[[j]][[k]][l,m] = P(Item K = category m | Item J = category l)`

This is calculated as the number of respondents who selected both
category l for item J and category m for item K, divided by the total
number of respondents who selected category l for item J.

## Examples

``` r
# example code
# Calculate CSR using sample dataset J5S1000
CSR(J5S1000)
#> [[1]]
#> [[1]][[1]]
#>         V1-Cat1 V1-Cat2 V1-Cat3 V1-Cat4
#> V1-Cat1       1       0       0       0
#> V1-Cat2       0       1       0       0
#> V1-Cat3       0       0       1       0
#> V1-Cat4       0       0       0       1
#> 
#> [[1]][[2]]
#>           V2-Cat1   V2-Cat2   V2-Cat3
#> V1-Cat1 0.4299517 0.4009662 0.1690821
#> V1-Cat2 0.2635135 0.5337838 0.2027027
#> V1-Cat3 0.2581818 0.4763636 0.2654545
#> V1-Cat4 0.1853659 0.4292683 0.3853659
#> 
#> [[1]][[3]]
#>           V3-Cat1   V3-Cat2   V3-Cat3   V3-Cat4
#> V1-Cat1 0.3205742 0.3349282 0.2057416 0.1387560
#> V1-Cat2 0.2040134 0.2775920 0.2608696 0.2575251
#> V1-Cat3 0.1684982 0.2783883 0.3150183 0.2380952
#> V1-Cat4 0.1250000 0.2355769 0.2644231 0.3750000
#> 
#> [[1]][[4]]
#>           V4-Cat1   V4-Cat2   V4-Cat3
#> V1-Cat1 0.4182692 0.4086538 0.1730769
#> V1-Cat2 0.3010033 0.4414716 0.2575251
#> V1-Cat3 0.2028986 0.5108696 0.2862319
#> V1-Cat4 0.1256039 0.4541063 0.4202899
#> 
#> [[1]][[5]]
#>           V5-Cat1   V5-Cat2   V5-Cat3   V5-Cat4
#> V1-Cat1 0.3492823 0.3062201 0.2200957 0.1244019
#> V1-Cat2 0.2709030 0.2976589 0.2474916 0.1839465
#> V1-Cat3 0.2072727 0.2472727 0.3272727 0.2181818
#> V1-Cat4 0.1153846 0.2163462 0.3317308 0.3365385
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#>           V1-Cat1   V1-Cat2   V1-Cat3   V1-Cat4
#> V2-Cat1 0.3224638 0.2826087 0.2572464 0.1376812
#> V2-Cat2 0.1804348 0.3434783 0.2847826 0.1913043
#> V2-Cat3 0.1417004 0.2429150 0.2955466 0.3198381
#> 
#> [[2]][[2]]
#>         V2-Cat1 V2-Cat2 V2-Cat3
#> V2-Cat1       1       0       0
#> V2-Cat2       0       1       0
#> V2-Cat3       0       0       1
#> 
#> [[2]][[3]]
#>           V3-Cat1   V3-Cat2   V3-Cat3   V3-Cat4
#> V2-Cat1 0.2877698 0.3453237 0.2086331 0.1582734
#> V2-Cat2 0.1782609 0.2913043 0.2826087 0.2478261
#> V2-Cat3 0.1491935 0.1935484 0.2903226 0.3669355
#> 
#> [[2]][[4]]
#>            V4-Cat1   V4-Cat2   V4-Cat3
#> V2-Cat1 0.42599278 0.4584838 0.1155235
#> V2-Cat2 0.25379610 0.4598698 0.2863341
#> V2-Cat3 0.08835341 0.4618474 0.4497992
#> 
#> [[2]][[5]]
#>           V5-Cat1   V5-Cat2   V5-Cat3   V5-Cat4
#> V2-Cat1 0.3309353 0.2877698 0.2482014 0.1330935
#> V2-Cat2 0.2359307 0.3030303 0.2662338 0.1948052
#> V2-Cat3 0.1290323 0.1854839 0.3467742 0.3387097
#> 
#> 
#> [[3]]
#> [[3]][[1]]
#>           V1-Cat1   V1-Cat2   V1-Cat3   V1-Cat4
#> V3-Cat1 0.3350000 0.3050000 0.2300000 0.1300000
#> V3-Cat2 0.2517986 0.2985612 0.2733813 0.1762590
#> V3-Cat3 0.1641221 0.2977099 0.3282443 0.2099237
#> V3-Cat4 0.1164659 0.3092369 0.2610442 0.3132530
#> 
#> [[3]][[2]]
#>           V2-Cat1   V2-Cat2   V2-Cat3
#> V3-Cat1 0.4020101 0.4120603 0.1859296
#> V3-Cat2 0.3453237 0.4820144 0.1726619
#> V3-Cat3 0.2230769 0.5000000 0.2769231
#> V3-Cat4 0.1767068 0.4578313 0.3654618
#> 
#> [[3]][[3]]
#>         V3-Cat1 V3-Cat2 V3-Cat3 V3-Cat4
#> V3-Cat1       1       0       0       0
#> V3-Cat2       0       1       0       0
#> V3-Cat3       0       0       1       0
#> V3-Cat4       0       0       0       1
#> 
#> [[3]][[4]]
#>           V4-Cat1   V4-Cat2   V4-Cat3
#> V3-Cat1 0.4455446 0.3960396 0.1584158
#> V3-Cat2 0.3082437 0.4982079 0.1935484
#> V3-Cat3 0.1692308 0.4769231 0.3538462
#> V3-Cat4 0.1587302 0.4404762 0.4007937
#> 
#> [[3]][[5]]
#>           V5-Cat1   V5-Cat2   V5-Cat3   V5-Cat4
#> V3-Cat1 0.3415842 0.2623762 0.2326733 0.1633663
#> V3-Cat2 0.2928571 0.2714286 0.2785714 0.1571429
#> V3-Cat3 0.2068966 0.2681992 0.2528736 0.2720307
#> V3-Cat4 0.1274900 0.2669323 0.3545817 0.2509960
#> 
#> 
#> [[4]]
#> [[4]][[1]]
#>           V1-Cat1   V1-Cat2   V1-Cat3   V1-Cat4
#> V4-Cat1 0.3359073 0.3474903 0.2162162 0.1003861
#> V4-Cat2 0.1880531 0.2920354 0.3119469 0.2079646
#> V4-Cat3 0.1290323 0.2759857 0.2831541 0.3118280
#> 
#> [[4]][[2]]
#>           V2-Cat1   V2-Cat2    V2-Cat3
#> V4-Cat1 0.4591440 0.4552529 0.08560311
#> V4-Cat2 0.2797357 0.4669604 0.25330396
#> V4-Cat3 0.1159420 0.4782609 0.40579710
#> 
#> [[4]][[3]]
#>           V3-Cat1   V3-Cat2   V3-Cat3   V3-Cat4
#> V4-Cat1 0.3461538 0.3307692 0.1692308 0.1538462
#> V4-Cat2 0.1762115 0.3061674 0.2731278 0.2444934
#> V4-Cat3 0.1146953 0.1935484 0.3297491 0.3620072
#> 
#> [[4]][[4]]
#>         V4-Cat1 V4-Cat2 V4-Cat3
#> V4-Cat1       1       0       0
#> V4-Cat2       0       1       0
#> V4-Cat3       0       0       1
#> 
#> [[4]][[5]]
#>           V5-Cat1   V5-Cat2   V5-Cat3   V5-Cat4
#> V4-Cat1 0.3601533 0.2835249 0.2375479 0.1187739
#> V4-Cat2 0.2263736 0.2945055 0.2769231 0.2021978
#> V4-Cat3 0.1397849 0.2043011 0.3333333 0.3225806
#> 
#> 
#> [[5]]
#> [[5]][[1]]
#>           V1-Cat1   V1-Cat2   V1-Cat3   V1-Cat4
#> V5-Cat1 0.3106383 0.3446809 0.2425532 0.1021277
#> V5-Cat2 0.2406015 0.3345865 0.2556391 0.1691729
#> V5-Cat3 0.1648746 0.2652330 0.3225806 0.2473118
#> V5-Cat4 0.1232227 0.2606635 0.2843602 0.3317536
#> 
#> [[5]][[2]]
#>           V2-Cat1   V2-Cat2   V2-Cat3
#> V5-Cat1 0.3948498 0.4678112 0.1373391
#> V5-Cat2 0.3007519 0.5263158 0.1729323
#> V5-Cat3 0.2482014 0.4424460 0.3093525
#> V5-Cat4 0.1753555 0.4265403 0.3981043
#> 
#> [[5]][[3]]
#>           V3-Cat1   V3-Cat2   V3-Cat3   V3-Cat4
#> V5-Cat1 0.2911392 0.3459916 0.2278481 0.1350211
#> V5-Cat2 0.1992481 0.2857143 0.2631579 0.2518797
#> V5-Cat3 0.1678571 0.2785714 0.2357143 0.3178571
#> V5-Cat4 0.1563981 0.2085308 0.3364929 0.2985782
#> 
#> [[5]][[4]]
#>           V4-Cat1   V4-Cat2   V4-Cat3
#> V5-Cat1 0.3983051 0.4364407 0.1652542
#> V5-Cat2 0.2792453 0.5056604 0.2150943
#> V5-Cat3 0.2206406 0.4483986 0.3309609
#> V5-Cat4 0.1455399 0.4319249 0.4225352
#> 
#> [[5]][[5]]
#>         V5-Cat1 V5-Cat2 V5-Cat3 V5-Cat4
#> V5-Cat1       1       0       0       0
#> V5-Cat2       0       1       0       0
#> V5-Cat3       0       0       1       0
#> V5-Cat4       0       0       0       1
#> 
#> 

# Extract the conditional selection rates from item 1 to item 2
csr_1_2 <- CSR(J5S1000)[[1]][[2]]
# This shows the probability of selecting each category in item 2
# given that a specific category was selected in item 1
```
