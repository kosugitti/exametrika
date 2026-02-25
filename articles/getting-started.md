# Getting Started with exametrika

## Overview

The `exametrika` package provides comprehensive Test Data Engineering
tools for analyzing educational test data. Based on the methods
described in Shojima (2022), this package enables researchers and
practitioners to:

- Analyze test response patterns and item characteristics
- Classify respondents using various psychometric models
- Investigate latent structures in test data
- Examine local dependencies between items
- Perform network analysis of item relationships

## Installation

``` r
# Install from CRAN
install.packages("exametrika")

# Or install the development version from GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("kosugitti/exametrika")
```

## Data Format

``` r
library(exametrika)
```

### Data Requirements

Exametrika accepts both binary and polytomous response data:

- **Binary data** (0/1): 0 = incorrect, 1 = correct
- **Polytomous data**: ordinal response categories or multiple score
  levels
- **Missing values**: NA values supported; custom missing value codes
  can be specified

### Data Formatting

The
[`dataFormat()`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
function processes input data before analysis:

``` r
# Format raw data for analysis
data <- dataFormat(J15S500)
str(data)
#> List of 7
#>  $ ID           : chr [1:500] "Student001" "Student002" "Student003" "Student004" ...
#>  $ ItemLabel    : chr [1:15] "Item01" "Item02" "Item03" "Item04" ...
#>  $ Z            : num [1:500, 1:15] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:15] "Item01" "Item02" "Item03" "Item04" ...
#>  $ w            : num [1:15] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ response.type: chr "binary"
#>  $ categories   : Named int [1:15] 2 2 2 2 2 2 2 2 2 2 ...
#>   ..- attr(*, "names")= chr [1:15] "Item01" "Item02" "Item03" "Item04" ...
#>  $ U            : num [1:500, 1:15] 0 1 1 1 1 1 0 0 1 1 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:15] "Item01" "Item02" "Item03" "Item04" ...
#>  - attr(*, "class")= chr [1:2] "exametrika" "exametrikaData"
```

### Sample Datasets

The package includes sample datasets from Shojima (2022). The naming
convention is `JxxSxxx` where J = number of items and S = sample size.

| Dataset  | Items | Examinees | Type              | Use Case                     |
|----------|------:|----------:|-------------------|------------------------------|
| J5S10    |     5 |        10 | Binary            | Quick testing                |
| J5S1000  |     5 |     1,000 | Ordinal           | GRM examples                 |
| J12S5000 |    12 |     5,000 | Binary            | LDLRA examples               |
| J15S500  |    15 |       500 | Binary            | IRT, LCA examples            |
| J15S3810 |    15 |     3,810 | Ordinal (4-point) | Ordinal LRA                  |
| J20S400  |    20 |       400 | Binary            | BNM examples                 |
| J20S600  |    20 |       600 | Nominal (4-cat)   | Nominal Biclustering         |
| J35S500  |    35 |       500 | Ordinal (5-cat)   | Ordinal Biclustering         |
| J35S515  |    35 |       515 | Binary            | Biclustering, network models |
| J35S5000 |    35 |     5,000 | Multiple-choice   | Nominal LRA                  |
| J50S100  |    50 |       100 | Binary            | Small sample testing         |

## Basic Statistics

### Test Statistics

``` r
TestStatistics(J15S500)
#> Test Statistics
#>                   value
#> TestLength   15.0000000
#> SampleSize  500.0000000
#> Mean          9.6640000
#> SEofMean      0.1190738
#> Variance      7.0892826
#> SD            2.6625707
#> Skewness     -0.4116220
#> Kurtosis     -0.4471624
#> Min           2.0000000
#> Max          15.0000000
#> Range        13.0000000
#> Q1.25%        8.0000000
#> Median.50%   10.0000000
#> Q3.75%       12.0000000
#> IQR           4.0000000
#> Stanine.4%    5.0000000
#> Stanine.11%   6.0000000
#> Stanine.23%   7.0000000
#> Stanine.40%   9.0000000
#> Stanine.60%  11.0000000
#> Stanine.77%  12.0000000
#> Stanine.89%  13.0000000
#> Stanine.96%  14.0000000
```

### Item Statistics

``` r
ItemStatistics(J15S500)
#> Item Statistics
#>    ItemLabel  NR   CRR  ODDs Threshold Entropy ITCrr
#> 1     Item01 500 0.746 2.937    -0.662   0.818 0.375
#> 2     Item02 500 0.754 3.065    -0.687   0.805 0.393
#> 3     Item03 500 0.726 2.650    -0.601   0.847 0.321
#> 4     Item04 500 0.776 3.464    -0.759   0.767 0.503
#> 5     Item05 500 0.804 4.102    -0.856   0.714 0.329
#> 6     Item06 500 0.864 6.353    -1.098   0.574 0.377
#> 7     Item07 500 0.716 2.521    -0.571   0.861 0.483
#> 8     Item08 500 0.588 1.427    -0.222   0.978 0.405
#> 9     Item09 500 0.364 0.572     0.348   0.946 0.225
#> 10    Item10 500 0.662 1.959    -0.418   0.923 0.314
#> 11    Item11 500 0.286 0.401     0.565   0.863 0.455
#> 12    Item12 500 0.274 0.377     0.601   0.847 0.468
#> 13    Item13 500 0.634 1.732    -0.342   0.948 0.471
#> 14    Item14 500 0.764 3.237    -0.719   0.788 0.485
#> 15    Item15 500 0.706 2.401    -0.542   0.874 0.413
```

### Classical Test Theory

``` r
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
```

## Next Steps

- [Item Response
  Theory](https://kosugitti.github.io/exametrika/articles/irt.md): IRT
  and GRM models
- [Latent Class and Rank
  Analysis](https://kosugitti.github.io/exametrika/articles/latent-class-rank.md):
  LCA and LRA
- [Biclustering](https://kosugitti.github.io/exametrika/articles/biclustering.md):
  Biclustering, Ranklustering, and IRM
- [Network
  Models](https://kosugitti.github.io/exametrika/articles/network-models.md):
  BNM, LDLRA, LDB, and BINET

## Reference

Shojima, Kojiro (2022) *Test Data Engineering: Latent Rank Analysis,
Biclustering, and Bayesian Network* (Behaviormetrics: Quantitative
Approaches to Human Behavior, 13), Springer.
