# Print Method for Exametrika Objects

S3 method for printing objects of class "exametrika". This function
formats and displays appropriate summary information based on the
specific subclass of the exametrika object. Different types of analysis
results (IRT, LCA, network models, etc.) are presented with customized
formatting to highlight the most relevant information.

## Usage

``` r
# S3 method for class 'exametrika'
print(x, digits = 3, fit_indices = c("both", "pattern", "margin"), ...)
```

## Arguments

- x:

  An object of class "exametrika" with various possible subclasses

- digits:

  Integer indicating the number of decimal places to display. Default is
  3.

- fit_indices:

  For models that can carry both: which set of fit indices to show.
  "both" (default), "pattern" for the response-pattern based ones, or
  "margin" for the margin-based ones from
  [`add_M2`](https://kosugitti.github.io/exametrika/reference/add_M2.md).
  The two are built from chi-squares that live in different worlds and
  are never combined into a single set.

- ...:

  Additional arguments passed to print methods (not currently used)

## Value

Prints a formatted summary of the exametrika object to the console, with
content varying by object subclass:

- TestStatistics:

  Basic descriptive statistics of the test

- Dimensionality:

  Eigenvalue analysis results with scree plot

- ItemStatistics:

  Item-level statistics and psychometric properties

- QitemStatistics:

  Item statistics for polytomous items

- exametrikaData:

  Data structure details including response patterns and weights

- IIAnalysis:

  Item-item relationship measures (tetrachoric correlations, etc.)

- CTT:

  Classical Test Theory reliability measures

- IRT/GRM:

  Item parameters, ability estimates, and fit indices

- LCA/LRA:

  Class/Rank profiles, distribution information, and model fit
  statistics

- Biclustering/Biclustering_IRM:

  Cluster profiles, field distributions, and model diagnostics

- LDLRA/LDB/BINET:

  Network visualizations, parameter estimates, and conditional
  probabilities

## Details

The function identifies the specific subclass of the exametrika object
and tailors the output accordingly. For most analysis types, the
function displays:

- Basic model description and parameters

- Estimation results (e.g., item parameters, latent class profiles)

- Model fit statistics and diagnostics

- Visual representations where appropriate (e.g., graphs for network
  models, scree plots for dimensionality analysis)

When printing network-based models (LDLRA, LDB, BINET), this function
visualizes the network structure using graphs, which can help in
interpreting complex relationships between items or latent variables.

## Examples

``` r
# \donttest{
# Print IRT analysis results with 4 decimal places
result <- IRT(J15S500)
print(result, digits = 4)
#> Item Parameters
#>         slope location PSD(slope) PSD(location)
#> Item01 0.6982  -1.6838    0.10931        0.2659
#> Item02 0.8104  -1.5531    0.11662        0.2209
#> Item03 0.5591  -1.8388    0.09876        0.3382
#> Item04 1.4162  -1.1787    0.15687        0.1134
#> Item05 0.6808  -2.2423    0.11517        0.3599
#> Item06 0.9966  -2.1632    0.14989        0.2733
#> Item07 1.0843  -1.0400    0.12808        0.1303
#> Item08 0.6938  -0.5582    0.10021        0.1528
#> Item09 0.3472   1.6292    0.07659        0.4273
#> Item10 0.4918  -1.4214    0.09065        0.3058
#> Item11 1.1222   1.0197    0.13139        0.1245
#> Item12 1.2161   1.0305    0.13849        0.1171
#> Item13 0.8751  -0.7204    0.11112        0.1332
#> Item14 1.1995  -1.2322    0.14069        0.1338
#> Item15 0.8227  -1.2036    0.11274        0.1798
#> 
#> Item Fit Indices
#>        model_log_like bench_log_like null_log_like model_Chi_sq null_Chi_sq
#> Item01      -263.5262      -240.1896     -283.3432      46.6731     86.3072
#> Item02      -252.9125      -235.4364     -278.9486      34.9522     87.0245
#> Item03      -281.0828      -260.9064     -293.5981      40.3527     65.3834
#> Item04      -205.8387      -192.0718     -265.9618      27.5338    147.7800
#> Item05      -232.0733      -206.5372     -247.4032      51.0722     81.7320
#> Item06      -173.9331      -153.9397     -198.8174      39.9867     89.7553
#> Item07      -252.0373      -228.3788     -298.3455      47.3171    139.9335
#> Item08      -313.7555      -293.2252     -338.7888      41.0607     91.1272
#> Item09      -325.6907      -300.4923     -327.8422      50.3966     54.6997
#> Item10      -309.4496      -288.1984     -319.8497      42.5026     63.3026
#> Item11      -250.8297      -224.0855     -299.2653      53.4885    150.3596
#> Item12      -240.2314      -214.7967     -293.5981      50.8694    157.6029
#> Item13      -291.8217      -262.0307     -328.3959      59.5819    132.7304
#> Item14      -224.3306      -204.9528     -273.2123      38.7556    136.5190
#> Item15      -273.1223      -254.7637     -302.8469      36.7173     96.1665
#>        model_df null_df    NFI    RFI    IFI    TLI    CFI  RMSEA     AIC
#> Item01       12      13 0.4592 0.4142 0.5334 0.4876 0.5270 0.0761 22.6731
#> Item02       12      13 0.5984 0.5649 0.6941 0.6641 0.6899 0.0619 10.9522
#> Item03       12      13 0.3828 0.3314 0.4689 0.4136 0.4587 0.0688 16.3527
#> Item04       12      13 0.8137 0.7982 0.8856 0.8751 0.8847 0.0509  3.5338
#> Item05       12      13 0.3751 0.3231 0.4397 0.3842 0.4315 0.0808 27.0722
#> Item06       12      13 0.5545 0.5174 0.6401 0.6050 0.6354 0.0684 15.9867
#> Item07       12      13 0.6619 0.6337 0.7239 0.6986 0.7218 0.0768 23.3171
#> Item08       12      13 0.5494 0.5119 0.6327 0.5970 0.6280 0.0697 17.0607
#> Item09       12      13 0.0787 0.0019 0.1008 0.0025 0.0792 0.0801 26.3966
#> Item10       12      13 0.3286 0.2726 0.4054 0.3431 0.3936 0.0714 18.5026
#> Item11       12      13 0.6443 0.6146 0.7001 0.6728 0.6980 0.0832 29.4885
#> Item12       12      13 0.6772 0.6503 0.7330 0.7088 0.7312 0.0806 26.8694
#> Item13       12      13 0.5511 0.5137 0.6059 0.5695 0.6026 0.0891 35.5819
#> Item14       12      13 0.7161 0.6925 0.7851 0.7653 0.7834 0.0668 14.7556
#> Item15       12      13 0.6182 0.5864 0.7063 0.6780 0.7028 0.0642 12.7173
#>            CAIC      BIC
#> Item01 -39.9021 -27.9021
#> Item02 -51.6231 -39.6231
#> Item03 -46.2226 -34.2226
#> Item04 -59.0415 -47.0415
#> Item05 -35.5031 -23.5031
#> Item06 -46.5886 -34.5886
#> Item07 -39.2582 -27.2582
#> Item08 -45.5146 -33.5146
#> Item09 -36.1787 -24.1787
#> Item10 -44.0727 -32.0727
#> Item11 -33.0868 -21.0868
#> Item12 -35.7059 -23.7059
#> Item13 -26.9934 -14.9934
#> Item14 -47.8197 -35.8197
#> Item15 -49.8580 -37.8580
#> 
#> Model Fit Indices
#>                     value
#> model_log_like -3890.6353
#> bench_log_like -3560.0051
#> null_log_like  -4350.2170
#> model_Chi_sq     661.2604
#> null_Chi_sq     1580.4238
#> model_df         180.0000
#> null_df          195.0000
#> NFI                0.5816
#> RFI                0.5467
#> IFI                0.6563
#> TLI                0.6237
#> CFI                0.6526
#> RMSEA              0.0732
#> AIC              301.2604
#> CAIC            -637.3691
#> BIC             -457.3691

# Print Latent Class Analysis results
result_lca <- LCA(J15S500, ncls = 3)
print(result_lca)
#> 
#> Item Reference Profile
#>          IRP1  IRP2  IRP3
#> Item01 0.5952 0.761 0.877
#> Item02 0.5597 0.820 0.875
#> Item03 0.5922 0.782 0.799
#> Item04 0.5027 0.838 0.979
#> Item05 0.6764 0.859 0.872
#> Item06 0.6864 0.972 0.927
#> Item07 0.4390 0.807 0.893
#> Item08 0.3602 0.690 0.705
#> Item09 0.3441 0.242 0.509
#> Item10 0.5138 0.766 0.699
#> Item11 0.0831 0.190 0.582
#> Item12 0.0749 0.156 0.589
#> Item13 0.3351 0.826 0.728
#> Item14 0.5155 0.799 0.970
#> Item15 0.4587 0.820 0.830
#> 
#> Test Profile
#>                               Class 1 Class 2 Class 3
#> Test Reference Profile          6.737  10.329  11.833
#> Latent Class Ditribution      157.000 171.000 172.000
#> Class Membership Distribution 162.321 171.048 166.631
#> 
#> Item Fit Indices
#>        model_log_like bench_log_like null_log_like model_Chi_sq null_Chi_sq
#> Item01       -265.586       -240.190      -283.343       50.792      86.307
#> Item02       -254.618       -235.436      -278.949       38.363      87.025
#> Item03       -283.074       -260.906      -293.598       44.336      65.383
#> Item04       -205.405       -192.072      -265.962       26.667     147.780
#> Item05       -235.564       -206.537      -247.403       58.053      81.732
#> Item06       -166.780       -153.940      -198.817       25.680      89.755
#> Item07       -252.085       -228.379      -298.345       47.412     139.933
#> Item08       -313.021       -293.225      -338.789       39.591      91.127
#> Item09       -314.543       -300.492      -327.842       28.101      54.700
#> Item10       -307.337       -288.198      -319.850       38.278      63.303
#> Item11       -242.986       -224.085      -299.265       37.802     150.360
#> Item12       -230.028       -214.797      -293.598       30.462     157.603
#> Item13       -280.068       -262.031      -328.396       36.074     132.730
#> Item14       -220.731       -204.953      -273.212       31.556     136.519
#> Item15       -268.593       -254.764      -302.847       27.658      96.166
#>        model_df null_df   NFI   RFI   IFI   TLI   CFI RMSEA    AIC    CAIC
#> Item01       11      13 0.411 0.304 0.472 0.358 0.457 0.085 28.792 -28.569
#> Item02       11      13 0.559 0.479 0.640 0.563 0.630 0.071 16.363 -40.998
#> Item03       11      13 0.322 0.199 0.387 0.248 0.364 0.078 22.336 -35.025
#> Item04       11      13 0.820 0.787 0.885 0.863 0.884 0.053  4.667 -52.694
#> Item05       11      13 0.290 0.161 0.335 0.191 0.315 0.093 36.053 -21.308
#> Item06       11      13 0.714 0.662 0.814 0.774 0.809 0.052  3.680 -53.681
#> Item07       11      13 0.661 0.600 0.718 0.661 0.713 0.081 25.412 -31.948
#> Item08       11      13 0.566 0.487 0.643 0.568 0.634 0.072 17.591 -39.770
#> Item09       11      13 0.486 0.393 0.609 0.515 0.590 0.056  6.101 -51.259
#> Item10       11      13 0.395 0.285 0.478 0.359 0.458 0.070 16.278 -41.083
#> Item11       11      13 0.749 0.703 0.808 0.769 0.805 0.070 15.802 -41.559
#> Item12       11      13 0.807 0.772 0.867 0.841 0.865 0.060  8.462 -48.899
#> Item13       11      13 0.728 0.679 0.794 0.753 0.791 0.068 14.074 -43.287
#> Item14       11      13 0.769 0.727 0.836 0.803 0.834 0.061  9.556 -47.805
#> Item15       11      13 0.712 0.660 0.804 0.763 0.800 0.055  5.658 -51.703
#>            BIC
#> Item01 -17.569
#> Item02 -29.998
#> Item03 -24.025
#> Item04 -41.694
#> Item05 -10.308
#> Item06 -42.681
#> Item07 -20.948
#> Item08 -28.770
#> Item09 -40.259
#> Item10 -30.083
#> Item11 -30.559
#> Item12 -37.899
#> Item13 -32.287
#> Item14 -36.805
#> Item15 -40.703
#> 
#> Model Fit Indices
#> Number of Latent class: 3
#> Number of EM cycle: 95 
#>                    value
#> model_log_like -3840.417
#> bench_log_like -3560.005
#> null_log_like  -4350.217
#> model_Chi_sq     560.824
#> null_Chi_sq     1580.424
#> model_df         165.000
#> null_df          195.000
#> NFI                0.645
#> RFI                0.581
#> IFI                0.720
#> TLI                0.662
#> CFI                0.714
#> RMSEA              0.069
#> AIC              230.824
#> CAIC            -629.587
#> BIC             -464.587
# }
```
