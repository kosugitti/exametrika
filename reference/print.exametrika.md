# Print Method for Exametrika Objects

S3 method for printing objects of class "exametrika". This function
formats and displays appropriate summary information based on the
specific subclass of the exametrika object. Different types of analysis
results (IRT, LCA, network models, etc.) are presented with customized
formatting to highlight the most relevant information.

## Usage

``` r
# S3 method for class 'exametrika'
print(x, digits = 3, ...)
```

## Arguments

- x:

  An object of class "exametrika" with various possible subclasses

- digits:

  Integer indicating the number of decimal places to display. Default is
  3.

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
#> iter 1 LogLik -3915.61                                                          
#> iter 2 LogLik -3901.1                                                           
#> iter 3 LogLik -3896.89                                                          
#> iter 4 LogLik -3894.98                                                          
#> iter 5 LogLik -3894.02                                                          
#> iter 6 LogLik -3893.53                                                          
#> iter 7 LogLik -3893.28                                                          
#> iter 8 LogLik -3893.15                                                          
#> iter 9 LogLik -3893.08                                                          
#> iter 10 LogLik -3893.04                                                         
#> iter 11 LogLik -3893.03                                                         
print(result, digits = 4)
#> Item Parameters
#>         slope location PSD(slope) PSD(location)
#> Item01 0.6983  -1.6830    0.10932        0.2658
#> Item02 0.8104  -1.5525    0.11663        0.2209
#> Item03 0.5591  -1.8382    0.09877        0.3382
#> Item04 1.4160  -1.1781    0.15686        0.1134
#> Item05 0.6808  -2.2416    0.11517        0.3599
#> Item06 0.9967  -2.1624    0.14990        0.2733
#> Item07 1.0843  -1.0393    0.12808        0.1303
#> Item08 0.6938  -0.5575    0.10022        0.1528
#> Item09 0.3472   1.6298    0.07659        0.4273
#> Item10 0.4918  -1.4207    0.09065        0.3058
#> Item11 1.1221   1.0203    0.13140        0.1245
#> Item12 1.2159   1.0312    0.13848        0.1172
#> Item13 0.8752  -0.7197    0.11113        0.1332
#> Item14 1.1996  -1.2316    0.14069        0.1338
#> Item15 0.8228  -1.2030    0.11274        0.1798
#> 
#> Item Fit Indices
#>        model_log_like bench_log_like null_log_like model_Chi_sq null_Chi_sq
#> Item01      -263.5243      -240.1896     -283.3432      46.6693     86.3072
#> Item02      -252.9135      -235.4364     -278.9486      34.9543     87.0245
#> Item03      -281.0830      -260.9064     -293.5981      40.3532     65.3834
#> Item04      -205.8510      -192.0718     -265.9618      27.5585    147.7800
#> Item05      -232.0722      -206.5372     -247.4032      51.0699     81.7320
#> Item06      -173.9301      -153.9397     -198.8174      39.9807     89.7553
#> Item07      -252.0388      -228.3788     -298.3455      47.3201    139.9335
#> Item08      -313.7538      -293.2252     -338.7888      41.0573     91.1272
#> Item09      -325.6916      -300.4923     -327.8422      50.3986     54.6997
#> Item10      -309.4483      -288.1984     -319.8497      42.4998     63.3026
#> Item11      -250.8358      -224.0855     -299.2653      53.5007    150.3596
#> Item12      -240.2466      -214.7967     -293.5981      50.8999    157.6029
#> Item13      -291.8161      -262.0307     -328.3959      59.5709    132.7304
#> Item14      -224.3296      -204.9528     -273.2123      38.7536    136.5190
#> Item15      -273.1202      -254.7637     -302.8469      36.7131     96.1665
#>        model_df null_df    NFI    RFI    IFI    TLI    CFI  RMSEA     AIC
#> Item01       12      13 0.4593 0.4142 0.5334 0.4877 0.5271 0.0761 22.6693
#> Item02       12      13 0.5983 0.5649 0.6940 0.6641 0.6899 0.0619 10.9543
#> Item03       12      13 0.3828 0.3314 0.4689 0.4136 0.4587 0.0688 16.3532
#> Item04       12      13 0.8135 0.7980 0.8854 0.8749 0.8846 0.0510  3.5585
#> Item05       12      13 0.3752 0.3231 0.4397 0.3842 0.4316 0.0808 27.0699
#> Item06       12      13 0.5546 0.5174 0.6401 0.6051 0.6355 0.0684 15.9807
#> Item07       12      13 0.6618 0.6337 0.7239 0.6986 0.7217 0.0768 23.3201
#> Item08       12      13 0.5495 0.5119 0.6328 0.5971 0.6281 0.0697 17.0573
#> Item09       12      13 0.0786 0.0019 0.1007 0.0024 0.0792 0.0801 26.3986
#> Item10       12      13 0.3286 0.2727 0.4055 0.3431 0.3937 0.0714 18.4998
#> Item11       12      13 0.6442 0.6145 0.7001 0.6727 0.6979 0.0833 29.5007
#> Item12       12      13 0.6770 0.6501 0.7328 0.7086 0.7310 0.0806 26.8999
#> Item13       12      13 0.5512 0.5138 0.6060 0.5696 0.6027 0.0891 35.5709
#> Item14       12      13 0.7161 0.6925 0.7851 0.7654 0.7834 0.0668 14.7536
#> Item15       12      13 0.6182 0.5864 0.7064 0.6781 0.7028 0.0642 12.7131
#>            CAIC      BIC
#> Item01 -39.9060 -27.9060
#> Item02 -51.6210 -39.6210
#> Item03 -46.2221 -34.2221
#> Item04 -59.0168 -47.0168
#> Item05 -35.5054 -23.5054
#> Item06 -46.5946 -34.5946
#> Item07 -39.2552 -27.2552
#> Item08 -45.5180 -33.5180
#> Item09 -36.1767 -24.1767
#> Item10 -44.0755 -32.0755
#> Item11 -33.0746 -21.0746
#> Item12 -35.6754 -23.6754
#> Item13 -27.0044 -15.0044
#> Item14 -47.8217 -35.8217
#> Item15 -49.8622 -37.8622
#> 
#> Model Fit Indices
#>                     value
#> model_log_like -3890.6551
#> bench_log_like -3560.0051
#> null_log_like  -4350.2170
#> model_Chi_sq     661.2999
#> null_Chi_sq     1580.4238
#> model_df         180.0000
#> null_df          195.0000
#> NFI                0.5816
#> RFI                0.5467
#> IFI                0.6563
#> TLI                0.6236
#> CFI                0.6526
#> RMSEA              0.0732
#> AIC              301.2999
#> CAIC            -637.3296
#> BIC             -457.3296

# Print Latent Class Analysis results
result_lca <- LCA(J15S500, ncls = 3)
#> iter 1 log_lik -3955.4                                                          
#> iter 2 log_lik -3904.63                                                         
#> iter 3 log_lik -3890.82                                                         
#> iter 4 log_lik -3880                                                            
#> iter 5 log_lik -3870.82                                                         
#> iter 6 log_lik -3863.52                                                         
#> iter 7 log_lik -3857.89                                                         
#> iter 8 log_lik -3853.58                                                         
#> iter 9 log_lik -3850.31                                                         
#> iter 10 log_lik -3847.86                                                        
#> iter 11 log_lik -3846.05                                                        
#> iter 12 log_lik -3844.72                                                        
#> iter 13 log_lik -3843.74                                                        
#> iter 14 log_lik -3843.02                                                        
#> iter 15 log_lik -3842.48                                                        
#> iter 16 log_lik -3842.07                                                        
#> iter 17 log_lik -3841.76                                                        
print(result_lca)
#> 
#> Item Reference Profile
#>          IRP1  IRP2  IRP3
#> Item01 0.5972 0.749 0.885
#> Item02 0.5604 0.809 0.882
#> Item03 0.5914 0.778 0.800
#> Item04 0.4927 0.851 0.968
#> Item05 0.6757 0.853 0.876
#> Item06 0.6872 0.961 0.933
#> Item07 0.4314 0.807 0.894
#> Item08 0.3586 0.680 0.712
#> Item09 0.3403 0.261 0.493
#> Item10 0.5154 0.749 0.712
#> Item11 0.0867 0.159 0.606
#> Item12 0.0732 0.171 0.570
#> Item13 0.3298 0.826 0.727
#> Item14 0.5176 0.788 0.974
#> Item15 0.4533 0.821 0.829
#> 
#> Test Profile
#>                               Class 1 Class 2 Class 3
#> Test Reference Profile          6.711  10.263  11.861
#> Latent Class Ditribution      156.000 178.000 166.000
#> Class Membership Distribution 159.885 172.129 167.987
#> 
#> Item Fit Indices
#>        model_log_like bench_log_like null_log_like model_Chi_sq null_Chi_sq
#> Item01       -264.789       -240.190      -283.343       49.199      86.307
#> Item02       -254.621       -235.436      -278.949       38.370      87.025
#> Item03       -283.141       -260.906      -293.598       44.469      65.383
#> Item04       -206.729       -192.072      -265.962       29.314     147.780
#> Item05       -235.601       -206.537      -247.403       58.127      81.732
#> Item06       -169.064       -153.940      -198.817       30.248      89.755
#> Item07       -250.627       -228.379      -298.345       44.496     139.933
#> Item08       -313.073       -293.225      -338.789       39.697      91.127
#> Item09       -317.685       -300.492      -327.842       34.384      54.700
#> Item10       -308.505       -288.198      -319.850       40.614      63.303
#> Item11       -235.022       -224.085      -299.265       21.873     150.360
#> Item12       -235.443       -214.797      -293.598       41.293     157.603
#> Item13       -279.431       -262.031      -328.396       34.800     132.730
#> Item14       -220.099       -204.953      -273.212       30.292     136.519
#> Item15       -267.926       -254.764      -302.847       26.324      96.166
#>        model_df null_df   NFI   RFI   IFI   TLI   CFI RMSEA    AIC    CAIC
#> Item01       11      13 0.430 0.326 0.493 0.384 0.479 0.083 27.199 -30.162
#> Item02       11      13 0.559 0.479 0.640 0.563 0.630 0.071 16.370 -40.991
#> Item03       11      13 0.320 0.196 0.385 0.245 0.361 0.078 22.469 -34.892
#> Item04       11      13 0.802 0.766 0.866 0.839 0.864 0.058  7.314 -50.046
#> Item05       11      13 0.289 0.160 0.334 0.190 0.314 0.093 36.127 -21.234
#> Item06       11      13 0.663 0.602 0.756 0.704 0.749 0.059  8.248 -49.112
#> Item07       11      13 0.682 0.624 0.740 0.688 0.736 0.078 22.496 -34.864
#> Item08       11      13 0.564 0.485 0.642 0.566 0.633 0.072 17.697 -39.664
#> Item09       11      13 0.371 0.257 0.465 0.337 0.439 0.065 12.384 -44.976
#> Item10       11      13 0.358 0.242 0.434 0.304 0.411 0.073 18.614 -38.746
#> Item11       11      13 0.855 0.828 0.922 0.906 0.921 0.045 -0.127 -57.487
#> Item12       11      13 0.738 0.690 0.793 0.752 0.791 0.074 19.293 -38.068
#> Item13       11      13 0.738 0.690 0.804 0.765 0.801 0.066 12.800 -44.561
#> Item14       11      13 0.778 0.738 0.846 0.815 0.844 0.059  8.292 -49.069
#> Item15       11      13 0.726 0.676 0.820 0.782 0.816 0.053  4.324 -53.037
#>            BIC
#> Item01 -19.162
#> Item02 -29.991
#> Item03 -23.892
#> Item04 -39.046
#> Item05 -10.234
#> Item06 -38.112
#> Item07 -23.864
#> Item08 -28.664
#> Item09 -33.976
#> Item10 -27.746
#> Item11 -46.487
#> Item12 -27.068
#> Item13 -33.561
#> Item14 -38.069
#> Item15 -42.037
#> 
#> Model Fit Indices
#> Number of Latent class: 3
#> Number of EM cycle: 17 
#>                    value
#> model_log_like -3841.755
#> bench_log_like -3560.005
#> null_log_like  -4350.217
#> model_Chi_sq     563.500
#> null_Chi_sq     1580.424
#> model_df         165.000
#> null_df          195.000
#> NFI                0.643
#> RFI                0.579
#> IFI                0.718
#> TLI                0.660
#> CFI                0.712
#> RMSEA              0.070
#> AIC              233.500
#> CAIC            -626.910
#> BIC             -461.910
# }
```
