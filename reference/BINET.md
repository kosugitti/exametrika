# Bicluster Network Model

Bicluster Network Model: BINET is a model that combines the Bayesian
network model and Biclustering. BINET is very similar to LDB and LDR.
The most significant difference is that in LDB, the nodes represent the
fields, whereas in BINET, they represent the class. BINET explores the
local dependency structure among latent classes at each latent field,
where each field is a locus.

## Usage

``` r
BINET(
  U,
  na = NULL,
  Z = NULL,
  w = NULL,
  conf = NULL,
  ncls = NULL,
  nfld = NULL,
  g_list = NULL,
  adj_list = NULL,
  adj_file = NULL,
  verbose = FALSE,
  beta1 = 1,
  beta2 = 1
)
```

## Arguments

- U:

  U is either a data class of exametrika, or raw data. When raw data is
  given, it is converted to the exametrika class with the
  [dataFormat](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  function.

- na:

  na argument specifies the numbers or characters to be treated as
  missing values.

- Z:

  Z is a missing indicator matrix of the type matrix or data.frame

- w:

  w is item weight vector

- conf:

  For the confirmatory parameter, you can input either a vector with
  items and corresponding fields in sequence, or a field membership
  profile matrix. In the case of the former, the field membership
  profile matrix will be generated internally. When providing a
  membership profile matrix, it needs to be either matrix or data.frame.
  The number of fields(nfld) will be overwrite to the number of columns
  of this matrix.

- ncls:

  number of classes

- nfld:

  number of fields

- g_list:

  A list compiling graph-type objects for each rank/class.

- adj_list:

  A list compiling matrix-type adjacency matrices for each rank/class.

- adj_file:

  A file detailing the relationships of the graph for each rank/class,
  listed in the order of starting point, ending point, and rank(class).

- verbose:

  verbose output Flag. default is FALSE

- beta1:

  Beta distribution parameter 1 for prior density of the conditional
  correct response rates. Default is 1. Increase this (together with
  `beta2`) if estimation fails because some class-by-field cell has zero
  non-missing observations (common with many classes/fields combined
  with missing data).

- beta2:

  Beta distribution parameter 2 for prior density of the conditional
  correct response rates. Default is 1.

## Value

- nobs:

  Sample size. The number of rows in the dataset.

- msg:

  A character string indicating the model type.

- testlength:

  Length of the test. The number of items included in the test.

- n_class:

  Optimal number of classes.

- n_field:

  Optimal number of fields.

- crr:

  Correct Response Rate

- ItemLabel:

  Label of Items

- FieldLabel:

  Label of Fields

- all_adj:

  Integrated Adjacency matrix used to plot graph.

- all_g:

  Integrated graph object used to plot graph.see also
  [plot.exametrika](https://kosugitti.github.io/exametrika/reference/plot.exametrika.md)

- adj_list:

  List of Adjacency matrix used in the model

- params:

  A list of the estimated conditional probabilities. It indicates which
  path was obtained from which parent node(class) to which child
  node(class), held by `parent`, `child`, and `field`. The item Items
  contained in the field is in `fld`. Named `chap` includes the
  conditional correct response answer rate of the child node, while
  `pap` contains the pass rate of the parent node.

- PSRP:

  Response pattern by the students belonging to the parent classes of
  Class c. A more comprehensible arrangement of `params.`

- LCD:

  Latent Class Distribution. see also
  [plot.exametrika](https://kosugitti.github.io/exametrika/reference/plot.exametrika.md)

- LFD:

  Latent Field Distribution. see also
  [plot.exametrika](https://kosugitti.github.io/exametrika/reference/plot.exametrika.md)

- CMD:

  Class Membership Distribution.

- FRP:

  Marginal bicluster reference matrix.

- FRPIndex:

  Index of FFP includes the item location parameters B and Beta, the
  slope parameters A and Alpha, and the monotonicity indices C and
  Gamma.

- TRP:

  Test Reference Profile

- LDPSR:

  A rearranged set of parameters for output. It includes the field the
  items contained within that field, and the conditional correct
  response rate of parent nodes(class) and child node(class).

- FieldEstimated:

  Given vector which correspondence between items and the fields.

- Students:

  Rank Membership Profile matrix.The s-th row vector of \\\hat{M}\_R\\,
  \\\hat{m}\_R\\, is the rank membership profile of Student s, namely
  the posterior probability distribution representing the student's
  belonging to the respective latent classes.

- NextStage:

  The next class that easiest for students to move to, its membership
  probability, class-up odds, and the field required for more.

- MG_FitIndices:

  Multigroup as Null model.See also
  [TestFit](https://kosugitti.github.io/exametrika/reference/TestFit.md)

- SM_FitIndices:

  Saturated Model as Null model.See also
  [TestFit](https://kosugitti.github.io/exametrika/reference/TestFit.md)

## Examples

``` r
# \donttest{
# Example: Bicluster Network Model (BINET)
# BINET combines Bayesian network model and Biclustering to explore
# local dependency structure among latent classes at each field

# Create field configuration vector based on field assignments
conf <- c(
  1, 5, 5, 5, 9, 9, 6, 6, 6, 6, 2, 7, 7, 11, 11, 7, 7,
  12, 12, 12, 2, 2, 3, 3, 4, 4, 4, 8, 8, 12, 1, 1, 6, 10, 10
)

# Create edge data for network structure between classes
edges_data <- data.frame(
  "From Class (Parent) >>>" = c(
    1, 2, 3, 4, 5, 7, 2, 4, 6, 8, 10, 6, 6, 11, 8, 9, 12
  ),
  ">>> To Class (Child)" = c(
    2, 4, 5, 5, 6, 11, 3, 7, 9, 12, 12, 10, 8, 12, 12, 11, 13
  ),
  "At Field (Locus)" = c(
    1, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 7, 8, 8, 9, 9, 12
  )
)

# Save edge data to temporary CSV file
tmp_file <- tempfile(fileext = ".csv")
write.csv(edges_data, file = tmp_file, row.names = FALSE)

# Fit Bicluster Network Model
result.BINET <- BINET(
  J35S515,
  ncls = 13, # Maximum class number from edges (13)
  nfld = 12, # Maximum field number from conf (12)
  conf = conf, # Field configuration vector
  adj_file = tmp_file # Path to the CSV file
)

# Clean up temporary file
unlink(tmp_file)

# Display model results
print(result.BINET)
#> Total Graph
#>         Class01 Class02 Class03 Class04 Class05 Class06 Class07 Class08 Class09
#> Class01       0       1       0       0       0       0       0       0       0
#> Class02       0       0       1       1       0       0       0       0       0
#> Class03       0       0       0       0       1       0       0       0       0
#> Class04       0       0       0       0       1       0       1       0       0
#> Class05       0       0       0       0       0       1       0       0       0
#> Class06       0       0       0       0       0       0       0       1       1
#> Class07       0       0       0       0       0       0       0       0       0
#> Class08       0       0       0       0       0       0       0       0       0
#> Class09       0       0       0       0       0       0       0       0       0
#> Class10       0       0       0       0       0       0       0       0       0
#> Class11       0       0       0       0       0       0       0       0       0
#> Class12       0       0       0       0       0       0       0       0       0
#> Class13       0       0       0       0       0       0       0       0       0
#>         Class10 Class11 Class12 Class13
#> Class01       0       0       0       0
#> Class02       0       0       0       0
#> Class03       0       0       0       0
#> Class04       0       0       0       0
#> Class05       0       0       0       0
#> Class06       1       0       0       0
#> Class07       0       1       0       0
#> Class08       0       0       1       0
#> Class09       0       1       0       0
#> Class10       0       0       1       0
#> Class11       0       0       1       0
#> Class12       0       0       0       1
#> Class13       0       0       0       0

#> Estimation of Parameter set
#> Field 1 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1   0.000                     
#> Class 2   0.553  0.556  0.647       
#> Class 3   0.740                     
#> Class 4   0.859                     
#> Class 5   0.874                     
#> Class 6   0.906                     
#> Class 7   0.868                     
#> Class 8   0.898                     
#> Class 9   0.961                     
#> Class 10  0.933                     
#> Class 11  0.897                     
#> Class 12  0.973                     
#> Class 13  1.000                     
#> Field 2 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.0000                     
#> Class 2  0.0110                     
#> Class 3  0.0352                     
#> Class 4  0.6826  0.786  0.638       
#> Class 5  0.4054  0.726  0.695       
#> Class 6  0.6839                     
#> Class 7  0.8301                     
#> Class 8  0.8313                     
#> Class 9  1.0000                     
#> Class 10 0.9833                     
#> Class 11 1.0000                     
#> Class 12 1.0000                     
#> Class 13 1.0000                     
#> Field 3 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1   0.000                     
#> Class 2   0.176                     
#> Class 3   0.220                     
#> Class 4   0.205                     
#> Class 5   0.183  0.251              
#> Class 6   1.000                     
#> Class 7   1.000                     
#> Class 8   1.000                     
#> Class 9   0.986                     
#> Class 10  1.000                     
#> Class 11  0.974                     
#> Class 12  1.000                     
#> Class 13  1.000                     
#> Field 4 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.0000                     
#> Class 2  0.0113                     
#> Class 3  0.1236                     
#> Class 4  0.0472                     
#> Class 5  0.1141                     
#> Class 6  0.6161  0.442  0.181       
#> Class 7  0.9730                     
#> Class 8  0.9616                     
#> Class 9  0.9757                     
#> Class 10 0.9651                     
#> Class 11 0.9421  0.935  0.767       
#> Class 12 1.0000                     
#> Class 13 1.0000                     
#> Field 5 
#>          PSRP 1 PSRP 2  PSRP 3 PSRP 4
#> Class 1  0.0000                      
#> Class 2  0.0145                      
#> Class 3  0.0723  0.332 0.06771       
#> Class 4  0.9617                      
#> Class 5  0.1036                      
#> Class 6  0.2321                      
#> Class 7  0.1505  0.270 0.00641       
#> Class 8  0.9707                      
#> Class 9  0.2913  0.294 0.11830       
#> Class 10 0.8219                      
#> Class 11 0.9167                      
#> Class 12 1.0000  1.000 1.00000       
#> Class 13 1.0000                      
#> Field 6 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1   0.000                     
#> Class 2   0.235                     
#> Class 3   0.277                     
#> Class 4   0.450                     
#> Class 5   0.409                     
#> Class 6   0.301                     
#> Class 7   0.418                     
#> Class 8   0.488                     
#> Class 9   0.559                     
#> Class 10  0.562                     
#> Class 11  0.612                     
#> Class 12  0.762                     
#> Class 13  1.000                     
#> Field 7 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.0000                     
#> Class 2  0.0724                     
#> Class 3  0.0817                     
#> Class 4  0.1939                     
#> Class 5  0.1585                     
#> Class 6  0.1274                     
#> Class 7  0.1283                     
#> Class 8  0.1855                     
#> Class 9  0.7527                     
#> Class 10 0.9823  0.881  0.932  0.975
#> Class 11 0.3059                     
#> Class 12 0.8033                     
#> Class 13 1.0000                     
#> Field 8 
#>            PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.00e+00                     
#> Class 2  7.29e-12                     
#> Class 3  3.67e-02                     
#> Class 4  3.95e-02                     
#> Class 5  4.29e-02                     
#> Class 6  6.45e-02                     
#> Class 7  4.44e-01                     
#> Class 8  1.93e-01  0.222              
#> Class 9  6.15e-01                     
#> Class 10 3.78e-01                     
#> Class 11 3.20e-01                     
#> Class 12 1.00e+00  1.000              
#> Class 13 1.00e+00                     
#> Field 9 
#>            PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.00e+00                     
#> Class 2  3.90e-17                     
#> Class 3  1.61e-02                     
#> Class 4  6.17e-01                     
#> Class 5  3.10e-02                     
#> Class 6  7.77e-02                     
#> Class 7  2.82e-16                     
#> Class 8  1.85e-01                     
#> Class 9  9.52e-18                     
#> Class 10 8.04e-01                     
#> Class 11 1.00e+00  1.000              
#> Class 12 7.96e-01  0.717              
#> Class 13 1.00e+00                     
#> Field 10 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.0000                     
#> Class 2  0.0948                     
#> Class 3  0.1802                     
#> Class 4  0.1728                     
#> Class 5  0.1575                     
#> Class 6  0.1784                     
#> Class 7  0.1213                     
#> Class 8  0.1529                     
#> Class 9  0.2238                     
#> Class 10 0.2575                     
#> Class 11 0.1757                     
#> Class 12 0.3187                     
#> Class 13 1.0000                     
#> Field 11 
#>            PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.00e+00                     
#> Class 2  3.53e-16                     
#> Class 3  6.99e-17                     
#> Class 4  8.13e-02                     
#> Class 5  2.44e-02                     
#> Class 6  2.06e-02                     
#> Class 7  2.41e-02                     
#> Class 8  3.83e-17                     
#> Class 9  2.45e-01                     
#> Class 10 4.27e-01                     
#> Class 11 3.88e-02                     
#> Class 12 6.01e-01                     
#> Class 13 1.00e+00                     
#> Field 12 
#>            PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.00e+00                     
#> Class 2  2.51e-03                     
#> Class 3  5.56e-02                     
#> Class 4  0.00e+00                     
#> Class 5  1.94e-02                     
#> Class 6  1.96e-02                     
#> Class 7  1.99e-02                     
#> Class 8  4.98e-02                     
#> Class 9  1.80e-02                     
#> Class 10 2.59e-02                     
#> Class 11 6.67e-17                     
#> Class 12 2.23e-01                     
#> Class 13 1.00e+00      1      1      1
#> Local Dependence Passing Student Rate
#>     Field Field Item 1 Field Item 2 Field Item 3 Field Item 4 Parent Class
#> 1   1.000       Item01       Item31       Item32                     1.000
#> 2   2.000       Item11       Item21       Item22                     2.000
#> 3   2.000       Item11       Item21       Item22                     3.000
#> 4   3.000       Item23       Item24                                  4.000
#> 5   4.000       Item25       Item26       Item27                     5.000
#> 6   4.000       Item25       Item26       Item27                     7.000
#> 7   5.000       Item02       Item03       Item04                     2.000
#> 8   5.000       Item02       Item03       Item04                     4.000
#> 9   5.000       Item02       Item03       Item04                     6.000
#> 10  5.000       Item02       Item03       Item04                     8.000
#> 11  5.000       Item02       Item03       Item04                    10.000
#> 12  7.000       Item12       Item13       Item16       Item17        6.000
#> 13  8.000       Item28       Item29                                  6.000
#> 14  8.000       Item28       Item29                                 11.000
#> 15  9.000       Item05       Item06                                  8.000
#> 16  9.000       Item05       Item06                                  9.000
#> 17 12.000       Item18       Item19       Item20       Item30       12.000
#>    Parent CCR 1 Parent CCR 2 Parent CCR 3 Parent CCR 4 Child Class Child CCR 1
#> 1         0.000        0.000        0.000                    2.000       0.553
#> 2         0.006        0.022        0.004                    4.000       0.683
#> 3         0.031        0.061        0.014                    5.000       0.405
#> 4         0.222        0.188                                 5.000       0.183
#> 5         0.147        0.051        0.145                    6.000       0.616
#> 6         0.999        0.988        0.932                   11.000       0.942
#> 7         0.004        0.037        0.002                    3.000       0.072
#> 8         0.996        0.997        0.892                    7.000       0.151
#> 9         0.287        0.343        0.066                    9.000       0.291
#> 10        0.992        0.983        0.938                   12.000       1.000
#> 11        0.941        0.797        0.727                   12.000       1.000
#> 12        0.176        0.141        0.034        0.159      10.000       0.982
#> 13        0.007        0.122                                 8.000       0.193
#> 14        0.364        0.277                                12.000       1.000
#> 15        0.238        0.132                                12.000       0.796
#> 16        0.000        0.000                                11.000       1.000
#> 17        0.160        0.168        0.207        0.358      13.000       1.000
#>    Child CCR 2 Child CCR 3 Child CCR 4
#> 1        0.556       0.647            
#> 2        0.786       0.638            
#> 3        0.726       0.695            
#> 4        0.251                        
#> 5        0.442       0.181            
#> 6        0.935       0.767            
#> 7        0.332       0.068            
#> 8        0.270       0.006            
#> 9        0.294       0.118            
#> 10       1.000       1.000            
#> 11       1.000       1.000            
#> 12       0.881       0.932       0.975
#> 13       0.222                        
#> 14       1.000                        
#> 15       0.717                        
#> 16       1.000                        
#> 17       1.000       1.000       1.000
#> Marginal Bicluster Reference Matrix
#>         Class1 Class2 Class3 Class4 Class5 Class6 Class7 Class8 Class9 Class10
#> Field1       0  0.586  0.740  0.859  0.874  0.906  0.868  0.898  0.961   0.933
#> Field2       0  0.011  0.035  0.702  0.609  0.684  0.830  0.831  1.000   0.983
#> Field3       0  0.176  0.220  0.205  0.217  1.000  1.000  1.000  0.986   1.000
#> Field4       0  0.011  0.124  0.047  0.114  0.413  0.973  0.962  0.976   0.965
#> Field5       0  0.015  0.157  0.962  0.104  0.232  0.142  0.971  0.234   0.822
#> Field6       0  0.235  0.277  0.450  0.409  0.301  0.418  0.488  0.559   0.562
#> Field7       0  0.072  0.082  0.194  0.159  0.127  0.128  0.186  0.753   0.943
#> Field8       0  0.000  0.037  0.039  0.043  0.064  0.444  0.208  0.615   0.378
#> Field9       0  0.000  0.016  0.617  0.031  0.078  0.000  0.185  0.000   0.804
#> Field10      0  0.095  0.180  0.173  0.157  0.178  0.121  0.153  0.224   0.258
#> Field11      0  0.000  0.000  0.081  0.024  0.021  0.024  0.000  0.245   0.427
#> Field12      0  0.003  0.056  0.000  0.019  0.020  0.020  0.050  0.018   0.026
#>         Class11 Class12 Class13
#> Field1    0.897   0.973       1
#> Field2    1.000   1.000       1
#> Field3    0.974   1.000       1
#> Field4    0.881   1.000       1
#> Field5    0.917   1.000       1
#> Field6    0.612   0.762       1
#> Field7    0.306   0.803       1
#> Field8    0.320   1.000       1
#> Field9    1.000   0.756       1
#> Field10   0.176   0.319       1
#> Field11   0.039   0.601       1
#> Field12   0.000   0.223       1
#>                               Class 1 Class 2 Class 3 Class 4 Class 5 Class 6
#> Test Reference Profile          0.000   3.883   6.009  12.968   8.805  11.479
#> Latent Class Ditribution        2.000  96.000  71.000  36.000  60.000  48.000
#> Class Membership Dsitribution   1.986  82.318  86.009  37.289  61.103  44.277
#>                               Class 7 Class 8 Class 9 Class 10 Class 11
#> Test Reference Profile         14.301  17.457  19.527   23.527   20.386
#> Latent Class Ditribution       44.000  28.000  35.000   18.000   36.000
#> Class Membership Dsitribution  44.056  28.710  34.422   19.801   34.927
#>                               Class 12 Class 13
#> Test Reference Profile          27.185       35
#> Latent Class Ditribution        26.000       15
#> Class Membership Dsitribution   25.103       15
#> 
#> Model Fit Indices
#>                Multigroup Model Saturated Moodel
#> model_log_like -5782.556        -5782.556       
#> bench_log_like -5891.314        0               
#> null_log_like  -9862.114        -9862.114       
#> model_Chi_sq   -217.517         11565.11        
#> null_Chi_sq    7941.601         19724.23        
#> model_df       1005             16895           
#> null_df        1155             17045           
#> NFI            1                0.4136596       
#> RFI            1                0.4084539       
#> IFI            1                1               
#> TLI            1                1               
#> CFI            1                1               
#> RMSEA          0                0               
#> AIC            -2227.517        -22224.89       
#> CAIC           -7497.905        -110825.1       
#> BIC            -6492.905        -93930.09       

# Visualize different aspects of the model
plot(result.BINET, type = "Array") # Show bicluster structure

plot(result.BINET, type = "TRP") # Test Response Profile

plot(result.BINET, type = "LRD") # Latent Rank Distribution

plot(result.BINET,
  type = "RMP", # Rank Membership Profiles
  students = 1:9, nc = 3, nr = 3
)

plot(result.BINET,
  type = "FRP", # Field Reference Profiles
  nc = 3, nr = 2
)


plot(result.BINET,
  type = "LDPSR", # Locally Dependent Passing Student Rates
  nc = 3, nr = 2
)



# }
```
