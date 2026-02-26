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
  Z = NULL,
  w = NULL,
  na = NULL,
  conf = NULL,
  ncls = NULL,
  nfld = NULL,
  g_list = NULL,
  adj_list = NULL,
  adj_file = NULL,
  verbose = FALSE
)
```

## Arguments

- U:

  U is either a data class of exametrika, or raw data. When raw data is
  given, it is converted to the exametrika class with the
  [dataFormat](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  function.

- Z:

  Z is a missing indicator matrix of the type matrix or data.frame

- w:

  w is item weight vector

- na:

  na argument specifies the numbers or characters to be treated as
  missing values.

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

  verbose output Flag. default is TRUE

## Value

- nobs:

  Sample size. The number of rows in the dataset.

- msg:

  A character string indicating the model type.

- testlength:

  Length of the test. The number of items included in the test.

- Nclass:

  Optimal number of classes.

- Nfield:

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
#> No ID column detected. All columns treated as response data. Sequential IDs (Student1, Student2, ...) were generated. Use id= parameter to specify the ID column explicitly.
#> No ID column detected. All columns treated as response data. Sequential IDs (Student1, Student2, ...) were generated. Use id= parameter to specify the ID column explicitly.

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
#> Class 2   0.554  0.558  0.649       
#> Class 3   0.740                     
#> Class 4   0.859                     
#> Class 5   0.875                     
#> Class 6   0.910                     
#> Class 7   0.868                     
#> Class 8   0.889                     
#> Class 9   0.961                     
#> Class 10  0.932                     
#> Class 11  0.898                     
#> Class 12  0.975                     
#> Class 13  1.000                     
#> Field 2 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.0000                     
#> Class 2  0.0090                     
#> Class 3  0.0396                     
#> Class 4  0.6813  0.785  0.637       
#> Class 5  0.4040  0.728  0.696       
#> Class 6  0.6877                     
#> Class 7  0.8316                     
#> Class 8  0.8218                     
#> Class 9  1.0000                     
#> Class 10 0.9836                     
#> Class 11 1.0000                     
#> Class 12 1.0000                     
#> Class 13 1.0000                     
#> Field 3 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1   0.000                     
#> Class 2   0.177                     
#> Class 3   0.219                     
#> Class 4   0.206                     
#> Class 5   0.189  0.253              
#> Class 6   1.000                     
#> Class 7   1.000                     
#> Class 8   1.000                     
#> Class 9   0.986                     
#> Class 10  1.000                     
#> Class 11  0.973                     
#> Class 12  1.000                     
#> Class 13  1.000                     
#> Field 4 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.0000                     
#> Class 2  0.0127                     
#> Class 3  0.1228                     
#> Class 4  0.0468                     
#> Class 5  0.1131                     
#> Class 6  0.6131  0.436  0.179       
#> Class 7  0.9775                     
#> Class 8  0.9539                     
#> Class 9  0.9751                     
#> Class 10 0.9660                     
#> Class 11 0.9411  0.925  0.757       
#> Class 12 1.0000                     
#> Class 13 1.0000                     
#> Field 5 
#>          PSRP 1 PSRP 2  PSRP 3 PSRP 4
#> Class 1  0.0000                      
#> Class 2  0.0157                      
#> Class 3  0.0731  0.330 0.06789       
#> Class 4  0.9626                      
#> Class 5  0.1028                      
#> Class 6  0.2199                      
#> Class 7  0.1446  0.265 0.00602       
#> Class 8  0.9403                      
#> Class 9  0.2936  0.298 0.12080       
#> Class 10 0.8255                      
#> Class 11 0.9123                      
#> Class 12 1.0000  1.000 1.00000       
#> Class 13 1.0000                      
#> Field 6 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1   0.000                     
#> Class 2   0.236                     
#> Class 3   0.275                     
#> Class 4   0.449                     
#> Class 5   0.414                     
#> Class 6   0.302                     
#> Class 7   0.415                     
#> Class 8   0.469                     
#> Class 9   0.560                     
#> Class 10  0.564                     
#> Class 11  0.614                     
#> Class 12  0.764                     
#> Class 13  1.000                     
#> Field 7 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.0000                     
#> Class 2  0.0731                     
#> Class 3  0.0810                     
#> Class 4  0.1924                     
#> Class 5  0.1596                     
#> Class 6  0.1316                     
#> Class 7  0.1263                     
#> Class 8  0.1792                     
#> Class 9  0.7542                     
#> Class 10 0.9818  0.883  0.933  0.975
#> Class 11 0.3047                     
#> Class 12 0.7862                     
#> Class 13 1.0000                     
#> Field 8 
#>            PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.00e+00                     
#> Class 2  9.83e-05                     
#> Class 3  3.70e-02                     
#> Class 4  3.91e-02                     
#> Class 5  4.21e-02                     
#> Class 6  6.88e-02                     
#> Class 7  4.56e-01                     
#> Class 8  1.65e-01  0.192              
#> Class 9  6.15e-01                     
#> Class 10 3.88e-01                     
#> Class 11 3.16e-01                     
#> Class 12 1.00e+00  1.000              
#> Class 13 1.00e+00                     
#> Field 9 
#>            PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.00e+00                     
#> Class 2  2.47e-16                     
#> Class 3  1.61e-02                     
#> Class 4  6.15e-01                     
#> Class 5  3.46e-02                     
#> Class 6  5.26e-02                     
#> Class 7  1.44e-11                     
#> Class 8  2.09e-01                     
#> Class 9  9.51e-18                     
#> Class 10 8.09e-01                     
#> Class 11 1.00e+00  1.000              
#> Class 12 7.81e-01  0.703              
#> Class 13 1.00e+00                     
#> Field 10 
#>          PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.0000                     
#> Class 2  0.0952                     
#> Class 3  0.1798                     
#> Class 4  0.1741                     
#> Class 5  0.1594                     
#> Class 6  0.1789                     
#> Class 7  0.1208                     
#> Class 8  0.1550                     
#> Class 9  0.2228                     
#> Class 10 0.2602                     
#> Class 11 0.1724                     
#> Class 12 0.3109                     
#> Class 13 1.0000                     
#> Field 11 
#>            PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.00e+00                     
#> Class 2  6.09e-14                     
#> Class 3  8.84e-07                     
#> Class 4  8.14e-02                     
#> Class 5  2.46e-02                     
#> Class 6  2.13e-02                     
#> Class 7  2.56e-02                     
#> Class 8  2.76e-16                     
#> Class 9  2.44e-01                     
#> Class 10 4.30e-01                     
#> Class 11 3.84e-02                     
#> Class 12 5.86e-01                     
#> Class 13 1.00e+00                     
#> Field 12 
#>            PSRP 1 PSRP 2 PSRP 3 PSRP 4
#> Class 1  0.00e+00                     
#> Class 2  2.35e-03                     
#> Class 3  5.57e-02                     
#> Class 4  0.00e+00                     
#> Class 5  2.02e-02                     
#> Class 6  1.67e-02                     
#> Class 7  1.93e-02                     
#> Class 8  4.62e-02                     
#> Class 9  1.85e-02                     
#> Class 10 2.54e-02                     
#> Class 11 5.68e-15                     
#> Class 12 2.26e-01                     
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
#> 1         0.000        0.000        0.000                    2.000       0.554
#> 2         0.005        0.018        0.003                    4.000       0.681
#> 3         0.034        0.068        0.016                    5.000       0.404
#> 4         0.221        0.190                                 5.000       0.189
#> 5         0.147        0.050        0.142                    6.000       0.613
#> 6         0.999        0.991        0.943                   11.000       0.941
#> 7         0.005        0.040        0.002                    3.000       0.073
#> 8         0.996        0.998        0.893                    7.000       0.145
#> 9         0.263        0.334        0.063                    9.000       0.294
#> 10        0.980        0.958        0.882                   12.000       1.000
#> 11        0.943        0.800        0.733                   12.000       1.000
#> 12        0.181        0.146        0.037        0.162      10.000       0.982
#> 13        0.009        0.129                                 8.000       0.165
#> 14        0.359        0.273                                12.000       1.000
#> 15        0.266        0.152                                12.000       0.781
#> 16        0.000        0.000                                11.000       1.000
#> 17        0.158        0.178        0.217        0.352      13.000       1.000
#>    Child CCR 2 Child CCR 3 Child CCR 4
#> 1        0.558       0.649            
#> 2        0.785       0.637            
#> 3        0.728       0.696            
#> 4        0.253                        
#> 5        0.436       0.179            
#> 6        0.925       0.757            
#> 7        0.330       0.068            
#> 8        0.265       0.006            
#> 9        0.298       0.121            
#> 10       1.000       1.000            
#> 11       1.000       1.000            
#> 12       0.883       0.933       0.975
#> 13       0.192                        
#> 14       1.000                        
#> 15       0.703                        
#> 16       1.000                        
#> 17       1.000       1.000       1.000
#> Marginal Bicluster Reference Matrix
#>         Class1 Class2 Class3 Class4 Class5 Class6 Class7 Class8 Class9 Class10
#> Field1       0  0.587  0.740  0.859  0.875  0.910  0.868  0.889  0.961   0.932
#> Field2       0  0.009  0.040  0.701  0.609  0.688  0.832  0.822  1.000   0.984
#> Field3       0  0.177  0.219  0.206  0.221  1.000  1.000  1.000  0.986   1.000
#> Field4       0  0.013  0.123  0.047  0.113  0.410  0.978  0.954  0.975   0.966
#> Field5       0  0.016  0.157  0.963  0.103  0.220  0.138  0.940  0.237   0.825
#> Field6       0  0.236  0.275  0.449  0.414  0.302  0.415  0.469  0.560   0.564
#> Field7       0  0.073  0.081  0.192  0.160  0.132  0.126  0.179  0.754   0.943
#> Field8       0  0.000  0.037  0.039  0.042  0.069  0.456  0.179  0.615   0.388
#> Field9       0  0.000  0.016  0.615  0.035  0.053  0.000  0.209  0.000   0.809
#> Field10      0  0.095  0.180  0.174  0.159  0.179  0.121  0.155  0.223   0.260
#> Field11      0  0.000  0.000  0.081  0.025  0.021  0.026  0.000  0.244   0.430
#> Field12      0  0.002  0.056  0.000  0.020  0.017  0.019  0.046  0.019   0.025
#>         Class11 Class12 Class13
#> Field1    0.898   0.975       1
#> Field2    1.000   1.000       1
#> Field3    0.973   1.000       1
#> Field4    0.874   1.000       1
#> Field5    0.912   1.000       1
#> Field6    0.614   0.764       1
#> Field7    0.305   0.786       1
#> Field8    0.316   1.000       1
#> Field9    1.000   0.742       1
#> Field10   0.172   0.311       1
#> Field11   0.038   0.586       1
#> Field12   0.000   0.226       1
#>                               Class 1 Class 2 Class 3 Class 4 Class 5 Class 6
#> Test Reference Profile          0.000   3.900   6.001  12.951   8.853  11.428
#> Latent Class Ditribution        2.000  95.000  73.000  37.000  60.000  44.000
#> Class Membership Dsitribution   1.987  82.567  86.281  37.258  60.781  43.222
#>                               Class 7 Class 8 Class 9 Class 10 Class 11
#> Test Reference Profile         14.305  17.148  19.544   23.589   20.343
#> Latent Class Ditribution       43.000  30.000  34.000   18.000   37.000
#> Class Membership Dsitribution  43.062  30.087  34.435   20.063   34.811
#>                               Class 12 Class 13
#> Test Reference Profile          27.076       35
#> Latent Class Ditribution        27.000       15
#> Class Membership Dsitribution   25.445       15
#> 
#> Model Fit Indices
#>                Multigroup Model Saturated Moodel
#> model_log_like -5786.942        -5786.942       
#> bench_log_like -5891.314        0               
#> null_log_like  -9862.114        -9862.114       
#> model_Chi_sq   -208.744         11573.88        
#> null_Chi_sq    7941.601         19724.23        
#> model_df       1005             16895           
#> null_df        1155             17045           
#> NFI            1                0.4132149       
#> RFI            1                0.4080052       
#> IFI            1                1               
#> TLI            1                1               
#> CFI            1                1               
#> RMSEA          0                0               
#> AIC            -2218.744        -22216.12       
#> CAIC           -7489.132        -110816.3       
#> BIC            -6484.132        -93921.32       

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
