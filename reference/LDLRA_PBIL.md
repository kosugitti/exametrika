# Structure Learning for LDLRA by PBIL algorithm

Generating DAG list from data using Population-Based Incremental
learning

## Usage

``` r
LDLRA_PBIL(
  U,
  Z = NULL,
  w = NULL,
  na = NULL,
  seed = 123,
  ncls = 2,
  method = "R",
  population = 20,
  Rs = 0.5,
  Rm = 0.002,
  maxParents = 2,
  maxGeneration = 100,
  successiveLimit = 5,
  elitism = 0,
  alpha = 0.05,
  estimate = 1,
  filename = NULL,
  verbose = TRUE,
  beta1 = 2,
  beta2 = 2
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

- seed:

  seed for random.

- ncls:

  number of latent class(rank). The default is 2.

- method:

  specify the model to analyze the data.Local dependence latent class
  model is set to "C", latent rank model is set "R". The default is "R".

- population:

  Population size. The default is 20

- Rs:

  Survival Rate. The default is 0.5

- Rm:

  Mutation Rate. The default is 0.002

- maxParents:

  Maximum number of edges emanating from a single node. The default is
  2.

- maxGeneration:

  Maximum number of generations.

- successiveLimit:

  Termination conditions. If the optimal individual does not change for
  this number of generations, it is considered to have converged.

- elitism:

  Number of elites that remain without crossover when transitioning to
  the next generation.

- alpha:

  Learning rate. The default is 0.05

- estimate:

  In PBIL for estimating the adjacency matrix, specify by number from
  the following four methods: 1. Optimal adjacency matrix, 2. Rounded
  average of individuals in the last generation, 3. Rounded average of
  survivors in the last generation, 4. Rounded generational gene of the
  last generation. The default is 1.

- filename:

  Specify the filename when saving the generated adjacency matrix in CSV
  format. The default is null, and no output is written to the file.

- verbose:

  verbose output Flag. default is TRUE

- beta1:

  Beta distribution parameter 1 for prior density. Default is 2.

- beta2:

  Beta distribution parameter 2 for prior density. Default is 2.

## Value

- nobs:

  Sample size. The number of rows in the dataset.

- testlength:

  Length of the test. The number of items included in the test.

- crr:

  correct response ratio

- adj_list:

  adjacency matrix list

- g_list:

  graph list

- referenceMatrix:

  Learned Parameters.A three-dimensional array of patterns where item x
  rank x pattern.

- IRP:

  Marginal Item Reference Matrix

- IRPIndex:

  IRP Indices which include Alpha, Beta, Gamma.

- TRP:

  Test Reference Profile matrix.

- LRD:

  latent Rank/Class Distribution

- RMD:

  Rank/Class Membership Distribution

- TestFitIndices:

  Overall fit index for the test.See also
  [TestFit](https://kosugitti.github.io/exametrika/reference/TestFit.md)

- Estimation_table:

  Estimated parameters tables.

- CCRR_table:

  Correct Response Rate tables

- Studens:

  Student information. It includes estimated class membership,
  probability of class membership, RUO, and RDO.

## Details

This function performs structural learning for each classes by using the
Population-Based Incremental Learning model(PBIL) proposed by Fukuda et
al.(2014) within the genetic algorithm framework. Instead of learning
the adjacency matrix itself, the 'genes of genes' that generate the
adjacency matrix are updated with each generation. For more details,
please refer to Fukuda(2014) and Section 9.4.3 of the
text(Shojima,2022).

## References

Fukuda, S., Yamanaka, Y., & Yoshihiro, T. (2014). A Probability-based
evolutionary algorithm with mutations to learn Bayesian networks.
International Journal of Artificial Intelligence and Interactive
Multimedia, 3, 7–13. DOI: 10.9781/ijimai.2014.311

## Examples

``` r
# \donttest{
# Perform Structure Learning for LDLRA using PBIL algorithm
# This process may take considerable time due to evolutionary optimization
result.LDLRA.PBIL <- LDLRA_PBIL(J35S515,
  seed = 123, # Set random seed for reproducibility
  ncls = 5, # Number of latent ranks
  maxGeneration = 10,
  method = "R", # Use rank model (vs. class model)
  elitism = 1, # Keep best solution in each generation
  successiveLimit = 15 # Convergence criterion
)
#> local dependence latent Rank model is chosen.
#> Warning: Too many survivers. Limit to 5
#> Gen 1 ID.2 BIC -422.327 BEST -709.604 limit count 0                             
#> Gen 1 ID.3 BIC -526.255 BEST -709.604 limit count 0                             
#> Gen 1 ID.4 BIC -419.884 BEST -709.604 limit count 0                             
#> Gen 1 ID.5 BIC -427.353 BEST -709.604 limit count 0                             
#> Gen 1 ID.6 BIC -545.166 BEST -709.604 limit count 0                             
#> Gen 1 ID.7 BIC -569.055 BEST -709.604 limit count 0                             
#> Gen 1 ID.8 BIC -320.897 BEST -709.604 limit count 0                             
#> Gen 1 ID.9 BIC -468.407 BEST -709.604 limit count 0                             
#> Gen 1 ID.10 BIC -273.710 BEST -709.604 limit count 0                            
#> Gen 1 ID.11 BIC -520.677 BEST -709.604 limit count 0                            
#> Gen 1 ID.12 BIC -277.125 BEST -709.604 limit count 0                            
#> Gen 1 ID.13 BIC -449.793 BEST -709.604 limit count 0                            
#> Gen 1 ID.14 BIC -513.190 BEST -709.604 limit count 0                            
#> Gen 1 ID.15 BIC -316.163 BEST -709.604 limit count 0                            
#> Gen 1 ID.16 BIC -455.311 BEST -709.604 limit count 0                            
#> Gen 1 ID.17 BIC -308.502 BEST -709.604 limit count 0                            
#> Gen 1 ID.18 BIC -302.723 BEST -709.604 limit count 0                            
#> Gen 1 ID.19 BIC -377.327 BEST -709.604 limit count 0                            
#> Gen 1 ID.20 BIC -656.913 BEST -709.604 limit count 0                            
#> Gen 2 ID.2 BIC -453.639 BEST -709.604 limit count 1                             
#> Gen 2 ID.3 BIC -515.942 BEST -709.604 limit count 1                             
#> Gen 2 ID.4 BIC -640.801 BEST -709.604 limit count 1                             
#> Gen 2 ID.5 BIC -523.601 BEST -709.604 limit count 1                             
#> Gen 2 ID.6 BIC -546.148 BEST -709.604 limit count 1                             
#> Gen 2 ID.7 BIC -467.266 BEST -709.604 limit count 1                             
#> Gen 2 ID.8 BIC -717.150 BEST -709.604 limit count 1                             
#> Gen 2 ID.9 BIC -306.418 BEST -709.604 limit count 1                             
#> Gen 2 ID.10 BIC -468.441 BEST -709.604 limit count 1                            
#> Gen 2 ID.11 BIC -673.189 BEST -709.604 limit count 1                            
#> Gen 2 ID.12 BIC -321.404 BEST -709.604 limit count 1                            
#> Gen 2 ID.13 BIC -495.686 BEST -709.604 limit count 1                            
#> Gen 2 ID.14 BIC -600.350 BEST -709.604 limit count 1                            
#> Gen 2 ID.15 BIC -364.983 BEST -709.604 limit count 1                            
#> Gen 2 ID.16 BIC -343.162 BEST -709.604 limit count 1                            
#> Gen 2 ID.17 BIC -386.411 BEST -709.604 limit count 1                            
#> Gen 2 ID.18 BIC -449.807 BEST -709.604 limit count 1                            
#> Gen 2 ID.19 BIC -452.180 BEST -709.604 limit count 1                            
#> Gen 2 ID.20 BIC -467.237 BEST -709.604 limit count 1                            
#> Gen 3 ID.2 BIC -414.726 BEST -717.150 limit count 0                             
#> Gen 3 ID.3 BIC -434.751 BEST -717.150 limit count 0                             
#> Gen 3 ID.4 BIC -524.950 BEST -717.150 limit count 0                             
#> Gen 3 ID.5 BIC -502.991 BEST -717.150 limit count 0                             
#> Gen 3 ID.6 BIC -539.057 BEST -717.150 limit count 0                             
#> Gen 3 ID.7 BIC -302.488 BEST -717.150 limit count 0                             
#> Gen 3 ID.8 BIC -280.548 BEST -717.150 limit count 0                             
#> Gen 3 ID.9 BIC -560.465 BEST -717.150 limit count 0                             
#> Gen 3 ID.10 BIC -513.876 BEST -717.150 limit count 0                            
#> Gen 3 ID.11 BIC -486.990 BEST -717.150 limit count 0                            
#> Gen 3 ID.12 BIC -405.836 BEST -717.150 limit count 0                            
#> Gen 3 ID.13 BIC -411.440 BEST -717.150 limit count 0                            
#> Gen 3 ID.14 BIC -540.975 BEST -717.150 limit count 0                            
#> Gen 3 ID.15 BIC -400.949 BEST -717.150 limit count 0                            
#> Gen 3 ID.16 BIC -567.635 BEST -717.150 limit count 0                            
#> Gen 3 ID.17 BIC -456.976 BEST -717.150 limit count 0                            
#> Gen 3 ID.18 BIC -256.588 BEST -717.150 limit count 0                            
#> Gen 3 ID.19 BIC -500.122 BEST -717.150 limit count 0                            
#> Gen 3 ID.20 BIC -570.932 BEST -717.150 limit count 0                            
#> Gen 4 ID.2 BIC -547.669 BEST -717.150 limit count 1                             
#> Gen 4 ID.3 BIC -356.111 BEST -717.150 limit count 1                             
#> Gen 4 ID.4 BIC -423.911 BEST -717.150 limit count 1                             
#> Gen 4 ID.5 BIC -484.318 BEST -717.150 limit count 1                             
#> Gen 4 ID.6 BIC -501.737 BEST -717.150 limit count 1                             
#> Gen 4 ID.7 BIC -418.426 BEST -717.150 limit count 1                             
#> Gen 4 ID.8 BIC -661.804 BEST -717.150 limit count 1                             
#> Gen 4 ID.9 BIC -408.163 BEST -717.150 limit count 1                             
#> Gen 4 ID.10 BIC -392.889 BEST -717.150 limit count 1                            
#> Gen 4 ID.11 BIC -272.223 BEST -717.150 limit count 1                            
#> Gen 4 ID.12 BIC -370.923 BEST -717.150 limit count 1                            
#> Gen 4 ID.13 BIC -391.890 BEST -717.150 limit count 1                            
#> Gen 4 ID.14 BIC -306.415 BEST -717.150 limit count 1                            
#> Gen 4 ID.15 BIC -589.410 BEST -717.150 limit count 1                            
#> Gen 4 ID.16 BIC -328.944 BEST -717.150 limit count 1                            
#> Gen 4 ID.17 BIC -449.927 BEST -717.150 limit count 1                            
#> Gen 4 ID.18 BIC -307.293 BEST -717.150 limit count 1                            
#> Gen 4 ID.19 BIC -383.285 BEST -717.150 limit count 1                            
#> Gen 4 ID.20 BIC -501.448 BEST -717.150 limit count 1                            
#> Gen 5 ID.2 BIC -483.929 BEST -717.150 limit count 2                             
#> Gen 5 ID.3 BIC -486.842 BEST -717.150 limit count 2                             
#> Gen 5 ID.4 BIC -461.940 BEST -717.150 limit count 2                             
#> Gen 5 ID.5 BIC -363.225 BEST -717.150 limit count 2                             
#> Gen 5 ID.6 BIC -458.415 BEST -717.150 limit count 2                             
#> Gen 5 ID.7 BIC -401.020 BEST -717.150 limit count 2                             
#> Gen 5 ID.8 BIC -384.352 BEST -717.150 limit count 2                             
#> Gen 5 ID.9 BIC -425.682 BEST -717.150 limit count 2                             
#> Gen 5 ID.10 BIC -420.133 BEST -717.150 limit count 2                            
#> Gen 5 ID.11 BIC -524.994 BEST -717.150 limit count 2                            
#> Gen 5 ID.12 BIC -563.555 BEST -717.150 limit count 2                            
#> Gen 5 ID.13 BIC -509.630 BEST -717.150 limit count 2                            
#> Gen 5 ID.14 BIC -506.360 BEST -717.150 limit count 2                            
#> Gen 5 ID.15 BIC -486.600 BEST -717.150 limit count 2                            
#> Gen 5 ID.16 BIC -385.122 BEST -717.150 limit count 2                            
#> Gen 5 ID.17 BIC -340.111 BEST -717.150 limit count 2                            
#> Gen 5 ID.18 BIC -497.859 BEST -717.150 limit count 2                            
#> Gen 5 ID.19 BIC -485.895 BEST -717.150 limit count 2                            
#> Gen 5 ID.20 BIC -535.386 BEST -717.150 limit count 2                            
#> Gen 6 ID.2 BIC -397.522 BEST -717.150 limit count 3                             
#> Gen 6 ID.3 BIC -327.956 BEST -717.150 limit count 3                             
#> Gen 6 ID.4 BIC -515.616 BEST -717.150 limit count 3                             
#> Gen 6 ID.5 BIC -580.154 BEST -717.150 limit count 3                             
#> Gen 6 ID.6 BIC -406.641 BEST -717.150 limit count 3                             
#> Gen 6 ID.7 BIC -420.194 BEST -717.150 limit count 3                             
#> Gen 6 ID.8 BIC -333.668 BEST -717.150 limit count 3                             
#> Gen 6 ID.9 BIC -519.661 BEST -717.150 limit count 3                             
#> Gen 6 ID.10 BIC -609.741 BEST -717.150 limit count 3                            
#> Gen 6 ID.11 BIC -417.211 BEST -717.150 limit count 3                            
#> Gen 6 ID.12 BIC -513.470 BEST -717.150 limit count 3                            
#> Gen 6 ID.13 BIC -490.287 BEST -717.150 limit count 3                            
#> Gen 6 ID.14 BIC -481.605 BEST -717.150 limit count 3                            
#> Gen 6 ID.15 BIC -549.045 BEST -717.150 limit count 3                            
#> Gen 6 ID.16 BIC -445.122 BEST -717.150 limit count 3                            
#> Gen 6 ID.17 BIC -469.131 BEST -717.150 limit count 3                            
#> Gen 6 ID.18 BIC -360.529 BEST -717.150 limit count 3                            
#> Gen 6 ID.19 BIC -487.935 BEST -717.150 limit count 3                            
#> Gen 6 ID.20 BIC -413.372 BEST -717.150 limit count 3                            
#> Gen 7 ID.2 BIC -566.711 BEST -717.150 limit count 4                             
#> Gen 7 ID.3 BIC -501.363 BEST -717.150 limit count 4                             
#> Gen 7 ID.4 BIC -491.278 BEST -717.150 limit count 4                             
#> Gen 7 ID.5 BIC -519.399 BEST -717.150 limit count 4                             
#> Gen 7 ID.6 BIC -679.150 BEST -717.150 limit count 4                             
#> Gen 7 ID.7 BIC -472.540 BEST -717.150 limit count 4                             
#> Gen 7 ID.8 BIC -364.919 BEST -717.150 limit count 4                             
#> Gen 7 ID.9 BIC -646.398 BEST -717.150 limit count 4                             
#> Gen 7 ID.10 BIC -552.374 BEST -717.150 limit count 4                            
#> Gen 7 ID.11 BIC -577.703 BEST -717.150 limit count 4                            
#> Gen 7 ID.12 BIC -544.654 BEST -717.150 limit count 4                            
#> Gen 7 ID.13 BIC -465.313 BEST -717.150 limit count 4                            
#> Gen 7 ID.14 BIC -414.232 BEST -717.150 limit count 4                            
#> Gen 7 ID.15 BIC -478.800 BEST -717.150 limit count 4                            
#> Gen 7 ID.16 BIC -560.269 BEST -717.150 limit count 4                            
#> Gen 7 ID.17 BIC -336.031 BEST -717.150 limit count 4                            
#> Gen 7 ID.18 BIC -336.339 BEST -717.150 limit count 4                            
#> Gen 7 ID.19 BIC -340.626 BEST -717.150 limit count 4                            
#> Gen 7 ID.20 BIC -522.170 BEST -717.150 limit count 4                            
#> Gen 8 ID.2 BIC -603.689 BEST -717.150 limit count 5                             
#> Gen 8 ID.3 BIC -359.055 BEST -717.150 limit count 5                             
#> Gen 8 ID.4 BIC -791.319 BEST -717.150 limit count 5                             
#> Gen 8 ID.5 BIC -480.978 BEST -717.150 limit count 5                             
#> Gen 8 ID.6 BIC -491.674 BEST -717.150 limit count 5                             
#> Gen 8 ID.7 BIC -622.546 BEST -717.150 limit count 5                             
#> Gen 8 ID.8 BIC -567.817 BEST -717.150 limit count 5                             
#> Gen 8 ID.9 BIC -646.625 BEST -717.150 limit count 5                             
#> Gen 8 ID.10 BIC -350.987 BEST -717.150 limit count 5                            
#> Gen 8 ID.11 BIC -554.654 BEST -717.150 limit count 5                            
#> Gen 8 ID.12 BIC -551.042 BEST -717.150 limit count 5                            
#> Gen 8 ID.13 BIC -417.637 BEST -717.150 limit count 5                            
#> Gen 8 ID.14 BIC -341.436 BEST -717.150 limit count 5                            
#> Gen 8 ID.15 BIC -387.708 BEST -717.150 limit count 5                            
#> Gen 8 ID.16 BIC -543.029 BEST -717.150 limit count 5                            
#> Gen 8 ID.17 BIC -363.872 BEST -717.150 limit count 5                            
#> Gen 8 ID.18 BIC -544.050 BEST -717.150 limit count 5                            
#> Gen 8 ID.19 BIC -512.891 BEST -717.150 limit count 5                            
#> Gen 8 ID.20 BIC -615.260 BEST -717.150 limit count 5                            
#> Gen 9 ID.2 BIC -585.402 BEST -791.319 limit count 0                             
#> Gen 9 ID.3 BIC -388.290 BEST -791.319 limit count 0                             
#> Gen 9 ID.4 BIC -487.699 BEST -791.319 limit count 0                             
#> Gen 9 ID.5 BIC -492.375 BEST -791.319 limit count 0                             
#> Gen 9 ID.6 BIC -503.756 BEST -791.319 limit count 0                             
#> Gen 9 ID.7 BIC -540.984 BEST -791.319 limit count 0                             
#> Gen 9 ID.8 BIC -553.193 BEST -791.319 limit count 0                             
#> Gen 9 ID.9 BIC -409.575 BEST -791.319 limit count 0                             
#> Gen 9 ID.10 BIC -415.866 BEST -791.319 limit count 0                            
#> Gen 9 ID.11 BIC -737.831 BEST -791.319 limit count 0                            
#> Gen 9 ID.12 BIC -627.219 BEST -791.319 limit count 0                            
#> Gen 9 ID.13 BIC -440.463 BEST -791.319 limit count 0                            
#> Gen 9 ID.14 BIC -505.030 BEST -791.319 limit count 0                            
#> Gen 9 ID.15 BIC -312.347 BEST -791.319 limit count 0                            
#> Gen 9 ID.16 BIC -569.392 BEST -791.319 limit count 0                            
#> Gen 9 ID.17 BIC -561.062 BEST -791.319 limit count 0                            
#> Gen 9 ID.18 BIC -539.849 BEST -791.319 limit count 0                            
#> Gen 9 ID.19 BIC -534.001 BEST -791.319 limit count 0                            
#> Gen 9 ID.20 BIC -380.962 BEST -791.319 limit count 0                            
#> Gen 10 ID.2 BIC -527.606 BEST -791.319 limit count 1                            
#> Gen 10 ID.3 BIC -531.567 BEST -791.319 limit count 1                            
#> Gen 10 ID.4 BIC -606.660 BEST -791.319 limit count 1                            
#> Gen 10 ID.5 BIC -468.857 BEST -791.319 limit count 1                            
#> Gen 10 ID.6 BIC -616.972 BEST -791.319 limit count 1                            
#> Gen 10 ID.7 BIC -399.066 BEST -791.319 limit count 1                            
#> Gen 10 ID.8 BIC -498.710 BEST -791.319 limit count 1                            
#> Gen 10 ID.9 BIC -418.221 BEST -791.319 limit count 1                            
#> Gen 10 ID.10 BIC -493.903 BEST -791.319 limit count 1                           
#> Gen 10 ID.11 BIC -536.070 BEST -791.319 limit count 1                           
#> Gen 10 ID.12 BIC -599.931 BEST -791.319 limit count 1                           
#> Gen 10 ID.13 BIC -539.202 BEST -791.319 limit count 1                           
#> Gen 10 ID.14 BIC -750.540 BEST -791.319 limit count 1                           
#> Gen 10 ID.15 BIC -572.636 BEST -791.319 limit count 1                           
#> Gen 10 ID.16 BIC -666.334 BEST -791.319 limit count 1                           
#> Gen 10 ID.17 BIC -417.831 BEST -791.319 limit count 1                           
#> Gen 10 ID.18 BIC -635.378 BEST -791.319 limit count 1                           
#> Gen 10 ID.19 BIC -438.893 BEST -791.319 limit count 1                           
#> Gen 10 ID.20 BIC -573.246 BEST -791.319 limit count 1                           
#> Gen 11 ID.2 BIC -680.050 BEST -791.319 limit count 2                            
#> Gen 11 ID.3 BIC -471.660 BEST -791.319 limit count 2                            
#> Gen 11 ID.4 BIC -577.087 BEST -791.319 limit count 2                            
#> Gen 11 ID.5 BIC -484.611 BEST -791.319 limit count 2                            
#> Gen 11 ID.6 BIC -699.088 BEST -791.319 limit count 2                            
#> Gen 11 ID.7 BIC -582.781 BEST -791.319 limit count 2                            
#> Gen 11 ID.8 BIC -586.223 BEST -791.319 limit count 2                            
#> Gen 11 ID.9 BIC -485.876 BEST -791.319 limit count 2                            
#> Gen 11 ID.10 BIC -547.534 BEST -791.319 limit count 2                           
#> Gen 11 ID.11 BIC -676.029 BEST -791.319 limit count 2                           
#> Gen 11 ID.12 BIC -511.559 BEST -791.319 limit count 2                           
#> Gen 11 ID.13 BIC -336.848 BEST -791.319 limit count 2                           
#> Gen 11 ID.14 BIC -748.717 BEST -791.319 limit count 2                           
#> Gen 11 ID.15 BIC -479.641 BEST -791.319 limit count 2                           
#> Gen 11 ID.16 BIC -423.676 BEST -791.319 limit count 2                           
#> Gen 11 ID.17 BIC -648.916 BEST -791.319 limit count 2                           
#> Gen 11 ID.18 BIC -413.484 BEST -791.319 limit count 2                           
#> Gen 11 ID.19 BIC -689.508 BEST -791.319 limit count 2                           
#> Gen 11 ID.20 BIC -508.428 BEST -791.319 limit count 2                           
#> The maximum generation has been reached

# Examine the learned network structure
# Plot Item Response Profiles showing item patterns across ranks
plot(result.LDLRA.PBIL, type = "IRP", nc = 4, nr = 3)




# Plot Test Response Profile showing overall response patterns
plot(result.LDLRA.PBIL, type = "TRP")


# Plot Latent Rank Distribution showing student distribution
plot(result.LDLRA.PBIL, type = "LRD")

# }
```
