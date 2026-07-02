# Structure Learning for BNM by PBIL

Generating a DAG from data using a Population-Based Incremental Learning

## Usage

``` r
BNM_PBIL(
  U,
  na = NULL,
  Z = NULL,
  w = NULL,
  seed = 123,
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
  verbose = FALSE
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

- seed:

  seed for random.

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

  verbose output Flag. default is FALSE

## Value

- adj:

  Optimal adjacency matrix

- testlength:

  Length of the test. The number of items included in the test.

- TestFitIndices:

  Overall fit index for the test.See also
  [TestFit](https://kosugitti.github.io/exametrika/reference/TestFit.md)

- nobs:

  Sample size. The number of rows in the dataset.

- testlength:

  Length of the test. The number of items included in the test.

- crr:

  correct response ratio

- TestFitIndices:

  Overall fit index for the test.See also
  [TestFit](https://kosugitti.github.io/exametrika/reference/TestFit.md)

- param:

  Learned Parameters

- CCRR_table:

  Correct Response Rate tables

## Details

This function performs structural learning using the Population-Based
Incremental Learning model(PBIL) proposed by Fukuda et al.(2014) within
the genetic algorithm framework. Instead of learning the adjacency
matrix itself, the 'genes of genes' that generate the adjacency matrix
are updated with each generation. For more details, please refer to
Fukuda(2014) and Section 8.5.2 of the text(Shojima,2022).

## References

Fukuda, S., Yamanaka, Y., & Yoshihiro, T. (2014). A Probability-based
evolutionary algorithm with mutations to learn Bayesian networks.
International Journal of Artificial Intelligence and Interactive
Multimedia, 3, 7–13. DOI: 10.9781/ijimai.2014.311

## Examples

``` r
# \donttest{
# Perform Structure Learning for Bayesian Network Model using PBIL
# (Population-Based Incremental Learning)
BNM_PBIL(J5S10,
  population = 20, # Size of population in each generation
  Rs = 0.5, # 50% survival rate for next generation
  Rm = 0.005, # 0.5% mutation rate for genetic diversity
  maxParents = 2, # Maximum of 2 parent nodes per item
  alpha = 0.05, # Learning rate for probability update
  estimate = 4 # Use rounded generational gene method
)
#> Warning: Too many survivers. Limit to 5
#> Adjacency Matrix
#>        Item01 Item02 Item03 Item04 Item05
#> Item01      0      0      0      1      0
#> Item02      0      0      0      0      0
#> Item03      1      0      0      0      0
#> Item04      0      0      0      0      0
#> Item05      0      0      0      0      0
#> [1] "Your graph is an acyclic graph."

#> 
#> Parameter Learning
#>        PIRP 1 PIRP 2
#> Item01    0.0  0.667
#> Item02    0.4       
#> Item03    0.9       
#> Item04    0.0  0.500
#> Item05    0.4       
#> 
#> Conditional Correct Response Rate
#>   Child Item N of Parents Parent Items       PIRP Conditional CRR
#> 1     Item01            1       Item03          0           0.000
#> 2     Item01            1       Item03          1           0.667
#> 3     Item02            0   No Parents No Pattern           0.400
#> 4     Item03            0   No Parents No Pattern           0.900
#> 5     Item04            1       Item01          0           0.000
#> 6     Item04            1       Item01          1           0.500
#> 7     Item05            0   No Parents No Pattern           0.400
#> 
#> Model Fit Indices
#>                  value
#> model_log_like -26.599
#> bench_log_like  -8.935
#> null_log_like  -28.882
#> model_Chi_sq    35.327
#> null_Chi_sq     39.894
#> model_df        23.000
#> null_df         25.000
#> NFI              0.114
#> RFI              0.037
#> IFI              0.270
#> TLI              0.100
#> CFI              0.172
#> RMSEA            0.244
#> AIC            -10.673
#> CAIC           -40.633
#> BIC            -17.633
# }
```
