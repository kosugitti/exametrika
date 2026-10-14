# Latent Rank Analysis

A general function for estimating Latent Rank Analysis across different
response types. This function automatically dispatches to the
appropriate method based on the response type:

- For binary data (`LRA.binary`): Analysis using either SOM or GTM
  method

- For ordinal data (`LRA.ordinal`): Analysis using the GTM method with
  category thresholds

- For rated data (`LRA.rated`): Analysis using the GTM method with
  rating categories

Latent Rank Analysis identifies underlying rank structures in test data
and assigns examinees to these ranks based on their response patterns.

## Usage

``` r
LRA(U, ...)

# Default S3 method
LRA(U, na = NULL, Z = NULL, w = NULL, ...)

# S3 method for class 'binary'
LRA(
  U,
  nrank = 2,
  method = "isotonic",
  mic = FALSE,
  maxiter = 1000,
  BIC.check = FALSE,
  seed = NULL,
  verbose = FALSE,
  beta1 = 1,
  beta2 = 1,
  conf = NULL,
  ...
)

# S3 method for class 'ordinal'
LRA(
  U,
  nrank = 2,
  method = "isotonic",
  mic = FALSE,
  maxiter = 1000,
  trapezoidal = 0,
  alpha = 1,
  eps = 1e-04,
  verbose = FALSE,
  ...
)

# S3 method for class 'rated'
LRA(
  U,
  nrank = 2,
  mic = FALSE,
  maxiter = 1000,
  trapezoidal = 0,
  eps = 1e-04,
  minFreqRatio = 0,
  verbose = FALSE,
  ...
)
```

## Arguments

- U:

  Either an object of class "exametrika" or raw data. When raw data is
  given, it is converted to the exametrika class with the
  [`dataFormat`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  function.

- ...:

  Additional arguments passed to specific methods.

- na:

  Values to be treated as missing values.

- Z:

  Missing indicator matrix of type matrix or data.frame. 1 indicates
  observed values, 0 indicates missing values.

- w:

  Item weight vector.

- nrank:

  Number of latent ranks to estimate. Must be between 2 and 20.

- method:

  Estimation method. One of "isotonic" (order-restricted MAP; rank
  ordering imposed in the M-step by the Fenchel-dual stochastic-order
  solver, no filter) or "GTM" (Gaussian Topographic Mapping; filter
  smoothing). Default is "isotonic".

- mic:

  Logical; if TRUE, forces Item Reference Profiles to be monotonically
  increasing. Default is FALSE.

- maxiter:

  Maximum number of iterations for estimation. Default is 1000. For the
  SOM method this also sets the length of the annealing schedule, so
  changing it changes the solution rather than merely allowing more
  iterations.

- BIC.check:

  For binary data with SOM method only. If TRUE, estimation stops early
  once the change in BIC falls below a threshold. This is an
  early-stopping option, not a convergence test: SOM has no convergence
  criterion. Default is FALSE.

- seed:

  For binary data with SOM method only. Random seed for reproducibility.
  It affects only which presentation orders are drawn, not the algorithm
  itself.

- verbose:

  Logical; if TRUE, displays detailed progress during estimation.
  Default is FALSE.

- beta1:

  Beta distribution parameter 1 for prior density of rank reference
  matrix (GTM method only). Default is 1.

- beta2:

  Beta distribution parameter 2 for prior density of rank reference
  matrix (GTM method only). Default is 1.

- conf:

  Confirmatory IRP matrix (items x nrank) for test equating. Same format
  as the IRP output. Non-NA values are fixed throughout estimation, NA
  values are freely estimated. Fixed values must be in the open interval
  (0, 1). When row names are present, items are matched by label;
  otherwise by position. Default is NULL (fully exploratory).

- trapezoidal:

  Specifies the height of both tails when using a trapezoidal prior
  distribution. Must be less than 1/nrank. The default value is 0, which
  results in a uniform prior distribution. Used by `method = "GTM"`.

- alpha:

  Dirichlet concentration for the category probabilities, used by
  `method = "isotonic"` (the polytomous analogue of the binary
  `beta1`/`beta2`). `alpha = 1` gives the maximum-likelihood
  (flat-prior) estimate. Default is 1.

- eps:

  Convergence threshold for parameter updates. Default is 1e-4.

- minFreqRatio:

  Minimum frequency ratio for response categories (default = 0).
  Categories with occurrence rates below this threshold will be excluded
  from analysis. For example, if set to 0.1, response categories that
  appear in less than 10% of responses for an item will be omitted.

## Value

A list of class "exametrika" and the specific subclass (e.g., "LRA",
"LRAordinal", "LRArated") containing the following common elements:

- msg:

  A character string indicating the model type.

- testlength:

  Length of the test (number of items).

- nobs:

  Sample size (number of rows in the dataset).

- n_rank:

  Number of latent ranks specified.

- n_cycle:

  Number of EM algorithm iterations performed.

- converge:

  Logical value indicating whether the algorithm converged within
  maxiter iterations. The SOM method has no convergence criterion and
  runs its annealing schedule to completion, so this is TRUE unless
  BIC.check early stopping failed to trigger.

- TRP:

  Test Reference Profile vector showing expected scores at each rank.

- LRD:

  Latent Rank Distribution vector showing the number of examinees at
  each rank.

- RMD:

  Rank Membership Distribution vector showing the sum of probabilities
  for each rank.

- Students:

  Rank Membership Profile matrix showing the posterior probabilities of
  examinees belonging to each rank, along with their estimated ranks and
  odds ratios.

- ItemFitIndices:

  Fit indices for each item. See also
  [`ItemFit`](https://kosugitti.github.io/exametrika/reference/ItemFit.md).

- TestFitIndices:

  Overall fit indices for the test. See also
  [`TestFit`](https://kosugitti.github.io/exametrika/reference/TestFit.md).

- elbo:

  Evidence lower bound of the observed-data log-likelihood under the
  posterior memberships the EM produced. Exact (equal to the
  observed-data log-likelihood) for a one-mode model; a lower bound for
  biclustering, where the respondent and item posteriors are taken
  independent. Since 2.2.0.

- log_lik_marginal:

  Observed-data log-likelihood of the respondent-side mixture with the
  items fixed at their MAP field assignment. Equal to `elbo` for a
  one-mode model.

- entropy:

  Posterior entropy of the memberships (named: `class`, and `field` for
  biclustering). `elbo` minus this is the expected complete-data
  log-likelihood.

- ICL:

  Integrated completed likelihood criterion (Biernacki, Celeux and
  Govaert, 2000) in its BIC-type approximation, on the same scale as AIC
  and BIC (-2 log scale, smaller is better).

Each subclass returns additional specific elements, detailed in their
respective documentation.

For binary data (`LRA.binary`), the returned list additionally includes:

- IRP:

  Item Reference Profile matrix showing the probability of correct
  response for each item across different ranks.

- IRPIndex:

  Item Response Profile indices including the location parameters B and
  Beta, slope parameters A and Alpha, and monotonicity indices C and
  Gamma.

For ordinal data (`LRA.ordinal`), the returned list additionally
includes:

- msg:

  A character string indicating the model type.

- converge:

  Logical value indicating whether the algorithm converged within
  maxiter iterations

- ScoreReport:

  Descriptive statistics of test performance, including sample size,
  test length, central tendency, variability, distribution
  characteristics, and reliability.

- ItemReport:

  Basic statistics for each item including category proportions and
  item-total correlations.

- ICBR:

  Item Category Boundary Reference matrix showing cumulative
  probabilities for rank-category combinations.

- ICRP:

  Item Category Reference Profile matrix showing probability of response
  in each category by rank.

- CategoryMap, LogPrior:

  The raw codes behind the rows of `ICRP` for each item, and the log
  prior over ranks; used by
  [`predict.exametrika()`](https://kosugitti.github.io/exametrika/reference/predict.exametrika.md).

- ScoreRankCorr:

  Spearman's correlation between test scores and estimated ranks.

- RankQuantCorr:

  Spearman's correlation between estimated ranks and quantile groups.

- ScoreRank:

  Contingency table of raw scores by estimated ranks.

- ScoreMembership:

  Expected rank memberships for each raw score.

- RankQuantile:

  Cross-tabulation of rank frequencies and quantile groups.

- MembQuantile:

  Cross-tabulation of rank membership probabilities and quantile groups.

- CatQuant:

  Response patterns across item categories and quantile groups.

- log_lik:

  Expected log-posterior at the final EM cycle, the quantity the EM
  monitors; the observed-data log-likelihood is `elbo`.

- elbo:

  Evidence lower bound of the observed-data log-likelihood under the
  posterior memberships the EM produced. Exact (equal to the
  observed-data log-likelihood) for a one-mode model; a lower bound for
  biclustering, where the respondent and item posteriors are taken
  independent. Since 2.2.0.

- log_lik_marginal:

  Observed-data log-likelihood of the respondent-side mixture with the
  items fixed at their MAP field assignment. Equal to `elbo` for a
  one-mode model.

- entropy:

  Posterior entropy of the memberships (named: `class`, and `field` for
  biclustering). `elbo` minus this is the expected complete-data
  log-likelihood.

- ICL:

  Integrated completed likelihood criterion (Biernacki, Celeux and
  Govaert, 2000) in its BIC-type approximation, on the same scale as AIC
  and BIC (-2 log scale, smaller is better).

For rated data (`LRA.rated`), the returned list additionally includes:

- msg:

  A character string indicating the model type.

- converge:

  Logical value indicating whether the algorithm converged within
  maxiter iterations

- ScoreReport:

  Descriptive statistics of test performance, including sample size,
  test length, central tendency, variability, distribution
  characteristics, and reliability.

- ItemReport:

  Basic statistics for each item including category proportions and
  item-total correlations.

- ICRP:

  Item Category Reference Profile matrix showing probability of response
  in each category by rank.

- CategoryMap, CollapseCode, LogPrior:

  The raw codes behind the rows of `ICRP` for each item (`CollapseCode`
  marks the pooled rare distractors), and the log prior over ranks; used
  by
  [`predict.exametrika()`](https://kosugitti.github.io/exametrika/reference/predict.exametrika.md).

- ScoreRankCorr:

  Spearman's correlation between test scores and estimated ranks.

- RankQuantCorr:

  Spearman's correlation between estimated ranks and quantile groups.

- ScoreRank:

  Contingency table of raw scores by estimated ranks.

- ScoreMembership:

  Expected rank memberships for each raw score.

- RankQuantile:

  Cross-tabulation of rank frequencies and quantile groups.

- MembQuantile:

  Cross-tabulation of rank membership probabilities and quantile groups.

- ItemQuantileRef:

  Reference values for each item across quantile groups.

- CatQuant:

  Response patterns across item categories and quantile groups.

- log_lik:

  Expected log-posterior at the final EM cycle, the quantity the EM
  monitors; the observed-data log-likelihood is `elbo`.

- elbo:

  Evidence lower bound of the observed-data log-likelihood under the
  posterior memberships the EM produced. Exact (equal to the
  observed-data log-likelihood) for a one-mode model; a lower bound for
  biclustering, where the respondent and item posteriors are taken
  independent. Since 2.2.0.

- log_lik_marginal:

  Observed-data log-likelihood of the respondent-side mixture with the
  items fixed at their MAP field assignment. Equal to `elbo` for a
  one-mode model.

- entropy:

  Posterior entropy of the memberships (named: `class`, and `field` for
  biclustering). `elbo` minus this is the expected complete-data
  log-likelihood.

- ICL:

  Integrated completed likelihood criterion (Biernacki, Celeux and
  Govaert, 2000) in its BIC-type approximation, on the same scale as AIC
  and BIC (-2 log scale, smaller is better).

## Binary Data Method

`LRA.binary` analyzes dichotomous (0/1) response data using either
Self-Organizing Maps (SOM) or Gaussian Topographic Mapping (GTM).

## Ordinal Data Method

`LRA.ordinal` analyzes ordered categorical data with multiple
thresholds, such as Likert-scale responses or graded items.

## Rated Data Method

`LRA.rated` analyzes data with ratings assigned to each response, such
as partially-credited items or preference scales where response
categories have different weights.

## See also

[`plot.exametrika`](https://kosugitti.github.io/exametrika/reference/plot.exametrika.md)
for visualizing LRA results.

## Examples

``` r
# \donttest{
# Binary data example
# Fit a Latent Rank Analysis model with 6 ranks to binary data
result.LRA <- LRA(J15S500, nrank = 6)

# Display the first few rows of student rank membership profiles
head(result.LRA$Students)
#>            Membership 1 Membership 2 Membership 3 Membership 4 Membership 5
#> Student001  0.373362828  0.428218832   0.11222243   0.03023517  0.055960736
#> Student002  0.020038540  0.080823095   0.57927688   0.28286515  0.036996142
#> Student003  0.006211982  0.213060043   0.54320920   0.14635238  0.076437420
#> Student004  0.001004123  0.009115719   0.19879539   0.29307173  0.133726701
#> Student005  0.252721804  0.727486696   0.01394724   0.00375769  0.001962579
#> Student006  0.000130071  0.002051624   0.04590205   0.06767055  0.766752034
#>            Membership 6 Estimate Rank-Up Odds Rank-Down Odds
#> Student001 6.586004e-09        2   0.26206795     0.87189726
#> Student002 1.929370e-07        3   0.48830733     0.13952412
#> Student003 1.472897e-02        3   0.26942176     0.39222466
#> Student004 3.642863e-01        6           NA     0.36709228
#> Student005 1.239901e-04        2   0.01917182     0.34739028
#> Student006 1.174937e-01        5   0.15323555     0.08825611

# Plot Item Reference Profiles (IRP) for the first 6 items
plot(result.LRA, type = "IRP", items = 1:6, nc = 2, nr = 3)


# Plot Test Reference Profile (TRP) showing expected scores at each rank
plot(result.LRA, type = "TRP")

# }

# \donttest{
# Ordinal data example
# Fit a Latent Rank Analysis model with 3 ranks to ordinal data
result.LRAord <- LRA(J15S3810, nrank = 3, mic = TRUE)

# Plot score distributions
plot(result.LRAord, type = "ScoreFreq")

plot(result.LRAord, type = "ScoreRank")


# Plot category response patterns for items 1-6
plot(result.LRAord, type = "ICBR", items = 1:6, nc = 3, nr = 2)

plot(result.LRAord, type = "ICRP", items = 1:6, nc = 3, nr = 2)

# }

# \donttest{
# Rated data example
# Fit a Latent Rank Analysis model with 10 ranks to rated data
result.LRArated <- LRA(J35S5000, nrank = 10, mic = TRUE)
#> Warning: LRA: the following categories had no responses and were dropped; the categories above them were renumbered for that item.
#>   V30: category 5

# Plot score distributions
plot(result.LRArated, type = "ScoreFreq")

plot(result.LRArated, type = "ScoreRank")


# Plot category response patterns for items 1-6
plot(result.LRArated, type = "ICRP", items = 1:6, nc = 3, nr = 2)

# }
```
