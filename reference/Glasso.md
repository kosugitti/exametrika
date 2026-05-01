# Graphical Lasso for Gaussian Graphical Models

Estimates a sparse precision matrix from ordinal item response data via
the Graphical Lasso (Friedman, Hastie, Tibshirani 2008). The polychoric
correlation matrix is used as input. The optimal regularization
parameter lambda is selected by the Extended Bayesian Information
Criterion (Foygel and Drton 2010).

## Usage

``` r
Glasso(
  U,
  na = NULL,
  Z = NULL,
  w = NULL,
  gamma = 0.5,
  n_lambda = 50,
  lambda_ratio = 0.01,
  penalize_diagonal = TRUE,
  max_iter = 100,
  eps = 1e-06,
  edge_tol = 1e-08,
  verbose = FALSE,
  ...
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

- gamma:

  EBIC tuning parameter in 0, 1. Default is 0.5.

- n_lambda:

  Number of lambda values in the search grid. Default is 50.

- lambda_ratio:

  Ratio of lambda_min to lambda_max. Default is 0.01.

- penalize_diagonal:

  Logical. If TRUE, the L1 penalty is applied to the diagonal of Theta.
  Default is TRUE.

- max_iter:

  Maximum number of outer iterations for the block coordinate descent
  algorithm. Default is 100.

- eps:

  Convergence tolerance. Default is 1e-6.

- edge_tol:

  Threshold below which an off-diagonal element of Theta is considered
  zero (no edge). Default is 1e-8.

- verbose:

  Logical. If TRUE, progress messages are displayed.

## Value

- theta:

  Estimated precision matrix at the optimal lambda.

- W:

  Working covariance matrix at the optimal lambda.

- lambda_opt:

  Selected lambda value.

- gamma:

  EBIC tuning parameter used.

- ebic_opt:

  Minimum EBIC value.

- n_edge:

  Number of edges in the selected model.

- path:

  Data frame with columns lambda, ebic, n_edge over the search grid.

## Details

The Graphical Lasso estimates the precision matrix Theta by maximizing
the penalized log-likelihood:

log det(Theta) - tr(S Theta) - lambda \* \|\|Theta\|\|\_1

subject to Theta being positive definite. Optimization is performed by
block coordinate descent (Algorithm 17.2 of Hastie, Tibshirani, Friedman
2009) with cyclical coordinate descent for the inner lasso step.

Lambda is searched on a log-scale grid from lambda_max (the maximum
absolute off-diagonal of S) down to lambda_max \* lambda_ratio. For each
lambda, the EBIC is computed and the lambda minimizing EBIC is returned.
Warm-starting (W and beta cache from the previous lambda) is used to
accelerate the search.

## References

Friedman, J., Hastie, T., and Tibshirani, R. (2008). Sparse inverse
covariance estimation with the graphical lasso. Biostatistics, 9(3),
432-441.

Foygel, R., and Drton, M. (2010). Extended Bayesian Information Criteria
for Gaussian Graphical Models. Advances in Neural Information Processing
Systems 23.

## See also

[PolychoricCorrelationMatrix](https://kosugitti.github.io/exametrika/reference/PolychoricCorrelationMatrix.md)

## Examples

``` r
# \donttest{
# Estimate a sparse precision matrix from ordinal data
result.Glasso <- Glasso(J15S3810)
result.Glasso$lambda_opt
#> [1] 0.005241616
result.Glasso$n_edge
#> [1] 98
# }
```
