# Grid Search for Optimal Parameters

Performs a grid search to find optimal parameters for different analysis
methods. Supports Biclustering, LCA (Latent Class Analysis), and LRA
(Latent Rank Analysis).

## Usage

``` r
GridSearch(
  obj,
  max_ncls = 10,
  max_nfld = 10,
  fun = "Biclustering",
  index = "BIC",
  verbose = TRUE,
  ...
)
```

## Arguments

- obj:

  Input data matrix or object to be analyzed

- max_ncls:

  Maximum number of classes/clusters to test (default: 10)

- max_nfld:

  Maximum number of fields to test for Biclustering (default: 10)

- fun:

  Function name to use for analysis. Options: "Biclustering", "LCA",
  "LRA" (default: "Biclustering")

- index:

  Fit index to optimize from TestFitIndices returned by each function.
  Valid options: "BIC" (default), "AIC", "CAIC", "model_log_like",
  "model_Chi_sq", "RMSEA", "NFI", "RFI", "IFI", "TLI", "CFI". Aliases
  are also accepted: "loglik", "log_lik", "LogLik", "LL" (all map to
  "model_log_like"), "Chi_sq", "chi_sq" (map to "model_Chi_sq").

- verbose:

  Logical; if TRUE, displays detailed progress messages during grid
  search. Default is TRUE.

- ...:

  Additional arguments passed to the analysis function

## Value

A list containing: For Biclustering:

- index_matrix:

  Matrix of fit indices for each ncls/nfld combination

- optimal_ncls:

  Optimal number of classes/clusters

- optimal_nfld:

  Optimal number of fields

- optimal_result:

  Analysis result using optimal parameters

- failed_settings:

  List of parameter combinations that failed to converge

For LCA/LRA:

- index_vec:

  Vector of fit indices for each ncls

- optimal_ncls:

  Optimal number of classes/clusters

- optimal_result:

  Analysis result using optimal parameters

- failed_settings:

  List of parameter combinations that failed to converge

## Examples

``` r
if (FALSE) { # \dontrun{
# Grid search for Biclustering
result <- grid_serch(data_matrix, max_ncls = 5, max_nfld = 5)

# Grid search for LCA
result <- grid_serch(data_matrix, max_ncls = 8, fun = "LCA")
} # }
```
