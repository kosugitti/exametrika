# Pairwise Chatterjee's xi correlation matrix

Computes the p x p asymmetric matrix of Chatterjee's xi correlations
between all item pairs. Entry j, k is xi(item_j, item_k), which in
general differs from xi(item_k, item_j); this asymmetry is what enables
direction detection in subsequent graph-construction steps. Each
off-diagonal entry is computed by
[`xi_stable()`](https://kosugitti.github.io/exametrika/reference/xi_stable.md)
to average over tie-breaking.

## Usage

``` r
chatterjee_matrix(
  U,
  na = NULL,
  Z = NULL,
  w = NULL,
  B = 1000,
  seed = NULL,
  verbose = FALSE
)
```

## Arguments

- U:

  Either an exametrika object or raw data.

- na:

  Values to be treated as missing.

- Z:

  Missing indicator matrix.

- w:

  Item weight vector.

- B:

  Number of bootstrap replications per pair. Default 1000.

- seed:

  Optional integer seed for reproducibility.

- verbose:

  Logical. If TRUE, progress messages are displayed.

## Value

A p x p numeric matrix with item labels as row/column names. The
diagonal is 1; off-diagonal entries are asymmetric.

## Details

Pairs are computed with pairwise-complete observations: for each pair
(j, k), rows where either Qi, j or Qi, k is missing are excluded. The
diagonal is set to 1.

## References

Chatterjee, S. (2021). A new coefficient of correlation. Journal of the
American Statistical Association, 116(536), 2009-2022.

## Examples

``` r
# \donttest{
xi_mat <- chatterjee_matrix(J15S3810, B = 500, seed = 42)
# }
```
