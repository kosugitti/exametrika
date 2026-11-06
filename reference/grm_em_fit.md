# EM cycles for the graded response model

Runs Bock-Aitkin EM cycles from a starting point in target coding and
stops when the item parameters move by less than `tol` (or after `maxit`
cycles). The E-step is `grm_em_estep_cpp()`; the M-step solves each
item's small expected-count problem with BFGS. Used by
[`GRM()`](https://kosugitti.github.io/exametrika/reference/GRM.md).

## Usage

``` r
grm_em_fit(
  target,
  dat,
  nitems,
  ncat,
  n_quad_points = 51,
  maxit = 500,
  tol = 1e-05,
  verbose = FALSE
)
```

## Arguments

- target:

  starting parameters in target coding

- dat:

  integer response matrix coded 1..ncat with NA for missing

- nitems:

  number of items

- ncat:

  number of categories per item

- n_quad_points:

  number of quadrature points

- maxit:

  maximum number of EM cycles

- tol:

  largest absolute change in any item parameter (slope or threshold) at
  which the cycles stop

- verbose:

  print the marginal log-likelihood every ten cycles

## Value

a list with the final `target`, the marginal `loglik` at the last E-step
and the number of `cycles`
