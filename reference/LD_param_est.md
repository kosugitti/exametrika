# LDparam set

A function that extracts only the estimation of graph parameters after
the rank estimation is completed.

## Usage

``` r
LD_param_est(
  tmp,
  adj_list,
  classRefMat,
  ncls,
  smoothpost,
  beta1 = 2,
  beta2 = 2,
  fit_only = FALSE,
  bench = NULL
)
```

## Arguments

- tmp:

  tmp

- adj_list:

  adj_list

- classRefMat:

  values returned from emclus

- ncls:

  ncls

- smoothpost:

  smoothpost

- beta1:

  Beta distribution parameter 1 for prior density. Default is 2.

- beta2:

  Beta distribution parameter 2 for prior density. Default is 2. Unlike
  the other network models (which default to 1), the default of 2
  follows the original Mathematica implementation of LDLRA.

- fit_only:

  If TRUE, return only the BIC (list with element `BIC`), skipping the
  posterior distribution, IRP, and full TestFit construction. Used by
  the LDLRA_PBIL fitness loop, where only the BIC is consumed. Requires
  `bench`.

- bench:

  Cached benchmark-model statistics from `BNM_bench_stats()`, consumed
  only when `fit_only = TRUE`. They depend only on the data, so the
  structure-search loop computes them once instead of re-running
  [`TestFit()`](https://kosugitti.github.io/exametrika/reference/TestFit.md)
  per candidate.
