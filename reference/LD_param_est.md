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
  beta2 = 2
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

  Beta distribution parameter 2 for prior density. Default is 2.
