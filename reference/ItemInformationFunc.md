# IIF for 4PLM

Item Information Function for 4PLM

## Usage

``` r
ItemInformationFunc(a = 1, b, c = 0, d = 1, theta)
```

## Arguments

- a:

  slope parameter

- b:

  location parameter

- c:

  lower asymptote parameter

- d:

  upper asymptote parameter

- theta:

  ability parameter

## Value

Returns a numeric vector representing the item information at each
ability level theta. The information is calculated based on the first
derivative of the log-likelihood of the 4PL model with respect to theta.
