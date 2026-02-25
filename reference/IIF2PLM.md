# IIF for 2PLM

Item Information Function for 2PLM

## Usage

``` r
IIF2PLM(a, b, theta)
```

## Arguments

- a:

  slope parameter

- b:

  location parameter

- theta:

  ability parameter

## Value

Returns a numeric vector representing the item information at each
ability level theta. The information is calculated as: \\I(\theta) =
a^2P(\theta)(1-P(\theta))\\
