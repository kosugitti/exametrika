# IIF for 3PLM

Item Information Function for 3PLM

## Usage

``` r
IIF3PLM(a, b, c, theta)
```

## Arguments

- a:

  slope parameter

- b:

  location parameter

- c:

  lower asymptote parameter

- theta:

  ability parameter

## Value

Returns a numeric vector representing the item information at each
ability level theta. The information is calculated as: \\I(\theta) =
\frac{a^2(1-P(\theta))(P(\theta)-c)^2}{(1-c)^2P(\theta)}\\
