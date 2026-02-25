# TIF for IRT

Test Information Function for 4PLM

## Usage

``` r
TestInformationFunc(params, theta)
```

## Arguments

- params:

  parameter matrix

- theta:

  ability parameter

## Value

Returns a numeric vector representing the test information at each
ability level theta. The test information is the sum of item information
functions for all items in the test: \\I\_{test}(\theta) = \sum\_{j=1}^n
I_j(\theta)\\
