# Three-Parameter Logistic Model

The three-parameter logistic model is a model where the lower asymptote
parameter c is added to the 2PLM

## Usage

``` r
ThreePLM(a, b, c, theta)
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

Returns a numeric vector of probabilities between c and 1, representing
the probability of a correct response given the ability level theta. The
probability is calculated using the formula: \\P(\theta) = c +
\frac{1-c}{1 + e^{-a(\theta-b)}}\\
