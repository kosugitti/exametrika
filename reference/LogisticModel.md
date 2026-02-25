# Four-Parameter Logistic Model

The four-parameter logistic model is a model where one additional
parameter d, called the upper asymptote parameter, is added to the 3PLM.

## Usage

``` r
LogisticModel(a = 1, b, c = 0, d = 1, theta)
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

Returns a numeric vector of probabilities between c and d, representing
the probability of a correct response given the ability level theta. The
probability is calculated using the formula: \\P(\theta) = c +
\frac{(d-c)}{1 + e^{-a(\theta-b)}}\\
