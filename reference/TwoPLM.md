# Two-Parameter Logistic Model

The two-parameter logistic model is a classic model that defines the
probability of a student with ability theta successfully answering item
j, using both a slope parameter and a location parameter.

## Usage

``` r
TwoPLM(a, b, theta)
```

## Arguments

- a:

  slope parameter

- b:

  location parameter

- theta:

  ability parameter

## Value

Returns a numeric vector of probabilities between 0 and 1, representing
the probability of a correct response given the ability level theta. The
probability is calculated using the formula: \\P(\theta) = \frac{1}{1 +
e^{-a(\theta-b)}}\\
