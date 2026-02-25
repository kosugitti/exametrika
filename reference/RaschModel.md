# Rasch Model

The one-parameter logistic model is a model with only one parameter b.
This model is a 2PLM model in which a is constrained to 1. This model is
also called the Rasch model.

## Usage

``` r
RaschModel(b, theta)
```

## Arguments

- b:

  slope parameter

- theta:

  ability parameter

## Value

Returns a numeric vector of probabilities between 0 and 1, representing
the probability of a correct response given the ability level theta. The
probability is calculated using the formula: \\P(\theta) = \frac{1}{1 +
e^{-(\theta-b)}}\\
