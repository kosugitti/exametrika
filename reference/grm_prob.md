# Probability function for GRM

Calculates the probability of selecting each category given a latent
trait value and item parameters.

## Usage

``` r
grm_prob(theta, a, b)
```

## Arguments

- theta:

  Latent trait value of the subject

- a:

  Discrimination parameter of IRF

- b:

  Vector of difficulty parameters (thresholds) of IRF

## Value

Vector of category selection probabilities

## Examples

``` r
if (FALSE) { # \dontrun{
# Example for an item with 3 categories
a <- 1.5
b <- c(-1.0, 1.0)
theta <- 0
grm_prob(theta, a, b)
} # }
```
