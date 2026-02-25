# Item Information Function for GRM

Calculates the value of the Item Information Function for the Graded
Response Model.

## Usage

``` r
grm_iif(theta, a, b)
```

## Arguments

- theta:

  Latent trait value of the subject

- a:

  Discrimination parameter of IRF

- b:

  Vector of difficulty parameters (thresholds) of IRF

## Value

Value of the Item Information Function

## Examples

``` r
if (FALSE) { # \dontrun{
# Example for an item with 3 categories
a <- 1.5
b <- c(-1.0, 1.0)
thetas <- seq(-3, 3, by = 0.1)
info <- sapply(thetas, function(t) grm_iif(t, a, b))
plot(thetas, info, type = "l", xlab = "Theta", ylab = "Information")
} # }
```
