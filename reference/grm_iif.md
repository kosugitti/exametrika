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

## Details

The information is Samejima's (1969) item information for the GRM,
\$\$I(\theta) = \sum\_{k=1}^{K}
\frac{\[P_k'(\theta)\]^2}{P_k(\theta)},\$\$ where \\P_k(\theta) =
P\_{k-1}^\*(\theta) - P_k^\*(\theta)\\ is the category response
probability, \\P_k^\*(\theta)\\ is the cumulative (boundary) probability
with \\P_0^\* = 1\\ and \\P_K^\* = 0\\, and \\P_k^{\*\prime}(\theta) = a
P_k^\*(\theta) \[1 - P_k^\*(\theta)\]\\. The logistic metric of the
estimation routine is used as is (no 1.702 scaling constant), so the
information is consistent with the parameters returned by
[`GRM`](https://kosugitti.github.io/exametrika/reference/GRM.md) and
with the posterior standard deviations (PSD).

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
