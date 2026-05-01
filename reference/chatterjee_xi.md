# Chatterjee's xi correlation coefficient

Computes Chatterjee's (2021) rank-based correlation coefficient xi.
Unlike Pearson or Spearman, xi is asymmetric: xi(x, y) and xi(y, x) may
differ. This asymmetry is the basis for direction-detection in graphical
models.

## Usage

``` r
chatterjee_xi(x, y, ties_method = "random")
```

## Arguments

- x:

  Numeric or ordered factor (predictor)

- y:

  Numeric or ordered factor (response)

- ties_method:

  How to break ties in x. Default "random".

## Value

Numeric scalar: Chatterjee's xi value.

## Details

For tied x values, ranks are broken using `ties_method`. With `"random"`
(the default), each call may produce a slightly different value due to
random tie-breaking. Use
[`xi_stable()`](https://kosugitti.github.io/exametrika/reference/xi_stable.md)
to average over many randomizations.

## References

Chatterjee, S. (2021). A new coefficient of correlation. Journal of the
American Statistical Association, 116(536), 2009-2022.

## Examples

``` r
# \donttest{
x <- rnorm(100)
y <- x^2 + rnorm(100, sd = 0.1)
chatterjee_xi(x, y) # near 1, since y is determined by x
#> [1] 0.7008701
chatterjee_xi(y, x) # smaller, since x is not determined by y
#> [1] 0.289529
# }
```
