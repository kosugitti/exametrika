# Bootstrap-averaged Chatterjee's xi

Computes Chatterjee's xi B times with random tie-breaking and returns
the average. This stabilizes the estimate against tie-breaking
variability, which is important for ordinal data with many ties (e.g.,
Likert-scale items).

## Usage

``` r
xi_stable(x, y, B = 1000, seed = NULL)
```

## Arguments

- x:

  Numeric or ordered factor (predictor)

- y:

  Numeric or ordered factor (response)

- B:

  Number of bootstrap replications. Default 1000.

- seed:

  Optional integer seed for reproducibility.

## Value

A list with elements:

- xi:

  Mean of the B Chatterjee xi values.

- sd:

  Standard deviation of the B values (tie-breaking spread).

- se:

  Standard error of the mean (sd / sqrt(B)).

- B:

  Number of bootstrap replications used.

## Details

For each of the B replications, ties in x are broken randomly via
`rank(x, ties.method = "random")` inside
[`chatterjee_xi()`](https://kosugitti.github.io/exametrika/reference/chatterjee_xi.md).
The standard error of the mean shrinks as 1 / sqrt(B); B = 1000
typically yields SE around 0.001 for moderately tied data.

## References

Chatterjee, S. (2021). A new coefficient of correlation. Journal of the
American Statistical Association, 116(536), 2009-2022.

## Examples

``` r
# \donttest{
x <- rnorm(100)
y <- x^2 + rnorm(100, sd = 0.1)
xi_stable(x, y, B = 500, seed = 42)
#> $xi
#> [1] 0.7740774
#> 
#> $sd
#> [1] 0
#> 
#> $se
#> [1] 0
#> 
#> $B
#> [1] 500
#> 
# }
```
