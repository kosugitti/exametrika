# Scalability Coefficients (Loevinger's H)

Computes the scalability coefficients of Mokken scale analysis: the
item-pair coefficients \\H\_{ij}\\, the item coefficients \\H_j\\, and
the test coefficient \\H\\. For polytomous items these are the weighted
Loevinger coefficients of Molenaar (1991).

## Usage

``` r
Scalability(U, na = NULL, Z = NULL, w = NULL)
```

## Arguments

- U:

  Either an object of class "exametrika" or raw data. When raw data is
  given, it is converted to the exametrika class with the
  [`dataFormat`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  function.

- na:

  Values to be treated as missing values.

- Z:

  Missing indicator matrix of type matrix or data.frame. Values of 1
  indicate observed responses, while 0 indicates missing data.

- w:

  Item weight vector. Not used.

## Value

An object of class `c("exametrika", "Scalability")` with

- ItemLabel:

  Item labels.

- Hij:

  Matrix of item-pair coefficients (diagonal is `NA`).

- Hj:

  Vector of item coefficients.

- H:

  Test coefficient.

## Details

Each coefficient is a normed covariance. For a pair of items,
\$\$H\_{ij} = \mathrm{Cov}(X_i, X_j) / \mathrm{Cov}(X_i,
X_j)\_{\max},\$\$ where the denominator is the largest covariance
attainable given the two marginal distributions. It is obtained by
pairing the sorted scores of the two items (the comonotone arrangement).
The item and test coefficients pool the pair-wise numerators and
denominators (the item coefficient is Mokken's; Loevinger's own
item-test coefficient was defined differently), \$\$H_j = \sum\_{i \ne
j} \mathrm{Cov}(X_i, X_j) / \sum\_{i \ne j} \mathrm{Cov}(X_i,
X_j)\_{\max},\$\$ \$\$H = \sum\_{i \< j} \mathrm{Cov}(X_i, X_j) /
\sum\_{i \< j} \mathrm{Cov}(X_i, X_j)\_{\max}.\$\$

Unidimensionality, local independence and latent monotonicity imply \\0
\le H\_{ij} \le 1\\, \\0 \le H_j \le 1\\ and \\0 \le H \le 1\\ (Sijtsma
& Molenaar, 2002). A negative coefficient therefore points to an item
scored in the opposite direction (for example, a reverse-keyed item that
has not been recoded) or to an item whose relation to the others is not
monotone. Mokken (1971) suggested the rules of thumb \\0.3 \le H \<
0.4\\ (weak scale), \\0.4 \le H \< 0.5\\ (moderate) and \\H \ge 0.5\\
(strong).

Missing responses are handled pair-wise: each pair of items uses the
examinees who answered both. With complete data the results agree with
`coefH()` of the mokken package. Standard errors are not computed. Item
weights `w` are ignored.

The function only reports; it never recodes or drops items. Whether a
reverse-keyed item should be recoded is a decision for the person who
prepares the data.

## References

Loevinger, J. (1948). The technic of homogeneous tests compared with
some aspects of "scale analysis" and factor analysis. *Psychological
Bulletin*, 45, 507–529.

Mokken, R. J. (1971). *A theory and procedure of scale analysis*. De
Gruyter.

Molenaar, I. W. (1991). A weighted Loevinger H-coefficient extending
Mokken scaling to multicategory items. *Kwantitatieve Methoden*, 12(37),
97–117.

Sijtsma, K., & Molenaar, I. W. (2002). *Introduction to nonparametric
item response theory*. Sage.

van der Ark, L. A. (2007). Mokken scale analysis in R. *Journal of
Statistical Software*, 20(11), 1–19.

## Examples

``` r
# \donttest{
sc <- Scalability(J15S3810)
sc$H
#> [1] 0.2225775
sc$Hj
#>         V1         V2         V3         V4         V5         V6         V7 
#> 0.28477623 0.23807893 0.21606542 0.30217293 0.25051347 0.34964498 0.19578510 
#>         V8         V9        V10        V11        V12        V13        V14 
#> 0.15666992 0.16071280 0.15946396 0.25047250 0.08892962 0.27451454 0.09377406 
#>        V15 
#> 0.24099386 
# }
```
