# Manifest Monotonicity (Rest-Score Regression)

Checks, item by item, whether the probability of reaching each category
rises with the rest score, the observable counterpart of latent
monotonicity in Mokken scale analysis (Molenaar & Sijtsma, 2000; van der
Ark, 2007). A single-peaked or U-shaped item, which the item-rest
correlation and
[`Scalability`](https://kosugitti.github.io/exametrika/reference/Scalability.md)
can show as merely weak, appears here as a run of decreases.

## Usage

``` r
ManifestMonotonicity(
  U,
  na = NULL,
  Z = NULL,
  w = NULL,
  minvi = 0.03,
  minsize = NULL
)
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

- minvi:

  Smallest decrease counted as a violation. Default 0.03.

- minsize:

  Smallest rest-score group. `NULL` (default) uses the rule described in
  Details.

## Value

An object of class `c("exametrika", "ManifestMonotonicity")` with

- ItemLabel:

  Item labels.

- summary:

  Data frame, one row per item: the item scalability coefficient `Hj`
  and the totals over item steps of active pairs, violations, violations
  per active pair, largest violation, sum of violations, sum per active
  pair, largest \\z\\ and number of significant violations.

- groups:

  List, one data frame per item: rest-score range, size, category
  frequencies, mean item score and \\P(X \ge x)\\ for each group.

- violations:

  List, one matrix per item: the same statistics per item step, with the
  two groups involved in the largest \\z\\.

- minsize, minvi:

  The settings used.

- n_dropped:

  Number of respondents dropped for missing responses.

## Details

For item \\j\\ the rest score \\R\_{-j}\\ is the sum of the other items.
Respondents are sorted by rest score and cut into groups of at least
`minsize` people, never splitting a rest score; a remainder smaller than
`minsize` is merged into the last group. Within each group the item-step
proportions \\P(X_j \ge x)\\ are computed for every category \\x\\ above
the lowest. Manifest monotonicity requires \$\$P(X_j \ge x \mid R\_{-j}
\in \text{higher group}) \ge P(X_j \ge x \mid R\_{-j} \in \text{lower
group})\$\$ for every pair of groups. A decrease larger than `minvi`
counts as a violation, and each violation gets the normal approximation
\$\$z = \frac{2\left\|\sqrt{(a+1)(d+1)} -
\sqrt{bc}\right\|}{\sqrt{n-1}},\$\$ where \\a\\ and \\b\\ are the
numbers at or above and below \\x\\ in the higher group, \\c\\ and \\d\\
the same in the lower group, and \\n = a + b + c + d\\. A \\z\\ above
1.6449 (one-sided 5\\ multiplicity correction) is counted as
significant.

The default `minsize` follows the mokken package: \\N/10\\ for \\N \ge
500\\, \\N/5\\ for \\250 \< N \< 500\\, \\N/3\\ for \\N \le 250\\ and 50
for \\N \< 150\\ (all rounded down). It is advisable to repeat the check
with a few values of `minsize`.

Respondents with any missing response are dropped (listwise), because a
rest score over different item sets is not comparable; a message reports
how many. With complete data the results agree with
`check.monotonicity()` of the mokken package.

For polytomous items latent monotonicity does not strictly imply
manifest monotonicity (Junker & Sijtsma, 2000); the check is used in
practice as if it did. The function only reports; it never recodes or
drops items.

## References

Junker, B. W., & Sijtsma, K. (2000). Latent and manifest monotonicity in
item response models. *Applied Psychological Measurement*, 24, 65–81.

Molenaar, I. W., & Sijtsma, K. (2000). *User's manual MSP5 for Windows*.
IEC ProGAMMA.

van der Ark, L. A. (2007). Mokken scale analysis in R. *Journal of
Statistical Software*, 20(11), 1–19.

## Examples

``` r
# \donttest{
mm <- ManifestMonotonicity(J5S1000)
#> ManifestMonotonicity: 26 respondents with missing responses were dropped.
mm$summary
#>   Item        Hj n_active n_violation vi_per_active max_violation sum_violation
#> 1   V1 0.2344842       63           1    0.01587302    0.03103070    0.03103070
#> 2   V2 0.2591719       42           0    0.00000000    0.00000000    0.00000000
#> 3   V3 0.2244265       63           1    0.01587302    0.07020516    0.07020516
#> 4   V4 0.2887626       42           0    0.00000000    0.00000000    0.00000000
#> 5   V5 0.2345297       63           1    0.01587302    0.04356061    0.04356061
#>   sum_per_active     z_max n_significant
#> 1   0.0004925508 0.5967635             0
#> 2   0.0000000000 0.0000000             0
#> 3   0.0011143676 1.0828149             0
#> 4   0.0000000000 0.0000000             0
#> 5   0.0006914382 0.7464788             0
# }
```
