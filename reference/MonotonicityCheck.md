# Check Unidimensionality and Monotonicity Before an Ordered Analysis

Runs the checks that an order-restricted (isotonic) analysis takes for
granted: that every item rises with the same latent order, with no
reverse-keyed item left unrecoded and no item that peaks in the middle.
For each item it reports the item-rest correlation, the loading on the
first principal component, the scalability coefficient \\H_j\\, and the
violations of manifest monotonicity, and flags the items that look
reverse-keyed, non-monotone or weak.

## Usage

``` r
MonotonicityCheck(
  U,
  na = NULL,
  Z = NULL,
  w = NULL,
  fields = NULL,
  minvi = 0.03,
  minsize = NULL,
  weak_h = 0.3,
  max_rounds = NULL
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

- fields:

  Optional vector with one entry per item giving its subscale (field).
  `NULL` (default) treats all items as one scale.

- minvi:

  Smallest decrease counted as a violation. Default 0.03.

- minsize:

  Smallest rest-score group. `NULL` (default) uses the rule described in
  Details.

- weak_h:

  Threshold of \\H_j\\ below which an item is flagged as weak. Default
  0.3.

- max_rounds:

  Maximum number of rounds of the iteration. `NULL` (default) allows as
  many rounds as there are items.

## Value

An object of class `c("exametrika", "MonotonicityCheck")` with

- items:

  Data frame, one row per item: field, item, item-rest correlation,
  first-component loading, \\H_j\\, number of violations, number of
  significant violations, largest \\z\\, and the flags (all against the
  clean rest score of the final round).

- fields:

  Data frame, one row per field: number of items and respondents used,
  \\H\\ over all items, \\H\\ over the items left unflagged, number of
  flagged items and rounds used.

- rounds:

  List per field of the flagged items in each round.

- detail:

  List per field of the rest-score group tables of the final round, as
  in
  [`ManifestMonotonicity`](https://kosugitti.github.io/exametrika/reference/ManifestMonotonicity.md).

- minsize, minvi, weak_h:

  The settings used.

## Details

**Reference set and iteration.** An item is judged against the rest
score, but a reverse-keyed or non-monotone item inside the rest score
distorts the yardstick for every other item, so that sound items also
show violations. The rest score is therefore built from a reference set
of items that is cleaned step by step. It starts from the items whose
first-component loading has the majority sign. In each round every item
is judged against the reference set (excluding itself); if an item in
the set is flagged as reverse-keyed or non-monotone, the worst one (most
significant violations, then lowest item-rest correlation) leaves the
set and the round is repeated. Removing one item at a time keeps a
heavily contaminated first round from discarding every item. The items
outside the final set after each round are kept in `rounds`.

**Subscales.** When `fields` is given, each subscale is checked on its
own. This matters: on a multidimensional scale the item-total relations
are dominated by the mixture of dimensions, and reverse-keyed items can
stay hidden when all items are pooled.

**Flags.** An item is flagged `"reverse?"` when its correlation with the
clean rest score is negative, `"non-monotone"` when it has at least one
significant violation of manifest monotonicity (see
[`ManifestMonotonicity`](https://kosugitti.github.io/exametrika/reference/ManifestMonotonicity.md);
with large samples even a small decrease becomes significant, so read
the flag together with the number of violations and the largest \\z\\),
and `"weak"` when \\H_j\\ against the clean items is below `weak_h`
(0.3, Mokken's rule of thumb). The first-component loading is computed
once on all items of the subscale (tetrachoric or polychoric
correlations) and reported for reference; its sign is oriented to sum to
a positive value, so it marks items against the majority rather than
against a known direction.

Respondents with a missing response in a subscale are dropped for that
subscale. The function only reports; recoding or dropping items is left
to the person who prepares the data.

## See also

[`Scalability`](https://kosugitti.github.io/exametrika/reference/Scalability.md),
[`ManifestMonotonicity`](https://kosugitti.github.io/exametrika/reference/ManifestMonotonicity.md),
[`Dimensionality`](https://kosugitti.github.io/exametrika/reference/Dimensionality.md)

## Examples

``` r
# \donttest{
mc <- MonotonicityCheck(J5S1000)
#> MonotonicityCheck: 26 respondents with missing responses were dropped for field 'All'.
mc
#> Unidimensionality and Monotonicity Check
#> minvi = 0.03, weak if Hj < 0.3
#> 
#> Field All: 5 items, 974 respondents, H = 0.245 (unflagged items: 0.245), 1 round(s)
#>  Item r_rest loading    Hj n_violation n_significant z_max flag
#>    V1  0.336   0.611 0.234           1             0 0.597 weak
#>    V2  0.367   0.685 0.259           0             0 0.000 weak
#>    V3  0.320   0.601 0.224           1             0 1.083 weak
#>    V4  0.413   0.730 0.289           0             0 0.000 weak
#>    V5  0.338   0.618 0.235           1             0 0.746 weak
#> 
#> Flags are judged against the rest score over the unflagged items.
#> A single significant violation sets 'non-monotone'; with large samples, check n_significant and z_max.
# }
```
