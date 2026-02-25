# Inter-Item Analysis for Psychometric Data

Calculates various relationship metrics between pairs of items in test
data. This analysis helps identify item interdependencies, content
overlaps, and potential local dependence. For binary data, metrics
include joint response rates, conditional probabilities, and several
correlation measures. For ordinal/rated data, appropriate correlation
measures are calculated.

The following metrics are calculated for binary data:

- JSS: Joint Sample Size - number of examinees responding to both items

- JCRR: Joint Correct Response Rate - proportion of examinees answering
  both items correctly

- CCRR: Conditional Correct Response Rate - probability of answering one
  item correctly given a correct response to another item

- IL: Item Lift - ratio of joint correct response rate to the product of
  marginal rates

- MI: Mutual Information - measure of mutual dependence between items

- Phi: Phi Coefficient - correlation coefficient for binary variables

- Tetrachoric: Tetrachoric Correlation - estimate of Pearson correlation
  for underlying continuous variables

For ordinal/rated data, the function calculates:

- JSS: Joint Sample Size

- JSR: Joint Selection Rate

- CSR: Conditional Selection Rate

- MI: Mutual Information

- Polychoric: Polychoric Correlation - extension of tetrachoric
  correlation for ordinal data

## Usage

``` r
InterItemAnalysis(U, na = NULL, Z = NULL, w = NULL)
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

  Item weight vector specifying the relative importance of each item.

## Value

For binary data, an object of class "exametrika" and "IIAnalysis"
containing:

- JSS:

  Joint Sample Size matrix - N(i,j) shows number of examinees who
  responded to both items i and j

- JCRR:

  Joint Correct Response Rate matrix - P(Xi=1, Xj=1) shows probability
  of correct responses to both items

- CCRR:

  Conditional Correct Response Rate matrix - P(Xi=1\|Xj=1) shows
  probability of correct response to item i given correct response to
  item j

- IL:

  Item Lift matrix - P(Xi=1, Xj=1)/(P(Xi=1)\*P(Xj=1)) measures
  association strength

- MI:

  Mutual Information matrix - measures information shared between items

- Phi:

  Phi Coefficient matrix - correlation coefficient between binary
  variables

- Tetrachoric:

  Tetrachoric Correlation matrix - correlation between underlying
  continuous variables

For ordinal/rated data, an object of class "exametrika" and
"IIAnalysis.ordinal" containing:

- JSS:

  Joint Sample Size matrix

- JSR:

  Joint Selection Rate matrix - frequencies of joint category selections

- CSR:

  Conditional Selection Rate matrix - probabilities of response
  categories conditional on other items

- MI:

  Mutual Information matrix

- Polychoric:

  Polychoric Correlation matrix - correlations between underlying
  continuous variables

## Details

This function automatically detects the data type and applies
appropriate analysis methods:

- For binary data: Calculates tetrachoric correlations and related
  statistics

- For ordinal/rated data: Calculates polychoric correlations and related
  statistics

- For nominal data: Returns an error (not supported)

Inter-item analysis is useful for:

- Identifying groups of highly related items

- Detecting local dependence between items

- Evaluating test dimensionality

- Informing item selection and test construction

## See also

[`dataFormat`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
for data preparation,
[`CTT`](https://kosugitti.github.io/exametrika/reference/CTT.md) for
Classical Test Theory analysis

## Examples

``` r
# \donttest{
# Basic usage with binary data
ii_analysis <- InterItemAnalysis(J15S500)

# View joint sample sizes
head(ii_analysis$JSS)
#>        Item01 Item02 Item03 Item04 Item05 Item06 Item07 Item08 Item09 Item10
#> Item01    500    500    500    500    500    500    500    500    500    500
#> Item02    500    500    500    500    500    500    500    500    500    500
#> Item03    500    500    500    500    500    500    500    500    500    500
#> Item04    500    500    500    500    500    500    500    500    500    500
#> Item05    500    500    500    500    500    500    500    500    500    500
#> Item06    500    500    500    500    500    500    500    500    500    500
#>        Item11 Item12 Item13 Item14 Item15
#> Item01    500    500    500    500    500
#> Item02    500    500    500    500    500
#> Item03    500    500    500    500    500
#> Item04    500    500    500    500    500
#> Item05    500    500    500    500    500
#> Item06    500    500    500    500    500

# View tetrachoric correlations
head(ii_analysis$Tetrachoric)
#>             Item01    Item02      Item03    Item04    Item05    Item06
#> Item01 1.000000000 0.1298956 0.003782285 0.1321743 0.2539693 0.2544285
#> Item02 0.129895557 1.0000000 0.170811256 0.3373172 0.1513059 0.2219054
#> Item03 0.003782285 0.1708113 1.000000000 0.2515549 0.1493038 0.1907252
#> Item04 0.132174341 0.3373172 0.251554925 1.0000000 0.1833419 0.2686524
#> Item05 0.253969298 0.1513059 0.149303836 0.1833419 1.0000000 0.1154091
#> Item06 0.254428484 0.2219054 0.190725175 0.2686524 0.1154091 1.0000000
#>           Item07     Item08       Item09     Item10    Item11    Item12
#> Item01 0.1245681 0.21491939  0.104683556 0.06886875 0.2360705 0.1905543
#> Item02 0.1816224 0.13417787 -0.003852422 0.09291573 0.2791884 0.2339634
#> Item03 0.2385781 0.08517081 -0.002110193 0.07632639 0.1111715 0.1203869
#> Item04 0.4291136 0.23246009  0.031605237 0.11033538 0.3396950 0.4669774
#> Item05 0.1867420 0.08519707 -0.082309679 0.09480535 0.1552386 0.1549072
#> Item06 0.3121046 0.29565142 -0.054304938 0.14597700 0.1247872 0.1640495
#>           Item13     Item14    Item15
#> Item01 0.1865077 0.24813330 0.1694867
#> Item02 0.1648315 0.26860082 0.1922461
#> Item03 0.1698151 0.05143166 0.0641843
#> Item04 0.3373263 0.30943525 0.3423764
#> Item05 0.2606245 0.19607642 0.1261159
#> Item06 0.1908793 0.33871186 0.3381048

# Find pairs of items with high mutual information (potential local dependence)
high_MI <- which(ii_analysis$MI > 0.2 & upper.tri(ii_analysis$MI), arr.ind = TRUE)
if (nrow(high_MI) > 0) {
  print("Item pairs with high mutual information:")
  print(high_MI)
}

# Example with ordinal data
ordinal_analysis <- InterItemAnalysis(J15S3810)

# View polychoric correlations for ordinal data
head(ordinal_analysis$Polychoric)
#>           V1        V2        V3        V4        V5        V6        V7
#> V1 1.0000000 0.3191660 0.2411084 0.4109230 0.3744870 0.4942103 0.2148141
#> V2 0.3191660 1.0000000 0.2275373 0.3009195 0.3052488 0.3589587 0.2547572
#> V3 0.2411084 0.2275373 1.0000000 0.2716839 0.2144433 0.2317651 0.2791930
#> V4 0.4109230 0.3009195 0.2716839 1.0000000 0.3283697 0.4487219 0.3133418
#> V5 0.3744870 0.3052488 0.2144433 0.3283697 1.0000000 0.4369462 0.1824471
#> V6 0.4942103 0.3589587 0.2317651 0.4487219 0.4369462 1.0000000 0.3633916
#>           V8        V9       V10       V11        V12       V13       V14
#> V1 0.1712727 0.2052580 0.2639335 0.3366127 0.13849536 0.3969568 0.1393994
#> V2 0.1513702 0.1945815 0.2332355 0.2787861 0.10373650 0.2785615 0.0820387
#> V3 0.1280090 0.2524095 0.2480967 0.2114377 0.14053512 0.2152988 0.1293748
#> V4 0.3741303 0.1804921 0.2739118 0.3779745 0.14475243 0.3984835 0.1744977
#> V5 0.2455684 0.1174138 0.2421298 0.2876756 0.06470476 0.3550042 0.1138602
#> V6 0.3609023 0.2889176 0.2837686 0.5241616 0.17546119 0.4782207 0.1271876
#>          V15
#> V1 0.3007959
#> V2 0.2645578
#> V3 0.2611432
#> V4 0.3250253
#> V5 0.2863886
#> V6 0.3132519
# }
```
