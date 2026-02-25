# Generate Item Report for Non-Binary Test Data

Calculates item-level statistics for non-binary test data, including
response rates, basic descriptive statistics, and item-total
correlations.

## Usage

``` r
ItemReport(U, na = NULL, Z = NULL, w = NULL)
```

## Arguments

- U:

  Either an object of class "exametrika" or raw data. When raw data is
  given, it is converted to the exametrika class with the
  [`dataFormat`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  function.

- na:

  Values to be treated as missing values

- Z:

  Missing indicator matrix of type matrix or data.frame. Values of 1
  indicate observed responses, while 0 indicates missing data.

- w:

  Item weight vector specifying the relative importance of each item

## Value

An object of class "exametrika" and "QitemStatistics" containing:

- ItemLabel:

  Labels identifying each item

- Obs:

  Number of valid responses for each item

- ObsRatio:

  Proportion of valid responses for each item (range: 0-1)

- ItemMean:

  Mean score of each item

- ItemSD:

  Standard deviation of each item score

- ItemCORR:

  Item-total correlation coefficients - correlation between item scores
  and total test scores

- ItemCORR_R:

  Corrected item-total correlation coefficients - correlation between
  item scores and total test scores excluding the target item

## Details

This function is intended for non-binary (ordinal or rated) response
data. It provides detailed statistics for each item in the test,
focusing on response patterns and the relationship between individual
items and overall test performance. If binary data is provided, an error
message will be displayed.

## Examples

``` r
# \donttest{
# Generate item report for sample ordinal data
item_stats <- ItemReport(J15S3810)

# View first few rows of the item report
head(item_stats)
#> $ItemLabel
#>  [1] "V1"  "V2"  "V3"  "V4"  "V5"  "V6"  "V7"  "V8"  "V9"  "V10" "V11" "V12"
#> [13] "V13" "V14" "V15"
#> 
#> $Obs
#>   V1   V2   V3   V4   V5   V6   V7   V8   V9  V10  V11  V12  V13  V14  V15 
#> 3810 3810 3810 2046 2046 2046  985  985  985 1513 1513 1513 3076 3076 3076 
#> 
#> $ObsRatio
#>        V1        V2        V3        V4        V5        V6        V7        V8 
#> 1.0000000 1.0000000 1.0000000 0.5370079 0.5370079 0.5370079 0.2585302 0.2585302 
#>        V9       V10       V11       V12       V13       V14       V15 
#> 0.2585302 0.3971129 0.3971129 0.3971129 0.8073491 0.8073491 0.8073491 
#> 
#> $ItemMean
#>        V1        V2        V3        V4        V5        V6        V7        V8 
#> 1.8230971 1.8149606 2.0582677 1.5948192 1.6544477 1.4745846 1.3908629 0.7096447 
#>        V9       V10       V11       V12       V13       V14       V15 
#> 1.3461929 2.0363516 1.4183741 1.8460013 1.4801691 1.7282185 1.4548114 
#> 
#> $ItemSD
#>        V1        V2        V3        V4        V5        V6        V7        V8 
#> 0.9327474 0.8213917 0.8552853 0.9288219 0.9175956 1.1054337 0.8188873 0.7927068 
#>        V9       V10       V11       V12       V13       V14       V15 
#> 0.9095925 0.8270863 0.9661041 0.8223249 0.9284883 0.8756923 0.7570043 
#> 
#> $ItemCORR
#>        V1        V2        V3        V4        V5        V6        V7        V8 
#> 0.6301239 0.5336564 0.4931032 0.6336335 0.5949090 0.7091589 0.5541948 0.4540134 
#>        V9       V10       V11       V12       V13       V14       V15 
#> 0.5221033 0.4855674 0.5927007 0.3602861 0.6164254 0.3534315 0.5262670 
#> 

# Example with rated data including custom missing value indicator
item_stats2 <- ItemReport(J35S5000, na = -99)
# }
```
