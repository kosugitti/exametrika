# Stanine Scores

The Stanine (Standard Nine) scoring system divides students into nine
groups based on a normalized distribution. This function is applicable
only to binary response data.

These groups correspond to the following percentile ranges:

- Stanine 1: lowest 4% (percentiles 1-4)

- Stanine 2: next 7% (percentiles 5-11)

- Stanine 3: next 12% (percentiles 12-23)

- Stanine 4: next 17% (percentiles 24-40)

- Stanine 5: middle 20% (percentiles 41-60)

- Stanine 6: next 17% (percentiles 61-77)

- Stanine 7: next 12% (percentiles 78-89)

- Stanine 8: next 7% (percentiles 90-96)

- Stanine 9: highest 4% (percentiles 97-100)

## Usage

``` r
stanine(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
stanine(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
stanine(U, na = NULL, Z = NULL, w = NULL)
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

A list containing two elements:

- stanine:

  The score boundaries for each stanine level

- stanineScore:

  The stanine score (1-9) for each student

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.

Stanine scores provide a normalized scale with:

- Mean = 5

- Standard deviation = 2

- Scores range from 1 to 9

- Score of 5 represents average performance

## References

Angoff, W. H. (1984). Scales, norms, and equivalent scores. Educational
Testing Service. (Reprint of chapter in R. L. Thorndike (Ed.) (1971)
Educational Measurement (2nd Ed.). American Council on Education.

## Examples

``` r
result <- stanine(J15S500)
# View score boundaries
result$stanine
#>  4% 11% 23% 40% 60% 77% 89% 96% 
#>   5   6   7   9  11  12  13  14 
# View individual scores
result$stanineScore
#>   [1] 4 5 5 6 3 7 5 5 9 5 5 4 8 7 2 2 7 4 7 3 8 4 5 5 5 2 5 6 6 1 2 6 2 2 9 4 7
#>  [38] 6 4 5 6 9 5 5 7 6 5 7 6 8 6 7 5 6 8 7 5 6 9 7 9 6 5 6 5 2 4 3 7 3 2 5 5 5
#>  [75] 7 4 8 7 7 1 5 2 4 4 7 4 6 4 7 6 4 3 4 7 4 6 5 3 6 7 5 3 8 2 6 4 6 6 3 7 7
#> [112] 6 3 4 2 8 6 4 4 4 5 4 7 8 5 8 1 6 7 7 5 5 6 2 4 2 8 6 6 5 6 2 8 1 7 4 2 7
#> [149] 5 8 8 6 9 5 3 6 5 8 8 9 6 4 4 2 5 2 5 4 9 3 3 8 5 7 4 5 6 4 5 4 4 3 6 4 6
#> [186] 6 4 6 5 5 5 4 5 3 1 7 6 8 5 6 7 6 2 7 5 4 7 6 5 6 5 9 5 4 5 5 8 7 1 3 2 6
#> [223] 5 5 5 5 5 6 4 5 4 1 7 1 5 5 4 9 7 6 7 6 5 4 6 2 6 9 4 9 5 4 6 5 6 5 4 4 4
#> [260] 5 4 8 7 4 7 6 6 4 7 4 7 5 3 5 5 6 4 3 5 4 5 7 4 7 5 3 8 5 8 9 5 9 1 7 3 4
#> [297] 6 7 2 6 4 8 5 5 5 7 3 7 6 5 5 6 7 4 6 6 8 4 3 5 5 8 5 8 8 8 4 5 6 5 7 4 5
#> [334] 8 6 5 5 7 8 5 1 5 5 9 4 6 5 7 5 7 5 4 6 4 7 8 4 9 1 5 7 8 4 6 5 6 9 3 5 5
#> [371] 9 6 8 5 5 5 5 5 9 7 1 4 4 4 7 4 4 4 7 5 3 4 6 4 6 4 6 7 3 2 8 4 5 7 5 5 1
#> [408] 6 5 7 7 5 5 5 5 9 5 8 3 5 5 4 5 8 5 5 8 2 6 6 5 7 6 8 5 9 6 5 4 6 6 5 6 6
#> [445] 4 3 9 7 5 8 5 8 4 7 8 1 8 6 5 3 4 4 3 1 5 8 7 8 2 3 2 3 5 3 7 5 6 9 4 7 4
#> [482] 8 5 6 9 8 6 6 6 5 3 7 5 5 4 3 4 4 5 4
#> Levels: 1 2 3 4 5 6 7 8 9
```
