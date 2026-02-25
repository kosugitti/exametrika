# Simple Test Statistics

Calculates descriptive statistics for test scores, providing a
comprehensive summary of central tendency, variability, and distribution
shape. Different statistics are calculated based on the data type
(binary, ordinal, rated, or nominal).

## Usage

``` r
TestStatistics(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
TestStatistics(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
TestStatistics(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'ordinal'
TestStatistics(U, na = NULL, Z = NULL, w = NULL)
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

The returned object depends on the data type:

For binary data, a list of class c("exametrika", "TestStatistics")
containing:

- TestLength:

  Length of the test. The number of items included in the test.

- SampleSize:

  Sample size. The number of rows in the dataset.

- Mean:

  Average number of correct answers.

- SEofMean:

  Standard error of mean.

- Variance:

  Variance of test scores.

- SD:

  Standard Deviation of test scores.

- Skewness:

  Skewness of score distribution (measure of asymmetry).

- Kurtosis:

  Kurtosis of score distribution (measure of tail extremity).

- Min:

  Minimum score.

- Max:

  Maximum score.

- Range:

  Range of scores (Max - Min).

- Q1:

  First quartile. Same as the 25th percentile.

- Median:

  Median. Same as the 50th percentile.

- Q3:

  Third quartile. Same as the 75th percentile.

- IQR:

  Interquartile range. Calculated by subtracting Q1 from Q3.

- Stanine:

  Stanine score boundaries, see
  [`stanine`](https://kosugitti.github.io/exametrika/reference/stanine.md).

For ordinal and rated data, the function calls
[`ScoreReport`](https://kosugitti.github.io/exametrika/reference/ScoreReport.md)
and returns its result. See
[`ScoreReport`](https://kosugitti.github.io/exametrika/reference/ScoreReport.md)
for details of the returned object.

For nominal data, an error is returned as this function does not support
nominal data.

## Examples

``` r
# Basic usage
stats <- TestStatistics(J15S500)
print(stats)
#> Test Statistics
#>                   value
#> TestLength   15.0000000
#> SampleSize  500.0000000
#> Mean          9.6640000
#> SEofMean      0.1190738
#> Variance      7.0892826
#> SD            2.6625707
#> Skewness     -0.4116220
#> Kurtosis     -0.4471624
#> Min           2.0000000
#> Max          15.0000000
#> Range        13.0000000
#> Q1.25%        8.0000000
#> Median.50%   10.0000000
#> Q3.75%       12.0000000
#> IQR           4.0000000
#> Stanine.4%    5.0000000
#> Stanine.11%   6.0000000
#> Stanine.23%   7.0000000
#> Stanine.40%   9.0000000
#> Stanine.60%  11.0000000
#> Stanine.77%  12.0000000
#> Stanine.89%  13.0000000
#> Stanine.96%  14.0000000

# Extract specific statistics
cat("Mean score:", stats$Mean, "\n")
#> Mean score: 9.664 
cat("Standard deviation:", stats$SD, "\n")
#> Standard deviation: 2.662571 

# View score distribution summary
summary_stats <- data.frame(
  Min = stats$Min,
  Q1 = stats$Q1,
  Median = stats$Median,
  Mean = stats$Mean,
  Q3 = stats$Q3,
  Max = stats$Max
)
print(summary_stats)
#>     Min Q1 Median  Mean Q3 Max
#> 25%   2  8     10 9.664 12  15
```
