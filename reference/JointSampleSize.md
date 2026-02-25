# Joint Sample Size

The joint sample size is a matrix whose elements are the number of
individuals who responded to each pair of items.

## Usage

``` r
JointSampleSize(U, na = NULL, Z = NULL, w = NULL)

# Default S3 method
JointSampleSize(U, na = NULL, Z = NULL, w = NULL)

# S3 method for class 'binary'
JointSampleSize(U, na = NULL, Z = NULL, w = NULL)
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

Returns a matrix of class c("exametrika", "matrix") where each element
(i,j) represents the number of students who responded to both item i and
item j. The diagonal elements represent the total number of responses
for each item.

## Note

This function is implemented using a binary data compatibility wrapper
and will raise an error if used with polytomous data.
