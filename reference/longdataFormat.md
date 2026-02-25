# Long Format Data Conversion

A function to reshape long data into a dataset suitable for exametrika.

## Usage

``` r
longdataFormat(
  data,
  na = NULL,
  Sid = NULL,
  Qid = NULL,
  Resp = NULL,
  w = NULL,
  response.type = NULL,
  CA = NULL
)
```

## Arguments

- data:

  is a data matrix of the type matrix or data.frame. This must contain
  at least three columns to identify the student, the item, and the
  response. Additionally, it can include a column for the weight of the
  items.

- na:

  na argument specifies the numbers or characters to be treated as
  missing values.

- Sid:

  Specify the column number containing the student ID label vector.

- Qid:

  Specify the column number containing the Question label vector.

- Resp:

  Specify the column number containing the Response value vector.

- w:

  Specify the column number containing the weight vector.

- response.type:

  Character string specifying the type of response data: "binary" for
  dichotomous data, "ordinal" for ordered polytomous data, "rated" for
  polytomous data with correct answers, "nominal" for unordered
  polytomous data. If NULL (default), the type is automatically
  detected.

- CA:

  A numeric vector specifying the correct answers for rated polytomous
  data. Required when response.type is "rated".

## Value

- U:

  For binary response data. A matrix with rows representing the sample
  size and columns representing the number of items, where elements are
  either 0 or 1. \\u\_{ij}=1\\ indicates that student i correctly
  answered item j, while \\u\_{ij}=0\\ means that student i answered
  item j incorrectly.

- Q:

  For polytomous response data. A matrix with rows representing the
  sample size and columns representing the number of items, where
  elements are non-negative integers. When input data is in factor
  format, the factor levels are converted to consecutive integers
  starting from 1.

- ID:

  The ID label given by the designated column or function.

- ItemLabel:

  The item names given by the provided column names or function.

- Z:

  Missing indicator matrix. \\z\_{ij}=1\\ indicates that item j is
  presented to Student i, while \\z\_{ij}=0\\ indicates item j is NOT
  presented to Student i.

- w:

  Item weight vector

- response.type:

  Character string indicating the type of response data: "binary",
  "ordinal", "rated", or "nominal"

- CategoryLabel:

  List containing the original factor labels when polytomous responses
  are provided as factors. NULL if no factor data is present.

- categories:

  Numeric vector containing the number of response categories for each
  item.

- CA:

  For rated polytomous data, a numeric vector of correct answers. NULL
  for other types.
