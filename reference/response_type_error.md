# Generate Error Message for Invalid Response Type

Internal function to generate standardized error messages when a
function is called with an incompatible response type.

## Usage

``` r
response_type_error(response_type, fun_name)
```

## Arguments

- response_type:

  character. One of "binary", "rated", "ordinal", or "nominal"

- fun_name:

  character. Name of the calling function

## Value

Never returns; always stops with an error message
