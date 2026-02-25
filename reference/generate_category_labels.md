# Generate category labels for response data

Generate category labels for response data

## Usage

``` r
generate_category_labels(data_column, item_name)
```

## Arguments

- data_column:

  A vector containing response data for a single item

- item_name:

  Character string of the item name

## Value

A character vector of category labels

## Details

If the input is a factor, returns its levels. Otherwise, generates
labels in the format "Item name-Category-N"
