# TRF for IRT

Calculates the expected score across all items on a test for a given
ability level (theta) using Item Response Theory. The Test Response
Function (TRF) is essentially the sum of the Item Characteristic Curves
(ICCs) for all items in the test.

## Usage

``` r
TestResponseFunc(params, theta)
```

## Arguments

- params:

  parameter matrix

- theta:

  ability parameter

## Value

A numeric vector with the same length as theta, containing the expected
total score for each ability level.

## Details

The Test Response Function computes the expected total score for an
examinee with a given ability level (theta) across all items in the
test. For each item, the function uses the logistic model with
parameters a (discrimination), b (difficulty), c (guessing), and d
(upper asymptote).
