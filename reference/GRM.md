# Graded Response Model (GRM)

Implements Samejima's (1969) Graded Response Model (GRM), which is an
Item Response Theory model for ordered categorical response data. The
model estimates discrimination parameters and category threshold
parameters for each item. It is widely used in psychological
measurement, educational assessment, and other fields that deal with
multi-step rating scales.

## Usage

``` r
GRM(U, na = NULL, Z = NULL, w = NULL, verbose = TRUE)
```

## Arguments

- U:

  Either an object of class "exametrika" or raw data. When raw data is
  given, it is converted to the exametrika class using the
  [`dataFormat`](https://kosugitti.github.io/exametrika/reference/dataFormat.md)
  function.

- na:

  Specifies numbers or characters to be treated as missing values.

- Z:

  Missing indicator matrix of type matrix or data.frame. 1 indicates
  observed values, 0 indicates missing values.

- w:

  Item weight vector

- verbose:

  Logical; if TRUE, shows progress of iterations (default: TRUE)

## Value

A list of class "exametrika" and "GRM" containing the following
elements:

- testlength:

  Length of the test (number of items)

- nobs:

  Sample size (number of rows in the dataset)

- log_lik:

  Log-likelihood value at convergence

- iterations:

  Number of iterations and function evaluations from optimization

- params:

  Matrix containing the estimated item parameters

- EAP:

  Ability parameters of examinees estimated by EAP method

- MAP:

  Ability parameters of examinees estimated by MAP method

- PSD:

  Posterior standard deviation of the ability parameters

- ItemFitIndices:

  Fit indices for each item. See also
  [`ItemFit`](https://kosugitti.github.io/exametrika/reference/ItemFit.md)

- TestFitIndices:

  Overall fit indices for the test. See also
  [`TestFit`](https://kosugitti.github.io/exametrika/reference/TestFit.md)

## References

Samejima, F. (1969). Estimation of latent ability using a response
pattern of graded scores. Psychometrika Monograph Supplement, 34(4, Pt.
2), 1-100.

## Examples

``` r
if (FALSE) { # \dontrun{
# Apply GRM to example data
result <- GRM(J5S1000)
print(result)
plot(result, type = "IRF")
plot(result, type = "IIF")
plot(result, type = "TIF")
} # }
```
