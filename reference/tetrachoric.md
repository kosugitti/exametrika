# Tetrachoric Correlation

Tetrachoric Correlation is superior to the phi coefficient as a measure
of the relation of an item pair. See Divgi, 1979; Olsson, 1979;Harris,
1988.

## Usage

``` r
tetrachoric(x, y)
```

## Arguments

- x:

  binary vector x

- y:

  binary vector y

## Value

Returns a single numeric value of class "exametrika" representing the
tetrachoric correlation coefficient between the two binary variables.
The value ranges from -1 to 1, where:

- 1 indicates perfect positive correlation

- -1 indicates perfect negative correlation

- 0 indicates no correlation

## References

Divgi, D. R. (1979). Calculation of the tetrachoric correlation
coefficient. Psychometrika, 44, 169–172.

Olsson, U. (1979). Maximum likelihood estimation of the polychoric
correlation coefficient. Psychometrika,44, 443–460.

Harris, B. (1988). Tetrachoric correlation coefficient. In L. Kotz, & N.
L. Johnson (Eds.), Encyclopedia of statistical sciences (Vol. 9, pp.
223–225). Wiley.
