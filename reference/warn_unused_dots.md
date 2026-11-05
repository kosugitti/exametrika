# Warn about arguments no method accepts

The model functions
([`LCA()`](https://kosugitti.github.io/exametrika/reference/LCA.md),
[`LRA()`](https://kosugitti.github.io/exametrika/reference/LRA.md),
[`Biclustering()`](https://kosugitti.github.io/exametrika/reference/Biclustering.md),
[`Biclustering_IRM()`](https://kosugitti.github.io/exametrika/reference/Biclustering_IRM.md))
take `...` only so that S3 dispatch can hand the arguments on to the
method for the response type. Once that method has taken what it knows,
anything left over is a misspelt or misplaced argument (`dataType =` for
`response.type =`, `max_iter =` for `maxiter =`) that would otherwise
vanish without a trace, leaving the default in force. Each terminal
method calls this with its remaining dots.

## Usage

``` r
warn_unused_dots(dots, fun_name, allowed = character())
```

## Arguments

- dots:

  `list(...)` of the calling method.

- fun_name:

  character. Name of the generic, for the message.

- allowed:

  character. Names that are handled elsewhere (deprecated spellings) and
  must not be reported.

## Value

Invisibly `NULL`; a warning is issued when something is left.
