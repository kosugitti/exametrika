#' @title create Binary Function
#' @description
#' This function was introduced in exametrika version 1.1 to handle the possibility
#' of polytomous data input. Since functions prior to v1.1 were designed exclusively
#' for binary data, this factory function serves as a filter and recursively generates
#' functions that maintain backward compatibility by ensuring binary data input.
#' @param fun A function that performs the actual analysis on binary data
#' @param fun_name Optional character string specifying the function name for error messages
#' @keywords internal

createBinaryFunction <- function(fun, fun_name = NULL) {
  function(U, na = NULL, Z = NULL, w = NULL, ...) {
    # Format data if needed
    if (!inherits(U, "exametrika")) {
      U <- dataFormat(U, na = na, Z = Z, w = w)
    }

    # Check if binary
    if (U$response.type != "binary") {
      stop(sprintf(
        "Function %s is only applicable to binary response data.",
        fun_name %||% deparse(sys.calls()[[sys.nframe() - 1]][[1]])
      ))
    }

    # Apply the actual analysis function
    fun(U, ...)
  }
}
