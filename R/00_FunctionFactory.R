#' Data type validation for exametrika functions
#'
#' @param U An exametrika object
#' @param allowed_types Character vector specifying allowed response types
#' @param fun_name Function name for error messages
#' @return NULL if validation passes, otherwise stops with an error
#' @keywords internal
validate_data_type <- function(U, allowed_types, fun_name = NULL) {
  if (!inherits(U, "exametrika")) {
    stop("Input must be an exametrika object")
  }
  if (!U$response.type %in% allowed_types) {
    stop(sprintf(
      "Function %s is only applicable to %s response data.",
      fun_name %||% deparse(sys.calls()[[sys.nframe() - 1]][[1]]),
      paste(allowed_types, collapse = " or ")
    ))
  }
}

#' Create a function factory for common implementation across data types
#'
#' @description
#' Creates a function that can handle both binary and polytomous response data
#' with the same implementation. This factory function includes data type
#' validation and proper error handling.
#'
#' @param fun Implementation function that works for both binary and polytomous data
#' @param fun_name Character string of function name (for error messages)
#'
#' @return A function that accepts an exametrika object and applies the same
#'         implementation regardless of whether the data is binary or polytomous
#'
#' @details
#' The created function will:
#' 1. Validate that the input is an exametrika object
#' 2. Check that the response type is either binary or polytomous
#' 3. Apply the provided implementation function
#'
#' @examples
#' \dontrun{
#' JointSampleSize <- createCommonFunction(
#'   function(U, ...) {
#'     S_jk <- t(U$Z) %*% U$Z
#'     structure(S_jk, class = c("exametrika", "matrix"))
#'   },
#'   "JointSampleSize"
#' )
#' }
#'
#' @keywords internal
#'
createCommonFunction <- function(fun, fun_name = NULL) {
  function(U, na = NULL, Z = NULL, w = NULL, ...) {
    if (!inherits(U, "exametrika")) {
      U <- dataFormat(U, na = na, Z = Z, w = w)
    }
    validate_data_type(U, c("binary", "poly"), fun_name)
    fun(U, ...)
  }
}

#' Create a function factory for binary-only implementation
#'
#' @description
#' Creates a function that can handle only binary response data.
#' Includes data type validation and proper error handling.
#'
#' @inheritParams createCommonFunction
#' @return A function that accepts an exametrika object with binary response data
#'
#' @keywords internal
createBinaryFunction <- function(fun, fun_name = NULL) {
  function(U, na = NULL, Z = NULL, w = NULL, ...) {
    if (!inherits(U, "exametrika")) {
      U <- dataFormat(U, na = na, Z = Z, w = w)
    }
    validate_data_type(U, "binary", fun_name)
    fun(U, ...)
  }
}

#' Create a function factory for polytomous-only implementation
#'
#' @description
#' Creates a function that can handle only polytomous response data.
#' Includes data type validation and proper error handling.
#'
#' @inheritParams createCommonFunction
#' @return A function that accepts an exametrika object with polytomous response data
#'
#' @keywords internal
#'
createPolyFunction <- function(fun, fun_name = NULL) {
  function(U, na = NULL, Z = NULL, w = NULL, ...) {
    if (!inherits(U, "exametrika")) {
      U <- dataFormat(U, na = na, Z = Z, w = w)
    }
    validate_data_type(U, "poly", fun_name)
    fun(U, ...)
  }
}
