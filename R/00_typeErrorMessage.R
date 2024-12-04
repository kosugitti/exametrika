#' Generate Error Message for Invalid Response Type
#'
#' Internal function to generate standardized error messages when a function
#' is called with an incompatible response type.
#'
#' @param response_type character. One of "binary", "rated", "ordinal", or "nominal"
#' @param fun_name character. Name of the calling function
#'
#' @return Never returns; always stops with an error message
#'
#' @keywords internal
response_type_error <- function(response_type, fun_name) {
  switch(response_type,
    "binary" = stop(sprintf("Function %s is not applicable to binary response data.", fun_name)),
    "rated" = stop(sprintf("Function %s is not applicable to rated response data.", fun_name)),
    "ordinal" = stop(sprintf("Function %s is not applicable to ordinal response data.", fun_name)),
    "nominal" = stop(sprintf("Function %s is not applicable to nominal response data.", fun_name))
  )
}
