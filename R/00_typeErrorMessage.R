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

#' Warn about arguments no method accepts
#'
#' The model functions (`LCA()`, `LRA()`, `Biclustering()`,
#' `Biclustering_IRM()`) take `...` only so that S3 dispatch can hand the
#' arguments on to the method for the response type. Once that method has
#' taken what it knows, anything left over is a misspelt or misplaced
#' argument (`dataType =` for `response.type =`, `max_iter =` for
#' `maxiter =`) that would otherwise vanish without a trace, leaving the
#' default in force. Each terminal method calls this with its remaining
#' dots.
#'
#' @param dots `list(...)` of the calling method.
#' @param fun_name character. Name of the generic, for the message.
#' @param allowed character. Names that are handled elsewhere (deprecated
#'   spellings) and must not be reported.
#' @return Invisibly `NULL`; a warning is issued when something is left.
#' @keywords internal
warn_unused_dots <- function(dots, fun_name, allowed = character()) {
  if (length(dots) == 0) {
    return(invisible(NULL))
  }
  nms <- names(dots)
  if (is.null(nms)) {
    nms <- rep("", length(dots))
  }
  unnamed <- sum(nms == "")
  unknown <- setdiff(nms[nms != ""], allowed)
  parts <- character()
  if (length(unknown) > 0) {
    parts <- c(parts, paste0("`", unknown, "`"))
  }
  if (unnamed > 0) {
    parts <- c(parts, sprintf("%d unnamed argument%s", unnamed, if (unnamed > 1) "s" else ""))
  }
  if (length(parts) == 0) {
    return(invisible(NULL))
  }
  warning(
    sprintf(
      "%s(): unknown argument%s ignored: %s. Check the spelling against ?%s.",
      fun_name, if (length(parts) > 1 || unnamed > 1) "s" else "",
      paste(parts, collapse = ", "), fun_name
    ),
    call. = FALSE
  )
  return(invisible(NULL))
}
