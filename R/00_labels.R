# Attach respondent IDs and item labels to model output
# Internal helpers called from the model constructors.

#' Fields whose rows (or elements) are indexed by respondent
#'
#' `Students` is deliberately included even though several constructors already
#' set its row names by hand: applying the labels once more is a no-op, and the
#' hand-written line can then be dropped without changing behaviour.
#' @noRd
student_axis_fields <- function() {
  c(
    "ClassEstimated", "ClassMembership", "SmoothedMembership", "Students",
    "StudentRank", "StudentClass", "ability", "Theta", "EAP", "MAP", "PSD"
  )
}

#' Fields whose rows (or elements) are indexed by item
#'
#' `FieldAnalysis` is NOT in this list. Its rows are sorted by correct response
#' rate and field, so the item order no longer matches the input; labelling it
#' positionally would attach the wrong names. It carries labels already,
#' inherited from the matrix it is built from.
#' @noRd
item_axis_fields <- function() {
  c(
    "FieldEstimated", "FieldMembership", "ItemLabel_check",
    "ReliabilityExcludingItem", "ItemFitIndices", "params", "Difficulty",
    "Discrimination", "Slope", "Location"
  )
}

#' Attach labels along one axis
#'
#' Applies `labs` as `names()` for a plain vector and as `rownames()` for a
#' matrix or data frame, and only when the extent matches. Anything else is
#' returned untouched, so a field that is a list, a scalar, or of another length
#' cannot be mislabelled.
#' @noRd
apply_axis_labels <- function(v, labs) {
  if (is.null(v) || is.null(labs)) {
    return(v)
  }
  if (is.matrix(v) || is.data.frame(v)) {
    if (NROW(v) == length(labs)) rownames(v) <- labs
    return(v)
  }
  if (is.atomic(v) && is.null(dim(v)) && length(v) == length(labs)) {
    names(v) <- labs
  }
  v
}

#' Label a result object along both axes
#'
#' Called once, immediately before a constructor returns. Respondent-indexed
#' fields get the IDs from `dataFormat()`, item-indexed fields get the item
#' labels, and the membership matrices get column names for the latent classes
#' or ranks and for the fields.
#'
#' Both `id` and `item` come from the formatted data, so they are always
#' available; the guard on length means a field that does not line up is left
#' alone rather than silently mislabelled.
#'
#' @param ret the result list, before `structure()` or after it
#' @param id respondent IDs (`tmp$ID`)
#' @param item item labels (`tmp$ItemLabel`)
#' @param class_prefix "Class" or "Rank"; `NULL` skips class column names
#' @param field_prefix usually "Field"; `NULL` skips field column names
#' @noRd
label_result <- function(ret, id = NULL, item = NULL,
                         class_prefix = NULL, field_prefix = "Field") {
  for (nm in intersect(names(ret), student_axis_fields())) {
    ret[[nm]] <- apply_axis_labels(ret[[nm]], id)
  }
  for (nm in intersect(names(ret), item_axis_fields())) {
    ret[[nm]] <- apply_axis_labels(ret[[nm]], item)
  }

  set_cols <- function(v, prefix) {
    if (is.null(v) || is.null(prefix) || !is.matrix(v) || is.null(NCOL(v))) {
      return(v)
    }
    if (is.null(colnames(v))) colnames(v) <- paste0(prefix, seq_len(NCOL(v)))
    v
  }
  if (!is.null(class_prefix)) {
    for (nm in intersect(names(ret), c("ClassMembership", "SmoothedMembership"))) {
      ret[[nm]] <- set_cols(ret[[nm]], class_prefix)
    }
  }
  for (nm in intersect(names(ret), "FieldMembership")) {
    ret[[nm]] <- set_cols(ret[[nm]], field_prefix)
  }
  ret
}

#' Put respondent IDs on the rows of the response matrices
#'
#' `dataFormat()` already puts the item labels on the columns; the rows carried
#' nothing, so every downstream copy of `U`/`Q`/`Z` had to be read positionally.
#' Applied to the list before it becomes an `exametrikaData` object.
#' @noRd
label_data_matrices <- function(lst) {
  id <- lst$ID
  if (is.null(id)) {
    return(lst)
  }
  for (nm in c("U", "Q", "Z")) {
    v <- lst[[nm]]
    if (!is.null(v) && is.matrix(v) && NROW(v) == length(id)) {
      rownames(v) <- id
      lst[[nm]] <- v
    }
  }
  lst
}
