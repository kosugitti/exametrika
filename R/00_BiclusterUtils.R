#' @title Initialize field membership profile probabilities
#' @description
#' Calculates initial probabilities for the field membership profile matrix
#' in biclustering analysis. This function generates probability distributions
#' based on class and field effects to initialize the profile matrix.
#' @param Q Number of categories
#' @param class_effect Class effect parameter (numeric value influencing class-based probabilities)
#' @param field_effect Field effect parameter (numeric value influencing field-based probabilities)
#' @return A normalized probability vector of length Q for initializing the field membership profile matrix
#' @noRd
init_field_membership_probs <- function(Q, class_effect, field_effect) {
  q_vals <- 1:Q / Q
  high_influence <- q_vals * class_effect
  low_influence <- q_vals[order(q_vals, decreasing = TRUE)] * field_effect
  probs <- high_influence + low_influence
  return(probs / sum(probs))
}

#' @title Check for empty fields in Biclustering
#' @description
#' Checks if any fields have no items assigned after Biclustering estimation.
#' This can occur when the specified number of fields is too large for the data,
#' causing some fields to be empty. Emits a warning message if empty fields are detected.
#' @param fld Vector of field assignments for each item
#' @param nfld Number of specified fields
#' @noRd
check_empty_fields <- function(fld, nfld) {
  empty_fields <- setdiff(1:nfld, unique(fld))
  if (length(empty_fields) > 0) {
    n_effective <- nfld - length(empty_fields)
    warning(
      sprintf(
        "Only %d of %d specified fields have items assigned. Fields %s are empty. Consider reducing nfld.",
        n_effective, nfld, paste(empty_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

#' @title Create filter matrix for clustering
#' @description
#' Generates a filter matrix used in clustering algorithms. The matrix is constructed
#' with diagonal elements based on the number of classes, and off-diagonal elements
#' that create a tridiagonal structure. The first and last columns are normalized.
#' @param ncls Number of classes (integer value determining the matrix dimensions)
#' @return A square filter matrix of size ncls x ncls with normalized first and last columns
#' @noRd
create_filter_matrix <- function(ncls) {
  f0 <- ifelse(ncls < 5, 1.05 - 0.05 * ncls,
    ifelse(ncls < 10, 1.00 - 0.04 * ncls,
      0.80 - 0.02 * ncls
    )
  )
  f1 <- diag(0, ncls)
  f1[row(f1) == col(f1) - 1] <- (1 - f0) / 2
  Fil <- diag(rep(f0, ncls)) + t(f1) + f1
  Fil[, 1] <- Fil[, 1] / sum(Fil[, 1])
  Fil[, ncls] <- Fil[, ncls] / sum(Fil[, ncls])

  return(Fil)
}

#' @title Shift polytomous category codes to start at 1
#' @description
#' Models whose blocks share category probabilities across items (the
#' biclustering family) need code q to mean the same category on every item,
#' so the codes are only shifted, never renumbered per item: if the smallest
#' observed code is below 1 (0-based coding), every code is moved up by the
#' same amount. A category nobody chose keeps its slot with a zero count.
#' Missing cells (-1) are left untouched.
#' @param Q Response matrix (nobs x nitems) with -1 marking missing cells
#' @return Q with codes starting at 1
#' @noRd
shift_category_codes <- function(Q) {
  valid <- Q != -1
  if (any(valid)) {
    lo <- min(Q[valid])
    if (lo < 1) {
      Q[valid] <- Q[valid] + (1 - lo)
    }
  }
  return(Q)
}

#' @title Offset applied by shift_category_codes()
#' @description Stored in the fit so that predict() can code new data the same way.
#' @param Q Response matrix (nobs x nitems) with -1 marking missing cells
#' @return the integer added to every valid code (0 when none)
#' @noRd
category_shift <- function(Q) {
  valid <- Q != -1
  if (!any(valid)) {
    return(0)
  }
  lo <- min(Q[valid])
  return(if (lo < 1) 1 - lo else 0)
}

#' @title Observed codes of each item, in the order of a per-item renumbering
#' @description Element j holds the raw codes of item j that remap_category_codes()
#' turns into 1..k. Stored in item-level fits for predict().
#' @param Q Response matrix (nobs x nitems) with -1 marking missing cells
#' @return list of sorted code vectors, one per item
#' @noRd
category_map <- function(Q) {
  return(lapply(seq_len(NCOL(Q)), function(j) sort(unique(Q[Q[, j] != -1, j]))))
}

#' @title Warn about categories dropped by a per-item renumbering
#' @description
#' Item-level models (LCA, LRA, GRM) renumber each item's observed codes to
#' 1..k, because a category with no responses leaves its threshold or
#' probability unidentified. That is harmless for the item itself, but the
#' user should know which categories were removed and that the codes above
#' them moved down. A category counts as dropped when it lies between the
#' smallest code in the data and the item's own largest code; an item that
#' simply has fewer categories than another is not reported.
#' @param Q raw response matrix (-1 = missing)
#' @param item_labels item labels for the message
#' @param model name used in the message
#' @noRd
warn_dropped_categories <- function(Q, item_labels = NULL, model = "") {
  valid <- Q != -1 & !is.na(Q)
  if (!any(valid)) {
    return(invisible(NULL))
  }
  if (is.null(item_labels)) {
    item_labels <- paste0("Item", seq_len(NCOL(Q)))
  }
  lo <- min(Q[valid])
  msgs <- character(0)
  for (j in seq_len(NCOL(Q))) {
    u <- sort(unique(Q[valid[, j], j]))
    if (length(u) == 0) {
      next
    }
    gap <- setdiff(seq(lo, max(u)), u)
    if (length(gap) > 0) {
      msgs <- c(msgs, sprintf(
        "%s: category %s", item_labels[j], paste(gap, collapse = ", ")
      ))
    }
  }
  if (length(msgs) > 0) {
    warning(
      model, ": the following categories had no responses and were dropped; ",
      "the categories above them were renumbered for that item.\n  ",
      paste(msgs, collapse = "\n  "),
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

#' @title Remap polytomous category codes to contiguous 1-based indices
#' @description
#' Per-item renumbering for the item-level models (LCA, M2 on LCA/LRA). The
#' biclustering family uses shift_category_codes() instead. Call
#' warn_dropped_categories() on the raw codes to report what this drops.
#'
#' Response matrices produced by dataFormat() preserve the caller's raw
#' category codes (e.g. 0-indexed 0..3, or codes with gaps), with missing
#' cells marked as -1. Biclustering/Biclustering_IRM's one-hot encoding
#' indexes an array directly by these raw codes, so a raw code of 0 is
#' silently dropped (R's array-index semantics treat 0 as "select nothing")
#' and any non-contiguous coding misaligns with the array's category
#' dimension. This remaps each item's own observed codes to `1..ncat[j]`,
#' leaving -1 (missing) untouched, mirroring the equivalent remap already
#' done in GRM().
#' @param Q Response matrix (nobs x nitems) with -1 marking missing cells
#' @return Q with each column's non-missing values remapped to `1..ncat[j]`
#' @noRd
remap_category_codes <- function(Q) {
  for (j in seq_len(NCOL(Q))) {
    valid_j <- Q[, j] != -1
    Q[valid_j, j] <- match(Q[valid_j, j], sort(unique(Q[valid_j, j])))
  }
  Q
}
