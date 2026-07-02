#' @title Distractor Analysis
#' @description
#' Performs distractor analysis for rated (multiple-choice) models.
#' For each item and rank/class, computes observed category frequency tables,
#' chi-square tests against chance level (uniform distribution), and
#' Cramer's V as an effect size measure.
#'
#' This function works with results from \code{\link{LRA}} (rated data) and
#' \code{\link{Biclustering}} / \code{\link{Biclustering_IRM}} (rated data).
#'
#' @param x A result object from a rated model (class \code{LRArated} or
#'   \code{ratedBiclustering}).
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{c("exametrika", "DistractorAnalysis")} containing:
#' \describe{
#'   \item{freq_table}{List of frequency matrices (nrank x maxQ), one per item.}
#'   \item{prop_table}{List of proportion matrices (nrank x maxQ), one per item.}
#'   \item{chisq_table}{Matrix (nitems x nrank) of chi-square statistics.}
#'   \item{pvalue_table}{Matrix (nitems x nrank) of p-values.}
#'   \item{cramersv_table}{Matrix (nitems x nrank) of Cramer's V effect sizes.}
#'   \item{CA}{Correct answer vector.}
#'   \item{n_rank}{Number of ranks/classes.}
#'   \item{maxQ}{Number of response categories.}
#'   \item{nitems}{Number of items.}
#'   \item{ItemLabel}{Item label vector.}
#'   \item{n_field}{Number of fields (Biclustering only).}
#'   \item{FieldEstimated}{Field assignment vector (Biclustering only).}
#'   \item{field_items}{List of item indices per field (Biclustering only).}
#' }
#'
#' @examples
#' \donttest{
#' # LRA.rated example
#' result_lra <- LRA(J21S300, nrank = 5, mic = TRUE)
#' da <- DistractorAnalysis(result_lra)
#' print(da)
#' print(da, items = 1:3)
#' print(da, ranks = c(1, 5))
#' plot(da)
#' plot(da, items = 1:6, nc = 3, nr = 2)
#'
#' # Biclustering.rated example
#' result_bic <- Biclustering(J21S300, ncls = 5, nfld = 3, method = "R")
#' da_bic <- DistractorAnalysis(result_bic)
#' print(da_bic, items = 1:7)
#' plot(da_bic, items = 1:6, nc = 3, nr = 2)
#' }
#'
#' @importFrom stats pchisq
#' @export
DistractorAnalysis <- function(x, ...) {
  UseMethod("DistractorAnalysis")
}

#' @rdname DistractorAnalysis
#' @export
DistractorAnalysis.LRArated <- function(x, ...) {
  tmp <- x$U
  da <- distractor_core(
    Q = tmp$Q, Z = tmp$Z,
    rank_est = as.numeric(x$Students[, "Estimate"]),
    CA = tmp$CA,
    nrank = x$n_rank,
    maxQ = max(tmp$categories),
    item_labels = tmp$ItemLabel
  )
  da$msg <- "Rank"
  return(da)
}

#' @rdname DistractorAnalysis
#' @export
DistractorAnalysis.ratedBiclustering <- function(x, ...) {
  da <- distractor_core(
    Q = x$Q, Z = x$Z,
    rank_est = x$ClassEstimated,
    CA = if (!is.null(x$U)) {
      # Recover CA from U and Q: CA[j] = category q where U[s,j]==1 for any observed s
      apply(x$Q * x$U * x$Z, 2, function(col) {
        vals <- col[col > 0]
        if (length(vals) > 0) vals[1] else NA
      })
    } else {
      stop("Cannot determine correct answers from result object")
    },
    nrank = x$n_class,
    maxQ = dim(x$FRP)[3],
    item_labels = colnames(x$Q)
  )
  da$msg <- x$msg

  # Attach field metadata
  nfld <- x$n_field
  da$n_field <- nfld
  da$FieldEstimated <- x$FieldEstimated
  da$field_items <- lapply(1:nfld, function(f) which(x$FieldEstimated == f))
  names(da$field_items) <- paste0("Field", 1:nfld)

  return(da)
}


# --- Internal core computation -------------------------------------------

#' @noRd
distractor_core <- function(Q, Z, rank_est, CA, nrank, maxQ, item_labels) {
  nitems <- NCOL(Q)

  # The stored Q keeps the raw category codes, but maxQ comes from the model
  # output (FRP dimension / category count), which is based on codes remapped
  # to 1..K per item (remap_category_codes). Remap Q and CA the same way here,
  # otherwise 0-based or gapped codes are counted against the wrong column
  # (a category-0 response would vanish entirely).
  for (j in seq_len(nitems)) {
    valid_j <- Z[, j] == 1
    lv <- sort(unique(Q[valid_j, j]))
    Q[valid_j, j] <- match(Q[valid_j, j], lv)
    CA[j] <- match(CA[j], lv)
  }

  # Frequency table: item x rank x category
  freq_table <- list()
  for (j in 1:nitems) {
    mat <- matrix(0, nrow = nrank, ncol = maxQ)
    for (r in 1:nrank) {
      students_r <- which(rank_est == r)
      for (q in 1:maxQ) {
        mat[r, q] <- sum(Q[students_r, j] == q & Z[students_r, j] == 1)
      }
    }
    rownames(mat) <- paste0("Rank", 1:nrank)
    colnames(mat) <- paste0("Cat", 1:maxQ)
    freq_table[[j]] <- mat
  }
  names(freq_table) <- item_labels

  # Proportion table
  prop_table <- list()
  for (j in 1:nitems) {
    row_sums <- rowSums(freq_table[[j]])
    row_sums[row_sums == 0] <- 1
    prop_table[[j]] <- freq_table[[j]] / row_sums
  }
  names(prop_table) <- item_labels

  # Chi-square, p-value, Cramer's V
  chisq_table <- matrix(NA, nrow = nitems, ncol = nrank)
  pvalue_table <- matrix(NA, nrow = nitems, ncol = nrank)
  cramersv_table <- matrix(NA, nrow = nitems, ncol = nrank)
  rownames(chisq_table) <- rownames(pvalue_table) <- rownames(cramersv_table) <- item_labels
  colnames(chisq_table) <- colnames(pvalue_table) <- colnames(cramersv_table) <- paste0("Rank", 1:nrank)

  for (j in 1:nitems) {
    for (r in 1:nrank) {
      observed <- freq_table[[j]][r, ]
      n_total <- sum(observed)
      if (n_total > 0) {
        expected <- rep(n_total / maxQ, maxQ)
        chisq_val <- sum((observed - expected)^2 / expected)
        df <- maxQ - 1
        pvalue_table[j, r] <- pchisq(chisq_val, df, lower.tail = FALSE)
        chisq_table[j, r] <- chisq_val
        cramersv_table[j, r] <- sqrt(chisq_val / (n_total * df))
      }
    }
  }

  structure(list(
    freq_table = freq_table,
    prop_table = prop_table,
    chisq_table = chisq_table,
    pvalue_table = pvalue_table,
    cramersv_table = cramersv_table,
    CA = CA,
    n_rank = nrank,
    maxQ = maxQ,
    nitems = nitems,
    ItemLabel = item_labels
  ), class = c("DistractorAnalysis", "exametrika"))
}


# --- Print method --------------------------------------------------------

#' @param items Integer vector of item indices to display. NULL for all items.
#' @param ranks Integer vector of rank/class indices to display. NULL for all ranks.
#' @param digits Number of digits for rounding. Default is 4.
#' @rdname DistractorAnalysis
#' @export
print.DistractorAnalysis <- function(x, items = NULL, ranks = NULL, digits = 4, ...) {
  if (is.null(items)) items <- 1:x$nitems
  if (is.null(ranks)) ranks <- 1:x$n_rank

  rank_cols <- paste0("Rank", ranks)
  msg <- if (!is.null(x$msg)) x$msg else "Rank"

  cat("Distractor Analysis\n\n")

  # Field info (Biclustering only)
  if (!is.null(x$n_field)) {
    cat("Field Grouping:\n")
    for (f in 1:x$n_field) {
      fi <- x$field_items[[f]]
      cat(sprintf("  Field %d: %s\n", f, paste(x$ItemLabel[fi], collapse = ", ")))
    }
    cat("\n")
  }

  # Group items by field (or single group for LRA)
  if (!is.null(x$n_field)) {
    field_groups <- list()
    for (f in 1:x$n_field) {
      field_groups[[f]] <- intersect(x$field_items[[f]], items)
    }
    names(field_groups) <- paste0("Field ", 1:x$n_field)
  } else {
    field_groups <- list(items)
    names(field_groups) <- NULL
  }

  for (g in seq_along(field_groups)) {
    g_items <- field_groups[[g]]
    if (length(g_items) == 0) next

    if (!is.null(names(field_groups)[g])) {
      cat(sprintf("\n--- %s ---\n", names(field_groups)[g]))
    }

    cat("\nCramer's V\n")
    print(round(x$cramersv_table[g_items, ranks, drop = FALSE], digits))

    cat("\nP-values\n")
    print(signif(x$pvalue_table[g_items, ranks, drop = FALSE], digits))

    cat("\nCategory Proportions by", msg, "\n")
    for (j in g_items) {
      ca <- x$CA[j]
      cat(sprintf("\n  %s (CA = Cat%d)\n", x$ItemLabel[j], ca))
      sub <- x$prop_table[[j]][ranks, , drop = FALSE]
      rownames(sub) <- paste0(msg, ranks)
      print(round(sub, digits))
    }
  }

  invisible(x)
}


# --- Plot method ---------------------------------------------------------

#' @param type Plot type. Currently only "Distractor" is supported.
#' @param nc Number of columns in the plot grid.
#' @param nr Number of rows in the plot grid.
#' @rdname DistractorAnalysis
#' @export
plot.DistractorAnalysis <- function(x, type = "Distractor",
                                    items = NULL, ranks = NULL,
                                    nc = 1, nr = 1, ...) {
  if (is.null(items)) items <- 1:x$nitems
  if (is.null(ranks)) ranks <- 1:x$n_rank

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(nr, nc), mar = c(3, 4, 3, 1))

  for (j in items) {
    freq <- x$freq_table[[j]][ranks, , drop = FALSE]
    row_sums <- rowSums(freq)
    row_sums[row_sums == 0] <- 1
    props <- freq / row_sums
    props[is.nan(props)] <- 0

    ca <- x$CA[j]
    colors <- rep("gray70", x$maxQ)
    colors[ca] <- "steelblue"

    msg <- if (!is.null(x$msg)) x$msg else "Rank"

    barplot(t(props),
      beside = FALSE,
      col = colors,
      main = paste0(x$ItemLabel[j], " (CA=Cat", ca, ")"),
      xlab = msg, ylab = "Proportion",
      names.arg = paste0(substr(msg, 1, 1), ranks),
      cex.main = 0.9
    )
  }
}
