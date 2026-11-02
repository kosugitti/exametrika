# print.exametrika() case bodies for CTT/basic-statistics/IRT-family classes
# (TestStatistics, Dimensionality, ItemStatistics, QitemStatistics,
# exametrikaData, IIAnalysis, IIAnalysis.ordinal, CTT, IRT, GRM).
# Split out of R/00_exametrikaPrint.R (2026-07-01) for maintainability;
# no logic changed, only moved into named functions called from the
# print.exametrika() switch() in R/00_exametrikaPrint.R.

#' @title Print helper: TestStatistics
#' @noRd
print_test_statistics_case <- function(x, digits) {
  cat("Test Statistics\n")
  tmp <- as.data.frame(unlist(x))
  colnames(tmp) <- "value"
  print(tmp)
}

#' @title Print helper: Dimensionality
#' @noRd
print_dimensionality_case <- function(x, digits) {
  cat("Dimensionality Analysis\n")
  cat("Eigenvalues\n")
  print(x$Eigenvalue)
  cat("Percentage Of Variance\n")
  print(x$PerOfVar)
  cat("Cumulative Percentage\n")
  print(x$CumOfPer)
  if (!is.null(x$first_loading)) {
    cat("Loadings on the First Component\n")
    print(round(x$first_loading, digits))
    neg <- names(x$first_loading)[x$first_loading < 0]
    if (length(neg) > 0) {
      cat("Opposite sign to the rest (reverse-keyed?):", paste(neg, collapse = ", "), "\n")
    }
  }
  xdim <- x$Component
  ydim <- x$Eigenvalue
  plot(xdim, ydim,
    xlab = "Number of Eigenvalues",
    ylab = "Eigenvalue", type = "b"
  )
}

#' @title Print helper: ItemStatistics
#' @noRd
print_item_statistics_case <- function(x, digits) {
  cat("Item Statistics\n")
  tmp <- as.data.frame(unclass(x))
  rownames(tmp) <- NULL
  print(tmp, digits = digits)
}

#' @title Print helper: QitemStatistics
#' @noRd
print_qitem_statistics_case <- function(x, digits) {
  cat("Item Statistics\n")
  tmp <- as.data.frame(unclass(x))
  rownames(tmp) <- NULL
  print(tmp, digits = digits)
}

#' @title Print helper: Scalability
#' @noRd
print_scalability_case <- function(x, digits) {
  strength <- if (!is.finite(x$H)) {
    "not available"
  } else if (x$H >= 0.5) {
    "strong"
  } else if (x$H >= 0.4) {
    "moderate"
  } else if (x$H >= 0.3) {
    "weak"
  } else {
    "below 0.3: not a scale by Mokken's rule of thumb"
  }
  cat("Scalability Coefficients (Loevinger's H)\n")
  cat(sprintf("Test H = %s (%s)\n\n", format(round(x$H, digits), nsmall = digits), strength))
  flag <- ifelse(x$Hj < 0, "negative: reverse-keyed or non-monotone?",
    ifelse(x$Hj < 0.3, "below 0.3", "")
  )
  cat("Item coefficients\n")
  print(data.frame(Item = x$ItemLabel, Hj = round(x$Hj, digits), Note = flag),
    row.names = FALSE
  )
  cat("\nItem-pair coefficients (Hij)\n")
  print(round(x$Hij, digits))
  return(invisible(x))
}

#' @title Print helper: ManifestMonotonicity
#' @noRd
print_monotonicity_case <- function(x, digits) {
  cat("Manifest Monotonicity (rest-score regression)\n")
  cat(sprintf(
    "minsize = %d, minvi = %s%s\n\n", as.integer(x$minsize), format(x$minvi),
    if (x$n_dropped > 0) sprintf(", %d respondents dropped for missing responses", x$n_dropped) else ""
  ))
  s <- x$summary
  s$Note <- ifelse(s$n_significant > 0, "significant violation",
    ifelse(s$n_violation > 0, "violation (not significant)", "")
  )
  num <- vapply(s, is.numeric, logical(1))
  s[num] <- lapply(s[num], function(v) round(v, digits))
  print(s, row.names = FALSE)
  return(invisible(x))
}

#' @title Print helper: exametrikaData
#' @noRd
print_exametrika_data_case <- function(x, digits) {
  cat("Response Type:", x$response.type, "\n")
  if (x$response.type == "binary") {
    cat("Binary Response Pattern\n")
    print(x$U)
  } else {
    cat("Polytomous Response Pattern (", x$response.type, ")\n")
    print(x$Q)
    if (x$response.type == "rated") {
      cat("\nCorrect Answers\n")
      print(x$CA)
    }
  }
  cat("\nMissing Pattern\n")
  print(x$Z)
  cat("\nWeight\n")
  print(x$w)
  if (!is.null(x$factor_labels)) {
    cat("\nFactor Labels\n")
    print(x$factor_labels)
  }
}

#' @title Print helper: IIAnalysis
#' @noRd
print_ii_analysis_case <- function(x, digits) {
  cat("Joint Sample Size\n")
  print(x$JSS, digits = digits)
  cat("\nJoint Correct Response Ratio\n")
  print(x$JCRR, digits = digits)
  cat("\nConditional Correct Response Ratio\n")
  print(x$CCRR, digits = digits)
  cat("\nItem Lift\n")
  print(x$IL, digits = digits)
  cat("\nMutual Information\n")
  print(x$MI, digits = digits)
  cat("\nPhi coefficient\n")
  print(x$Phi, digits = digits)
  cat("\nCorrelation Matrix\n")
  print(x$Tetrachoric, digits = digits)
}

#' @title Print helper: IIAnalysis.ordinal
#' @noRd
print_ii_analysis_ordinal_case <- function(x, digits) {
  cat("Joint Sample Size\n")
  print(x$JSS, digits = digits)
  cat("\nJoint Selection Ratio\n")
  print(x$JSR, digits = digits)
  cat("\nConditional Selection Ratio\n")
  print(x$CSR, digits = digits)
  cat("\nMutual Information\n")
  print(x$MI, digits = digits)
  cat("\nCorrelation Matrix\n")
  print(x$Polychoric, digits = digits)
}

#' @title Print helper: CTT
#' @noRd
print_ctt_case <- function(x, digits) {
  cat("Reliability\n")
  print(x$Reliability, digits = digits)
  cat("\nReliability Excluding Item\n")
  print(x$ReliabilityExcludingItem, digits = digits)
}

#' @title Print helper: IRT
#' @noRd
print_irt_case <- function(x, digits) {
  cat("Item Parameters\n")
  y <- cbind(x$params, x$itemPSD)
  print(y, digits = digits)
  cat("\nItem Fit Indices\n")
  y <- unclass(x$ItemFitIndices)
  y <- as.data.frame(y)
  print(round(y, digits))
  cat("\nModel Fit Indices\n")
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}

#' @title Print helper: GRM
#' @noRd
print_grm_case <- function(x, digits) {
  cat("Item Parameter\n")
  print(x$params, digits = digits)
  cat("\nItem Fit Indices\n")
  y <- unclass(x$ItemFitIndices)
  y <- as.data.frame(y)
  print(round(y, digits))
  cat("\nModel Fit Indices\n")
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}
