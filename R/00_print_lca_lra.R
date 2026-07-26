# print.exametrika() case bodies for LCA/LRA family classes
# (LCA, LRA, LRAordinal, LRArated).
# Split out of R/00_exametrikaPrint.R (2026-07-01) for maintainability;
# no logic changed, only moved into named functions called from the
# print.exametrika() switch() in R/00_exametrikaPrint.R.

#' @title Print helper: ratedLCA
#' @noRd
print_rated_lca_case <- function(x, digits, fit_indices = "both") {
  cat("\nItem Reference Profile\n")
  print(x$IRP, digits = digits)
  cat("\nTest Profile\n")
  y <- rbind(x$TRP, x$LCD, x$CMD)
  rownames(y) <- c(
    "Test Reference Profile",
    "Latent Class Ditribution",
    "Class Membership Distribution"
  )
  colnames(y) <- paste("Class", 1:x$n_class)
  print(round(y, digits))
  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent class:", x$n_class))
  cat(paste("\nNumber of EM cycle:", x$n_cycle, "\n"))
  cat("Binary layer (correct / incorrect)\n")
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
  if (fit_indices %in% c("both", "pattern")) {
    cat("\nNominal layer (all categories), response-pattern based\n")
    y <- unclass(x$TestFitIndicesNominal)
    y <- t(as.data.frame(y))
    colnames(y) <- "value"
    print(round(y, digits))
  }
  if (fit_indices %in% c("both", "margin") && !is.null(x$TestFitIndicesM2)) {
    cat("\nNominal layer (all categories), margin based (M2)\n")
    y <- unclass(x$TestFitIndicesM2)
    y <- t(as.data.frame(y))
    colnames(y) <- "value"
    print(round(y, digits))
  }
}

#' @title Print helper: nominalLCA
#' @noRd
print_nominal_lca_case <- function(x, digits, fit_indices = "both") {
  cat("\nItem Category Reference Profile\n")
  print(x$ICRP, digits = digits)
  cat("\nTest Profile\n")
  y <- rbind(x$LCD, x$CMD)
  rownames(y) <- c(
    "Latent Class Ditribution",
    "Class Membership Distribution"
  )
  colnames(y) <- paste("Class", 1:x$n_class)
  print(round(y, digits))
  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent class:", x$n_class))
  cat(paste("\nNumber of EM cycle:", x$n_cycle, "\n"))
  print_fit_two_worlds(x, digits, fit_indices)
}

#' @title Print the response-pattern and margin-based fit indices
#' @description
#' The two must not be mixed into one set of indices -- they are built from
#' chi-squares that live in different worlds (see \code{\link{M2}}) -- so they
#' are shown as two blocks. The margin-based block appears only after
#' \code{add_M2()} has been called, since it is expensive to compute.
#' @noRd
print_fit_two_worlds <- function(x, digits, fit_indices = c("both", "pattern", "margin")) {
  fit_indices <- match.arg(fit_indices)
  has_margin <- !is.null(x$TestFitIndicesM2)

  if (fit_indices %in% c("both", "pattern")) {
    if (has_margin || fit_indices == "both") {
      cat("\nResponse-pattern based\n")
    }
    y <- unclass(x$TestFitIndices)
    y <- t(as.data.frame(y))
    colnames(y) <- "value"
    print(round(y, digits))
  }

  if (fit_indices %in% c("both", "margin")) {
    if (has_margin) {
      cat("\nMargin based (M2)\n")
      y <- unclass(x$TestFitIndicesM2)
      y <- t(as.data.frame(y))
      colnames(y) <- "value"
      print(round(y, digits))
    } else if (fit_indices == "margin") {
      cat("\nMargin based (M2): not computed. Call add_M2() first.\n")
    }
  }
  return(invisible(NULL))
}

#' @title Print helper: LCA
#' @noRd
print_lca_case <- function(x, digits) {
  cat("\nItem Reference Profile\n")
  print(x$IRP, digits = digits)
  cat("\nTest Profile\n")
  y <- rbind(x$TRP, x$LCD, x$CMD)
  rownames(y) <- c(
    "Test Reference Profile",
    "Latent Class Ditribution",
    "Class Membership Distribution"
  )
  colnames(y) <- paste("Class", 1:x$Nclass)
  print(round(y, digits))
  cat("\nItem Fit Indices\n")
  y <- unclass(x$ItemFitIndices)
  y <- as.data.frame(y)
  print(round(y, digits))
  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent class:", x$Nclass))
  cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}

#' @title Print helper: LRA
#' @noRd
print_lra_case <- function(x, digits) {
  cat(paste("estimating method is ", x$method, "\n"))
  if (x$mic) {
    cat("\n Monotonic increasing IRP option is TRUE.\n")
  }
  cat("Item Reference Profile\n")
  print(x$IRP, digits = digits)
  cat("\nItem Reference Profile Indices\n")
  print(x$IRPIndex, digits = digits)
  cat("\nTest Profile\n")
  y <- rbind(x$TRP, x$LRD, x$RMD)
  rownames(y) <- c(
    "Test Reference Profile",
    "Latent Rank Ditribution",
    "Rank Membership Distribution"
  )
  colnames(y) <- paste("Rank", 1:x$Nrank)
  print(round(y, digits))
  cat("\nItem Fit Indices\n")
  y <- unclass(x$ItemFitIndices)
  y <- as.data.frame(y)
  print(round(y, digits))
  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent rank:", x$Nrank))
  cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}

#' @title Print helper: LRAordinal
#' @noRd
print_lra_ordinal_case <- function(x, digits) {
  if (x$mic) {
    cat("\n Monotonic increasing IRP option is TRUE.\n")
  }
  print(x$ScoreReport, digits = digits)
  print(x$ItemReport, digits = digits)
  cat("Item Category Reference Profile\n")
  print(x$ICRP, digits = digits)
  cat("\nTest Profile\n")
  y <- rbind(x$TRP, x$LRD, x$RMD)
  rownames(y) <- c(
    "Test Reference Profile",
    "Latent Rank Ditribution",
    "Rank Membership Distribution"
  )
  colnames(y) <- paste("Rank", 1:x$Nrank)
  print(round(y, digits))
  cat("\nItem Fit Indices\n")
  y <- unclass(x$ItemFitIndices)
  y <- as.data.frame(y)
  print(round(y, digits))
  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent rank:", x$Nrank))
  cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}

#' @title Print helper: LRArated
#' @noRd
print_lra_rated_case <- function(x, digits) {
  if (x$mic) {
    cat("\n Monotonic increasing IRP option is TRUE.\n")
  }
  print(x$ScoreReport, digits = digits)
  print(x$ItemReport, digits = digits)
  cat("Item Quantile Reference Matrix\n")
  print(x$ItemQuantileRef, digits = digits)
  cat("Item Category Reference Profile\n")
  print(x$ICRP, digits = digits)
  cat("\nTest Profile\n")
  y <- rbind(x$TRP, x$LRD, x$RMD)
  rownames(y) <- c(
    "Test Reference Profile",
    "Latent Rank Ditribution",
    "Rank Membership Distribution"
  )
  colnames(y) <- paste("Rank", 1:x$Nrank)
  print(round(y, digits))
  cat("\nItem Fit Indices\n")
  y <- unclass(x$ItemFitIndices)
  y <- as.data.frame(y)
  print(round(y, digits))
  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent rank:", x$Nrank))
  cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}
