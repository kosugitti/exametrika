# print.exametrika() case bodies for LCA/LRA family classes
# (LCA, LRA, LRAordinal, LRArated).
# Split out of R/00_exametrikaPrint.R (2026-07-01) for maintainability;
# no logic changed, only moved into named functions called from the
# print.exametrika() switch() in R/00_exametrikaPrint.R.

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
