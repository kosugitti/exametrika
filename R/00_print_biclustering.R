# print.exametrika() case bodies for Biclustering family classes
# (Biclustering, ratedBiclustering, nominalBiclustering,
# ordinalBiclustering, IRM).
# Split out of R/00_exametrikaPrint.R (2026-07-01) for maintainability;
# no logic changed, only moved into named functions called from the
# print.exametrika() switch() in R/00_exametrikaPrint.R.

#' @title Print helper: Biclustering
#' @noRd
print_biclustering_case <- function(x, digits) {
  model_name <- ifelse(x$model == 1, "Biclustering", "Ranklustering")
  mic_suffix <- if (x$mic) " [MIC]" else ""

  cat(sprintf("%s Analysis%s\n\n", model_name, mic_suffix))

  cat(paste(model_name, "Reference Matrix Profile\n"))
  print(x$FRP, digits = digits)

  cat("\nField Reference Profile Indices\n")
  print(x$FRPIndex, digits = digits)
  cat("\n")

  y <- rbind(x$TRP, x$LRD, x$CMD)
  rownames(y) <- c(
    "Test Reference Profile",
    paste("Latent", x$msg, "Ditribution"),
    paste(x$msg, "Membership Distribution")
  )
  colnames(y) <- paste(x$msg, 1:x$Nclass)
  print(round(y, digits))

  cat("\nField Membership Profile\n")
  y <- as.data.frame(x$FieldAnalysis)
  y$LFE <- as.integer(y$LFE)
  num_cols <- setdiff(names(y), "LFE")
  y[num_cols] <- lapply(y[num_cols], function(v) format(round(v, digits), nsmall = digits))
  print(y)

  cat("Latent Field Distribution\n")
  y <- matrix(x$LFD, byrow = T, nrow = 1)
  rownames(y) <- "N of Items"
  colnames(y) <- paste("Field", 1:x$Nfield)
  print(round(y, digits))
  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent", x$msg, ":", x$Nclass))
  cat(paste("\nNumber of Latent Field:", x$Nfield))
  cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
  if (x$SOACflg) {
    cat("Strongly Ordinal Alignment Condition is Satisfied.\n")
  }
  if (x$WOACflg) {
    cat("Weakly Ordinal Alignment Condition is Satisfied.\n")
  }
}

#' @title Print helper: ratedBiclustering
#' @noRd
print_rated_biclustering_case <- function(x, digits) {
  model_name <- ifelse(x$model == 1, "Biclustering", "Ranklustering")
  mic_suffix <- if (x$mic) " [MIC]" else ""

  cat(sprintf("%s Analysis (Rated)%s\n\n", model_name, mic_suffix))

  cat(paste(model_name, "Reference Matrix Profile (Nominal)\n"))
  for (q in 1:dim(x$FRP)[3]) {
    cat(paste("For category", q, "\n"))
    y <- x$FRP[, , q]
    colnames(y) <- paste(x$msg, 1:x$Nclass)
    rownames(y) <- paste("Field", 1:x$Nfield)
    print(y, digits = digits)
  }

  cat(paste("\nField Reference Profile (Binary)\n"))
  y <- x$FieldFRP
  colnames(y) <- paste(x$msg, 1:x$Nclass)
  rownames(y) <- paste("Field", 1:x$Nfield)
  print(round(y, digits))

  cat("\nField Reference Profile Indices\n")
  print(x$FRPIndex, digits = digits)
  cat("\n")

  y <- rbind(x$TRP, x$LRD, x$CMD)
  rownames(y) <- c(
    "Test Reference Profile",
    paste("Latent", x$msg, "Ditribution"),
    paste(x$msg, "Membership Distribution")
  )
  colnames(y) <- paste(x$msg, 1:x$Nclass)
  print(round(y, digits))

  cat("\nField Membership Profile\n")
  y <- as.data.frame(x$FieldAnalysis)
  y$LFE <- as.integer(y$LFE)
  num_cols <- setdiff(names(y), "LFE")
  y[num_cols] <- lapply(y[num_cols], function(v) format(round(v, digits), nsmall = digits))
  print(y)

  cat("Latent Field Distribution\n")
  y <- matrix(x$LFD, byrow = TRUE, nrow = 1)
  rownames(y) <- "N of Items"
  colnames(y) <- paste("Field", 1:x$Nfield)
  print(round(y, digits))

  # Layer 1: Binary fit indices
  cat("\nModel Fit Indices (Binary)\n")
  cat(paste("Number of Latent", x$msg, ":", x$Nclass))
  cat(paste("\nNumber of Latent Field:", x$Nfield))
  cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))

  # Layer 2: Nominal fit indices (AIC/BIC/CAIC/LogLik only)
  cat("\nModel Fit Indices (Nominal)\n")
  nom <- x$TestFitIndices_nominal
  y <- data.frame(value = c(
    nom$model_log_like,
    nom$AIC,
    nom$CAIC,
    nom$BIC
  ))
  rownames(y) <- c("model_log_like", "AIC", "CAIC", "BIC")
  print(round(y, digits))

  if (x$SOACflg) {
    cat("Strongly Ordinal Alignment Condition is Satisfied.\n")
  }
  if (x$WOACflg) {
    cat("Weakly Ordinal Alignment Condition is Satisfied.\n")
  }
}

#' @title Print helper: nominalBiclustering
#' @noRd
print_nominal_biclustering_case <- function(x, digits) {
  cat(paste("Biclustering Reference Matrix Profile\n"))
  for (q in 1:dim(x$FRP)[3]) {
    cat(paste("For category", q, "\n"))
    y <- x$FRP[, , q]
    colnames(y) <- paste(x$msg, 1:x$Nclass)
    rownames(y) <- paste("Field", 1:x$Nfield)
    print(y, digits = digits)
  }

  y <- rbind(x$LRD, x$CMD)
  rownames(y) <- c(
    paste("Latent", x$msg, "Ditribution"),
    paste(x$msg, "Membership Distribution")
  )
  colnames(y) <- paste(x$msg, 1:x$Nclass)
  print(round(y, digits))

  cat("Latent Field Distribution\n")
  y <- matrix(x$LFD, byrow = T, nrow = 1)
  rownames(y) <- "N of Items"
  colnames(y) <- paste("Field", 1:x$Nfield)
  print(round(y, digits))

  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent", x$msg, ":", x$Nclass))
  cat(paste("\nNumber of Latent Field:", x$Nfield))
  cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
  y <- unclass(x$TestFitIndices)
  y$LogLik <- x$LogLik
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}

#' @title Print helper: ordinalBiclustering
#' @noRd
print_ordinal_biclustering_case <- function(x, digits) {
  model_name <- ifelse(x$model == 1, "Biclustering", "Ranklustering")
  mic_suffix <- if (x$mic) " [MIC]" else ""

  cat(sprintf("%s Analysis%s\n\n", model_name, mic_suffix))

  cat(paste(model_name, "Reference Matrix Profile\n"))
  for (q in 1:dim(x$FRP)[3]) {
    cat(paste("For category", q, "\n"))
    y <- x$FRP[, , q]
    colnames(y) <- paste(x$msg, 1:x$Nclass)
    rownames(y) <- paste("Field", 1:x$Nfield)
    print(y, digits = digits)
  }
  y <- rbind(x$TRP, x$LRD, x$CMD)
  rownames(y) <- c(
    "Test Reference Profile",
    paste("Latent", x$msg, "Ditribution"),
    paste(x$msg, "Membership Distribution")
  )
  colnames(y) <- paste(x$msg, 1:x$Nclass)
  print(round(y, digits))
  cat("Latent Field Distribution\n")
  y <- matrix(x$LFD, byrow = T, nrow = 1)
  rownames(y) <- "N of Items"
  colnames(y) <- paste("Field", 1:x$Nfield)
  print(round(y, digits))

  cat("Boundary field reference profile\n")
  cat("Weighted\n")
  y <- x$BFRP$Weighted
  colnames(y) <- paste(x$msg, 1:x$Nclass)
  rownames(y) <- paste("Field", 1:x$Nfield)
  print(round(y, digits))
  cat("Observed\n")
  y <- x$BFRP$Observed
  colnames(y) <- paste(x$msg, 1:x$Nclass)
  rownames(y) <- paste("Field", 1:x$Nfield)
  print(round(y, digits))

  cat("\nField Reference Profile Indices\n")
  cat("(Based on normalized expected scores: (E[score]-1)/(maxQ-1))\n")
  print(x$FRPIndex, digits = digits)

  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent", x$msg, ":", x$Nclass))
  cat(paste("\nNumber of Latent Field:", x$Nfield))
  cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
  y <- unclass(x$TestFitIndices)
  y$LogLik <- x$LogLik
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
  if (x$WOACflg) {
    cat("Weakly Ordinal Alignment Condition is Satisfied.\n")
  }
}

#' @title Print helper: IRM
#' @noRd
print_irm_case <- function(x, digits) {
  cat(paste("Bicluster Reference Matrix\n"))
  print(x$FRP, digits = digits)

  y <- rbind(x$TRP, x$LCD)
  rownames(y) <- c(
    "Test Reference Profile",
    "Latent class Ditribution"
  )
  colnames(y) <- paste("class", 1:x$Nclass)
  print(round(y, digits))

  cat("Latent Field Distribution\n")
  y <- matrix(x$LFD, byrow = T, nrow = 1)
  rownames(y) <- "N of Items"
  colnames(y) <- paste("Field", 1:x$Nfield)
  print(round(y, digits))

  cat("\nModel Fit Indices\n")
  cat(paste("Number of Latent Class :", x$Nclass))
  cat(paste("\nNumber of Latent Field:", x$Nfield))
  cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}
