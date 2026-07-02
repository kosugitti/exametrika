# print.exametrika() case bodies for network-model classes
# (BNM, LDLRA, LDB, BINET).
# Split out of R/00_exametrikaPrint.R (2026-07-01) for maintainability;
# no logic changed, only moved into named functions called from the
# print.exametrika() switch() in R/00_exametrikaPrint.R.

#' @title Print helper: BNM
#' @noRd
print_bnm_case <- function(x, digits) {
  cat("Adjacency Matrix\n")
  print(x$adj)
  if (x$acyclicFLG == 1) {
    print("Your graph is an acyclic graph.")
  }
  if (x$connectedFLG == 1) {
    print("Your graph is connected DAG.")
  }
  lay.tree <- layout_on_grid(x$g)
  xcoord <- lay.tree[, 1]
  ycoord <- lay.tree[, 2]
  xcoord <- xcoord[order(rowSums(x$adj))]
  ycoord <- ycoord[order(x$crr, decreasing = TRUE)]

  plot.igraph(x$g, layout = cbind(xcoord, ycoord))

  cat("\nParameter Learning\n")
  p_table <- x$param
  rownames(p_table) <- x$ItemLabel
  colnames(p_table) <- paste("PIRP", 1:ncol(p_table))
  print(p_table, na.print = "", digits = digits)

  cat("\nConditional Correct Response Rate\n")
  for (j in 1:NROW(x$CCRR_table)) {
    if (is.nan(x$CCRR_table[j, 5])) {
      x$CCRR_table[j, 5] <- "NaN(0/0)"
    } else {
      x$CCRR_table[j, 5] <- sprintf(
        paste0("%.", digits, "f"),
        as.numeric(x$CCRR_table[j, 5])
      )
    }
  }
  print(x$CCRR_table)

  cat("\nModel Fit Indices\n")
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}

#' @title Print helper: LDLRA
#' @noRd
print_ldlra_case <- function(x, digits) {
  if (x$model == 1) {
    msg <- "Class"
  } else {
    msg <- "Rank"
  }
  cat("Adjacency Matrix\n")
  print(x$adj_list)
  lay.tree <- layout_on_grid(x$g_list[[1]])
  xcoord <- lay.tree[, 1]
  ycoord <- lay.tree[, 2]
  xcoord <- xcoord[order(rowSums(x$adj_list[[1]]))]
  ycoord <- ycoord[order(x$crr, decreasing = TRUE)]
  for (i in 1:x$Nclass) {
    plot.igraph(x$g_list[[i]],
      layout = cbind(xcoord, ycoord),
      main = paste("Graph of ", msg, i)
    )
  }

  cat("\nParameter Learning\n")
  y <- x$Estimation_table
  numeric_cols <- which(sapply(y[, 3:NCOL(y)], is.numeric))
  for (j in numeric_cols) {
    y[, j + 2] <- ifelse(is.na(y[, j + 2]), "",
      sprintf(paste0("%.", digits, "f"), y[, j + 2])
    )
  }
  print(y)

  cat("\nConditional Correct Response Rate\n")
  for (j in 1:NROW(x$CCRR_table)) {
    if (is.nan(x$CCRR_table[j, 6])) {
      x$CCRR_table[j, 6] <- "NaN(0/0)"
    } else {
      x$CCRR_table[j, 6] <- sprintf(
        paste0("%.", digits, "f"),
        as.numeric(x$CCRR_table[j, 6])
      )
    }
  }
  print(x$CCRR_table, digits = digits)
  cat("\nMarginal Item Reference Profile\n")
  print(x$IRP, na.print = "", digits = digits)
  cat("\nIRP Indices\n")
  print(x$IRPIndex, digits = digits)
  if (x$SOACflg) {
    print("Strongly ordinal alignment condition was satisfied.")
  }

  cat("\nTest reference Profile and Latent Rank Distribution\n")
  y <- rbind(x$TRP, x$LRD, x$RMD)
  rownames(y) <- c(
    "Test Reference Profile",
    paste("Latent", msg, "Ditribution"),
    paste(msg, "Membership Distribution")
  )
  colnames(y) <- paste(msg, 1:x$Nclass)
  print(round(y, digits))

  if (x$WOACflg) {
    print("Weakly ordinal alignment condition was satisfied.")
  }

  cat("\nModel Fit Indices\n")
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))
}

#' @title Print helper: LDB
#' @noRd
print_ldb_case <- function(x, digits) {
  cat("Adjacency Matrix\n")
  print(x$adj_list)
  lay.tree <- layout_on_grid(x$g_list[[1]])
  xcoord <- lay.tree[, 1]
  ycoord <- lay.tree[, 2]
  xcoord <- xcoord[order(rowSums(x$adj_list[[1]]))]
  ycoord <- ycoord[order(colSums(x$adj_list[[1]]))]
  for (i in 1:x$Nrank) {
    plot.igraph(x$g_list[[i]],
      layout = cbind(xcoord, ycoord),
      main = paste("Graph at Rank", i)
    )
  }

  cat("\nParameter Learning\n")
  for (i in 1:x$Nrank) {
    tbl <- x$IRP[i, , ]
    tbl[tbl == 0] <- NA
    rownames(tbl) <- x$FieldLabel
    colnames(tbl) <- paste("PIRP", 0:(NCOL(tbl) - 1))
    cat(paste("Rank", i, "\n"))
    print(tbl, na.print = "", digits = digits)
  }

  cat("\nMarginal Rankluster Reference Matrix\n")
  print(x$FRP, digits = digits)
  cat("\nIRP Indices\n")
  print(x$FRPIndex, digits = digits)

  y <- rbind(x$TRP, x$LRD, x$RMD)
  rownames(y) <- c(
    "Test Reference Profile",
    "Latent Rank Ditribution",
    "Rank Membership Dsitribution"
  )
  colnames(y) <- paste("Rank", 1:x$Nrank)
  print(round(y, digits))

  cat("\nLatent Field Distribution\n")
  y <- matrix(x$LFD, nrow = 1)
  rownames(y) <- "N of Items"
  colnames(y) <- paste("Field", 1:x$Nfield)
  print(y)

  cat("\nModel Fit Indices\n")
  y <- unclass(x$TestFitIndices)
  y <- t(as.data.frame(y))
  colnames(y) <- "value"
  print(round(y, digits))

  if (x$SOACflg & x$WOACflg) {
    message("Strongly ordinal alignment condition was satisfied.")
  }
  if (!x$SOACflg & x$WOACflg) {
    message("Weakly ordinal alignment condition was satisfied.")
  }
}

#' @title Print helper: BINET
#' @noRd
print_binet_case <- function(x, digits) {
  cat("Total Graph\n")
  print(x$all_adj)
  layout_fr <- layout_with_fr(x$all_g)
  plot.igraph(x$all_g,
    layout = layout_fr,
    edge.label = E(x$all_g)$Field
  )
  cat("Estimation of Parameter set\n")
  for (i in 1:length(x$PSRP)) {
    cat("Field", i, "\n")
    print(x$PSRP[[i]], na.print = "", digits = digits)
  }
  cat("Local Dependence Passing Student Rate\n")
  df <- x$LDPSR
  for (j in 1:NCOL(df)) {
    if (is.numeric(df[, j])) {
      df[, j] <- sprintf(paste0("%.", digits, "f"), df[, j])
    }
    flg <- df[, j] == "NA"
    df[, j][flg] <- ""
  }
  print(df, na.print = "")

  cat("Marginal Bicluster Reference Matrix\n")
  print(round(x$FRP, digits))

  y <- rbind(x$TRP, x$LCD, x$CMD)
  rownames(y) <- c(
    "Test Reference Profile",
    "Latent Class Ditribution",
    "Class Membership Dsitribution"
  )
  colnames(y) <- paste("Class", 1:x$Nclass)
  print(round(y, digits))

  cat("\nModel Fit Indices\n")
  y1 <- unclass(x$MG_FitIndices)
  y2 <- unclass(x$SM_FitIndices)
  y <- t(as.data.frame(rbind(y1, y2)))
  colnames(y) <- c("Multigroup Model", "Saturated Moodel")
  print(y)
}
