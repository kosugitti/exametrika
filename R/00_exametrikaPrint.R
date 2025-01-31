#' @title print.exametrika
#' @description
#' Output format for exametrika Class
#' @param x exametrika Class object
#' @param digits printed digits
#' @param ... other options
#' @importFrom utils tail
#' @importFrom igraph plot.igraph
#' @importFrom igraph layout_on_grid
#' @importFrom igraph layout_with_fr
#' @importFrom igraph E
#' @return Prints a formatted summary of the exametrika object to the console, with content
#'   varying by object class:
#'   \describe{
#'     \item{TestStatistics}{Basic descriptive statistics of the test}
#'     \item{Dimensionality}{Eigenvalue analysis results with scree plot}
#'     \item{ItemStatistics}{Item-level statistics}
#'     \item{CTT}{Classical Test Theory reliability measures}
#'     \item{IRT}{Item parameters and fit indices}
#'     \item{LCA/LRA}{Class/Rank profiles and model fit information}
#'     \item{Biclustering/IRM}{Cluster profiles and model diagnostics}
#'     \item{Network models (LDLRA/LDB/BINET)}{Network visualizations and parameter estimates}
#'   }
#' @export

print.exametrika <- function(x, digits = 3, ...) {
  value <- if (length(class(x)) > 1) tail(class(x), 1) else "all"

  switch(value,
    TestStatistics = {
      cat("Test Statics\n")
      tmp <- as.data.frame(unlist(x))
      colnames(tmp) <- "value"
      print(tmp)
    },
    Dimensionality = {
      cat("Dimensionality Analyeis\n")
      cat("Eigenvalues\n")
      print(x$Eigenvalue)
      cat("Percentage Of Variance\n")
      print(x$PerOfVar)
      cat("Cummurative Percentage\n")
      print(x$CumOfPer)
      xdim <- x$Component
      ydim <- x$Eigenvalue
      plot(xdim, ydim,
        xlab = "Number of Components",
        ylab = "Eigenvalue", type = "b"
      )
    },
    ItemStatistics = {
      cat("Item Statics\n")
      tmp <- as.data.frame(unclass(x))
      rownames(tmp) <- NULL
      print(tmp, digits = digits)
    },
    QitemStatistics = {
      cat("Item Statics\n")
      tmp <- as.data.frame(unclass(x))
      rownames(tmp) <- NULL
      print(tmp, digits = digits)
    },
    exametrikaData = {
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
    },
    IIAnalysis = {
      cat("Joint Sample Size\n")
      print(x$JSS, digits = digits)
      cat("\nJoint Correct Response Rate\n")
      print(x$JCRR, digits = digits)
      cat("\nItem Lift\n")
      print(x$IL, digits = digits)
      cat("\nMutual Information\n")
      print(x$MI, digits = digits)
      cat("\nPhi coefficient\n")
      print(x$Phi, digits = digits)
      cat("\nCorrelation Matrix\n")
      print(x$Tetrachoric, digits = digits)
    },
    CTT = {
      cat("Realiability\n")
      print(x$Reliability, digits = digits)
      cat("\nReliability Excluding Item\n")
      print(x$ReliabilityExcludingItem, digits = digits)
    },
    IRT_EAP_PSD = {
      cat("Ability")
      print(x$EAP, digits = digits)
    },
    IRT = {
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
    },
    LCA = {
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
    },
    LRA = {
      cat(paste("estimating method is ", x$method))
      if (x$mic) {
        cat("\n Monotonic increasing IRP option is TRUE.\n")
      }
      cat("Item Reference Profile\n")
      print(x$IRP, digits = digits)
      cat("\nItem Reference Profile Indices\n")
      print(x$IRPIndex, digits = digits)
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
    },
    Biclustering = {
      if (x$model == 1) {
        model <- "Biclustering"
        msg1 <- "Bicluster"
        msg2 <- "Class"
      } else {
        model <- "Ranklustering"
        msg1 <- "Rankluster"
        msg2 <- "Rank"
      }
      if (x$mic) {
        model <- paste(model, "with MIC option.")
      }
      cat(paste(msg1, "Matrix Profile\n"))
      print(x$FRP, digits = digits)

      cat("\nField Reference Profile Indices\n")
      print(x$FRPIndex, digits = digits)
      cat("\n")

      y <- rbind(x$TRP, x$LRD, x$CMD)
      rownames(y) <- c(
        "Test Reference Profile",
        paste("Latent", msg2, "Ditribution"),
        paste(msg2, "Membership Distribution")
      )
      colnames(y) <- paste(msg2, 1:x$Nclass)
      print(round(y, digits))

      if (x$model == 2) {
        cat("\nField Membership Profile\n")
        y <- format(
          round(as.data.frame(x$FieldMembership), digits),
          nsmall = digits
        )
        print(y)
      }

      cat("Latent Field Distribution\n")
      y <- matrix(x$LFD, byrow = T, nrow = 1)
      rownames(y) <- "N of Items"
      colnames(y) <- paste("Field", 1:x$Nfield)
      print(round(y, digits))
      cat("\nModel Fit Indices\n")
      cat(paste("Number of Latent", msg2, ":", x$Nclass))
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
    },
    FieldAnalysis = {
      cat("Field Analysis Matrix\n")
      nr <- NROW(x$FieldAnalysisMatrix)
      nc <- NCOL(x$FieldAnalysisMatrix)
      rnames <- rownames(x$FieldAnalysisMatrix)
      cnames <- colnames(x$FieldAnalysisMatrix)
      yy <- x$FieldAnalysisMatrix
      crr <- round(yy[, 1], digits)
      LFE <- yy[, 2]
      Fields <- round(yy[, -(1:2)], digits)
      y <- cbind(crr, LFE, Fields)
      print(y)
    },
    IRM = {
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
    },
    BNM = {
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
          x$CCRR_tabje[j, 5] <- sprintf(
            paste0("%.", digits, "f"),
            as.numeric(x$CCRR_tabje[j, 5])
          )
        }
      }
      print(x$CCRR_table)

      cat("\nModel Fit Indices\n")
      y <- unclass(x$TestFitIndices)
      y <- t(as.data.frame(y))
      colnames(y) <- "value"
      print(round(y, digits))
    },
    LDLRA = {
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
          x$CCRR_tabje[j, 6] <- sprintf(
            paste0("%.", digits, "f"),
            as.numeric(x$CCRR_tabje[j, 6])
          )
        }
      }
      print(x$CCRR_table, digits = digits)
      cat("\nMarginal Item Reference Profile\n")
      print(x$IRP, na.print = "", digits = digits)
      cat("\nIRP Indices\n")
      print(x$IRPIndex)
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
    },
    LDB = {
      cat("Adjacency Matrix\n")
      print(x$adj_list)
      lay.tree <- layout_on_grid(x$g_list[[1]])
      xcoord <- lay.tree[, 1]
      ycoord <- lay.tree[, 2]
      xcoord <- xcoord[order(rowSums(x$adj_list[[1]]))]
      ycoord <- ycoord[order(colSums(x$adj_list[[1]]))]
      for (i in 1:x$Nclass) {
        plot.igraph(x$g_list[[i]],
          layout = cbind(xcoord, ycoord),
          main = paste("Graph at Rank", i)
        )
      }

      cat("\nParameter Learning\n")
      for (i in 1:x$Nclass) {
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
      print(x$FRPIndex)

      y <- rbind(x$TRP, x$LRD, x$RMD)
      rownames(y) <- c(
        "Test Reference Profile",
        "Latent Rank Ditribution",
        "Rank Membership Dsitribution"
      )
      colnames(y) <- paste("Rank", 1:x$Nclass)
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
    },
    BINET = {
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
    },
    ModelFit = {
      tmp <- data.frame(unclass(x))
      print(tmp)
    },
    matrix = {
      class(x) <- "matrix"
      print(x, digits = digits)
    },
    all = {
      class(x) <- "list"
      print(x, digits = digits)
    },
  )
}
