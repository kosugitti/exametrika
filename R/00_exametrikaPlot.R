#' @title Plot Method for Objects of Class "exametrika"
#' @description
#' Creates visualizations for objects with class "exametrika".
#' The calculation results of the exametrika package have an exametrika class attribute,
#' along with the specific analysis model class (IRT, GRM, LCA, LRA, Biclustering, IRM, LDLRA, LDB,
#' BINET). Each model has its own compatible plot types, accessible by specifying the 'type' parameter.
#'
#' @param x An object of class "exametrika"
#' @param type Character string specifying the plot type. Available types vary by model:
#'   \describe{
#'     \item{IRF, ICC}{Item Response Function. Also known as 'ICC' (Item Characteristic Curve).}
#'     \item{TRF}{Test Response Function.}
#'     \item{IIF, IIC}{Item Information Function. Also known as 'IIC' (Item Information Curve).}
#'     \item{TIF, TIC}{Test Information Function. Also known as 'TIC' (Test Information Curve).}
#'     \item{IRP}{Item Reference Profile. Line graph with items and latent classes/ranks
#'       on the horizontal axis, and membership probability on the vertical axis.}
#'     \item{CRV, RRV}{Class/Rank Reference Vector. Plots correct answer rates for each class or rank,
#'       with fields on the horizontal axis and correct answer rates on the vertical axis.}
#'     \item{TRP}{Test Reference Profile. Shows latent classes/ranks on the horizontal axis,
#'       displaying members per class/rank as a bar graph and expected test scores as a line graph.}
#'     \item{LCD}{Latent Class Distribution. Displays latent classes on the horizontal axis,
#'       showing members per class as a bar graph and cumulative membership probability as a line.}
#'     \item{LRD}{Latent Rank Distribution. Similar to LCD but with ranks instead of classes on the horizontal axis.}
#'     \item{CMP}{Class Membership Profile. Line graph showing class membership probabilities of students.}
#'     \item{RMP}{Rank Membership Profile. Similar to CMP but with ranks instead of classes.}
#'     \item{ScoreFreq}{Frequency polygon of score distribution with rank thresholds.}
#'     \item{ScoreRank}{Heatmap of score membership probabilities for each rank.}
#'     \item{ICRP}{Visualizes ranks (x-axis) versus category response probabilities (y-axis).}
#'     \item{ICBR}{Visualizes ranks (x-axis) versus cumulative category probabilities (y-axis).}
#'     \item{FRP}{Field Reference Profile. Shows correspondence between fields and latent classes/ranks.}
#'     \item{Array}{Array plot for Biclustering/Ranklustering. Colored matrix cells where darker cells
#'       indicate larger values.}
#'     \item{FieldPIRP}{Shows correct response rates by number of correct answers in parent fields.
#'       Only available for LDB model.}
#'     \item{LDPSR}{Latent Dependence Passing Student Rate. Compares passing rates of parent and child classes.}
#'   }
#' @param items Numeric vector specifying which items to plot. If NULL, all items are included.
#'   When type is "IIF"/"IIC", specifying 0 will produce a TIF/TIC for the entire test.
#' @param students Numeric vector specifying which students to plot. If NULL, all students are included.
#' @param nc Integer specifying the number of columns for multiple plots. Default is 1.
#' @param nr Integer specifying the number of rows for multiple plots. Default is 1.
#' @param overlay Logical. If TRUE, elements such as IRFs will be overlaid on a single plot. Default is FALSE.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @details
#' Each model class supports specific plot types:
#'
#' \describe{
#'   \item{IRT}{Supports "IRF"/"ICC", "TRF", "IIF"/"IIC", "TIF"/"TIC"}
#'   \item{GRM}{Supports "IRF"/"ICC", "IIF"/"IIC", "TIF"/"TIC"}
#'   \item{LCA}{Supports "IRP", "FRP", "TRP", "LCD", "CMP"}
#'   \item{LRA}{Supports "IRP", "FRP", "TRP", "LRD", "RMP"}
#'   \item{LRAordinal}{Supports "ScoreFreq", "ScoreRank", "ICRP", "ICBR", "RMP"}
#'   \item{LRArated}{Supports "ScoreFreq", "ScoreRank", "ICRP", "RMP"}
#'   \item{Biclustering}{Supports "FRP", "TRP", "LCD", "LRD", "CMP", "RMP", "CRV", "RRV", "Array"}
#'   \item{IRM}{Supports "FRP", "TRP", "Array"}
#'   \item{LDLRA}{Supports "IRP", "TRP", "LRD", "RMP"}
#'   \item{LDB}{Supports "FRP", "TRP", "LRD", "RMP", "Array", "FieldPIRP"}
#'   \item{BINET}{Supports "FRP", "TRP", "LRD", "RMP", "Array", "LDPSR"}
#' }
#'
#' @return
#' Produces visualizations based on the model class and specified type:
#'
#' \describe{
#'   \item{IRT models}{IRF (Item Response Function), TRF (Test Response Function),
#'     IIF (Item Information Function), TIF (Test Information Function)}
#'   \item{LCA/LRA models}{IRP (Item Reference Profile), TRP (Test Reference Profile),
#'     LCD/LRD (Latent Class/Rank Distribution), CMP/RMP (Class/Rank Membership Profile)}
#'   \item{Biclustering/IRM models}{Array plots showing clustering patterns, FRP, TRP, etc.}
#'   \item{LDLRA/LDB/BINET models}{Network and profile plots specific to each model}
#' }
#'
#' @importFrom graphics curve title axis barplot mtext par text lines rect legend abline image
#' @importFrom grDevices gray
#' @importFrom stats density runif
#' @importFrom utils tail
#'
#' @examples
#' \dontrun{
#' # IRT model example
#' irt_result <- exametrika::IRT(U)
#' plot(irt_result, type = "IRF", items = 1:5)
#' plot(irt_result, type = "TIF")
#'
#' # LCA model example
#' lca_result <- exametrika::LCA(U)
#' plot(lca_result, type = "IRP")
#' plot(lca_result, type = "LCD")
#' }
#'
#' @export
#'
plot.exametrika <- function(x,
                            type = c(
                              "IRF", "TRF", "IIF", "TIF", "IIC", "ICC", "TIC",
                              "IRP", "TRP", "LCD", "CMP",
                              "FRP", "RMP", "LRD", "Array", "CRV", "RRV",
                              "FieldPIRP", "LDPSR",
                              "ScoreFreq", "ScoreRank", "ICRP", "ICBR"
                            ),
                            items = NULL,
                            students = NULL,
                            nc = 1,
                            nr = 1,
                            overlay = FALSE, ...) {
  value <- if (length(class(x)) > 1) tail(class(x), 1) else "None"
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(nr, nc))
  testlength <- x$testlength
  nobs <- x$nobs

  valid_types <- list(
    IRT = c("IRF", "TRF", "IIF", "TIF", "IIC", "ICC", "TIC"),
    GRM = c("IRF", "IIF", "TIF", "IIC", "TIC"),
    LCA = c("IRP", "TRP", "LCD", "CMP", "FRP"),
    LRA = c("IRP", "FRP", "TRP", "LRD", "RMP"),
    LRAordinal = c("ScoreFreq", "ScoreRank", "ICRP", "ICBR", "RMP"),
    LRArated = c("ScoreFreq", "ScoreRank", "ICRP", "RMP"),
    Biclustering = c("FRP", "TRP", "LCD", "LRD", "CMP", "RMP", "CRV", "RRV", "Array"),
    IRM = c("FRP", "TRP", "Array"),
    LDLRA = c("IRP", "TRP", "LRD", "RMP"),
    LDB = c("FRP", "TRP", "LRD", "RMP", "Array", "FieldPIRP"),
    BINET = c("FRP", "TRP", "LRD", "RMP", "Array", "LDPSR")
  )

  if (missing(type)) {
    stop("The 'type' argument must be specified.")
  }

  if (!type %in% valid_types[[value]]) {
    stop(paste("Warning: The type", type, "does not correspond to the value", value))
  }

  plotItemID <- if (!is.null(items)) {
    if (!is.numeric(items) || length(items) > testlength || any(items < 0 | items > testlength)) {
      stop("'items' must be a numeric vector of length at most ", testlength, " and contain numbers between 0 and ", testlength)
    }
    items
  } else {
    1:testlength
  }

  plotStudentID <- if (!is.null(students)) {
    if (!is.numeric(students) || length(students) > nobs || any(students < 0 | students > nobs)) {
      stop("'students' must be a numeric vector of length at most ", nobs, " and contain numbers between 0 and ", nobs)
    }
    students
  } else {
    1:nobs
  }

  graph_common <- function() {
    valid_types <- c("IRP", "TRP", "LCD", "LRD", "CMP", "RMP", "FRP", "CRV", "RRV")
    if (!(type %in% valid_types)) {
      stop("That type of output is not defined.")
    }

    if (type == "IRP") {
      # Item Reference Profile ----------------------------------------
      if (value == "LCA") {
        msg <- "Class"
      } else if (value == "LRA") {
        msg <- "Rank"
      } else if (value == "LDLRA") {
        msg <- "Rank"
      }
      params <- x$IRP[plotItemID, ]
      if (is.null(x$Nclass)) {
        steps <- x$Nrank
      } else {
        steps <- x$Nclass
      }
      for (i in 1:nrow(params)) {
        y <- params[i, ]
        plot(y,
          type = "b",
          ylab = "Correct Response Rate",
          xlab = paste("Latent", msg),
          ylim = c(0, 1),
          xaxt = "n",
          main = paste("Item", i)
        )
        axis(1, at = 1:steps)
      }
    }
    if (type == "FRP") {
      # Item Reference Profile ----------------------------------------
      params <- x$FRP
      if (value == "LDB") {
        msg <- "Rank"
      } else {
        msg <- "Class"
      }
      for (i in 1:nrow(params)) {
        y <- params[i, ]
        plot(y,
          type = "b",
          ylab = "Correct Response Rate",
          xlab = paste("Latent", msg),
          ylim = c(0, 1),
          main = paste("Field", i)
        )
      }
    }
    if (type == "CRV" | type == "RRV") {
      # Rank Reference Vector -------------------------------------------
      if (x$model == 1) {
        msg <- "Class"
      } else {
        msg <- "Rank"
      }
      RRV <- t(x$FRP)
      plot(1:x$Nfield, RRV[1, ],
        type = "n",
        ylim = c(0, 1.1),
        xlab = "Field",
        ylab = "Correct Response Rate",
        main = paste(msg, "Reference Vector"),
        xaxt = "n", bty = "n"
      )
      axis(1, at = 1:x$Nfield, labels = colnames(RRV))
      for (i in 1:x$Nclass) {
        lines(1:x$Nfield, RRV[i, ], type = "o", lty = i)
        for (j in 1:x$Nfield) {
          text(j, RRV[i, j], labels = i, pos = 3, offset = 0.5, cex = 0.8)
        }
      }
      legend("top",
        legend = rownames(RRV),
        lty = 1:x$Nclass,
        lwd = 2,
        ncol = x$Nclass,
        bty = "n"
      )
    }

    if (type == "TRP") {
      # Test Reference Profile ----------------------------------------
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(mar = c(5, 4, 4, 4) + 0.1)
      if (value == "LCA" | value == "IRM" | value == "BINET") {
        target <- x$LCD
        msg <- "Class"
      } else if (value == "Biclustering" | value == "LRA" | value == "LDLRA" | value == "LDB") {
        target <- x$LRD
        msg <- "Rank"
      }

      if (is.null(x$Nclass)) {
        steps <- x$Nrank
      } else {
        steps <- x$Nclass
      }
      names.arg <- 1:steps

      bp <- barplot(target,
        width = .9,
        ylim = c(0, max(target) + 10),
        xlim = c(0, steps + 1),
        xlab = paste("Latent", msg),
        ylab = "Number of Students"
      )
      text(x = bp, y = target, label = target, pos = 1, cex = 1.2)
      par(new = TRUE)
      plot(bp, x$TRP,
        type = "b", pch = 19, lty = 1,
        axes = FALSE, xaxt = "n", xlab = "", ylab = "",
        bty = "n",
        ylim = c(0, testlength),
        xlim = c(0, steps + 1),
      )
      axis(4, at = pretty(range(0, testlength)))
      mtext("Expected Score", side = 4, line = 3)
    }
    if (type == "LCD" | type == "LRD") {
      # Latent Class Distribution ----------------------------------------
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(mar = c(5, 4, 4, 4) + 0.1)
      if (value == "LCA" | value == "BINET") {
        target1 <- x$LCD
        target2 <- x$CMD
      } else if (value == "Biclustering" | value == "LRA" | value == "LDLRA" | value == "LDB") {
        target1 <- x$LRD
        target2 <- x$RMD
      }
      if ((value == "Biclustering" && x$model == 2) || value == "LRA") {
        msg <- "Rank"
      } else {
        msg <- "Class"
      }
      if (is.null(x$Nclass)) {
        steps <- x$Nrank
      } else {
        steps <- x$Nclass
      }
      bp <- barplot(target1,
        names.arg = 1:steps,
        width = .9,
        ylim = c(0, max(target1) + 10),
        xlim = c(0, steps + 1),
        xlab = paste("Latent", msg),
        ylab = "Number of Students"
      )
      text(x = bp, y = target1, label = target1, pos = 1, cex = 1.2)
      par(new = TRUE)
      plot(bp, target2,
        type = "b", pch = 19, lty = 1,
        axes = FALSE, xaxt = "n", xlab = "", ylab = "",
        bty = "n",
        ylim = c(0, max(target1) + 10),
        xlim = c(0, steps + 1),
      )
      axis(4, at = pretty(range(0, max(target2) + 10)))
      mtext("Frequency", side = 4, line = 3)
    }
    if (type == "CMP" | type == "RMP") {
      # Class Membership Profile ----------------------------------------
      if (type == "CMP") {
        msg <- "Class"
      } else {
        msg <- "Rank"
      }
      if (is.null(x$Nclass)) {
        steps <- x$Nrank
      } else {
        steps <- x$Nclass
      }
      params <- x$Students[plotStudentID, 1:steps]
      for (i in 1:NROW(params)) {
        y <- params[i, ]
        plot(y,
          type = "b",
          xlab = paste("Latent", msg),
          ylab = "Membership",
          ylim = c(0, 1),
          main = paste("Student", plotStudentID[i])
        )
      }
    }
  }



  array_plot <- function() {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(1, 2))
    stepx <- 300 / x$testlength
    stepy <- 600 / x$nobs
    ## Original Data
    plot(0, 0,
      type = "n", xlim = c(0, 300), ylim = c(0, 600),
      xlab = "", ylab = "", xaxt = "n", yaxt = "n",
      frame.plot = TRUE,
      main = "Original Data"
    )

    for (i in 1:x$nobs) {
      for (j in 1:x$testlength) {
        x1 <- (j - 1) * stepx
        y1 <- (i - 1) * stepy
        x2 <- j * stepx
        y2 <- i * stepy
        if (x$U[i, j] == 1) {
          rect(x1, y1, x2, y2, col = "black")
        }
      }
    }

    ## Clusterd Plot
    plot(0, 0,
      type = "n", xlim = c(0, 300), ylim = c(0, 600),
      xlab = "", ylab = "", xaxt = "n", yaxt = "n",
      frame.plot = TRUE,
      main = "Clusterd Plot"
    )
    sorted <- x$U[, order(x$FieldEstimated, decreasing = FALSE)]
    sorted <- sorted[order(x$ClassEstimated, decreasing = TRUE), ]
    for (i in 1:nobs) {
      for (j in 1:testlength) {
        x1 <- (j - 1) * stepx
        y1 <- (i - 1) * stepy
        x2 <- j * stepx
        y2 <- i * stepy
        if (sorted[i, j] == 1) {
          rect(x1, y1, x2, y2, col = "black")
        }
      }
    }

    vl <- cumsum(table(sort(x$FieldEstimated)))
    for (i in 1:(x$Nfield - 1)) {
      lines(x = c(vl[i] * stepx, vl[i] * stepx), y = c(0, 600), col = "red")
    }
    hl <- nobs - cumsum(table(sort(x$ClassEstimated)))
    if (is.null(x$Nclass)) {
      steps <- x$Nrank
    } else {
      steps <- x$Nclass
    }
    for (j in 1:(steps - 1)) {
      lines(x = c(0, 300), y = c(hl[j] * stepy, hl[j] * stepy), col = "red")
    }
  }

  field_PIRP <- function() {
    target <- x$IRP
    ## rank x field x nrs
    Nrank <- dim(target)[1]
    nfld <- dim(target)[2]
    nrs <- dim(target)[3]
    for (i in 1:Nrank) {
      mat <- target[i, , ]
      mat[mat == 0] <- NA
      x <- 0:nrs
      plot(x,
        y = runif(length(x)), type = "n",
        xlim = c(0, nrs), ylim = c(0, 1),
        xlab = "PIRP(Number-Right Score) in Parent Field(s)",
        ylab = "Correct Response Rate"
      )
      for (j in 1:NROW(mat)) {
        y <- as.vector(na.omit(mat[j, ]))
        x <- seq(0, nrs)[1:length(y)]
        labels <- rep(as.character(j), length(y))
        lines(x, y, type = "l", lwd = 2)
        text(x, y, labels = labels, pos = 1)
      }
      title(main = paste("Rank", i))
    }
  }

  LDPSR <- function() {
    for (i in 1:length(x$params)) {
      target <- x$params[[i]]
      ln <- length(target$fld)
      lb <- names(target$chap)
      y1 <- target$pap
      y2 <- target$chap
      plot(1:ln, y1,
        type = "b", xaxt = "n",
        col = 3, lwd = 2, ylim = c(0, 1),
        ylab = "Probability",
        xlab = "",
        main = paste("Field", i, "items")
      )
      lines(1:ln, y2, col = 2, lwd = 2, type = "b")
      axis(1, at = 1:ln, labels = lb)
      posx <- length(1:ln)
      text(
        x = posx, y = y1[posx], labels = paste("C", target$parent),
        col = 1, pos = 3, lwd = 2
      )
      text(
        x = posx, y = y2[posx], labels = paste("C", target$child),
        col = 1, pos = 3, lwd = 2
      )
    }
  }

  score_freq_plot <- function() {
    tmp <- as.data.frame(x$Students)
    sc <- tmp$Score
    rank <- tmp$Estimate
    thresholds <- numeric(length(unique(rank)) - 1)
    for (i in 1:(length(unique(rank)) - 1)) {
      max_rank_i <- max(sc[rank == i])
      min_rank_next <- min(sc[rank == (i + 1)])
      thresholds[i] <- (max_rank_i + min_rank_next) / 2
    }
    plot(density(sc),
      xlab = "Score",
      ylab = "Frequency",
      main = "Latent Rank"
    )
    abline(
      v = thresholds,
      col = "red",
      lty = 2
    )
  }

  score_rank_plot <- function() {
    score_rank_matrix <- x$ScoreRank
    image(
      x = 1:ncol(score_rank_matrix),
      y = as.numeric(rownames(score_rank_matrix)),
      z = t(score_rank_matrix),
      col = gray(seq(1, 0, length.out = 100)),
      xlab = "Latent Rank",
      ylab = "Score",
      main = "Score-Rank Distribution",
      xaxt = "n"
    )
    axis(1, at = 1:ncol(score_rank_matrix))
  }

  IC_RP_BR_plot <- function() {
    # par(mar = c(3, 3, 2, 1))
    label <- x$U$ItemLabel[plotItemID]
    if (type == "ICRP") {
      tmp <- x$ICRP[x$ICRP$ItemLabel %in% label, ]
    } else if (type == "ICBR") {
      tmp <- x$ICBR[x$ICBR$ItemLabel %in% label, ]
    }
    yRange <- range(tmp[, -c(1:2)])
    for (i in 1:length(plotItemID)) {
      slice <- tmp[tmp$ItemLabel == label[i], ]
      slice <- unname(as.matrix(slice[, -c(1:2)]))
      plot(slice[1, ],
        type = "l", lty = 1, col = 1, ylim = yRange,
        xlab = "Rank", ylab = "Probability", main = label[i],
        xaxt = "n"
      )
      axis(1, at = 1:ncol(slice), labels = 1:x$Nrank)
      for (j in 1:nrow(slice)) {
        lines(slice[j, ], lty = j)
        text(
          x = ncol(slice), y = slice[j, ncol(slice)],
          labels = j, cex = 0.8, ylim = yRange
        )
      }
    }
  }
  # Switching function (main) ----------------------------------------

  switch(value,
    IRT = {
      valid_types <- c("IRF", "TRF", "TIF", "IIF", "ICC", "IIC", "TIC")
      if (!(type %in% valid_types)) {
        stop("That type of output is not defined.")
      }
      # rename type option
      type <- switch(type,
        "ICC" = "IRF",
        "IIC" = "IIF",
        "TIC" = "TIF",
        type
      )
      if ((type %in% c("IIF", "TIF") && any(plotItemID == 0))) {
        type <- "TIF"
        plotItemID <- 1:testlength
      } else if (type == "IRF" && any(plotItemID == 0)) {
        type <- "TRF"
        plotItemID <- 1:testlength
      }
      if (length(plotItemID) == 0) {
        plotItemID <- 1:testlength
      }

      ### IRT curve function
      plotIRTCurve <- function(params, curveFunc, titleBase, ylab, overlay) {
        if (overlay) {
          plot(NULL,
            xlim = c(-4, 4),
            ylim = c(0, 1),
            xlab = "ability",
            ylab = ylab,
            main = titleBase
          )
          for (i in 1:nrow(params)) {
            a <- params[i, 1]
            b <- params[i, 2]
            c <- if (x$model > 2) params[i, 3] else 0
            d <- if (x$model > 3) params[i, 4] else 1
            curve(curveFunc(a, b, c, d, theta = x),
              from = -4,
              to = 4,
              add = TRUE,
              lty = i, # Different line types
              col = i, # Different colors
              lwd = 2
            )
          }
          legend("topleft",
            legend = paste("Item", plotItemID),
            lty = 1:nrow(params),
            col = 1:nrow(params),
            lwd = 2
          )
        } else {
          for (i in 1:nrow(params)) {
            a <- params[i, 1]
            b <- params[i, 2]
            c <- if (x$model > 2) params[i, 3] else 0
            d <- if (x$model > 3) params[i, 4] else 1
            title <- paste0(titleBase, ", item ", plotItemID[i])
            curve(curveFunc(a, b, c, d, theta = x),
              from = -4,
              to = 4,
              xlab = "ability", ylab = ylab,
              main = title
            )
          }
        }
      }

      params <- x$params[plotItemID, , drop = FALSE]
      if (type == "IRF") {
        plotIRTCurve(
          params, exametrika::LogisticModel,
          "Item Response Function", "probability",
          overlay
        )
      }
      if (type == "TRF") {
        curve(exametrika::TestResponseFunc(params, theta = x),
          from = -4,
          to = 4,
          xlab = "ability", ylab = "probability",
          main = "Test Response Function"
        )
      }
      if (type == "IIF") {
        plotIRTCurve(
          params, exametrika::ItemInformationFunc,
          "Item Information Function", "information",
          overlay
        )
      }
      if (type == "TIF") {
        curve(exametrika::TestInformationFunc(params, theta = x),
          from = -4,
          to = 4,
          xlab = "ability", ylab = "Information",
          main = "Test Informaiton Function"
        )
      }
    },
    GRM = {
      valid_types <- c("IRF", "TIF", "IIF", "ICC", "IIC", "TIC")
      if (!(type %in% valid_types)) {
        stop("That type of output is not defined.")
      }
      # rename type option
      type <- switch(type,
        "ICC" = "IRF",
        "IIC" = "IIF",
        "TIC" = "TIF",
        type
      )
      if ((type %in% c("IIF", "TIF") && any(plotItemID == 0))) {
        type <- "TIF"
        plotItemID <- 1:testlength
      } else if (type == "IRF" && any(plotItemID == 0)) {
        type <- "TRF"
        plotItemID <- 1:testlength
      }
      if (length(plotItemID) == 0) {
        plotItemID <- 1:testlength
      }
      ### GRM curve function
      grm_IRF <- function(a, b, title) {
        thetas <- seq(-4, 4, 0.01)
        n_theta <- length(thetas)
        K <- length(b) + 1
        colors <- c(
          "#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
        )
        probs <- matrix(0, nrow = n_theta, ncol = K)
        for (i in 1:n_theta) {
          probs[i, ] <- grm_prob(thetas[i], a, b)
        }
        plot(thetas, probs[, 1],
          type = "l",
          col = colors[1],
          ylim = c(0, 1),
          xlab = expression(theta),
          main = title
        )
        for (k in 2:K) {
          lines(thetas, probs[, k], col = colors[k])
        }
        category_labels <- paste("Category", 1:K)
        legend("topright", legend = category_labels, col = colors, lty = 1)
      }

      grm_IIC <- function(a, b, title) {
        thetas <- seq(-4, 4, 0.01)
        I <- sapply(thetas, function(theta) grm_iif(theta, a, b))

        plot(thetas, I,
          type = "l",
          xlab = expression(theta),
          ylab = "Item Information",
          main = title
        )
      }

      params <- x$params[plotItemID, , drop = FALSE]
      if (type == "IRF") {
        for (j in 1:nrow(params)) {
          a <- params[j, 1]
          b <- params[j, -1]
          b <- b[!is.na(b)]
          title <- paste("Test Response Function for item", j)
          grm_IRF(a, b, title)
        }
      }
      if (type == "IIF") {
        for (j in 1:nrow(params)) {
          a <- params[j, 1]
          b <- params[j, -1]
          b <- b[!is.na(b)]
          title <- paste("Item Information Function for item", j)
          grm_IIC(a, b, title)
        }
      }
      if (type == "TIF") {
        thetas <- seq(-4, 4, 0.01)
        I <- matrix(nrow = testlength, ncol = length(thetas))
        for (j in 1:NROW(params)) {
          a <- params[j, 1]
          b <- params[j, -1]
          b <- b[!is.na(b)]
          I[j, ] <- sapply(thetas, function(theta) grm_iif(theta, a, b))
        }
        TestInfo <- colSums(I, na.rm = T)
        plot(thetas,  TestInfo,
          type = "l",
          xlab = expression(theta),
          ylab = "Information",
          main = "Test Information Function"
        )
      }
    },
    LCA = {
      model <- class(x)[2]
      graph_common()
    },
    LRA = {
      model <- class(x)[2]
      graph_common()
    },
    LRAordinal = {
      if (type == "ScoreFreq") {
        score_freq_plot()
      } else if (type == "ScoreRank") {
        score_rank_plot()
      } else if (type == "ICBR") {
        IC_RP_BR_plot()
      } else if (type == "ICRP") {
        IC_RP_BR_plot()
      } else if (type == "RMP") {
        graph_common()
      }
    },
    LRArated = {
      if (type == "ScoreFreq") {
        score_freq_plot()
      } else if (type == "ScoreRank") {
        score_rank_plot()
      } else if (type == "ICRP") {
        IC_RP_BR_plot()
      } else if (type == "RMP") {
        graph_common()
      }
    },
    Biclustering = {
      if (type == "Array") {
        array_plot()
      } else {
        graph_common()
      }
    },
    IRM = {
      if (type == "Array") {
        array_plot()
      } else {
        graph_common()
      }
    },
    LDLRA = {
      graph_common()
    },
    LDB = {
      if (type == "FieldPIRP") {
        field_PIRP()
      } else if (type == "Array") {
        array_plot()
      } else {
        graph_common()
      }
    },
    BINET = {
      if (type == "LDPSR") {
        LDPSR()
      } else if (type == "Array") {
        array_plot()
      } else {
        graph_common()
      }
    },
    none = {
      stop("This object cannot be plotted. The object must be of class 'exametrika' with a valid model type.")
    }
  )
}
