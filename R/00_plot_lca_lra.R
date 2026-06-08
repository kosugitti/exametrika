# LCA / LRA / LRAordinal / LRArated plot functions
# Internal functions called from plot.exametrika()

#' Common profile plots (IRP / FRP / TRP / LCD / LRD / CMP / RMP / CRV / RRV)
#' @noRd
plot_common_profiles <- function(x, type, value, plotItemID, plotStudentID, testlength, dots = list()) {
  if (type == "IRP") {
    # Item Reference Profile ----------------------------------------
    msg <- x$msg
    params <- x$IRP[plotItemID, ]
    if (is.null(x$Nclass)) {
      steps <- x$Nrank
    } else {
      steps <- x$Nclass
    }
    for (i in 1:nrow(params)) {
      y <- params[i, ]
      call_plot(
        plot,
        list(
          x = y,
          type = "b",
          ylab = "Correct Response Rate",
          xlab = paste("Latent", msg),
          ylim = c(0, 1),
          xaxt = "n",
          main = paste("Item", i)
        ),
        dots
      )
      call_plot(graphics::axis, list(side = 1, at = 1:steps), dots)
    }
  }
  if (type == "FRP") {
    # Field Reference Profile ----------------------------------------
    params <- x$FRP
    msg <- x$msg
    for (i in 1:nrow(params)) {
      y <- params[i, ]
      call_plot(
        plot,
        list(
          x = y,
          type = "b",
          ylab = "Correct Response Rate",
          xlab = paste("Latent", msg),
          ylim = c(0, 1),
          main = paste("Field", i)
        ),
        dots
      )
    }
  }
  if (type == "CRV" | type == "RRV") {
    # Rank Reference Vector -------------------------------------------
    msg <- x$msg
    RRV <- t(x$FRP)
    call_plot(
      plot,
      list(
        x = 1:x$Nfield, y = RRV[1, ],
        type = "n",
        ylim = c(0, 1.1),
        xlab = "Field",
        ylab = "Correct Response Rate",
        main = paste(msg, "Reference Vector"),
        xaxt = "n", bty = "n"
      ),
      dots
    )
    call_plot(graphics::axis, list(side = 1, at = 1:x$Nfield, labels = colnames(RRV)), dots)
    for (i in 1:x$Nclass) {
      call_plot(graphics::lines, list(x = 1:x$Nfield, y = RRV[i, ], type = "o", lty = i), dots)
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
    on.exit(
      {
        restore_par <- old_par
        restore_par[c("pin", "fin", "plt", "usr")] <- NULL
        suppressWarnings(par(restore_par))
      },
      add = TRUE
    )
    par(mar = c(5, 4, 4, 4) + 0.1)
    if (value == "LCA" | value == "IRM" | value == "BINET") {
      target <- x$LCD
    } else if (value == "LRA" | value == "LDLRA" | value == "LDB") {
      target <- x$LRD
    } else if (value == "Biclustering" | value == "ordinalBiclustering" | value == "nominalBiclustering" |
      value == "ratedBiclustering") {
      target <- x$LRD
      if (is.null(target)) {
        target <- x$LCD
      }
    }
    msg <- x$msg

    if (is.null(x$Nclass)) {
      steps <- x$Nrank
    } else {
      steps <- x$Nclass
    }
    names.arg <- 1:steps

    bp <- call_plot(
      graphics::barplot,
      list(
        height = target,
        width = .9,
        ylim = c(0, max(target) + 10),
        xlim = c(0, steps + 1),
        xlab = paste("Latent", msg),
        ylab = "Number of Students"
      ),
      dots
    )
    text(x = bp, y = target, label = target, pos = 1, cex = 1.2)
    par(new = TRUE)
    call_plot(
      plot,
      list(
        x = bp, y = x$TRP,
        type = "b", pch = 19, lty = 1,
        axes = FALSE, xaxt = "n", xlab = "", ylab = "",
        bty = "n",
        ylim = c(0, testlength),
        xlim = c(0, steps + 1)
      ),
      dots
    )
    call_plot(graphics::axis, list(side = 4, at = pretty(range(0, testlength))), dots)
    mtext("Expected Score", side = 4, line = 3)
  }
  if (type == "LCD" | type == "LRD") {
    # Latent Class Distribution ----------------------------------------
    old_par <- par(no.readonly = TRUE)
    on.exit(
      {
        restore_par <- old_par
        restore_par[c("pin", "fin", "plt", "usr")] <- NULL
        suppressWarnings(par(restore_par))
      },
      add = TRUE
    )
    par(mar = c(5, 4, 4, 4) + 0.1)
    if (value == "LCA" | value == "BINET") {
      target1 <- x$LCD
      target2 <- x$CMD
    } else if (value == "Biclustering" | value == "ordinalBiclustering" | value == "nominalBiclustering" |
      value == "ratedBiclustering" | value == "LRA" | value == "LDLRA" | value == "LDB") {
      target1 <- x$LRD
      target2 <- x$RMD
    }
    msg <- x$msg
    if (is.null(x$Nclass)) {
      steps <- x$Nrank
    } else {
      steps <- x$Nclass
    }
    bp <- call_plot(
      graphics::barplot,
      list(
        height = target1,
        names.arg = 1:steps,
        width = .9,
        ylim = c(0, max(target1) + 10),
        xlim = c(0, steps + 1),
        xlab = paste("Latent", msg),
        ylab = "Number of Students"
      ),
      dots
    )
    text(x = bp, y = target1, label = target1, pos = 1, cex = 1.2)
    par(new = TRUE)
    call_plot(
      plot,
      list(
        x = bp, y = target2,
        type = "b", pch = 19, lty = 1,
        axes = FALSE, xaxt = "n", xlab = "", ylab = "",
        bty = "n",
        ylim = c(0, max(target1) + 10),
        xlim = c(0, steps + 1)
      ),
      dots
    )
    call_plot(graphics::axis, list(side = 4, at = pretty(range(0, max(target2) + 10))), dots)
    mtext("Frequency", side = 4, line = 3)
  }
  if (type == "CMP" | type == "RMP") {
    # Class Membership Profile ----------------------------------------
    msg <- x$msg
    if (is.null(x$Nclass)) {
      steps <- x$Nrank
    } else {
      steps <- x$Nclass
    }
    params <- x$Students[plotStudentID, 1:steps, drop = FALSE]
    for (i in 1:NROW(params)) {
      y <- params[i, ]
      call_plot(
        plot,
        list(
          x = y,
          type = "b",
          xlab = paste("Latent", msg),
          ylab = "Membership",
          ylim = c(0, 1),
          main = paste("Student", plotStudentID[i])
        ),
        dots
      )
    }
  }
}

#' LRA ordinal/rated plot dispatch
#' @noRd
plot_lra_ordinal <- function(x, type, plotItemID, dots = list()) {
  if (type == "ScoreFreq") {
    score_freq_plot(x, dots)
  } else if (type == "ScoreRank") {
    score_rank_plot(x, dots)
  } else if (type == "ICRP" || type == "ICBR") {
    IC_RP_BR_plot(x, type, plotItemID, dots)
  }
}

#' ScoreFreq: score distribution frequency polygon
#' @noRd
score_freq_plot <- function(x, dots = list()) {
  tmp <- as.data.frame(x$Students)
  sc <- tmp$Score
  rank <- tmp$Estimate
  thresholds <- numeric(length(unique(rank)) - 1)
  for (i in 1:(length(unique(rank)) - 1)) {
    max_rank_i <- max(sc[rank == i])
    min_rank_next <- min(sc[rank == (i + 1)])
    thresholds[i] <- (max_rank_i + min_rank_next) / 2
  }
  call_plot(
    plot,
    list(
      x = density(sc),
      xlab = "Score",
      ylab = "Frequency",
      main = "Latent Rank"
    ),
    dots
  )
  abline(
    v = thresholds,
    col = "red",
    lty = 2
  )
}

#' ScoreRank: score-membership probability heatmap
#' @noRd
score_rank_plot <- function(x, dots = list()) {
  score_rank_matrix <- x$ScoreRank
  call_plot(
    graphics::image,
    list(
      x = 1:ncol(score_rank_matrix),
      y = as.numeric(rownames(score_rank_matrix)),
      z = t(score_rank_matrix),
      col = gray(seq(1, 0, length.out = 100)),
      xlab = "Latent Rank",
      ylab = "Score",
      main = "Score-Rank Distribution",
      xaxt = "n"
    ),
    dots
  )
  call_plot(graphics::axis, list(side = 1, at = 1:ncol(score_rank_matrix)), dots)
}

#' ICRP / ICBR plot
#' @noRd
IC_RP_BR_plot <- function(x, type, plotItemID, dots = list()) {
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
    call_plot(
      plot,
      list(
        x = slice[1, ],
        type = "l", lty = 1, col = 1, ylim = yRange,
        xlab = "Rank", ylab = "Probability", main = label[i],
        xaxt = "n"
      ),
      dots
    )
    call_plot(graphics::axis, list(side = 1, at = 1:ncol(slice), labels = 1:x$Nrank), dots)
    for (j in 1:nrow(slice)) {
      call_plot(graphics::lines, list(x = slice[j, ], lty = j), dots)
      text(
        x = ncol(slice), y = slice[j, ncol(slice)],
        labels = j, cex = 0.8, ylim = yRange
      )
    }
  }
}
