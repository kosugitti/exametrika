# LCA / LRA / LRAordinal / LRArated プロット関数
# plot.exametrika() から呼び出される内部関数群

#' 共通プロファイルプロット（IRP / FRP / TRP / LCD / LRD / CMP / RMP / CRV / RRV）
#' @noRd
plot_common_profiles <- function(x, type, value, plotItemID, plotStudentID, testlength) {
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
    # Field Reference Profile ----------------------------------------
    params <- x$FRP
    msg <- x$msg
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
    msg <- x$msg
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
    } else if (value == "Biclustering" | value == "ordinalBiclustering" | value == "nominalBiclustering") {
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
      value == "LRA" | value == "LDLRA" | value == "LDB") {
      target1 <- x$LRD
      target2 <- x$RMD
    }
    msg <- x$msg
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
    msg <- x$msg
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

#' LRA ordinal/rated プロットディスパッチ
#' @noRd
plot_lra_ordinal <- function(x, type, plotItemID) {
  if (type == "ScoreFreq") {
    score_freq_plot(x)
  } else if (type == "ScoreRank") {
    score_rank_plot(x)
  } else if (type == "ICRP" || type == "ICBR") {
    IC_RP_BR_plot(x, type, plotItemID)
  }
}

#' ScoreFreq: 得点分布の頻度多角形
#' @noRd
score_freq_plot <- function(x) {
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

#' ScoreRank: 得点メンバーシップ確率のヒートマップ
#' @noRd
score_rank_plot <- function(x) {
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

#' ICRP / ICBR プロット
#' @noRd
IC_RP_BR_plot <- function(x, type, plotItemID) {
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
