# 多値 Biclustering プロット関数（ordinal / nominal）
# plot.exametrika() から呼び出される内部関数群

#' BCRM から期待得点行列を計算（FRP / RRV 共通ヘルパー）
#' @noRd
calc_expected_scores <- function(BCRM, stat) {
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]

  FRP_mat <- matrix(0, nrow = nfld, ncol = ncls)
  for (f in 1:nfld) {
    for (cc in 1:ncls) {
      probs <- BCRM[f, cc, ]
      if (stat == "mean") {
        FRP_mat[f, cc] <- sum((1:maxQ) * probs)
      } else if (stat == "median") {
        cum_probs <- cumsum(probs)
        FRP_mat[f, cc] <- min(which(cum_probs >= 0.5))
      } else if (stat == "mode") {
        FRP_mat[f, cc] <- which.max(probs)
      }
    }
  }
  FRP_mat
}

#' FRP: 期待得点折れ線プロット
#' @noRd
plot_poly_frp <- function(x, stat, nc, nr) {
  BCRM <- x$FRP
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]
  msg <- x$msg

  FRP_mat <- calc_expected_scores(BCRM, stat)

  for (f in 1:nfld) {
    y <- FRP_mat[f, ]
    plot(1:ncls, y,
      type = "b", pch = 19, lwd = 2,
      xlab = paste("Latent", msg),
      ylab = paste0("Expected Score (", stat, ")"),
      ylim = c(1, maxQ),
      xaxt = "n",
      main = paste("Field", f)
    )
    axis(1, at = 1:ncls)
    abline(h = 1:maxQ, col = "gray90", lty = 3)
  }
}

#' FCRP: カテゴリ確率プロット
#' @noRd
plot_poly_fcrp <- function(x, style, nc, nr) {
  BCRM <- x$FRP
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]
  msg <- x$msg
  cols <- get_cb_palette(maxQ)

  for (f in 1:nfld) {
    if (style == "line") {
      plot(1:ncls, BCRM[f, , 1],
        type = "n",
        xlab = paste("Latent", msg),
        ylab = "Category Probability",
        ylim = c(0, 1),
        xaxt = "n",
        main = paste("Field", f, "- Category Response")
      )
      axis(1, at = 1:ncls)
      for (q in 1:maxQ) {
        lines(1:ncls, BCRM[f, , q],
          type = "b", pch = q, lty = q, col = cols[q], lwd = 1.5
        )
      }
      legend("topright",
        legend = paste("Cat", 1:maxQ),
        col = cols[1:maxQ], lty = 1:maxQ, pch = 1:maxQ,
        cex = 0.7, bty = "n", ncol = min(maxQ, 3)
      )
    } else if (style == "bar") {
      bar_data <- t(BCRM[f, , ])
      barplot(bar_data,
        col = cols[1:maxQ],
        names.arg = paste0(substr(msg, 1, 1), 1:ncls),
        xlab = paste("Latent", msg),
        ylab = "Category Probability",
        ylim = c(0, 1),
        main = paste("Field", f, "- Category Response")
      )
      legend("topright",
        legend = paste("Cat", 1:maxQ),
        fill = cols[1:maxQ],
        cex = 0.7, bty = "n", ncol = min(maxQ, 3)
      )
    }
  }
}

#' FCBR: 境界確率プロット（ordinal Biclustering 専用）
#' @noRd
plot_poly_fcbr <- function(x, nc, nr) {
  BCRM <- x$FRP
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]
  msg <- x$msg
  n_boundaries <- maxQ - 1
  cols <- get_cb_palette(n_boundaries)

  for (f in 1:nfld) {
    boundary_probs <- matrix(0, nrow = n_boundaries, ncol = ncls)
    for (b in 1:n_boundaries) {
      q_threshold <- b + 1
      for (cc in 1:ncls) {
        boundary_probs[b, cc] <- sum(BCRM[f, cc, q_threshold:maxQ])
      }
    }

    plot(1:ncls, boundary_probs[1, ],
      type = "n",
      xlab = paste("Latent", msg),
      ylab = "Boundary Probability",
      ylim = c(0, 1),
      xaxt = "n",
      main = paste("Field", f, "- Boundary Prob")
    )
    axis(1, at = 1:ncls)
    # P(Q >= 1) = 1.0 reference line
    lines(1:ncls, rep(1, ncls),
      type = "b", pch = 0, lty = 1, col = "gray60", lwd = 1
    )
    for (b in 1:n_boundaries) {
      lines(1:ncls, boundary_probs[b, ],
        type = "b", pch = b, lty = b, col = cols[b], lwd = 1.5
      )
    }
    legend("topright",
      legend = c("P(Q>=1)", paste0("P(Q>=", 2:maxQ, ")")),
      col = c("gray60", cols[1:n_boundaries]),
      lty = c(1, 1:n_boundaries),
      pch = c(0, 1:n_boundaries),
      cex = 0.7, bty = "n"
    )
  }
}

#' ScoreField: 期待得点ヒートマップ
#' @noRd
plot_scorefield <- function(x) {
  BCRM <- x$FRP
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]
  msg <- x$msg

  score_mat <- matrix(0, nrow = nfld, ncol = ncls)
  for (f in 1:nfld) {
    for (cc in 1:ncls) {
      score_mat[f, cc] <- sum((1:maxQ) * BCRM[f, cc, ])
    }
  }

  par(mfrow = c(1, 1))
  image(
    x = 1:ncls, y = 1:nfld,
    z = t(score_mat),
    col = hcl.colors(50, "YlOrRd", rev = TRUE),
    xlab = paste("Latent", msg),
    ylab = "Field",
    main = "Score Field Heatmap",
    xaxt = "n", yaxt = "n"
  )
  axis(1, at = 1:ncls, labels = paste0(substr(msg, 1, 1), 1:ncls))
  axis(2, at = 1:nfld, labels = paste0("F", 1:nfld))

  for (f in 1:nfld) {
    for (cc in 1:ncls) {
      text(cc, f, sprintf("%.2f", score_mat[f, cc]), cex = 0.8)
    }
  }
}

#' RRV: フィールド横軸転置版（多値 Biclustering）
#' @noRd
plot_poly_rrv <- function(x, stat) {
  BCRM <- x$FRP
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]
  msg <- x$msg

  FRP_mat <- calc_expected_scores(BCRM, stat)
  RRV <- t(FRP_mat)

  par(mfrow = c(1, 1))
  plot(1:nfld, RRV[1, ],
    type = "n",
    ylim = c(1, maxQ),
    xlab = "Field",
    ylab = paste0("Expected Score (", stat, ")"),
    main = paste(msg, "Reference Vector (", stat, ")"),
    xaxt = "n", bty = "n"
  )
  axis(1, at = 1:nfld, labels = paste0("F", 1:nfld))
  abline(h = 1:maxQ, col = "gray90", lty = 3)

  cols <- get_cb_palette(ncls)
  for (i in 1:ncls) {
    lines(1:nfld, RRV[i, ], type = "o", lty = i, col = cols[i], lwd = 1.5)
    for (j in 1:nfld) {
      text(j, RRV[i, j], labels = i, pos = 3, offset = 0.5, cex = 0.8)
    }
  }
  legend("top",
    legend = paste0(substr(msg, 1, 1), 1:ncls),
    lty = 1:ncls, col = cols[1:ncls], lwd = 2,
    ncol = min(ncls, 5), bty = "n"
  )
}
