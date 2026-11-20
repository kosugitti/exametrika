# Polytomous Biclustering plot functions (ordinal / nominal)
# Internal functions called from plot.exametrika()

#' Compute expected score matrix from BCRM (shared helper for FRP / RRV)
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

#' FRP: expected score line plot
#' @noRd
plot_poly_frp <- function(x, stat, nc, nr, dots = list()) {
  BCRM <- x$FRP
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]
  msg <- x$msg

  FRP_mat <- calc_expected_scores(BCRM, stat)

  for (f in 1:nfld) {
    y <- FRP_mat[f, ]
    call_plot(
      plot,
      list(
        x = 1:ncls, y = y,
        type = "b", pch = 19, lwd = 2,
        xlab = paste("Latent", msg),
        ylab = paste0("Expected Score (", stat, ")"),
        ylim = c(1, maxQ),
        xaxt = "n",
        main = paste("Field", f)
      ),
      dots
    )
    call_plot(graphics::axis, list(side = 1, at = 1:ncls), dots)
    abline(h = 1:maxQ, col = "gray90", lty = 3)
  }
}

#' FCRP: category probability plot
#' @noRd
plot_poly_fcrp <- function(x, style, nc, nr, dots = list()) {
  BCRM <- x$FRP
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]
  msg <- x$msg
  cols <- get_cb_palette(maxQ)

  # nc = 1 stacks every field in one column and runs out of device height
  # from about seven fields (IRM decides nfld from the data). Honour nr, and
  # otherwise fall back to a near-square layout.
  if (nc <= 1 && nfld > 3) {
    nc <- if (nr > 1) ceiling(nfld / nr) else ceiling(sqrt(nfld))
  }
  setup_legend_layout(nfld, nc)

  for (f in 1:nfld) {
    if (style == "line") {
      call_plot(
        plot,
        list(
          x = 1:ncls, y = BCRM[f, , 1],
          type = "n",
          xlab = paste("Latent", msg),
          ylab = "Category Probability",
          ylim = c(0, 1),
          xaxt = "n",
          main = paste("Field", f, "- Category Response")
        ),
        dots
      )
      call_plot(graphics::axis, list(side = 1, at = 1:ncls), dots)
      for (q in 1:maxQ) {
        call_plot(
          graphics::lines,
          list(
            x = 1:ncls, y = BCRM[f, , q],
            type = "b", pch = q, lty = q, col = cols[q], lwd = 1.5
          ),
          dots
        )
      }
    } else if (style == "bar") {
      bar_data <- t(BCRM[f, , ])
      call_plot(
        graphics::barplot,
        list(
          height = bar_data,
          col = cols[1:maxQ],
          names.arg = paste0(substr(msg, 1, 1), 1:ncls),
          xlab = paste("Latent", msg),
          ylab = "Category Probability",
          ylim = c(0, 1),
          main = paste("Field", f, "- Category Response")
        ),
        dots
      )
    }
  }

  if (style == "line") {
    draw_legend_strip(
      legend = paste("Cat", 1:maxQ),
      col = cols[1:maxQ], lty = 1:maxQ, pch = 1:maxQ,
      cex = 1.0, bty = "n", ncol = min(maxQ, 5)
    )
  } else {
    draw_legend_strip(
      legend = paste("Cat", 1:maxQ),
      fill = cols[1:maxQ],
      cex = 1.0, bty = "n", ncol = min(maxQ, 5)
    )
  }
}

#' FCBR: boundary probability plot (ordinal Biclustering only)
#' @noRd
plot_poly_fcbr <- function(x, nc, nr, dots = list()) {
  BCRM <- x$FRP
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]
  msg <- x$msg
  n_boundaries <- maxQ - 1
  cols <- get_cb_palette(n_boundaries)

  # nc = 1 stacks every field in one column and runs out of device height
  # from about seven fields (IRM decides nfld from the data). Honour nr, and
  # otherwise fall back to a near-square layout.
  if (nc <= 1 && nfld > 3) {
    nc <- if (nr > 1) ceiling(nfld / nr) else ceiling(sqrt(nfld))
  }
  setup_legend_layout(nfld, nc)

  for (f in 1:nfld) {
    boundary_probs <- matrix(0, nrow = n_boundaries, ncol = ncls)
    for (b in 1:n_boundaries) {
      q_threshold <- b + 1
      for (cc in 1:ncls) {
        boundary_probs[b, cc] <- sum(BCRM[f, cc, q_threshold:maxQ])
      }
    }

    call_plot(
      plot,
      list(
        x = 1:ncls, y = boundary_probs[1, ],
        type = "n",
        xlab = paste("Latent", msg),
        ylab = "Boundary Probability",
        ylim = c(0, 1),
        xaxt = "n",
        main = paste("Field", f, "- Boundary Prob")
      ),
      dots
    )
    call_plot(graphics::axis, list(side = 1, at = 1:ncls), dots)
    # P(Q >= 1) = 1.0 reference line
    call_plot(
      graphics::lines,
      list(
        x = 1:ncls, y = rep(1, ncls),
        type = "b", pch = 0, lty = 1, col = "gray60", lwd = 1
      ),
      dots
    )
    for (b in 1:n_boundaries) {
      call_plot(
        graphics::lines,
        list(
          x = 1:ncls, y = boundary_probs[b, ],
          type = "b", pch = b, lty = b, col = cols[b], lwd = 1.5
        ),
        dots
      )
    }
  }

  draw_legend_strip(
    legend = c("P(Q>=1)", paste0("P(Q>=", 2:maxQ, ")")),
    col = c("gray60", cols[1:n_boundaries]),
    lty = c(1, 1:n_boundaries),
    pch = c(0, 1:n_boundaries),
    cex = 1.0, bty = "n", ncol = min(maxQ, 5)
  )
}

#' ScoreField: expected score heatmap
#' @noRd
plot_scorefield <- function(x, dots = list()) {
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
  call_plot(
    graphics::image,
    list(
      x = 1:ncls, y = 1:nfld,
      z = t(score_mat),
      col = hcl.colors(50, "YlOrRd", rev = TRUE),
      xlab = paste("Latent", msg),
      ylab = "Field",
      main = "Score Field Heatmap",
      xaxt = "n", yaxt = "n"
    ),
    dots
  )
  call_plot(graphics::axis, list(side = 1, at = 1:ncls, labels = paste0(substr(msg, 1, 1), 1:ncls)), dots)
  call_plot(graphics::axis, list(side = 2, at = 1:nfld, labels = paste0("F", 1:nfld)), dots)

  for (f in 1:nfld) {
    for (cc in 1:ncls) {
      text(cc, f, sprintf("%.2f", score_mat[f, cc]), cex = 0.8)
    }
  }
}

#' RRV: transposed field-axis version (polytomous Biclustering)
#' @noRd
plot_poly_rrv <- function(x, stat, dots = list()) {
  BCRM <- x$FRP
  nfld <- dim(BCRM)[1]
  ncls <- dim(BCRM)[2]
  maxQ <- dim(BCRM)[3]
  msg <- x$msg

  FRP_mat <- calc_expected_scores(BCRM, stat)
  RRV <- t(FRP_mat)

  par(mfrow = c(1, 1))
  call_plot(
    plot,
    list(
      x = 1:nfld, y = RRV[1, ],
      type = "n",
      ylim = c(1, maxQ),
      xlab = "Field",
      ylab = paste0("Expected Score (", stat, ")"),
      main = paste(msg, "Reference Vector (", stat, ")"),
      xaxt = "n", bty = "n"
    ),
    dots
  )
  call_plot(graphics::axis, list(side = 1, at = 1:nfld, labels = paste0("F", 1:nfld)), dots)
  abline(h = 1:maxQ, col = "gray90", lty = 3)

  cols <- get_cb_palette(ncls)
  for (i in 1:ncls) {
    call_plot(graphics::lines, list(x = 1:nfld, y = RRV[i, ], type = "o", lty = i, col = cols[i], lwd = 1.5), dots)
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

#' Heatmap of RankUpShift: which boundary moves at each rank step
#'
#' One panel per field: rows are the steps rank 1 -> 2, ..., (C - 1) -> C from
#' top to bottom, columns the category boundaries (answering b or above). A
#' cell is the increase of P(X >= b) when the rank goes up by one; values of
#' 0.05 and more are written in the cell. Increases are shaded white to orange;
#' a decrease (an order violation, possible under GTM) is shaded blue. A colour
#' key is drawn to the right. Ordinal Ranklustering only.
#' @noRd
plot_rank_up_shift <- function(x, colors = NULL, dots = list()) {
  D <- x$RankUpShift
  if (is.null(D)) {
    stop("RankUpShift is available for an ordinal Ranklustering (method = \"R\") only.",
      call. = FALSE
    )
  }
  nstep <- dim(D)[1]
  nfld <- dim(D)[2]
  nb <- dim(D)[3]
  hi <- max(D, 0)
  lo <- min(D, 0)
  pos_col <- if (!is.null(colors) && length(colors) >= 1) colors[1] else "darkorange3"
  neg_col <- if (!is.null(colors) && length(colors) >= 2) colors[2] else "steelblue"
  pos_ramp <- grDevices::colorRampPalette(c("white", pos_col))(101)
  neg_ramp <- grDevices::colorRampPalette(c("white", neg_col))(101)
  shade <- function(v) {
    out <- rep("white", length(v))
    p <- v > 0 & hi > 0
    n <- v < 0 & lo < 0
    out[p] <- pos_ramp[1 + round(100 * v[p] / hi)]
    out[n] <- neg_ramp[1 + round(100 * v[n] / lo)]
    return(out)
  }
  step_lab <- sub("^Rank([0-9]+)->Rank([0-9]+)$", "\\1\u2192\\2", dimnames(D)[[1]])
  bound_lab <- sub("^>=", "\u2265", dimnames(D)[[3]])

  # Shrink text and margins together on a small device (the default png of
  # plot() is 800 x 600 pixels at 300 dpi, under 3 x 2 inches)
  din <- graphics::par("din")
  sc <- max(0.4, min(1, din[1] / (2.2 * nfld + 1), din[2] / 4))
  layout(matrix(seq_len(nfld + 1), nrow = 1), widths = c(rep(1, nfld), 0.35))
  graphics::par(cex = sc, mex = sc)
  for (f in seq_len(nfld)) {
    M <- matrix(D[, f, ], nrow = nstep, ncol = nb)
    graphics::par(mar = c(4, if (f == 1) 4.5 else 1, 3, 0.5))
    graphics::plot.new()
    graphics::plot.window(xlim = c(0.5, nb + 0.5), ylim = c(nstep + 0.5, 0.5), xaxs = "i", yaxs = "i")
    for (s in seq_len(nstep)) {
      graphics::rect(seq_len(nb) - 0.5, s - 0.5, seq_len(nb) + 0.5, s + 0.5,
        col = shade(M[s, ]), border = "white", lwd = 1
      )
      big <- abs(M[s, ]) >= 0.05
      if (any(big)) {
        graphics::text(which(big), s, sprintf("%.2f", M[s, big]), cex = 0.8)
      }
    }
    call_plot(graphics::axis, list(side = 1, at = seq_len(nb), labels = bound_lab, tick = FALSE), dots)
    if (f == 1) {
      call_plot(graphics::axis, list(side = 2, at = seq_len(nstep), labels = step_lab, las = 1, tick = FALSE), dots)
      graphics::mtext("Rank step", side = 2, line = 3.2, cex = 0.8)
    }
    graphics::mtext("Category boundary (answering this or above)", side = 1, line = 2.5, cex = 0.7)
    call_plot(graphics::title, list(main = dimnames(D)[[2]][f]), dots)
  }
  # Colour key
  graphics::par(mar = c(4, 0.5, 3, 3))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(lo, max(hi, 1e-12)), yaxs = "i")
  key <- seq(lo, max(hi, 1e-12), length.out = 101)
  graphics::rect(0, key[-101], 0.6, key[-1], col = shade((key[-101] + key[-1]) / 2), border = NA)
  graphics::axis(4, las = 1, cex.axis = 0.8)
  graphics::mtext("Increase", side = 3, line = 0.5, cex = 0.8)
  return(invisible(NULL))
}

#' Box plots of the rank-up and rank-down odds by estimated rank
#'
#' Two panels: the rank-up odds (membership of the next rank up over that of
#' the estimated rank) and the rank-down odds, each by estimated rank, on a log
#' axis. Odds near 1 mark respondents on the border with the neighbouring
#' rank; a dashed line is drawn at 1. Odds below 1e-4 are drawn at 1e-4.
#' Ordinal Ranklustering only.
#' @noRd
plot_rank_odds <- function(x, dots = list()) {
  S <- x$Students
  if (!all(c("Rank-Up Odds", "Rank-Down Odds") %in% colnames(S))) {
    stop("The rank-up and rank-down odds are available for an ordinal Ranklustering ",
      "(method = \"R\") only.",
      call. = FALSE
    )
  }
  ncls <- x$n_class
  rank <- factor(S[, "Estimate"], levels = seq_len(ncls))
  floor_at <- 1e-4
  din <- graphics::par("din")
  sc <- max(0.4, min(1, din[1] / 6, din[2] / 6))
  layout(matrix(1:2, nrow = 2))
  graphics::par(mar = c(4, 6, 2.5, 1), cex = sc, mex = sc)
  for (nm in c("Rank-Up Odds", "Rank-Down Odds")) {
    v <- pmax(S[, nm], floor_at)
    # lower end: the power of ten below the smallest odds (never below 1e-4)
    lo <- 10^floor(log10(min(v, 1, na.rm = TRUE)))
    call_plot(
      graphics::boxplot,
      list(
        x = split(v, rank), log = "y", ylim = c(lo, max(1, v, na.rm = TRUE)),
        col = "grey85", outcex = 0.5, yaxt = "n",
        xlab = "Estimated rank", ylab = "", main = nm
      ),
      dots
    )
    ticks <- 10^seq(log10(lo), 0)
    labs <- format(ticks, scientific = FALSE, drop0trailing = TRUE)
    if (lo <= floor_at) labs[1] <- paste0("\u2264", format(floor_at, scientific = FALSE))
    call_plot(graphics::axis, list(side = 2, at = ticks, labels = labs, las = 1), dots)
    graphics::mtext("Odds (log scale)", side = 2, line = 4.5)
    graphics::abline(h = 1, lty = 2, col = "firebrick")
  }
  return(invisible(NULL))
}
