# Biclustering プロット関数（二値 Array + 共通カラーパレット）
# plot.exametrika() から呼び出される内部関数群

#' カラーブラインド対応パレット (Paul Tol Vibrant + Bright extension)
#' @noRd
get_cb_palette <- function(n) {
  base <- c(
    "#0077BB", # blue
    "#EE7733", # orange
    "#009988", # teal
    "#EE3377", # magenta
    "#CC3311", # red
    "#33BBEE", # cyan
    "#AA3377", # purple
    "#DDCC77", # sand
    "#332288", # indigo
    "#117733" # forest green
  )
  if (n <= length(base)) {
    return(base[1:n])
  }
  return(c(base, rainbow(n - length(base))))
}

#' パネルグリッド＋凡例ストリップのレイアウトを設定
#' mfrow を上書きし、最下行に薄い凡例領域を確保する
#' @noRd
setup_legend_layout <- function(n_panels, nc) {
  if (n_panels <= 1) {
    layout(matrix(c(1, 2), nrow = 2), heights = c(1, 0.2))
  } else {
    n_rows <- ceiling(n_panels / nc)
    layout_mat <- matrix(0, nrow = n_rows + 1, ncol = nc)
    for (i in seq_len(n_panels)) {
      r <- ceiling(i / nc)
      cc <- ((i - 1) %% nc) + 1
      layout_mat[r, cc] <- i
    }
    layout_mat[n_rows + 1, ] <- n_panels + 1
    layout(layout_mat, heights = c(rep(1, n_rows), 0.2))
  }
}

#' レイアウト最下行の凡例領域に凡例を描画する
#' setup_legend_layout() の後、全パネル描画後に呼ぶ
#' @noRd
draw_legend_strip <- function(...) {
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", ...)
}

#' Array プロット（Biclustering / IRM / LDB / BINET 共通）
#' @noRd
plot_array <- function(x, cell_width, cell_height, colors) {
  cell_w <- cell_width
  cell_h <- cell_height
  old_par <- par(no.readonly = TRUE)
  on.exit({
    restore_par <- old_par
    restore_par[c("pin", "fin", "plt", "usr")] <- NULL
    suppressWarnings(par(restore_par))
  })
  # Reduce margins to maximize plot area
  par(mfrow = c(1, 2), mar = c(1, 1, 2, 1), oma = c(0, 0, 0, 0))

  nrows <- x$nobs
  ncols <- x$testlength

  # Sort so that higher class numbers (higher correct response rates) appear at bottom
  case_order <- order(x$ClassEstimated, decreasing = FALSE)
  field_order <- order(x$FieldEstimated, decreasing = FALSE)
  raw_data <- x$U
  if (is.null(raw_data)) {
    raw_data <- x$Q
  }

  clusterd_data <- raw_data[case_order, field_order]

  sorted_class <- x$ClassEstimated[case_order]
  sorted_field <- x$FieldEstimated[field_order]

  class_breaks <- cumsum(table(sorted_class))
  field_breaks <- cumsum(table(sorted_field))

  class_lines <- (nrows - class_breaks[-length(class_breaks)]) * cell_h
  field_lines <- field_breaks[-length(field_breaks)] * cell_w

  ## colors
  # Use sort(unique(...)) to ensure consistent ordering: 0 (white), 1 (black)
  # Exclude missing values (NA and -1) from category colors
  all_values <- sort(unique(as.vector(as.matrix(raw_data))))
  all_values <- all_values[!is.na(all_values) & all_values != -1]
  n_categories <- length(all_values)

  if (is.null(colors)) {
    if (n_categories == 2) {
      colors <- c("#FFFFFF", "#000000")
    } else {
      colors <- c(
        "#E69F00", "#0173B2", "#DE8F05", "#029E73", "#CC78BC",
        "#CA9161", "#FBAFE4", "#949494", "#ECE133", "#56B4E9"
      )
    }
  }
  if (length(colors) < n_categories) {
    additional_colors <- c(
      "#D55E00", "#F0E442", "#009E73", "#CC79A7", "#0072B2",
      "#E8601C", "#7CAE00", "#C77CFF", "#00BFC4", "#F8766D"
    )
    colors <- c(colors, additional_colors)
    colors <- colors[1:n_categories]
  }

  # Missing value color: gray for binary (to distinguish from white/black), black for polytomous
  missing_color <- if (n_categories == 2) "#808080" else "#000000"

  # Plot area
  plot_width <- ncols * cell_w
  plot_height <- nrows * cell_h

  # original data
  plot(0, 0,
    type = "n",
    xlim = c(0, plot_width), ylim = c(0, plot_height),
    xlab = "", ylab = "", xaxt = "n", yaxt = "n",
    main = "Original Data", frame.plot = TRUE
  )
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      val <- raw_data[i, j]
      x1 <- (j - 1) * cell_w
      y1 <- (nrows - i) * cell_h
      x2 <- j * cell_w
      y2 <- (nrows - i + 1) * cell_h

      if (is.na(val) || val == -1) {
        fill_color <- missing_color
      } else {
        color_index <- match(val, all_values)
        fill_color <- colors[color_index]
      }

      rect(x1, y1, x2, y2, col = fill_color, border = "white", lwd = 0.1)
    }
  }

  ## Clusterd Plot
  plot(0, 0,
    type = "n",
    xlim = c(0, plot_width), ylim = c(0, plot_height),
    xlab = "", ylab = "", xaxt = "n", yaxt = "n",
    main = "Clusterd Data", frame.plot = TRUE
  )
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      val <- clusterd_data[i, j]
      x1 <- (j - 1) * cell_w
      y1 <- (nrows - i) * cell_h
      x2 <- j * cell_w
      y2 <- (nrows - i + 1) * cell_h

      if (is.na(val) || val == -1) {
        fill_color <- missing_color
      } else {
        color_index <- match(val, all_values)
        fill_color <- colors[color_index]
      }

      rect(x1, y1, x2, y2, col = fill_color, border = "white", lwd = 0.1)
    }
  }
  for (line_y in class_lines) {
    lines(c(0, plot_width), c(line_y, line_y),
      col = "red", lwd = 1
    )
  }
  for (line_x in field_lines) {
    lines(c(line_x, line_x), c(0, plot_height),
      col = "red", lwd = 1
    )
  }
}
