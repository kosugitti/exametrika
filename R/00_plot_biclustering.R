# Biclustering plot functions (binary Array + shared color palette)
# Internal functions called from plot.exametrika()

#' Merge default plotting arguments with user-supplied graphical parameters
#'
#' User-supplied arguments collected by \code{...} in \code{plot.exametrika()}
#' (passed down as \code{dots}) take precedence over the package defaults, so
#' callers can override \code{xlab}, \code{ylab}, \code{main}, etc., and add
#' standard graphical parameters such as \code{pch}, \code{las}, \code{cex}.
#' Base graphics functions (\code{plot}, \code{barplot}, \code{image},
#' \code{axis}, \code{lines}, \code{curve}) silently ignore graphical
#' parameters that do not apply, so the merged list can be forwarded verbatim.
#' @noRd
merge_plot_dots <- function(defaults, dots) {
  if (length(dots)) utils::modifyList(defaults, dots) else defaults
}

#' Call a base graphics function with defaults overridable by user dots
#' @noRd
call_plot <- function(.fun, defaults, dots = list()) {
  do.call(.fun, merge_plot_dots(defaults, dots))
}

#' Draw a curve by grid evaluation, honouring user graphical parameters
#'
#' Replacement for \code{curve()} that evaluates \code{fn} (a vectorised
#' function of the abscissa) on a grid and draws it via \code{plot}/\code{lines}
#' through \code{call_plot()}, so user-supplied \code{...} arguments are
#' applied. Using grid evaluation avoids the non-standard evaluation of
#' \code{curve()}'s first argument.
#' @noRd
draw_curve <- function(fn, from = -4, to = 4, n = 201, add = FALSE,
                       defaults = list(), dots = list()) {
  xx <- seq(from, to, length.out = n)
  yy <- fn(xx)
  if (add) {
    call_plot(graphics::lines, c(list(x = xx, y = yy), defaults), dots)
  } else {
    call_plot(plot, c(list(x = xx, y = yy, type = "l"), defaults), dots)
  }
}

#' Colorblind-friendly palette (Paul Tol Vibrant + Bright extension)
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

#' Set up panel grid + legend strip layout
#' Overrides mfrow, reserving a thin legend area in the bottom row
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

#' Draw legend in the bottom row of the layout
#' Call after setup_legend_layout() and all panels are drawn
#' @noRd
draw_legend_strip <- function(...) {
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", ...)
}

#' Array plot (shared by Biclustering / IRM / LDB / BINET)
#' @noRd
plot_array <- function(x, cell_width, cell_height, colors, dots = list()) {
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
  # For polytomous models, use Q (polytomous responses) instead of U (binary)
  if (!is.null(x$Q) && inherits(x, c("nominalBiclustering", "ordinalBiclustering", "ratedBiclustering"))) {
    raw_data <- x$Q
  } else {
    raw_data <- x$U
    if (is.null(raw_data)) raw_data <- x$Q
  }

  clustered_data <- raw_data[case_order, field_order]

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
  call_plot(
    plot,
    list(
      x = 0, y = 0,
      type = "n",
      xlim = c(0, plot_width), ylim = c(0, plot_height),
      xlab = "", ylab = "", xaxt = "n", yaxt = "n",
      main = "Original Data", frame.plot = TRUE
    ),
    dots
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

  ## Clustered Plot
  call_plot(
    plot,
    list(
      x = 0, y = 0,
      type = "n",
      xlim = c(0, plot_width), ylim = c(0, plot_height),
      xlab = "", ylab = "", xaxt = "n", yaxt = "n",
      main = "Clustered Data", frame.plot = TRUE
    ),
    dots
  )
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      val <- clustered_data[i, j]
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
