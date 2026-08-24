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
#'
#' The response matrix is drawn as a single raster rather than one
#' \code{rect()} per cell. The old per-cell version gave every cell a white
#' border, and a border cannot be drawn thinner than one device pixel: once the
#' rows outnumbered the pixels available to them the borders covered the fill,
#' so rows came out white in a pattern set by where the cell boundaries happened
#' to land on the pixel grid. Measured on an all-black 400x600 plot, mean
#' luminance rose from 0.01 at 50 rows to 0.29 at 821 rows; with the raster it
#' is 0.00 at every size. Drawing is also one call instead of nobs x testlength.
#'
#' Grid lines are drawn only when a cell is at least \code{min_grid_px} pixels
#' on both sides, which is where they can be seen without swallowing the cell.
#' @noRd
plot_array <- function(x, cell_width, cell_height, colors, dots = list(),
                       min_grid_px = 6) {
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

  # Colour matrix for one panel, row 1 at the top as rasterImage expects
  # @noRd
  as_color_matrix <- function(m) {
    m <- as.matrix(m)
    idx <- match(m, all_values)
    fill <- colors[idx]
    fill[is.na(m) | m == -1] <- missing_color
    matrix(fill, nrow = NROW(m), ncol = NCOL(m))
  }

  # Panel size in device pixels, as c(width, height)
  # @noRd
  panel_px <- function() {
    px <- tryCatch(
      {
        w <- grconvertX(plot_width, "user", "device") - grconvertX(0, "user", "device")
        h <- grconvertY(0, "user", "device") - grconvertY(plot_height, "user", "device")
        c(abs(w), abs(h))
      },
      error = function(e) c(NA_real_, NA_real_)
    )
    if (any(!is.finite(px)) || any(px <= 0)) px <- c(NA_real_, NA_real_)
    return(px)
  }

  draw_panel <- function(m, main) {
    call_plot(
      plot,
      list(
        x = 0, y = 0,
        type = "n",
        xlim = c(0, plot_width), ylim = c(0, plot_height),
        xlab = "", ylab = "", xaxt = "n", yaxt = "n",
        main = main, frame.plot = TRUE
      ),
      dots
    )
    rasterImage(
      grDevices::as.raster(downsample_nn(as_color_matrix(m), panel_px())),
      xleft = 0, ybottom = 0, xright = plot_width, ytop = plot_height,
      interpolate = FALSE
    )
    # Cell borders only where a cell is large enough for them to read
    px <- panel_px() / c(ncols, nrows)
    if (all(px >= min_grid_px)) {
      abline(v = seq(0, plot_width, by = cell_w), col = "white", lwd = 0.5)
      abline(h = seq(0, plot_height, by = cell_h), col = "white", lwd = 0.5)
    }
  }

  draw_panel(raw_data, "Original Data")
  draw_panel(clustered_data, "Clustered Data")

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

#' Thin a colour matrix to the pixel grid by nearest neighbour
#'
#' When the matrix has more rows (or columns) than the panel has pixels, the
#' graphics device resolves the raster by *averaging*: `interpolate = FALSE` is
#' not honoured on the way down. 4,000 alternating orange/blue rows drawn into
#' 400 pixels come out as one uniform blend that is in neither category's
#' colour — a fabricated colour in a plot whose colours are supposed to be
#' categorical. Picking one source row per output pixel keeps every drawn pixel
#' a colour that actually occurs in the data.
#'
#' The thinning is a subsample, so at extreme sizes each drawn row stands for
#' one of the respondents it covers rather than a summary of them. That is the
#' honest trade for a categorical raster: averaging would invent categories,
#' and a majority vote would still discard the minority while costing more.
#'
#' @param cm character matrix of colours
#' @param px c(width, height) of the panel in device pixels; NA disables
#' @noRd
downsample_nn <- function(cm, px) {
  if (any(!is.finite(px))) {
    return(cm)
  }
  nr <- NROW(cm)
  nc <- NCOL(cm)
  target_r <- max(1L, min(nr, as.integer(floor(px[2]))))
  target_c <- max(1L, min(nc, as.integer(floor(px[1]))))
  if (target_r >= nr && target_c >= nc) {
    return(cm)
  }
  ri <- unique(round(seq(1, nr, length.out = target_r)))
  ci <- unique(round(seq(1, nc, length.out = target_c)))
  return(cm[ri, ci, drop = FALSE])
}
