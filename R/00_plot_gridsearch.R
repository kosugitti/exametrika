#' @title Plot the fit index of a grid search against the number of classes
#' @description
#' Draws the fit index recorded by \code{\link{GridSearch}}. For LCA/LRA the
#' index is drawn against the number of classes or ranks. For Biclustering
#' the default is one line per number of fields; \code{style = "persp"}
#' draws a surface over classes and fields instead. Converged settings are
#' filled points, settings that left a class, rank or field empty are open
#' triangles, settings that failed to converge are left blank, and the
#' optimum is marked with a filled red point.
#' @param x A GridSearch result
#' @param style "line" or "persp" (Biclustering only)
#' @param colors Optional colour vector; for the line style, one colour per
#' line
#' @param dots Additional graphical parameters
#' @noRd
plot_gridsearch <- function(x, style = "line", colors = NULL, dots = list()) {
  index_name <- if (is.null(x$index)) "Fit index" else x$index
  unit <- if (identical(x$fun, "LRA")) "ranks" else "classes"
  xlab <- paste("Number of", unit)
  opt_col <- "#D55E00"

  if (!is.null(x$index_vec)) {
    # ------------------------------------------------------- LCA / LRA
    ncls <- as.integer(sub("^ncls", "", names(x$index_vec)))
    y <- as.numeric(x$index_vec)
    empty <- if (is.null(x$empty_vec)) rep(FALSE, length(y)) else as.logical(x$empty_vec)
    line_col <- if (is.null(colors)) "#0072B2" else colors[1]
    ok <- !is.na(y)
    args <- merge_dots(
      list(
        x = ncls[ok], y = y[ok], type = "b", pch = NA,
        xlab = xlab, ylab = index_name, col = line_col,
        main = paste(index_name, "by number of", unit),
        xaxt = "n"
      ),
      dots
    )
    do.call(plot, args)
    axis(1, at = ncls)
    points(ncls[ok & !empty], y[ok & !empty], pch = 16, col = line_col)
    points(ncls[ok & empty], y[ok & empty], pch = 2, col = line_col)
    opt <- which(ncls == x$optimal_ncls)
    if (length(opt) == 1 && ok[opt]) {
      points(ncls[opt], y[opt], pch = 16, col = opt_col, cex = 1.6)
      abline(v = ncls[opt], lty = 3, col = opt_col)
    }
    if (any(empty)) {
      legend("topright",
        legend = c("fit", "with empty class/rank", "optimum"),
        pch = c(16, 2, 16), col = c(line_col, line_col, opt_col),
        pt.cex = c(1, 1, 1.6), bty = "n"
      )
    }
    return(invisible(NULL))
  }

  # ------------------------------------------------------------ Biclustering
  mat <- x$index_matrix
  ncls <- as.integer(sub("^ncls ", "", rownames(mat)))
  nfld <- as.integer(sub("^nfld ", "", colnames(mat)))
  empty <- if (is.null(x$empty_matrix)) {
    matrix(FALSE, nrow(mat), ncol(mat))
  } else {
    x$empty_matrix
  }

  if (identical(style, "persp")) {
    args <- merge_dots(
      list(
        x = ncls, y = nfld, z = mat,
        xlab = xlab, ylab = "Number of fields", zlab = index_name,
        main = paste(index_name, "by number of", unit, "and fields"),
        theta = 35, phi = 25, ticktype = "detailed",
        col = if (is.null(colors)) "#CCE5F2" else colors[1]
      ),
      dots
    )
    do.call(persp, args)
    return(invisible(NULL))
  }

  pal <- if (is.null(colors)) {
    grDevices::hcl.colors(length(nfld), "Dark 3")
  } else {
    rep_len(colors, length(nfld))
  }
  ylim <- range(mat, na.rm = TRUE)
  args <- merge_dots(
    list(
      x = range(ncls), y = ylim, type = "n",
      xlab = xlab, ylab = index_name,
      main = paste(index_name, "by number of", unit, "and fields"),
      xaxt = "n"
    ),
    dots
  )
  do.call(plot, args)
  axis(1, at = ncls)
  for (j in seq_along(nfld)) {
    y <- mat[, j]
    ok <- !is.na(y)
    lines(ncls[ok], y[ok], col = pal[j])
    points(ncls[ok & !empty[, j]], y[ok & !empty[, j]], pch = 16, col = pal[j])
    points(ncls[ok & empty[, j]], y[ok & empty[, j]], pch = 2, col = pal[j])
  }
  oi <- which(ncls == x$optimal_ncls)
  oj <- which(nfld == x$optimal_nfld)
  if (length(oi) == 1 && length(oj) == 1 && !is.na(mat[oi, oj])) {
    points(ncls[oi], mat[oi, oj], pch = 16, col = opt_col, cex = 1.6)
  }
  leg <- list(
    legend = c(paste("nfld =", nfld), "optimum"),
    col = c(pal, opt_col), pch = rep(16, length(nfld) + 1),
    pt.cex = c(rep(1, length(nfld)), 1.6), lty = c(rep(1, length(nfld)), NA)
  )
  if (any(empty)) {
    leg$legend <- c(leg$legend, "with empty class/field")
    leg$col <- c(leg$col, "black")
    leg$pch <- c(leg$pch, 2)
    leg$pt.cex <- c(leg$pt.cex, 1)
    leg$lty <- c(leg$lty, NA)
  }
  do.call(legend, c(list("topright", bty = "n"), leg))
  return(invisible(NULL))
}

#' @title Let graphical parameters supplied via ... override the defaults
#' @param defaults Named list of default arguments
#' @param dots Named list from `list(...)`
#' @noRd
merge_dots <- function(defaults, dots) {
  if (length(dots) > 0) {
    defaults[names(dots)] <- dots
  }
  return(defaults)
}
