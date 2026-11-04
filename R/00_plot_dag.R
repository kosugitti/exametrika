# Base-graphics drawing of the directed graphs printed by the network models
# (BNM, LDLRA, LDB, BINET). No igraph: the layout is computed from the
# adjacency matrix and the picture is drawn with symbols() and arrows().

#' @title Layered coordinates for a directed graph
#' @description
#' Nodes are placed in layers by the longest path from a source (Kahn's
#' algorithm, removing all current sources at once), parents above children.
#' Within a layer the order is settled by a few barycenter sweeps (the
#' Sugiyama heuristic), which keeps edges short and crossings few. Every node
#' gets its own position. A cycle, which a learned structure should not
#' contain but a user-supplied one may, is broken at the node with the fewest
#' remaining parents.
#' @param adj Square 0/1 adjacency matrix, rows are parents, columns children
#' @return A data frame with one row per node: `x`, `y`, `layer`
#' @noRd
dag_layout_base <- function(adj) {
  adj <- (as.matrix(adj) != 0) * 1
  n <- nrow(adj)
  if (n == 0) {
    return(data.frame(x = numeric(0), y = numeric(0), layer = integer(0)))
  }
  layer <- kahn_layers(adj)$layer
  # --- order within layers (barycenter sweeps) --------------------------
  pos <- numeric(n)
  for (l in unique(sort(layer))) {
    idx <- which(layer == l)
    pos[idx] <- seq_along(idx)
  }
  nl <- max(layer)
  if (nl > 0 && sum(adj) > 0) {
    for (sweep in 1:4) {
      down <- sweep %% 2 == 1
      layers <- if (down) 1:nl else (nl - 1):0
      for (l in layers) {
        idx <- which(layer == l)
        if (length(idx) < 2) next
        bary <- vapply(idx, function(v) {
          nb <- if (down) which(adj[, v] == 1 & layer < l) else which(adj[v, ] == 1 & layer > l)
          if (length(nb) == 0) pos[v] else mean(pos[nb])
        }, numeric(1))
        # stable: ties keep the current order
        pos[idx] <- rank(bary, ties.method = "first")
      }
    }
  }
  # --- coordinates: unit spacing, each layer centred, sources on top -----
  x <- numeric(n)
  for (l in unique(layer)) {
    idx <- which(layer == l)
    k <- length(idx)
    x[idx] <- pos[idx] - (k + 1) / 2
  }
  return(data.frame(x = x, y = -layer, layer = layer))
}

#' @title Draw a directed graph with base graphics
#' @description
#' Nodes are circles large enough to hold their label, arrows end at the
#' circle's edge, and parents sit above children (see
#' [dag_layout_base()]). Replaces `igraph::plot.igraph()` in the print
#' methods of BNM, LDLRA, LDB and BINET (2.3.0).
#' @param adj Square 0/1 adjacency matrix with row names as node labels
#' @param labels Node labels; default the row names of `adj`
#' @param main Title
#' @param edge_labels Optional data frame with columns `From`, `To` and a
#'   third column holding the text to print at the middle of that edge
#' @param node_col,border_col,edge_col Colours
#' @param ... Further arguments to `plot()`
#' @noRd
#' @importFrom graphics arrows symbols strwidth strheight
plot_dag <- function(adj, labels = NULL, main = NULL, edge_labels = NULL,
                     node_col = "#DCE9F5", border_col = "#4A6FA5",
                     edge_col = "#555555", ...) {
  adj <- as.matrix(adj)
  n <- nrow(adj)
  if (is.null(labels)) labels <- rownames(adj)
  if (is.null(labels)) labels <- as.character(seq_len(n))
  lay <- dag_layout_base(adj)
  x <- lay$x
  y <- lay$y
  r <- 0.36 # node radius in user units (nodes are 1 unit apart)
  pad <- r + 0.3
  old_par <- par(mar = c(0.5, 0.5, if (is.null(main)) 0.5 else 2.5, 0.5))
  on.exit(par(old_par), add = TRUE)
  xr <- range(x)
  yr <- range(y)
  if (diff(xr) < 2) xr <- mean(xr) + c(-1, 1)
  if (diff(yr) < 2) yr <- mean(yr) + c(-1, 1)
  plot(NA,
    xlim = xr + c(-pad, pad), ylim = yr + c(-pad, pad), asp = 1,
    axes = FALSE, xlab = "", ylab = "", main = main, ...
  )
  # label size: fit the widest label inside the circle
  cex <- min(1, 1.6 * r / max(strwidth(labels, cex = 1)))
  cex <- max(cex, 0.4)
  # edges first, shortened by the radius at both ends. An edge that skips a
  # layer is bent sideways so that it does not run through the nodes in
  # between; the bend goes to the side the child lies on (right if aligned).
  edges <- which(adj != 0, arr.ind = TRUE)
  if (nrow(edges) > 0) {
    for (e in seq_len(nrow(edges))) {
      i <- edges[e, 1]
      j <- edges[e, 2]
      dx <- x[j] - x[i]
      dy <- y[j] - y[i]
      d <- sqrt(dx^2 + dy^2)
      if (d < 1e-8) next
      skip <- abs(lay$layer[j] - lay$layer[i]) >= 2
      if (!skip) {
        ux <- dx / d
        uy <- dy / d
        arrows(x[i] + ux * r, y[i] + uy * r, x[j] - ux * r, y[j] - uy * r,
          length = 0.08, angle = 20, col = edge_col, lwd = 1.2
        )
      } else {
        side <- if (dx >= 0) 1 else -1
        # control point: midpoint pushed sideways, more for longer jumps
        off <- 0.45 * abs(lay$layer[j] - lay$layer[i]) * side
        cx <- (x[i] + x[j]) / 2 + off * (-dy / d)
        cy <- (y[i] + y[j]) / 2 + off * (dx / d)
        tt <- seq(0, 1, length.out = 40)
        bx <- (1 - tt)^2 * x[i] + 2 * (1 - tt) * tt * cx + tt^2 * x[j]
        by <- (1 - tt)^2 * y[i] + 2 * (1 - tt) * tt * cy + tt^2 * y[j]
        keep <- sqrt((bx - x[i])^2 + (by - y[i])^2) > r &
          sqrt((bx - x[j])^2 + (by - y[j])^2) > r
        bx <- bx[keep]
        by <- by[keep]
        m <- length(bx)
        if (m >= 2) {
          lines(bx[-m], by[-m], col = edge_col, lwd = 1.2)
          arrows(bx[m - 1], by[m - 1], bx[m], by[m],
            length = 0.08, angle = 20, col = edge_col, lwd = 1.2
          )
        }
      }
    }
  }
  symbols(x, y,
    circles = rep(r, n), inches = FALSE, add = TRUE,
    bg = node_col, fg = border_col, lwd = 1.2
  )
  text(x, y, labels, cex = cex, col = "black")
  # edge labels on a white patch, 40% of the way from the parent so that
  # labels of edges converging on one child do not pile up at the midpoint;
  # several labels on one edge are joined with "/"
  if (!is.null(edge_labels) && NROW(edge_labels) > 0) {
    from <- match(edge_labels[[1]], labels)
    to <- match(edge_labels[[2]], labels)
    lab <- as.character(edge_labels[[3]])
    ok <- !is.na(from) & !is.na(to)
    if (any(ok)) {
      key <- paste(from[ok], to[ok])
      lab <- tapply(lab[ok], key, function(v) paste(unique(v), collapse = "/"))
      ij <- do.call(rbind, strsplit(names(lab), " "))
      i <- as.integer(ij[, 1])
      j <- as.integer(ij[, 2])
      tt <- 0.4
      skip <- abs(lay$layer[j] - lay$layer[i]) >= 2
      dx <- x[j] - x[i]
      dy <- y[j] - y[i]
      d <- sqrt(dx^2 + dy^2)
      side <- ifelse(dx >= 0, 1, -1)
      off <- ifelse(skip, 0.45 * abs(lay$layer[j] - lay$layer[i]) * side, 0)
      cx <- (x[i] + x[j]) / 2 + off * (-dy / d)
      cy <- (y[i] + y[j]) / 2 + off * (dx / d)
      mx <- (1 - tt)^2 * x[i] + 2 * (1 - tt) * tt * cx + tt^2 * x[j]
      my <- (1 - tt)^2 * y[i] + 2 * (1 - tt) * tt * cy + tt^2 * y[j]
      lcex <- 0.7 * cex
      w <- strwidth(lab, cex = lcex) / 2 + 0.02
      h <- strheight("X", cex = lcex) / 2 + 0.02
      rect(mx - w, my - h, mx + w, my + h, col = "white", border = NA)
      text(mx, my, lab, cex = lcex, col = "gray20")
    }
  }
  return(invisible(lay))
}
