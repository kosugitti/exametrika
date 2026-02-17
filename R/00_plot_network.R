# ネットワークモデル プロット関数（LDLRA / LDB / BINET）
# plot.exametrika() から呼び出される内部関数群

#' FieldPIRP プロット（LDB 専用）
#' @noRd
plot_field_pirp <- function(x) {
  target <- x$IRP
  ## rank x field x nrs
  Nrank <- dim(target)[1]
  nfld <- dim(target)[2]
  nrs <- dim(target)[3]
  for (i in 1:Nrank) {
    mat <- target[i, , ]
    mat[mat == 0] <- NA
    xvals <- 0:nrs
    plot(xvals,
      y = runif(length(xvals)), type = "n",
      xlim = c(0, nrs), ylim = c(0, 1),
      xlab = "PIRP(Number-Right Score) in Parent Field(s)",
      ylab = "Correct Response Rate"
    )
    for (j in 1:NROW(mat)) {
      y <- as.vector(na.omit(mat[j, ]))
      xvals_j <- seq(0, nrs)[1:length(y)]
      labels <- rep(as.character(j), length(y))
      lines(xvals_j, y, type = "l", lwd = 2)
      text(xvals_j, y, labels = labels, pos = 1)
    }
    title(main = paste("Rank", i))
  }
}

#' LDPSR プロット（BINET 専用）
#' @noRd
plot_ldpsr <- function(x) {
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
