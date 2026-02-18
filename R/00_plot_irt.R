# IRT / GRM プロット関数
# plot.exametrika() から呼び出される内部関数群

#' IRT モデルのプロット（IRF / TRF / IIF / TIF）
#' @noRd
plot_irt_model <- function(x, type, plotItemID, nc, nr, overlay, colors) {
  testlength <- x$testlength

  # rename type option
  type <- switch(type,
    "ICC" = "IRF",
    "IIC" = "IIF",
    "TIC" = "TIF",
    type
  )
  if ((type %in% c("IIF", "TIF") && any(plotItemID == 0))) {
    type <- "TIF"
    plotItemID <- 1:testlength
  } else if (type == "IRF" && any(plotItemID == 0)) {
    type <- "TRF"
    plotItemID <- 1:testlength
  }
  if (length(plotItemID) == 0) {
    plotItemID <- 1:testlength
  }

  ### IRT curve function
  plotIRTCurve <- function(params, curveFunc, titleBase, ylab, overlay) {
    if (overlay) {
      setup_legend_layout(1, nc)
      plot(NULL,
        xlim = c(-4, 4),
        ylim = c(0, 1),
        xlab = "ability",
        ylab = ylab,
        main = titleBase
      )
      for (i in 1:nrow(params)) {
        a <- params[i, 1]
        b <- params[i, 2]
        c <- if (x$model > 2) params[i, 3] else 0
        d <- if (x$model > 3) params[i, 4] else 1
        curve(curveFunc(a, b, c, d, theta = x),
          from = -4,
          to = 4,
          add = TRUE,
          lty = i, # Different line types
          col = i, # Different colors
          lwd = 2
        )
      }
      draw_legend_strip(
        legend = paste("Item", plotItemID),
        lty = 1:nrow(params),
        col = 1:nrow(params),
        lwd = 2, bty = "n",
        ncol = min(nrow(params), 5)
      )
    } else {
      for (i in 1:nrow(params)) {
        a <- params[i, 1]
        b <- params[i, 2]
        c <- if (x$model > 2) params[i, 3] else 0
        d <- if (x$model > 3) params[i, 4] else 1
        title <- paste0(titleBase, ", item ", plotItemID[i])
        curve(curveFunc(a, b, c, d, theta = x),
          from = -4,
          to = 4,
          xlab = "ability", ylab = ylab,
          main = title
        )
      }
    }
  }

  params <- x$params[plotItemID, , drop = FALSE]
  if (type == "IRF") {
    plotIRTCurve(
      params, exametrika::LogisticModel,
      "Item Response Function", "probability",
      overlay
    )
  }
  if (type == "TRF") {
    curve(exametrika::TestResponseFunc(params, theta = x),
      from = -4,
      to = 4,
      xlab = "ability", ylab = "probability",
      main = "Test Response Function"
    )
  }
  if (type == "IIF") {
    plotIRTCurve(
      params, exametrika::ItemInformationFunc,
      "Item Information Function", "information",
      overlay
    )
  }
  if (type == "TIF") {
    curve(exametrika::TestInformationFunc(params, theta = x),
      from = -4,
      to = 4,
      xlab = "ability", ylab = "Information",
      main = "Test Informaiton Function"
    )
  }
}

#' GRM モデルのプロット（IRF / IIF / TIF）
#' @noRd
plot_grm_model <- function(x, type, plotItemID, nc, nr, colors) {
  testlength <- x$testlength

  # rename type option
  type <- switch(type,
    "ICC" = "IRF",
    "IIC" = "IIF",
    "TIC" = "TIF",
    type
  )
  if ((type %in% c("IIF", "TIF") && any(plotItemID == 0))) {
    type <- "TIF"
    plotItemID <- 1:testlength
  } else if (type == "IRF" && any(plotItemID == 0)) {
    type <- "TRF"
    plotItemID <- 1:testlength
  }
  if (length(plotItemID) == 0) {
    plotItemID <- 1:testlength
  }

  ### GRM curve function
  grm_IRF <- function(a, b, title) {
    thetas <- seq(-4, 4, 0.01)
    n_theta <- length(thetas)
    K <- length(b) + 1
    grm_colors <- if (!is.null(colors)) {
      colors[-1]
    } else {
      c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8B4513")
    }
    probs <- matrix(0, nrow = n_theta, ncol = K)
    for (i in 1:n_theta) {
      probs[i, ] <- grm_prob(thetas[i], a, b)
    }
    plot(thetas, probs[, 1],
      type = "l",
      col = grm_colors[1],
      ylim = c(0, 1),
      xlab = expression(theta),
      main = title
    )
    for (k in 2:K) {
      lines(thetas, probs[, k], col = grm_colors[k])
    }
  }

  grm_IIC <- function(a, b, title) {
    thetas <- seq(-4, 4, 0.01)
    I <- sapply(thetas, function(theta) grm_iif(theta, a, b))

    plot(thetas, I,
      type = "l",
      xlab = expression(theta),
      ylab = "Item Information",
      main = title
    )
  }

  params <- x$params[plotItemID, , drop = FALSE]
  if (type == "IRF") {
    setup_legend_layout(nrow(params), nc)
    max_K <- 0
    for (j in 1:nrow(params)) {
      a <- params[j, 1]
      b <- params[j, -1]
      b <- b[!is.na(b)]
      K <- length(b) + 1
      if (K > max_K) max_K <- K
      title <- paste("Item Category Response Function for item", plotItemID[j])
      grm_IRF(a, b, title)
    }
    grm_colors <- if (!is.null(colors)) {
      colors[-1]
    } else {
      c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8B4513")
    }
    draw_legend_strip(
      legend = paste("Category", 1:max_K),
      col = grm_colors[1:max_K], lty = 1,
      cex = 1.0, bty = "n", ncol = min(max_K, 5)
    )
  }
  if (type == "IIF") {
    for (j in 1:nrow(params)) {
      a <- params[j, 1]
      b <- params[j, -1]
      b <- b[!is.na(b)]
      title <- paste("Item Information Function for item", plotItemID[j])
      grm_IIC(a, b, title)
    }
  }
  if (type == "TIF") {
    thetas <- seq(-4, 4, 0.01)
    I <- matrix(nrow = testlength, ncol = length(thetas))
    for (j in 1:NROW(params)) {
      a <- params[j, 1]
      b <- params[j, -1]
      b <- b[!is.na(b)]
      I[j, ] <- sapply(thetas, function(theta) grm_iif(theta, a, b))
    }
    TestInfo <- colSums(I, na.rm = T)
    plot(thetas, TestInfo,
      type = "l",
      xlab = expression(theta),
      ylab = "Information",
      main = "Test Information Function"
    )
  }
}
