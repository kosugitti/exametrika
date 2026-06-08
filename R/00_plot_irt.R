# IRT / GRM plot functions
# Internal functions called from plot.exametrika()

#' Plot IRT model (IRF / TRF / IIF / TIF)
#' @noRd
plot_irt_model <- function(x, type, plotItemID, nc, nr, overlay, colors, dots = list()) {
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
      call_plot(
        plot,
        list(
          x = NULL,
          xlim = c(-4, 4),
          ylim = c(0, 1),
          xlab = "ability",
          ylab = ylab,
          main = titleBase
        ),
        dots
      )
      for (i in 1:nrow(params)) {
        a <- params[i, 1]
        b <- params[i, 2]
        c <- if (x$model > 2) params[i, 3] else 0
        d <- if (x$model > 3) params[i, 4] else 1
        draw_curve(
          function(theta) curveFunc(a, b, c, d, theta = theta),
          from = -4, to = 4, add = TRUE,
          defaults = list(lty = i, col = i, lwd = 2),
          dots = dots
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
        draw_curve(
          function(theta) curveFunc(a, b, c, d, theta = theta),
          from = -4, to = 4,
          defaults = list(xlab = "ability", ylab = ylab, main = title),
          dots = dots
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
    draw_curve(
      function(theta) exametrika::TestResponseFunc(params, theta = theta),
      from = -4, to = 4,
      defaults = list(xlab = "ability", ylab = "probability", main = "Test Response Function"),
      dots = dots
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
    draw_curve(
      function(theta) exametrika::TestInformationFunc(params, theta = theta),
      from = -4, to = 4,
      defaults = list(xlab = "ability", ylab = "Information", main = "Test Information Function"),
      dots = dots
    )
  }
}

#' Plot GRM model (IRF / IIF / TIF)
#' @noRd
plot_grm_model <- function(x, type, plotItemID, nc, nr, colors, dots = list()) {
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
    call_plot(
      plot,
      list(
        x = thetas, y = probs[, 1],
        type = "l",
        col = grm_colors[1],
        ylim = c(0, 1),
        xlab = expression(theta),
        main = title
      ),
      dots
    )
    for (k in 2:K) {
      call_plot(graphics::lines, list(x = thetas, y = probs[, k], col = grm_colors[k]), dots)
    }
  }

  grm_IIC <- function(a, b, title) {
    thetas <- seq(-4, 4, 0.01)
    I <- sapply(thetas, function(theta) grm_iif(theta, a, b))

    call_plot(
      plot,
      list(
        x = thetas, y = I,
        type = "l",
        xlab = expression(theta),
        ylab = "Item Information",
        main = title
      ),
      dots
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
    call_plot(
      plot,
      list(
        x = thetas, y = TestInfo,
        type = "l",
        xlab = expression(theta),
        ylab = "Information",
        main = "Test Information Function"
      ),
      dots
    )
  }
}
