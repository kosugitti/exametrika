#' @title Plot Method for Objects of Class "exametrika"
#' @description
#' Creates visualizations for objects with class "exametrika".
#' The calculation results of the exametrika package have an exametrika class attribute,
#' along with the specific analysis model class (IRT, GRM, LCA, LRA, Biclustering, Biclustering_IRM, LDLRA, LDB,
#' BINET). Each model has its own compatible plot types, accessible by specifying the 'type' parameter.
#'
#' @param x An object of class "exametrika"
#' @param type Character string specifying the plot type. Available types vary by model:
#'   \describe{
#'     \item{IRF, ICC}{Item Response Function. Also known as 'ICC' (Item Characteristic Curve).}
#'     \item{TRF}{Test Response Function.}
#'     \item{IIF, IIC}{Item Information Function. Also known as 'IIC' (Item Information Curve).}
#'     \item{TIF, TIC}{Test Information Function. Also known as 'TIC' (Test Information Curve).}
#'     \item{IRP}{Item Reference Profile. Line graph with items and latent classes/ranks
#'       on the horizontal axis, and membership probability on the vertical axis.}
#'     \item{CRV, RRV}{Class/Rank Reference Vector. Plots correct answer rates for each class or rank,
#'       with fields on the horizontal axis and correct answer rates on the vertical axis.}
#'     \item{TRP}{Test Reference Profile. Shows latent classes/ranks on the horizontal axis,
#'       displaying members per class/rank as a bar graph and expected test scores as a line graph.}
#'     \item{LCD}{Latent Class Distribution. Displays latent classes on the horizontal axis,
#'       showing members per class as a bar graph and cumulative membership probability as a line.}
#'     \item{LRD}{Latent Rank Distribution. Similar to LCD but with ranks instead of classes on the horizontal axis.}
#'     \item{CMP}{Class Membership Profile. Line graph showing class membership probabilities of students.}
#'     \item{RMP}{Rank Membership Profile. Similar to CMP but with ranks instead of classes.}
#'     \item{ScoreFreq}{Frequency polygon of score distribution with rank thresholds.}
#'     \item{ScoreRank}{Heatmap of score membership probabilities for each rank.}
#'     \item{ICRP}{Visualizes ranks (x-axis) versus category response probabilities (y-axis).}
#'     \item{ICBR}{Visualizes ranks (x-axis) versus cumulative category probabilities (y-axis).}
#'     \item{FRP}{Field Reference Profile. Shows correspondence between fields and latent classes/ranks.}
#'     \item{Array}{Array plot for Biclustering/Ranklustering. Colored matrix cells where darker cells
#'       indicate larger values.}
#'     \item{FieldPIRP}{Shows correct response rates by number of correct answers in parent fields.
#'       Only available for LDB model.}
#'     \item{LDPSR}{Latent Dependence Passing Student Rate. Compares passing rates of parent and child classes.}
#'     \item{FCRP}{Field Category Response Profile. Category probability plot per field for polytomous Biclustering.
#'       Use \code{style} parameter to choose between "line" and "bar" display.}
#'     \item{FCBR}{Field Cumulative Boundary Reference. Boundary probability plot per field
#'       (ordinal Biclustering only).}
#'     \item{ScoreField}{Heatmap of expected scores across fields and latent classes/ranks
#'       for polytomous Biclustering.}
#'   }
#' @param items Numeric vector specifying which items to plot. If NULL, all items are included.
#'   When type is "IIF"/"IIC", specifying 0 will produce a TIF/TIC for the entire test.
#' @param students Numeric vector specifying which students to plot. If NULL, all students are included.
#' @param nc Integer specifying the number of columns for multiple plots. Default is 1.
#' @param nr Integer specifying the number of rows for multiple plots. Default is 1.
#' @param overlay Logical. If TRUE, elements such as IRFs will be overlaid on a single plot. Default is FALSE.
#' @param colors Character vector specifying custom color palette. If NULL, default colorblind-friendly palette is used.
#' For array plots, the first color should be for missing values, followed by response category colors.
#' @param cell_width Numeric value specifying the width of each cell in array plots. Default is 3.
#' @param cell_height Numeric value specifying the height of each cell in array plots. Default is 1.
#' @param filename Character string specifying output filename. If NULL, plot is displayed on screen.
#' Supported formats: png, pdf, jpeg, jpg. Format determined by file extension.
#' @param width Numeric value specifying plot width in pixels (for png/jpeg) or inches (for pdf). Default is 800.
#' @param height Numeric value specifying plot height in pixels (for png/jpeg) or inches (for pdf). Default is 600.
#' @param dpi Numeric value specifying resolution in dots per inch for raster formats (png/jpeg). Default is 300.
#' @param stat Character string specifying the summary statistic for polytomous FRP and RRV plots.
#' One of "mean" (default), "median", or "mode".
#' @param style Character string specifying the display style for FCRP plots.
#' One of "line" (default) or "bar" (stacked bar chart).
#' @param ... Additional graphical parameters passed to the underlying base R
#'   plotting functions. Standard parameters such as \code{pch}, \code{las},
#'   \code{cex}, \code{col}, \code{lty}, and \code{lwd} are forwarded
#'   consistently to all plot types (including manually drawn axes), so they
#'   can be used to customise point symbols, axis-label orientation, text
#'   sizes, and other graphical elements. User-supplied values take precedence
#'   over the package defaults (e.g. \code{xlab}, \code{ylab}, \code{main} can
#'   be overridden).
#'
#' @details
#' Each model class supports specific plot types:
#'
#' \describe{
#'   \item{IRT}{Supports "IRF"/"ICC", "TRF", "IIF"/"IIC", "TIF"/"TIC"}
#'   \item{GRM}{Supports "IRF"/"ICC", "IIF"/"IIC", "TIF"/"TIC"}
#'   \item{LCA}{Supports "IRP", "FRP", "TRP", "LCD", "CMP"}
#'   \item{LRA}{Supports "IRP", "FRP", "TRP", "LRD", "RMP"}
#'   \item{LRAordinal}{Supports "ScoreFreq", "ScoreRank", "ICRP", "ICBR", "RMP"}
#'   \item{LRArated}{Supports "ScoreFreq", "ScoreRank", "ICRP", "RMP"}
#'   \item{Biclustering}{Supports "FRP", "TRP", "LCD", "LRD", "CMP", "RMP", "CRV", "RRV", "Array"}
#'   \item{ordinalBiclustering}{Supports "FRP", "FCRP", "FCBR", "LCD", "LRD", "CMP", "RMP", "Array", "ScoreField", "RRV"}
#'   \item{nominalBiclustering}{Supports "FRP", "FCRP", "LCD", "LRD", "CMP", "Array", "ScoreField", "RRV"}
#'   \item{Biclustering_IRM}{Supports "FRP", "TRP", "Array"}
#'   \item{LDLRA}{Supports "IRP", "TRP", "LRD", "RMP"}
#'   \item{LDB}{Supports "FRP", "TRP", "LRD", "RMP", "Array", "FieldPIRP"}
#'   \item{BINET}{Supports "FRP", "TRP", "LRD", "RMP", "Array", "LDPSR"}
#' }
#'
#' @return
#' Produces visualizations based on the model class and specified type:
#'
#' \describe{
#'   \item{IRT models}{IRF (Item Response Function), TRF (Test Response Function),
#'     IIF (Item Information Function), TIF (Test Information Function)}
#'   \item{LCA/LRA models}{IRP (Item Reference Profile), TRP (Test Reference Profile),
#'     LCD/LRD (Latent Class/Rank Distribution), CMP/RMP (Class/Rank Membership Profile)}
#'   \item{Biclustering/Biclustering_IRM models}{Array plots showing clustering patterns, FRP, TRP, etc.}
#'   \item{LDLRA/LDB/BINET models}{Network and profile plots specific to each model}
#' }
#'
#' @importFrom graphics curve title axis barplot mtext par text lines rect legend abline image layout plot.new
#' @importFrom grDevices gray dev.off jpeg pdf png hcl.colors rainbow
#' @importFrom stats density runif
#' @importFrom utils tail
#'
#' @examples
#' \dontrun{
#' # IRT model example
#' irt_result <- exametrika::IRT(J15S500)
#' plot(irt_result, type = "IRF", items = 1:5)
#' plot(irt_result, type = "TIF")
#'
#' # LCA model example
#' lca_result <- exametrika::LCA(U)
#' plot(lca_result, type = "IRP")
#' plot(lca_result, type = "LCD")
#' }
#'
#' # Array plot with custom output
#' biclustering_result <- exametrika::Biclustering(J35S515)
#' # Custom colors and file output
#' my_colors <- c("#404040", "#E69F00", "#56B4E9", "#009E73", "#F0E442")
#' plot(biclustering_result, type = "Array", colors = my_colors)
#'
#' @export
#'
plot.exametrika <- function(x,
                            type = c(
                              "IRF", "TRF", "IIF", "TIF", "IIC", "ICC", "TIC",
                              "IRP", "TRP", "LCD", "CMP",
                              "FRP", "RMP", "LRD", "Array", "CRV", "RRV",
                              "FCRP", "FCBR", "ScoreField",
                              "FieldPIRP", "LDPSR",
                              "ScoreFreq", "ScoreRank", "ICRP", "ICBR"
                            ),
                            items = NULL,
                            students = NULL,
                            nc = 1,
                            nr = 1,
                            overlay = FALSE,
                            colors = NULL,
                            cell_width = 3,
                            cell_height = 1,
                            filename = NULL,
                            width = 800,
                            height = 600,
                            dpi = 300,
                            stat = "mean",
                            style = "line",
                            ...) {
  value <- if (length(class(x)) > 1) tail(class(x), 1) else "None"

  if (!is.null(filename)) {
    ext <- tolower(tools::file_ext(filename))
    if (ext == "") ext <- "png"
    switch(ext,
      "png" = png(filename, width = width, height = height, res = dpi),
      "pdf" = pdf(filename, width = width / 72, height = height / 72),
      "jpeg" = jpeg(filename, width = width, height = height, res = dpi),
      "jpg" = jpeg(filename, width = width, height = height, res = dpi),
      stop("Supported formats: png, pdf, jpeg, jpg")
    )
  }

  old_par <- par(no.readonly = TRUE)
  on.exit({
    # Safely restore parameters, excluding problematic read-only ones
    restore_par <- old_par
    restore_par[c("pin", "fin", "plt", "usr")] <- NULL
    suppressWarnings(par(restore_par))
    if (!is.null(filename)) dev.off()
  })

  if (missing(type)) {
    stop("The 'type' argument must be specified.")
  }

  # Skip mfrow for plot types that use layout() internally (FCRP, FCBR)
  uses_layout <- type %in% c("FCRP", "FCBR")
  if (!uses_layout) {
    par(mfrow = c(nr, nc))
  }
  testlength <- x$testlength
  nobs <- x$nobs

  valid_types <- list(
    IRT = c("IRF", "TRF", "IIF", "TIF", "IIC", "ICC", "TIC"),
    GRM = c("IRF", "IIF", "TIF", "IIC", "ICC", "TIC"),
    LCA = c("IRP", "TRP", "LCD", "CMP"),
    LRA = c("IRP", "TRP", "LRD", "RMP"),
    LRAordinal = c("ScoreFreq", "ScoreRank", "ICRP", "ICBR", "RMP"),
    LRArated = c("ScoreFreq", "ScoreRank", "ICRP", "RMP"),
    Biclustering = c("FRP", "TRP", "LCD", "LRD", "CMP", "RMP", "CRV", "RRV", "Array"),
    nominalBiclustering = c("FRP", "FCRP", "LCD", "LRD", "CMP", "Array", "ScoreField", "RRV"),
    ratedBiclustering = c("FRP", "FCRP", "LCD", "LRD", "CMP", "RMP", "Array", "ScoreField", "RRV"),
    ordinalBiclustering = c("FRP", "FCRP", "FCBR", "LCD", "LRD", "CMP", "RMP", "Array", "ScoreField", "RRV"),
    IRM = c("FRP", "TRP", "Array"),
    LDLRA = c("IRP", "TRP", "LRD", "RMP"),
    LDB = c("FRP", "TRP", "LRD", "RMP", "Array", "FieldPIRP"),
    BINET = c("FRP", "TRP", "LRD", "RMP", "Array", "LDPSR")
  )

  if (!type %in% valid_types[[value]]) {
    stop(paste("Warning: The type", type, "does not correspond to the value", value))
  }

  plotItemID <- if (!is.null(items)) {
    if (!is.numeric(items) || length(items) > testlength || any(items < 0 | items > testlength)) {
      stop("'items' must be a numeric vector of length at most ", testlength, " and contain numbers between 0 and ", testlength)
    }
    items
  } else {
    1:testlength
  }

  plotStudentID <- if (!is.null(students)) {
    if (!is.numeric(students) || length(students) > nobs || any(students < 0 | students > nobs)) {
      stop("'students' must be a numeric vector of length at most ", nobs, " and contain numbers between 0 and ", nobs)
    }
    students
  } else {
    1:nobs
  }

  # Additional graphical parameters supplied via ... are forwarded to the
  # underlying base graphics calls in every model-specific plot function,
  # so standard parameters (pch, las, cex, col, lty, lwd, ...) work
  # consistently across all plot types.
  dots <- list(...)

  # Dispatch to model-specific plot functions
  switch(value,
    IRT = plot_irt_model(x, type, plotItemID, nc, nr, overlay, colors, dots),
    GRM = plot_grm_model(x, type, plotItemID, nc, nr, colors, dots),
    LCA = ,
    LRA = ,
    LDLRA = plot_common_profiles(x, type, value, plotItemID, plotStudentID, testlength, dots),
    LRAordinal = ,
    LRArated = {
      if (type == "RMP") {
        plot_common_profiles(x, type, value, plotItemID, plotStudentID, testlength, dots)
      } else {
        plot_lra_ordinal(x, type, plotItemID, dots)
      }
    },
    Biclustering = {
      if (type == "Array") {
        plot_array(x, cell_width, cell_height, colors, dots)
      } else {
        plot_common_profiles(x, type, value, plotItemID, plotStudentID, testlength, dots)
      }
    },
    ordinalBiclustering = ,
    ratedBiclustering = ,
    nominalBiclustering = {
      if (type == "Array") {
        plot_array(x, cell_width, cell_height, colors, dots)
      } else if (type %in% c("LCD", "LRD", "CMP", "RMP")) {
        plot_common_profiles(x, type, value, plotItemID, plotStudentID, testlength, dots)
      } else if (type == "FRP") {
        plot_poly_frp(x, stat, nc, nr, dots)
      } else if (type == "FCRP") {
        plot_poly_fcrp(x, style, nc, nr, dots)
      } else if (type == "FCBR") {
        plot_poly_fcbr(x, nc, nr, dots)
      } else if (type == "ScoreField") {
        plot_scorefield(x, dots)
      } else if (type == "RRV") {
        plot_poly_rrv(x, stat, dots)
      }
    },
    IRM = {
      if (type == "Array") {
        plot_array(x, cell_width, cell_height, colors, dots)
      } else {
        plot_common_profiles(x, type, value, plotItemID, plotStudentID, testlength, dots)
      }
    },
    LDB = {
      if (type == "FieldPIRP") {
        plot_field_pirp(x, dots)
      } else if (type == "Array") {
        plot_array(x, cell_width, cell_height, colors, dots)
      } else {
        plot_common_profiles(x, type, value, plotItemID, plotStudentID, testlength, dots)
      }
    },
    BINET = {
      if (type == "LDPSR") {
        plot_ldpsr(x, dots)
      } else if (type == "Array") {
        plot_array(x, cell_width, cell_height, colors, dots)
      } else {
        plot_common_profiles(x, type, value, plotItemID, plotStudentID, testlength, dots)
      }
    },
    none = {
      stop("This object cannot be plotted. The object must be of class 'exametrika' with a valid model type.")
    }
  )
}
