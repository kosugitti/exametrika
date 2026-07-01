#' @title Print Method for Exametrika Objects
#' @description
#' S3 method for printing objects of class "exametrika". This function formats and displays
#' appropriate summary information based on the specific subclass of the exametrika object.
#' Different types of analysis results (IRT, LCA, network models, etc.) are presented
#' with customized formatting to highlight the most relevant information.
#'
#' @param x An object of class "exametrika" with various possible subclasses
#' @param digits Integer indicating the number of decimal places to display. Default is 3.
#' @param ... Additional arguments passed to print methods (not currently used)
#'
#' @details
#' The function identifies the specific subclass of the exametrika object and tailors the
#' output accordingly. For most analysis types, the function displays:
#'
#' * Basic model description and parameters
#' * Estimation results (e.g., item parameters, latent class profiles)
#' * Model fit statistics and diagnostics
#' * Visual representations where appropriate (e.g., graphs for network models, scree plots
#'   for dimensionality analysis)
#'
#' When printing network-based models (LDLRA, LDB, BINET), this function visualizes
#' the network structure using graphs, which can help in interpreting complex relationships
#' between items or latent variables.
#'
#' @return
#' Prints a formatted summary of the exametrika object to the console, with content
#' varying by object subclass:
#'
#' \describe{
#'   \item{TestStatistics}{Basic descriptive statistics of the test}
#'   \item{Dimensionality}{Eigenvalue analysis results with scree plot}
#'   \item{ItemStatistics}{Item-level statistics and psychometric properties}
#'   \item{QitemStatistics}{Item statistics for polytomous items}
#'   \item{exametrikaData}{Data structure details including response patterns and weights}
#'   \item{IIAnalysis}{Item-item relationship measures (tetrachoric correlations, etc.)}
#'   \item{CTT}{Classical Test Theory reliability measures}
#'   \item{IRT/GRM}{Item parameters, ability estimates, and fit indices}
#'   \item{LCA/LRA}{Class/Rank profiles, distribution information, and model fit statistics}
#'   \item{Biclustering/Biclustering_IRM}{Cluster profiles, field distributions, and model diagnostics}
#'   \item{LDLRA/LDB/BINET}{Network visualizations, parameter estimates, and conditional probabilities}
#' }
#'
#' @examples
#' \donttest{
#' # Print IRT analysis results with 4 decimal places
#' result <- IRT(J15S500)
#' print(result, digits = 4)
#'
#' # Print Latent Class Analysis results
#' result_lca <- LCA(J15S500, ncls = 3)
#' print(result_lca)
#' }
#'
#' @importFrom utils tail
#' @importFrom igraph plot.igraph layout_on_grid layout_with_fr E
#' @export
#'

print.exametrika <- function(x, digits = 3, ...) {
  value <- if (length(class(x)) > 1) tail(class(x), 1) else "all"

  switch(value,
    TestStatistics = print_test_statistics_case(x, digits),
    Dimensionality = print_dimensionality_case(x, digits),
    ItemStatistics = print_item_statistics_case(x, digits),
    QitemStatistics = print_qitem_statistics_case(x, digits),
    exametrikaData = print_exametrika_data_case(x, digits),
    IIAnalysis = print_ii_analysis_case(x, digits),
    IIAnalysis.ordinal = print_ii_analysis_ordinal_case(x, digits),
    CTT = print_ctt_case(x, digits),
    IRT = print_irt_case(x, digits),
    GRM = print_grm_case(x, digits),
    LCA = print_lca_case(x, digits),
    LRA = print_lra_case(x, digits),
    LRAordinal = print_lra_ordinal_case(x, digits),
    LRArated = print_lra_rated_case(x, digits),
    Biclustering = print_biclustering_case(x, digits),
    ratedBiclustering = print_rated_biclustering_case(x, digits),
    nominalBiclustering = print_nominal_biclustering_case(x, digits),
    ordinalBiclustering = print_ordinal_biclustering_case(x, digits),
    IRM = print_irm_case(x, digits),
    BNM = print_bnm_case(x, digits),
    LDLRA = print_ldlra_case(x, digits),
    LDB = print_ldb_case(x, digits),
    BINET = print_binet_case(x, digits),
    ModelFit = {
      tmp <- data.frame(unclass(x))
      print(tmp)
    },
    Glasso = {
      cat("Graphical Lasso\n")
      cat(sprintf("Optimal lambda: %.4f\n", x$lambda_opt))
      cat(sprintf("EBIC (gamma = %.2f): %.2f\n", x$gamma, x$ebic_opt))
      cat(sprintf("Number of edges: %d\n", x$n_edge))
      cat("\nPrecision matrix:\n")
      print(round(x$theta, digits))
    },
    matrix = {
      class(x) <- "matrix"
      print(x, digits = digits)
    },
    GridSearch = {
      cat("Grid Search Results\n")
      if (!is.null(x$index_matrix)) {
        cat("\nFit index by (ncls, nfld):\n")
        print(round(x$index_matrix, digits))
        cat(sprintf("\nOptimal: ncls = %d, nfld = %d\n", x$optimal_ncls, x$optimal_nfld))
      } else if (!is.null(x$index_vec)) {
        cat("\nFit index by ncls:\n")
        print(round(x$index_vec, digits))
        cat(sprintf("\nOptimal: ncls = %d\n", x$optimal_ncls))
      }
      if (length(x$failed_settings) > 0) {
        cat(sprintf("\n%d setting(s) failed to converge.\n", length(x$failed_settings)))
      }
      cat("\nUse `$optimal_result` to access the fitted model at the optimal setting.\n")
    },
    all = {
      class(x) <- "list"
      print(x, digits = digits)
    }
  )
}
