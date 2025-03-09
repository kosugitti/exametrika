#' @title Inter-Item Analysis for Psychometric Data
#' @description
#' Calculates various relationship metrics between pairs of items in test data. This analysis
#' helps identify item interdependencies, content overlaps, and potential local dependence.
#' For binary data, metrics include joint response rates, conditional probabilities,
#' and several correlation measures. For ordinal/rated data, appropriate correlation
#' measures are calculated.
#'
#' The following metrics are calculated for binary data:
#' \itemize{
#'   \item JSS: Joint Sample Size - number of examinees responding to both items
#'   \item JCRR: Joint Correct Response Rate - proportion of examinees answering both items correctly
#'   \item CCRR: Conditional Correct Response Rate - probability of answering one item correctly
#'     given a correct response to another item
#'   \item IL: Item Lift - ratio of joint correct response rate to the product of marginal rates
#'   \item MI: Mutual Information - measure of mutual dependence between items
#'   \item Phi: Phi Coefficient - correlation coefficient for binary variables
#'   \item Tetrachoric: Tetrachoric Correlation - estimate of Pearson correlation for underlying
#'     continuous variables
#' }
#'
#' For ordinal/rated data, the function calculates:
#' \itemize{
#'   \item JSS: Joint Sample Size
#'   \item JSR: Joint Selection Rate
#'   \item CSR: Conditional Selection Rate
#'   \item MI: Mutual Information
#'   \item Polychoric: Polychoric Correlation - extension of tetrachoric correlation for ordinal data
#' }
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#'
#' @return
#' For binary data, an object of class "exametrika" and "IIAnalysis" containing:
#' \describe{
#'   \item{JSS}{Joint Sample Size matrix - N(i,j) shows number of examinees who responded to both items i and j}
#'   \item{JCRR}{Joint Correct Response Rate matrix - P(Xi=1, Xj=1) shows probability of correct responses to both items}
#'   \item{CCRR}{Conditional Correct Response Rate matrix - P(Xi=1|Xj=1) shows probability of correct response to item i
#'     given correct response to item j}
#'   \item{IL}{Item Lift matrix - P(Xi=1, Xj=1)/(P(Xi=1)*P(Xj=1)) measures association strength}
#'   \item{MI}{Mutual Information matrix - measures information shared between items}
#'   \item{Phi}{Phi Coefficient matrix - correlation coefficient between binary variables}
#'   \item{Tetrachoric}{Tetrachoric Correlation matrix - correlation between underlying continuous variables}
#' }
#'
#' For ordinal/rated data, an object of class "exametrika" and "IIAnalysis.ordinal" containing:
#' \describe{
#'   \item{JSS}{Joint Sample Size matrix}
#'   \item{JSR}{Joint Selection Rate matrix - frequencies of joint category selections}
#'   \item{CSR}{Conditional Selection Rate matrix - probabilities of response categories conditional on other items}
#'   \item{MI}{Mutual Information matrix}
#'   \item{Polychoric}{Polychoric Correlation matrix - correlations between underlying continuous variables}
#' }
#'
#' @details
#' This function automatically detects the data type and applies appropriate analysis methods:
#' \itemize{
#'   \item For binary data: Calculates tetrachoric correlations and related statistics
#'   \item For ordinal/rated data: Calculates polychoric correlations and related statistics
#'   \item For nominal data: Returns an error (not supported)
#' }
#'
#' Inter-item analysis is useful for:
#' \itemize{
#'   \item Identifying groups of highly related items
#'   \item Detecting local dependence between items
#'   \item Evaluating test dimensionality
#'   \item Informing item selection and test construction
#' }
#'
#' @examples
#' \donttest{
#' # Basic usage with binary data
#' ii_analysis <- InterItemAnalysis(J15S500)
#'
#' # View joint sample sizes
#' head(ii_analysis$JSS)
#'
#' # View tetrachoric correlations
#' head(ii_analysis$Tetrachoric)
#'
#' # Find pairs of items with high mutual information (potential local dependence)
#' high_MI <- which(ii_analysis$MI > 0.2 & upper.tri(ii_analysis$MI), arr.ind = TRUE)
#' if (nrow(high_MI) > 0) {
#'   print("Item pairs with high mutual information:")
#'   print(high_MI)
#' }
#'
#' # Example with ordinal data
#' ordinal_analysis <- InterItemAnalysis(J15S3810)
#'
#' # View polychoric correlations for ordinal data
#' head(ordinal_analysis$Polychoric)
#' }
#'
#' @seealso \code{\link{dataFormat}} for data preparation, \code{\link{CTT}} for
#' Classical Test Theory analysis
#'
#' @export

InterItemAnalysis <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("InterItemAnalysis")
}

#' @export
InterItemAnalysis.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = InterItemAnalysis.binary(U, na, Z, w),
      "rated" = InterItemAnalysis.ordinal(U, na, Z, w),
      "ordinal" = InterItemAnalysis.ordinal(U, na, Z, w),
      "nominal" = response_type_error(U$response.type, "InterItemAnalysis")
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    InterItemAnalysis(U)
  }
}
#' @export
InterItemAnalysis.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Calculate all matrices
  JSS <- JointSampleSize(U)
  JCRR <- JCRR(U)
  CCRR <- CCRR(U)
  IL <- ItemLift(U)
  MI <- MutualInformation(U)
  Phi <- PhiCoefficient(U)
  Tet <- TetrachoricCorrelationMatrix(U)

  # Create return structure
  structure(
    list(
      JSS = JSS,
      JCRR = JCRR,
      CCRR = CCRR,
      IL = IL,
      MI = MI,
      Phi = Phi,
      Tetrachoric = Tet
    ),
    class = c("exametrika", "IIAnalysis")
  )
}

#' @export
InterItemAnalysis.ordinal <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  ret <- structure(
    list(
      JSS = JointSampleSize(tmp),
      JSR = JSR(tmp),
      CSR = CSR(tmp),
      MI = MutualInformation(tmp),
      Polychoric = PolychoricCorrelationMatrix(tmp)
    ),
    class = c("exametrika", "IIAnalysis.ordinal")
  )
  return(ret)
}
