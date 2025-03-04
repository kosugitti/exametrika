#' @title Inter-Item Analysis
#' @description
#' Inter-Item Analysis returns various metrics for analyzing relationships between pairs of items.
#' This function is applicable only to binary response data. The following metrics are calculated:
#' \itemize{
#'   \item JSS: Joint Sample Size
#'   \item JCRR: Joint Correct Response Rate
#'   \item CCRR: conditional Correct Response Rate
#'   \item IL: Item Lift
#'   \item MI: Mutual Information
#'   \item Phi: Phi Coefficient
#'   \item Tetrachoric: Tetrachoric Correlation
#' }
#' Each metric is returned in matrix form where element (i,j) represents the relationship
#' between items i and j.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A list of class "exametrika" and "IIAnalysis" containing the following matrices:
#' \describe{
#'   \item{JSS}{Joint Sample Size matrix}
#'   \item{JCRR}{Joint Correct Response Rate matrix}
#'   \item{CCRR}{conditonal Correct Response Rate matrix}
#'   \item{IL}{Item Lift matrix}
#'   \item{MI}{Mutual Information matrix}
#'   \item{Phi}{Phi Coefficient matrix}
#'   \item{Tetrachoric}{Tetrachoric Correlation matrix}
#' }
#' @examples
#' \donttest{
#' # example code
#' InterItemAnalysis(J15S500)
#' }
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
