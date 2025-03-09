#' @title Simple Test Statistics
#' @description
#' Calculates descriptive statistics for test scores, providing a comprehensive
#' summary of central tendency, variability, and distribution shape.
#' Different statistics are calculated based on the data type (binary, ordinal, rated, or nominal).
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#'
#' @return
#' The returned object depends on the data type:
#'
#' For binary data, a list of class c("exametrika", "TestStatistics") containing:
#' \describe{
#' \item{TestLength}{Length of the test. The number of items included in the test.}
#' \item{SampleSize}{Sample size. The number of rows in the dataset.}
#' \item{Mean}{Average number of correct answers.}
#' \item{SEofMean}{Standard error of mean.}
#' \item{Variance}{Variance of test scores.}
#' \item{SD}{Standard Deviation of test scores.}
#' \item{Skewness}{Skewness of score distribution (measure of asymmetry).}
#' \item{Kurtosis}{Kurtosis of score distribution (measure of tail extremity).}
#' \item{Min}{Minimum score.}
#' \item{Max}{Maximum score.}
#' \item{Range}{Range of scores (Max - Min).}
#' \item{Q1}{First quartile. Same as the 25th percentile.}
#' \item{Median}{Median. Same as the 50th percentile.}
#' \item{Q3}{Third quartile. Same as the 75th percentile.}
#' \item{IQR}{Interquartile range. Calculated by subtracting Q1 from Q3.}
#' \item{Stanine}{Stanine score boundaries, see \code{\link{stanine}}.}
#' }
#'
#' For ordinal and rated data, the function calls \code{\link{ScoreReport}} and returns
#' its result. See \code{\link{ScoreReport}} for details of the returned object.
#'
#' For nominal data, an error is returned as this function does not support nominal data.
#'
#' @importFrom stats sd var quantile
#' @examples
#' # Basic usage
#' stats <- TestStatistics(J15S500)
#' print(stats)
#'
#' # Extract specific statistics
#' cat("Mean score:", stats$Mean, "\n")
#' cat("Standard deviation:", stats$SD, "\n")
#'
#' # View score distribution summary
#' summary_stats <- data.frame(
#'   Min = stats$Min,
#'   Q1 = stats$Q1,
#'   Median = stats$Median,
#'   Mean = stats$Mean,
#'   Q3 = stats$Q3,
#'   Max = stats$Max
#' )
#' print(summary_stats)
#' @export
#'
TestStatistics <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("TestStatistics")
}

#' @rdname TestStatistics
#' @export
TestStatistics.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = TestStatistics.binary(U, na, Z, w),
      "rated" = TestStatistics.ordinal(U, na, Z, w),
      "ordinal" = TestStatistics.ordinal(U, na, Z, w),
      "nominal" = response_type_error(U$response.type, "TestStatistics")
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    TestStatistics(U)
  }
}

#' @rdname TestStatistics
#' @export
TestStatistics.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  tW <- nrs(tmp)
  TestLength <- NCOL(tmp$Z)
  SampleSize <- NROW(tmp$Z)
  Mean <- mean(tW)
  SEofMean <- sd(tW) / sqrt(SampleSize)
  Variance <- var(tW)
  SD <- sd(tW)
  SDs <- sqrt(mean((tW - Mean)^2))
  tmpZ <- (tW - Mean) / SDs
  Skewness <- mean(tmpZ^3)
  Kurtosis <- mean(tmpZ^4) - 3
  Min <- min(tW)
  Max <- max(tW)
  Range <- Max - Min
  Q1 <- quantile(tW, probs = 0.25, na.rm = TRUE)
  Median <- quantile(tW, probs = 0.5, na.rm = TRUE)
  Q3 <- quantile(tW, probs = 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  Stanine <- stanine(tmp)
  ret <-
    structure(list(
      TestLength = TestLength,
      SampleSize = SampleSize,
      Mean = Mean,
      SEofMean = SEofMean,
      Variance = Variance,
      SD = SD,
      Skewness = Skewness,
      Kurtosis = Kurtosis,
      Min = Min,
      Max = Max,
      Range = Range,
      Q1 = Q1,
      Median = Median,
      Q3 = Q3,
      IQR = IQR,
      Stanine = Stanine$stanine
    ), class = c("exametrika", "TestStatistics"))
  return(ret)
}

#' @rdname TestStatistics
#' @export
TestStatistics.ordinal <- function(U, na = NULL, Z = NULL, w = NULL) {
  # ordinal / rated common functions
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  ScoreReport(tmp)
}
