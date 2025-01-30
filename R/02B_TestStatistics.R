#' @title Simple Test Statistics
#' @description
#' Statistics regarding the total score.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return
#' For binary data:
#' \describe{
#' \item{TestLength}{Length of the test. The number of items included in the test.}
#' \item{SampleSize}{Sample size. The number of rows in the dataset.}
#' \item{Mean}{Average number of correct answers.}
#' \item{SEofMean}{Standard error of mean}
#' \item{Variance}{Variance}
#' \item{SD}{Standard Deviation}
#' \item{Skewness}{Skewness}
#' \item{Kurtosis}{Kurtosis}
#' \item{Min}{Minimum score}
#' \item{Max}{Max score}
#' \item{Range}{Range of score}
#' \item{Q1}{First quartile. Same as the 25th percentile.}
#' \item{Median}{Median. Same as the 50th percentile.}
#' \item{Q3}{Third quartile. Same as the 75th percentile.}
#' \item{IQR}{Interquartile range. It is calculated by subtracting the first quartile from the third quartile.}
#' \item{Stanine}{see [stanine]}
#' }
#' For rated polytomous data:
#' \describe{
#' \item{TestLength}{Length of the test. The number of items included in the test.}
#' \item{SampleSize}{Sample size. The number of rows in the dataset.}
#' \item{Mean}{Average number of correct answers.}
#' \item{SEofMean}{Standard error of mean}
#' \item{Variance}{Variance}
#' \item{SD}{Standard Deviation}
#' \item{Skewness}{Skewness}
#' \item{Kurtosis}{Kurtosis}
#' \item{Min}{Minimum score}
#' \item{Max}{Max score}
#' \item{Range}{Range of score}
#' #' }
#' For ordinal polytomous data:
#' \describe{
#' \item{TestLength}{Length of the test. The number of items included in the test.}
#' \item{SampleSize}{Sample size. The number of rows in the dataset.}
#' \item{Mean}{Average number of correct answers.}
#' \item{SEofMean}{Standard error of mean}
#' \item{Variance}{Variance}
#' \item{SD}{Standard Deviation}
#' \item{Skewness}{Skewness}
#' \item{Kurtosis}{Kurtosis}
#' \item{Min}{Minimum score}
#' \item{Max}{Max score}
#' \item{Range}{Range of score}
#' }
#' For nominal polytomous data:
#' \describe{
#' \item{Response Data}{Same as binary data, with scores representing response levels}
#' }
#' @importFrom stats sd var
#' @examples
#' # using sample dataset
#' TestStatistics(J15S500)
#' @export
TestStatistics <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("TestStatistics")
}
#' @export
TestStatistics.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = TestStatistics.binary(U, na, Z, w),
      "rated" = TestStatistics.ordinal(U, na, Z, w),
      "ordinal" = TestStatistics.ordinal(U, na, Z, w),
      "nominal" = TestStatistics.nominal(U, na, Z, w)
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    TestStatistics(U)
  }
}

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


#' @export
TestStatistics.ordinal <- function(U, na = NULL, Z = NULL, w = NULL) {
  # ordinal / rated common functions
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  scorereport <- ScoreReport(tmp)
}

#' @export
TestStatistics.nominal <- function(U, na = NULL, Z = NULL, w = NULL) {
  # nominal implementation
}
