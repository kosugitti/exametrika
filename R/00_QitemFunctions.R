#' Generate category labels for response data
#'
#' @param data_column A vector containing response data for a single item
#' @param item_name Character string of the item name
#' @return A character vector of category labels
#' @details
#' If the input is a factor, returns its levels.
#' Otherwise, generates labels in the format "Item name-Category-N"
#' @keywords internal
generate_category_labels <- function(data_column, item_name) {
  # For factor type data, preserve existing levels
  if (is.factor(data_column)) {
    return(levels(data_column))
  }

  # Get unique values excluding missing values (-1)
  unique_values <- sort(unique(data_column[data_column != -1]))
  n_categories <- length(unique_values)

  # Generate automatic category names
  paste0(item_name, "-Cat", seq_len(n_categories))
}

#' @title Score Report for non-binary data
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return
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
#' \item{Alpha}{Cronbach's alpha coefficient, a measure of internal consistency reliability. }
#' @importFrom stats sd median
#' @export
ScoreReport <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    dat <- dataFormat(U, na = na, Z = Z, w = w)
  } else {
    dat <- U
  }
  if (U$response.type == "binary") {
    response_type_error(dat$response.type, "ScoreReport")
  }
  if (U$response.type == "rated") {
    dat$Q <- U$U
  }

  nobs <- nrow(dat$Q)
  nitems <- ncol(dat$Q)
  ## score dist
  score <- rowSums(dat$Z * dat$Q)
  score.max <- max(score)
  score.median <- median(score)
  score.mean <- mean(score)
  score.min <- min(score)
  score.sd <- sd(score)
  score.range <- score.max - score.min

  m3 <- sum((score - mean(score))^3) / nobs
  s3 <- (sum((score - mean(score))^2) / nobs)^(3 / 2)
  score.skew <- m3 / s3
  m4 <- sum((score - mean(score))^4) / nobs
  s4 <- (sum((score - mean(score))^2) / nobs)^2
  score.kurt <- m4 / s4 - 3

  ##
  zzzTotal <- apply(dat$Z, 2, sum)
  zzzMean <- apply(dat$Z, 2, mean)
  itemMean <- apply(dat$Z * dat$Q, 2, sum) / zzzTotal
  itemSD <- sqrt(apply((dat$Z * dat$Q^2), 2, sum) / zzzTotal - itemMean^2)
  zzzScoreMean <- colSums(dat$Z * score) / colSums(dat$Z)
  zzzScoreSD <- sqrt(colSums(dat$Z * score^2) / colSums(dat$Z) - zzzScoreMean^2)
  cronbach01 <- (1 - (sum(itemSD^2) / var(score))) * nitems / (nitems - 1)

  ret <-
    structure(list(
      SampleSize = nobs,
      TestLength = nitems,
      Median = score.median,
      Max = score.max,
      Min = score.min,
      Range = score.range,
      Mean = score.mean,
      SD = score.sd,
      Skewness = score.skew,
      Kurtosis = score.kurt,
      Alpha = cronbach01
    ), class = c("exametrika", "TestStatistics"))
  return(ret)
}

#' @title Item Report for non-binary data
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return
#' \item{Obs}{Number of valid responses for each item}
#' \item{ObsRatio}{Proportion of valid responses for each item (range: 0-1)}
#' \item{ItemMean}{Mean score of each item}
#' \item{ItemSD}{Standard deviation of each item score}
#' \item{ItemCORR}{Item-total correlation coefficients.
#' Correlation between item scores and total test scores}
#' \item{ItemCORR_R}{Corrected item-total correlation coefficients.
#' Correlation between item scores and total test scores excluding the target item}
#' @export
ItemReport <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    dat <- dataFormat(U, na = na, Z = Z, w = w)
  } else {
    dat <- U
  }
  if (U$response.type == "binary") {
    response_type_error(dat$response.type, "ScoreReport")
  }
  score <- rowSums(dat$Z * dat$Q)
  maxscore <- sum(apply(dat$Q, 2, max))
  score.dist <- table(factor(score, levels = 0:maxscore))

  zzzTotal <- apply(dat$Z, 2, sum)
  zzzMean <- apply(dat$Z, 2, mean)
  itemMean <- apply(dat$Z * dat$Q, 2, sum) / zzzTotal
  itemSD <- sqrt(apply((dat$Z * dat$Q^2), 2, sum) / zzzTotal - itemMean^2)
  zzzScoreMean <- colSums(dat$Z * score) / colSums(dat$Z)
  zzzScoreSD <- sqrt(colSums(dat$Z * score^2) / colSums(dat$Z) - zzzScoreMean^2)
  itemScoreCorr <- (colSums(dat$Z * dat$Q * score) / colSums(dat$Z) - itemMean * zzzScoreMean) / (zzzScoreSD * itemSD)
  scoreRemainder <- matrix(0, nrow = nrow(dat$Q), ncol = ncol(dat$Q))
  for (j in 1:ncol(dat$Q)) {
    scoreRemainder[, j] <- dat$Z[, j] * (score - dat$Q[, j])
  }
  zzzScoreReminderMean <- colSums(scoreRemainder) / zzzTotal
  zzzScoreRemainderSD <- sqrt(colSums(scoreRemainder^2) / zzzTotal - zzzScoreReminderMean^2)

  itemScoreRemainderCorr <- rep(NA, ncol(dat$Q))
  for (j in 1:ncol(dat$Q)) {
    term1 <- sum((dat$Z[, j] * dat$Q[, j] * scoreRemainder[, j]) / zzzTotal[j])
    term2 <- itemMean[j] * sum((scoreRemainder[, j]) / zzzTotal[j])
    itemScoreRemainderCorr[j] <- (term1 - term2) / (zzzScoreRemainderSD[j] * itemSD[j])
  }
  ret <-
    structure(list(
      ItemLabel = dat$ItemLabel,
      Obs = zzzTotal,
      ObsRatio = zzzMean,
      ItemMean = itemMean,
      ItemSD = itemSD,
      ItemCORR = itemScoreCorr,
      ItemCORR_R = itemScoreRemainderCorr
    ), class = c("exametrika", "QitemStatistics"))
  return(ret)
}
