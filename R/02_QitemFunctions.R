#' @title Generate category labels for response data
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

#' @title bivariate normal CDF
#' @description
#' Calculates the cumulative distribution function (CDF) of a bivariate normal distribution.
#' This function computes P(X <= a, Y <= b) where X and Y follow a bivariate normal
#' distribution with correlation coefficient rho.
#'
#' @param a Numeric value, the upper limit for the first variable.
#' @param b Numeric value, the upper limit for the second variable.
#' @param rho Numeric value between -1 and 1, the correlation coefficient.
#'
#' @return The probability P(X <= a, Y <= b), a value between 0 and 1.
#'
#' @details
#' The implementation uses numerical integration with Gauss-Legendre quadrature
#' for accurate computation. Special cases for infinite bounds are handled separately.
#'
#' @keywords internal

qBiNormal <- function(a, b, rho) {
  if (is.na(a) || is.na(b) || is.na(rho)) {
    warning("NA values detected in qBiNormal inputs")
    return(NA)
  }
  # support function
  f <- function(x, y, aprime, bprime, rho) {
    exp(aprime * (2 * x - aprime) + bprime * (2 * y - bprime) +
      2 * rho * (x - aprime) * (y - bprime))
  }

  # special case : Inf/-Inf
  if (is.infinite(a) && is.infinite(b)) {
    if (a > 0 && b > 0) {
      return(1)
    }
    if (a < 0 && b < 0) {
      return(0)
    }
    return(0)
  }

  # othre special case of Inf/-Inf
  if (is.infinite(a) || is.infinite(b)) {
    if (any(c(a, b) == -Inf)) {
      return(0)
    }
    #
    if (is.infinite(a)) {
      return(pnorm(b))
    } else if (is.infinite(b)) {
      return(pnorm(a))
    }
  }

  # main loop
  if (a <= 0 && b <= 0 && rho <= 0) {
    aprime <- a / sqrt(2 * (1 - rho^2))
    bprime <- b / sqrt(2 * (1 - rho^2))

    # const
    A <- c(
      5.54433663102343E-02, 1.24027738987730E-01, 1.75290943892075E-01,
      1.91488340747342E-01, 1.63473797144070E-01, 1.05937637278492E-01,
      5.00270211534535E-02, 1.64429690052673E-02, 3.57320421428311E-03,
      4.82896509305201E-04, 3.74908650266318E-05, 1.49368411589636E-06,
      2.55270496934465E-08, 1.34217679136316E-10, 9.56227446736465E-14
    )

    B <- c(
      2.16869474675590E-02, 1.12684220347775E-01, 2.70492671421899E-01,
      4.86902370381935E-01, 7.53043683072978E-01, 1.06093100362236E+00,
      1.40425495820363E+00, 1.77864637941183E+00, 2.18170813144494E+00,
      2.61306084533352E+00, 3.07461811380851E+00, 3.57140815113714E+00,
      4.11373608977209E+00, 4.72351306243148E+00, 5.46048893578335E+00
    )

    # double summation
    sum <- 0
    for (i in 1:15) {
      for (j in 1:15) {
        sum <- sum + A[i] * A[j] * f(B[i], B[j], aprime, bprime, rho)
      }
    }

    return(sum * sqrt(1 - rho^2) / pi)
  }

  if (a * b * rho <= 0) {
    if (a <= 0 && b >= 0 && rho >= 0) {
      return(pnorm(a) - qBiNormal(a, -b, -rho))
    } else if (a >= 0 && b <= 0 && rho >= 0) {
      return(pnorm(b) - qBiNormal(-a, b, -rho))
    } else if (a >= 0 && b >= 0 && rho <= 0) {
      return(pnorm(a) + pnorm(b) - 1 + qBiNormal(-a, -b, rho))
    }
  }

  if (a * b * rho >= 0) {
    denum <- sqrt(a^2 - 2 * rho * a * b + b^2)
    rho1 <- ((rho * a - b) * sign(a)) / denum
    rho2 <- ((rho * b - a) * sign(b)) / denum
    delta <- (1 - sign(a) * sign(b)) / 4

    return(qBiNormal(a, 0, rho1) + qBiNormal(b, 0, rho2) - delta)
  }

  # error case
  return(-99.9)
}

#' @title Calculate Polychoric Correlation Likelihood
#'
#' @description
#' Calculates the negative log-likelihood for estimating polychoric correlation
#' from a contingency table of two ordinal variables.
#'
#' @param rho Numeric value between -1 and 1, the correlation coefficient
#' @param mat A contingency table matrix for two ordinal variables
#'
#' @return The negative log-likelihood value for the given correlation coefficient
#'
#' @details
#' The function estimates thresholds from the marginal distributions and
#' calculates the expected probabilities based on a bivariate normal distribution.
#' It then computes the log-likelihood by comparing observed and expected frequencies.
#'
#' @keywords internal
#'
polychoric_likelihood <- function(rho, mat) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  th_r <- rowSums(mat) / sum(mat)
  th_c <- colSums(mat) / sum(mat)
  a <- qnorm(pmin(cumsum(th_r), 1))
  b <- qnorm(pmin(cumsum(th_c), 1))

  exp_probs <- matrix(0, nrow = nr, ncol = nc)
  for (i in 1:nr) {
    for (j in 1:nc) {
      a_low <- if (i > 1) {
        a[i - 1]
      } else {
        -Inf
      }
      a_high <- a[i]
      b_low <- if (j > 1) {
        b[j - 1]
      } else {
        (-Inf)
      }
      b_high <- b[j]

      exp_probs[i, j] <- qBiNormal(a_high, b_high, rho) -
        qBiNormal(a_high, b_low, rho) -
        qBiNormal(a_low, b_high, rho) +
        qBiNormal(a_low, b_low, rho)
    }
  }

  log_lik <- sum(mat * log(pmax(exp_probs, 1e-10)))
  return(-log_lik)
}

#' @title Generate Score Report for Non-Binary Test Data
#'
#' @description
#' Calculates comprehensive descriptive statistics for a test, including measures of
#' central tendency, variability, distribution shape, and reliability.
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#' it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#' observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item
#' @param na Values to be treated as missing values
#'
#' @return An object of class "exametrika" and "TestStatistics" containing:
#' \describe{
#'   \item{TestLength}{Number of items included in the test}
#'   \item{SampleSize}{Number of examinees (rows) in the dataset}
#'   \item{Mean}{Average score across all examinees}
#'   \item{Median}{Median score}
#'   \item{SD}{Standard deviation of test scores}
#'   \item{Variance}{Variance of test scores}
#'   \item{Skewness}{Skewness of the score distribution (measure of asymmetry)}
#'   \item{Kurtosis}{Kurtosis of the score distribution (measure of tail extremity)}
#'   \item{Min}{Minimum score obtained}
#'   \item{Max}{Maximum score obtained}
#'   \item{Range}{Difference between maximum and minimum scores}
#'   \item{Alpha}{Cronbach's alpha coefficient, a measure of internal consistency reliability}
#' }
#'
#' @details
#' This function is intended for non-binary (ordinal or rated) response data. It calculates
#' descriptive statistics for the overall test performance. If binary data is provided,
#' an error message will be displayed.
#'
#' @importFrom stats median
#' @examples
#' \donttest{
#' # Generate score report for sample ordinal data
#' ScoreReport(J15S3810)
#'
#' # Example with rated data
#' ScoreReport(J35S5000)
#' }
#'
#' @export
ScoreReport <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  if (tmp$response.type == "binary") {
    response_type_error(U$response.type, "ScoreReport")
  }
  if (U$response.type == "rated") {
    tmp$Q <- tmp$U
  }

  nobs <- nrow(tmp$Q)
  nitems <- ncol(tmp$Q)
  ## score dist
  score <- rowSums(tmp$Z * tmp$Q)
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
  zzzTotal <- apply(tmp$Z, 2, sum)
  zzzMean <- apply(tmp$Z, 2, mean)
  itemMean <- apply(tmp$Z * tmp$Q, 2, sum) / zzzTotal
  itemSD <- sqrt(apply((tmp$Z * tmp$Q^2), 2, sum) / zzzTotal - itemMean^2)
  zzzScoreMean <- colSums(tmp$Z * score) / colSums(tmp$Z)
  zzzScoreSD <- sqrt(colSums(tmp$Z * score^2) / colSums(tmp$Z) - zzzScoreMean^2)
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

#' @title Generate Item Report for Non-Binary Test Data
#'
#' @description
#' Calculates item-level statistics for non-binary test data, including response rates,
#' basic descriptive statistics, and item-total correlations.
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#' it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#' observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item
#' @param na Values to be treated as missing values
#'
#' @return An object of class "exametrika" and "QitemStatistics" containing:
#' \describe{
#'   \item{ItemLabel}{Labels identifying each item}
#'   \item{Obs}{Number of valid responses for each item}
#'   \item{ObsRatio}{Proportion of valid responses for each item (range: 0-1)}
#'   \item{ItemMean}{Mean score of each item}
#'   \item{ItemSD}{Standard deviation of each item score}
#'   \item{ItemCORR}{Item-total correlation coefficients - correlation between
#'         item scores and total test scores}
#'   \item{ItemCORR_R}{Corrected item-total correlation coefficients - correlation between
#'         item scores and total test scores excluding the target item}
#' }
#'
#' @details
#' This function is intended for non-binary (ordinal or rated) response data. It provides
#' detailed statistics for each item in the test, focusing on response patterns and
#' the relationship between individual items and overall test performance.
#' If binary data is provided, an error message will be displayed.
#'
#' @examples
#' \donttest{
#' # Generate item report for sample ordinal data
#' item_stats <- ItemReport(J15S3810)
#'
#' # View first few rows of the item report
#' head(item_stats)
#'
#' # Example with rated data including custom missing value indicator
#' item_stats2 <- ItemReport(J35S5000, na = -99)
#' }
#'
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
  if (U$response.type == "rated") {
    dat$Q <- dat$U
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
