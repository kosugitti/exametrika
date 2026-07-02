# Student-level scoring functions (nrs, passage, sscore, percentile, stanine).
# Split out of R/02_TestItemFunctions.R (2026-07-01) for maintainability;
# no logic changed, only moved.

#' @title Number Right Score
#' @description
#' The Number-Right Score (NRS) function calculates the weighted sum of correct
#' responses for each examinee. This function is applicable only to binary
#' response data.
#'
#' For each examinee, the score is computed as:
#' \deqn{NRS_i = \sum_{j=1}^J z_{ij}u_{ij}w_j}
#' where:
#' \itemize{
#'   \item \eqn{z_{ij}} is the missing response indicator (0/1)
#'   \item \eqn{u_{ij}} is the response (0/1)
#'   \item \eqn{w_j} is the item weight
#' }
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector containing the Number-Right Score for each examinee.
#' The score represents the weighted sum of correct answers, where:
#' \itemize{
#'   \item Maximum score is the sum of all item weights
#'   \item Minimum score is 0
#'   \item Missing responses do not contribute to the score
#' }
#' @examples
#' # using sample dataset
#' nrs(J15S500)
#' @export
nrs <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("nrs")
}

#' @rdname nrs
#' @export
nrs.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "nrs")
  }
  nrs.binary(U, na, Z, w)
}

#' @rdname nrs
#' @export
nrs.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Calculate weighted sum of correct responses
  # Z * U gives correct answers accounting for missing data
  # Multiply by weights and sum across items
  total_weighted_score <- (U$Z * U$U) %*% U$w
  return(total_weighted_score)
}

#' @title Passage Rate of Student
#' @description
#' The Passage Rate for each student is calculated as their Number-Right Score (NRS)
#' divided by the number of items presented to them. This function is applicable
#' only to binary response data.
#'
#' The passage rate is calculated as:
#' \deqn{P_i = \frac{\sum_{j=1}^J z_{ij}u_{ij}w_j}{\sum_{j=1}^J z_{ij}}}
#' where:
#' \itemize{
#'   \item \eqn{z_{ij}} is the missing response indicator (0/1)
#'   \item \eqn{u_{ij}} is the response (0/1)
#'   \item \eqn{w_j} is the item weight
#' }
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector containing the passage rate for each student.
#' Values range from 0 to 1 (or maximum weight) where:
#' \itemize{
#'   \item 1: Perfect score on all attempted items
#'   \item 0: No correct answers
#'   \item NA: No items attempted
#' }
#' @note
#' The passage rate accounts for missing responses by only considering items that
#' were actually presented to each student. This provides a fair comparison
#' between students who attempted different numbers of items.
#' @examples
#' # using sample dataset
#' passage(J15S500)
#'
#' @export
passage <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("passage")
}

#' @rdname passage
#' @export
passage.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "passage")
  }
  passage.binary(U, na, Z, w)
}

#' @rdname passage
#' @export
passage.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Calculate Number-Right Score
  total_score <- nrs(U)

  # Calculate number of items presented to each student
  items_attempted <- NCOL(U$U) - rowSums(1 - U$Z)

  # Calculate passage rate
  # Divide total score by number of items attempted
  passage_rate <- total_score / items_attempted

  return(passage_rate)
}

#' @title Standardized Score
#' @description
#' The standardized score (z-score) indicates how far a student's performance
#' deviates from the mean in units of standard deviation. This function is
#' applicable only to binary response data.
#'
#' The score is calculated by standardizing the passage rates:
#' \deqn{Z_i = \frac{r_i - \bar{r}}{\sigma_r}}
#' where:
#' \itemize{
#'   \item \eqn{r_i} is student i's passage rate
#'   \item \eqn{\bar{r}} is the mean passage rate
#'   \item \eqn{\sigma_r} is the standard deviation of passage rates
#' }
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of standardized scores for each student. The scores follow
#' a standard normal distribution with:
#' \itemize{
#'   \item Mean = 0
#'   \item Standard deviation = 1
#'   \item Approximately 68% of scores between -1 and 1
#'   \item Approximately 95% of scores between -2 and 2
#'   \item Approximately 99% of scores between -3 and 3
#' }
#' @note
#' The standardization allows for comparing student performance across different
#' tests or groups. A positive score indicates above-average performance, while
#' a negative score indicates below-average performance.
#' @examples
#' # using sample dataset
#' sscore(J5S10)
#' @export
sscore <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("sscore")
}

#' @rdname sscore
#' @export
sscore.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "sscore")
  }
  sscore.binary(U, na, Z, w)
}

#' @rdname sscore
#' @export
sscore.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Get number of students
  S <- nrow(U$U)

  # Create unit vector
  OneS <- rep(1, length = S)

  # Calculate passage rates
  passage_rates <- passage(U)

  # Calculate mean passage rate
  mean_rate <- mean(passage_rates, na.rm = TRUE)

  # Calculate variance of passage rates
  centered_rates <- passage_rates - mean_rate
  var_rates <- sum(centered_rates^2, na.rm = TRUE) / (S - 1)

  # Calculate standardized scores
  z_scores <- centered_rates / sqrt(var_rates)

  return(z_scores)
}


#' @title Student Percentile Ranks
#' @description
#' The percentile function calculates each student's relative standing in the group,
#' expressed as a percentile rank (1-100). This function is applicable only to
#' binary response data.
#'
#' The percentile rank indicates the percentage of scores in the distribution
#' that fall below a given score. For example, a percentile rank of 75 means
#' the student performed better than 75% of the group.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of percentile ranks (1-100) for each student, where:
#' \itemize{
#'   \item 100: Highest performing student(s)
#'   \item 50: Median performance
#'   \item 1: Lowest performing student(s)
#' }
#' @note
#' Percentile ranks are calculated using the empirical cumulative distribution
#' function of standardized scores. Tied scores receive the same percentile rank.
#' The values are rounded up to the nearest integer to provide ranks from 1 to 100.
#' @examples
#' # using sample dataset
#' percentile(J5S10)
#'
#' @importFrom stats ecdf
#' @export
percentile <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("percentile")
}

#' @rdname percentile
#' @export
percentile.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "percentile")
  }
  percentile.binary(U, na, Z, w)
}

#' @rdname percentile
#' @export
percentile.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Calculate standardized scores
  standardized_scores <- sscore(U)

  # Calculate empirical cumulative distribution function
  empirical_dist <- ecdf(standardized_scores)

  # Convert to percentiles (1-100)
  # Ceiling function ensures minimum of 1 and handles rounding
  percentile_ranks <- ceiling(empirical_dist(standardized_scores) * 100)

  return(percentile_ranks)
}

#' @title Stanine Scores
#' @description
#' The Stanine (Standard Nine) scoring system divides students into nine groups
#' based on a normalized distribution. This function is applicable only to
#' binary response data.
#'
#' These groups correspond to the following percentile ranges:
#' \itemize{
#'   \item Stanine 1: lowest 4% (percentiles 1-4)
#'   \item Stanine 2: next 7% (percentiles 5-11)
#'   \item Stanine 3: next 12% (percentiles 12-23)
#'   \item Stanine 4: next 17% (percentiles 24-40)
#'   \item Stanine 5: middle 20% (percentiles 41-60)
#'   \item Stanine 6: next 17% (percentiles 61-77)
#'   \item Stanine 7: next 12% (percentiles 78-89)
#'   \item Stanine 8: next 7% (percentiles 90-96)
#'   \item Stanine 9: highest 4% (percentiles 97-100)
#' }
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A list containing two elements:
#' \describe{
#'   \item{stanine}{The score boundaries for each stanine level}
#'   \item{stanineScore}{The stanine score (1-9) for each student}
#' }
#' @note
#' Stanine scores provide a normalized scale with:
#' \itemize{
#'   \item Mean = 5
#'   \item Standard deviation = 2
#'   \item Scores range from 1 to 9
#'   \item Score of 5 represents average performance
#' }
#' @references
#' Angoff, W. H. (1984). Scales, norms, and equivalent scores. Educational Testing Service.
#' (Reprint of chapter in R. L. Thorndike (Ed.) (1971) Educational Measurement (2nd Ed.).
#' American Council on Education.
#' @importFrom stats quantile
#' @examples
#' result <- stanine(J15S500)
#' # View score boundaries
#' result$stanine
#' # View individual scores
#' result$stanineScore
#'
#' @export
stanine <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("stanine")
}

#' @rdname stanine
#' @export
stanine.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "stanine")
  }
  stanine.binary(U, na, Z, w)
}

#' @rdname stanine
#' @export
stanine.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Define stanine boundaries (cumulative proportions)
  stanine_bounds <- cumsum(c(0.04, 0.07, 0.12, 0.17, 0.20, 0.17, 0.12, 0.07))

  # Calculate raw scores
  raw_scores <- nrs(U)

  # Calculate score boundaries using raw scores
  stanine_boundaries <- quantile(raw_scores,
    probs = stanine_bounds,
    na.rm = TRUE
  )

  # Calculate percentile scores
  percentile_scores <- percentile(U)

  # Calculate stanine boundaries using percentile scores
  stanine_percentile_bounds <- quantile(percentile_scores,
    probs = stanine_bounds
  )
  stanine_percentile_bounds <- unique(stanine_percentile_bounds)
  if (length(stanine_percentile_bounds) != 8) {
    warning(
      "Standard stanine (9-level) division not possible due to data distribution.\n",
      "Merging duplicate boundaries for adjustment."
    )
  }
  # Assign stanine scores
  stanine_scores <- cut(percentile_scores,
    breaks = c(-Inf, stanine_percentile_bounds, Inf),
    right = FALSE,
    labels = 1:(length(stanine_percentile_bounds) + 1)
  )

  # Return results
  list(
    stanine = stanine_boundaries,
    stanineScore = stanine_scores
  )
}
