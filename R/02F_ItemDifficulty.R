# Item-level difficulty/quality statistics (crr, ItemOdds, ItemThreshold,
# ItemEntropy, ItemTotalCorr, BiserialCorrelation, ITBiserial).
# Split out of R/02_TestItemFunctions.R (2026-07-01) for maintainability;
# no logic changed, only moved.

#' @title Correct Response Rate
#' @description
#' The correct response rate (CRR) is one of the most basic and important
#' statistics for item analysis. This is an index of item difficulty and
#' a measure of how many students out of those who tried an item correctly
#' responded to it. This function is applicable only to binary response data.
#'
#' The CRR for each item is calculated as:
#' \deqn{p_j = \frac{\sum_{i=1}^n z_{ij}u_{ij}}{\sum_{i=1}^n z_{ij}}}
#' where \eqn{z_{ij}} is the missing indicator and \eqn{u_{ij}} is the response.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of weighted correct response rates for each item.
#' Values range from 0 to 1, where higher values indicate easier items
#' (more students answered correctly).
#' @examples
#' # using sample datasaet
#' crr(J15S500)
#' @export
crr <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("crr")
}

#' @rdname crr
#' @export
crr.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "crr")
  }
  crr.binary(U, na, Z, w)
}

#' @rdname crr
#' @export
crr.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Create unit vector for summation
  OneS <- rep(1, length = nrow(U$U))

  # Calculate correct response rate
  # (sum of correct responses) / (sum of non-missing responses)
  p <- t(U$Z * U$U) %*% OneS / t(U$Z) %*% OneS

  # Apply item weights
  pW <- U$w * p

  return(pW)
}

#' @title Item Odds
#' @description
#' Item Odds are defined as the ratio of Correct Response Rate to
#' Incorrect Response Rate:
#' \deqn{O_j = \frac{p_j}{1-p_j}}
#' where \eqn{p_j} is the correct response rate for item j.
#' This function is applicable only to binary response data.
#'
#' The odds value represents how many times more likely a correct response is
#' compared to an incorrect response. For example, an odds of 2 means students
#' are twice as likely to answer correctly as incorrectly.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of odds values for each item. Values range from 0 to infinity,
#' where:
#' \itemize{
#'   \item odds > 1: correct response more likely than incorrect
#'   \item odds = 1: equally likely
#'   \item odds < 1: incorrect response more likely than correct
#' }
#' @examples
#' # using sample dataset
#' ItemOdds(J5S10)
#' @export
ItemOdds <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("ItemOdds")
}

#' @rdname ItemOdds
#' @export
ItemOdds.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "ItemOdds")
  }
  ItemOdds.binary(U, na, Z, w)
}

#' @rdname ItemOdds
#' @export
ItemOdds.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Calculate correct response rates
  p <- crr(U)

  # Calculate odds
  o <- p / (1 - p)

  return(o)
}


#' @title Item Threshold
#' @description
#' Item threshold is a measure of difficulty based on a standard normal distribution.
#' This function is applicable only to binary response data.
#'
#' The threshold is calculated as:
#' \deqn{\tau_j = \Phi^{-1}(1-p_j)}
#' where \eqn{\Phi^{-1}} is the inverse standard normal distribution function
#' and \eqn{p_j} is the correct response rate for item j.
#'
#' Higher threshold values indicate more difficult items, as they represent the
#' point on the standard normal scale above which examinees tend to answer incorrectly.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of threshold values for each item on the standard normal scale.
#' Typical values range from about -3 to 3, where:
#' \itemize{
#'   \item Positive values indicate difficult items
#'   \item Zero indicates items of medium difficulty (50% correct)
#'   \item Negative values indicate easy items
#' }
#' @importFrom stats qnorm
#' @examples
#' # using sample dataset
#' ItemThreshold(J5S10)
#' @export
ItemThreshold <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("ItemThreshold")
}
#' @export
ItemThreshold.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = ItemThreshold.binary(U, na, Z, w),
      "rated" = ItemThreshold.ordinal(U, na, Z, w),
      "ordinal" = ItemThreshold.ordinal(U, na, Z, w),
      "nominal" = response_type_error(U$response.type, "ItemThreshold")
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    ItemThreshold(U)
  }
}

#' @rdname ItemThreshold
#' @export
ItemThreshold.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Calculate correct response rates
  p <- crr(U)
  # Calculate thresholds using inverse normal distribution
  tau <- qnorm(1 - p)
  return(tau)
}

#' @rdname ItemThreshold
#' @export
ItemThreshold.ordinal <- function(U, na = NULL, Z = NULL, w = NULL) {
  nitems <- ncol(U$Q)
  ncat <- U$categories
  U$Q[U$Z == 0] <- NA
  threshold <- vector("list", nitems)
  for (j in 1:nitems) {
    tmp <- qnorm(cumsum(tabulate(U$Q[, j]) / sum(tabulate(U$Q[, j]))))
    threshold[[j]] <- tmp[1:ncat[j] - 1]
  }
  return(threshold)
}


#' @title Item Entropy
#' @description
#' The item entropy is an indicator of the variability or randomness
#' of the responses. This function is applicable only to binary response data.
#'
#' The entropy value represents the uncertainty or information content of the
#' response pattern for each item, measured in bits. Maximum entropy (1 bit)
#' occurs when correct and incorrect responses are equally likely (p = 0.5).
#' @details
#' The item entropy is calculated as:
#' \deqn{e_j = -p_j\log_2p_j-(1-p_j)\log_2(1-p_j)}
#' where \eqn{p_j} is the correct response rate for item j.
#'
#' The entropy value has the following properties:
#' \itemize{
#'   \item Maximum value of 1 bit when p = 0.5 (most uncertainty)
#'   \item Minimum value of 0 bits when p = 0 or 1 (no uncertainty)
#'   \item Higher values indicate more balanced response patterns
#'   \item Lower values indicate more predictable response patterns
#' }
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of entropy values for each item, measured in bits.
#' Values range from 0 to 1, where:
#' \itemize{
#'   \item 1: maximum uncertainty (p = 0.5)
#'   \item 0: complete certainty (p = 0 or 1)
#'   \item Values near 1 indicate items with balanced response patterns
#'   \item Values near 0 indicate items with extreme response patterns
#' }
#' @examples
#' # using sample dataset
#' ItemEntropy(J5S10)
#' @export
ItemEntropy <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("ItemEntropy")
}

#' @rdname ItemEntropy
#' @export
ItemEntropy.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = ItemEntropy.binary(U, na, Z, w),
      "rated" = ItemEntropy.ordinal(U, na, Z, w),
      "ordinal" = ItemEntropy.ordinal(U, na, Z, w),
      "nominal" = response_type_error(U$response.type, "ItemEntropy")
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    ItemEntropy(U)
  }
}

#' @rdname ItemEntropy
#' @export
ItemEntropy.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Calculate correct response rates
  p <- crr(U)
  # Calculate entropy in bits
  # Using log base 2 for information content in bits
  itemE <- -p * log(p, base = 2) - (1 - p) * log(1 - p, base = 2)
  return(itemE)
}

#' @rdname ItemEntropy
#' @export
ItemEntropy.ordinal <- function(U, na = NULL, Z = NULL, w = NULL) {
  nitems <- ncol(U$Q)
  ncat <- U$categories
  U$Q[U$Z == 0] <- NA
  entropy <- rep(0, nitems)
  for (j in 1:nitems) {
    vec <- tabulate(U$Q[, j]) / sum(tabulate(U$Q[, j]))
    entropy[j] <- sum(vec * log(vec, base = ncat[j])) * -1
  }
  return(entropy)
}


#' @title Item-Total Correlation
#' @description
#' Item-Total correlation (ITC) is a Pearson's correlation of an item with
#' the Number-Right Score (NRS) or total score. This function is applicable
#' only to binary response data.
#'
#' The ITC is a measure of item discrimination, indicating how well an item
#' distinguishes between high and low performing examinees.
#' @details
#' The correlation is calculated between:
#' \itemize{
#'   \item Each item's responses (0 or 1)
#'   \item The total test score (sum of correct responses)
#' }
#' Higher positive correlations indicate items that better discriminate between
#' high and low ability examinees.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of item-total correlations. Values typically range
#' from -1 to 1, where:
#' \itemize{
#'   \item Values near 1: Strong positive discrimination
#'   \item Values near 0: No discrimination
#'   \item Negative values: Potential item problems (lower ability students
#'         performing better than higher ability students)
#' }
#' @note
#' Values below 0.2 might indicate problematic items that should be reviewed.
#' Values above 0.3 are generally considered acceptable.
#' @examples
#' # using sample dataset
#' ItemTotalCorr(J15S500)
#'
#' @export
ItemTotalCorr <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("ItemTotalCorr")
}

#' @rdname ItemTotalCorr
#' @export
ItemTotalCorr.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = ItemTotalCorr.binary(U, na, Z, w),
      "rated" = ItemTotalCorr.binary(U, na, Z, w),
      "ordinal" = ItemTotalCorr.ordinal(U, na, Z, w),
      "nominal" = response_type_error(U$response.type, "ItemTotalCorr")
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    ItemTotalCorr(U)
  }
}

#' @rdname ItemTotalCorr
#' @export
ItemTotalCorr.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Calculate item probabilities
  p <- crr(U)
  # Calculate total scores
  Zeta <- sscore(U)
  # Create probability matrix (repeating p for each student)
  TBL <- matrix(rep(p, each = NROW(U$U)),
    nrow = NROW(U$U),
    byrow = FALSE
  )
  # Handle missing values in response matrix
  Una <- ifelse(is.na(U$U), 0, U$U)
  # Calculate deviations from expected values
  dev <- U$Z * (Una - TBL)
  # Calculate item variances
  V <- colSums(dev^2) / (colSums(U$Z) - 1)
  SD <- sqrt(V)
  # Calculate correlations
  rho_Zi <- t(dev) %*% Zeta / SD / colSums(U$Z)
  return(rho_Zi)
}

#' @rdname ItemTotalCorr
#' @export
ItemTotalCorr.ordinal <- function(U, na = NULL, Z = NULL, w = NULL) {
  U$Q[U$Z == 0] <- NA
  total <- rowSums(U$Q, na.rm = TRUE)
  nitems <- NCOL(U$Z)
  rho_Zi <- numeric(nitems)
  for (j in 1:nitems) {
    rho_Zi[j] <- polyserial(total, U$Q[, j])
  }
  return(rho_Zi)
}


#' @title Biserial Correlation
#' @description
#' A biserial correlation is a correlation between dichotomous-ordinal and
#' continuous variables.
#' @param i i is a dichotomous-ordinal variable (0/1). x and y can also be the other way around.
#' @param t t is a continuous variable. x and y can also be the other way around.
#' @return The biserial correlation coefficient between the two variables.
#' @importFrom stats na.omit qnorm pnorm optim
#' @export
BiserialCorrelation <- function(i, t) {
  # Original function remains unchanged
  # Count unique values
  unique_i <- length(unique(na.omit(i)))
  unique_t <- length(unique(na.omit(t)))
  # Check if one is binary and the other is continuous
  if (!((unique_i == 2 && unique_t > 2) | (unique_t == 2 && unique_i > 2))) {
    stop("One argument must be binary and the other must be continuous.")
  }
  ## if switched...
  if (unique_i > 2) {
    tmp <- i
    i <- t
    t <- tmp
  }
  ## calcs correlation
  tau_j <- qnorm(1 - mean(i, na.rm = TRUE))
  ll <- function(rho, tau_j, i, t) {
    tmp <- (1 - i) %*% (log(pnorm(tau_j, mean = rho * t, sd = sqrt(1 - rho^2)))) +
      i %*% (log(1 - pnorm(tau_j, mean = rho * t, sd = sqrt(1 - rho^2))))
  }
  pairwise <- !is.na(i + t)
  ret <- optim(
    par = 0, # initial value
    fn = function(x) {
      -ll(rho = x, tau_j, i[pairwise], t[pairwise])
    },
    lower = -1, # lower limit
    upper = 1, # upper limit
    method = "Brent" # one-dimensional optimization method
  )
  return(ret$par)
}

#' @title Item-Total Biserial Correlation
#' @description
#' The Item-Total Biserial Correlation computes the biserial correlation
#' between each item and the total score. This function is applicable only
#' to binary response data.
#'
#' This correlation provides a measure of item discrimination, indicating how well
#' each item distinguishes between high and low performing examinees.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of item-total biserial correlations. Values range
#' from -1 to 1, where:
#' \itemize{
#'   \item Values near 1: Strong positive discrimination
#'   \item Values near 0: No discrimination
#'   \item Negative values: Potential item problems
#' }
#' @note
#' The biserial correlation is generally preferred over the point-biserial
#' correlation when the dichotomization is artificial (i.e., when the underlying
#' trait is continuous).
#' @examples
#' # using sample dataset
#' ITBiserial(J15S500)
#' @export
ITBiserial <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("ITBiserial")
}

#' @rdname ITBiserial
#' @export
ITBiserial.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "ITBiserial")
  }
  ITBiserial.binary(U, na, Z, w)
}

#' @rdname ITBiserial
#' @export
ITBiserial.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Calculate total scores
  Zeta <- sscore(U)

  # Handle missing values
  U_data <- U$U
  U_data[U$Z == 0] <- NA

  # Calculate biserial correlation for each item
  ITB <- vapply(seq_len(ncol(U_data)), function(i) {
    BiserialCorrelation(U_data[, i], Zeta)
  }, numeric(1))

  return(ITB)
}
