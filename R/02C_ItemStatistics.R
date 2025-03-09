#' @title Simple Item Statistics
#' @description
#' This function calculates statistics for each item, with different metrics available
#' depending on the data type (binary, ordinal, or rated).
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#'
#' @return
#' For binary data:
#' \describe{
#' \item{ItemLabel}{Label identifying each item}
#' \item{NR}{Number of Respondents for each item}
#' \item{CRR}{Correct Response Rate denoted as $p_j$.}
#' \item{ODDs}{Item Odds is the ratio of the correct response rate to the incorrect response rate.
#' Defined as \eqn{o_j = \frac{p_j}{1-p_j}}}
#' \item{Threshold}{Item Threshold is a measure of difficulty based on a standard normal distribution.}
#' \item{Entropy}{Item Entropy is an indicator of the variability or randomness of the responses.
#' Defined as \eqn{e_j=-p_j \log_2 p_j - (1-p_j)\log_2(1-p_j)}}
#' \item{ITCrr}{Item-total Correlation is a Pearson's correlation of an item with the Number-Right score.}
#' }
#'
#' For ordinal polytomous data:
#' \describe{
#' \item{ItemLabel}{Label identifying each item}
#' \item{NR}{Number of Respondents for each item}
#' \item{Threshold}{Matrix of threshold values for each item's category boundaries, based on
#' a standard normal distribution. For an item with K categories, there are K-1 thresholds.}
#' \item{Entropy}{Item Entropy calculated using the category probabilities. Unlike binary data,
#' this is calculated using the formula \eqn{e_j = -\sum_{k=1}^{K_j} p_{jk} \log_{K_j} p_{jk}},
#' where \eqn{K_j} is the number of categories for item j.}
#' \item{ITCrr}{Item-total Correlation calculated using polyserial correlation, which
#' accounts for the ordinal nature of the item responses and the continuous total score.}
#' }
#'
#' @note For rated data, the function processes the data as binary, with each response
#' being compared to the correct answer to determine correctness.
#'
#' @examples
#' # using sample dataset(binary)
#' ItemStatistics(J15S500)
#' @export

ItemStatistics <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("ItemStatistics")
}

#' @rdname ItemStatistics
#' @export

ItemStatistics.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = ItemStatistics.binary(U, na, Z, w),
      "rated" = ItemStatistics.ordinal(U, na, Z, w),
      "ordinal" = ItemStatistics.ordinal(U, na, Z, w),
      "nominal" = response_type_error(U$response.type, "ItemStatistics")
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    ItemStatistics(U)
  }
}

#' @rdname ItemStatistics
#' @export
ItemStatistics.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  ret <-
    structure(list(
      ItemLabel = tmp$ItemLabel,
      NR = colSums(tmp$Z),
      CRR = crr(tmp),
      ODDs = ItemOdds(tmp),
      Threshold = ItemThreshold(tmp),
      Entropy = ItemEntropy(tmp),
      ITCrr = ItemTotalCorr(tmp)
    ), class = c("exametrika", "ItemStatistics"))
  return(ret)
}

#' @rdname ItemStatistics
#' @export
ItemStatistics.ordinal <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  Threshold <- ItemThreshold(tmp)
  ncat <- sapply(Threshold, length)
  thr.mat <- matrix(nrow = length(Threshold), ncol = max(ncat))
  colnames(thr.mat) <- 1:max(ncat)
  for (j in 1:length(Threshold)) {
    thr.mat[j, 1:ncat[j]] <- Threshold[[j]]
  }

  ret <-
    structure(list(
      ItemLabel = tmp$ItemLabel,
      NR = colSums(tmp$Z),
      Threshold = thr.mat,
      Entropy = ItemEntropy(tmp),
      ITCrr = ItemTotalCorr(tmp)
    ), class = c("exametrika", "ItemStatistics"))
  return(ret)
}
