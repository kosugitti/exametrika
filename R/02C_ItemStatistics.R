#' @title Simple Item Statistics
#' @description
#' This function calculates statistics for each item.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return
#' \describe{
#' \item{NR}{Number of Respondents}
#' \item{CRR}{Correct Response Rate denoted as $p_j$.}
#' \item{ODDs}{Item Odds is the ratio of the correct response rate to the incorrect response rate.
#' Defined as \eqn{o_j = \frac{p_j}{1-p_j}}}
#' \item{Threshold}{Item Threshold is a measure of difficulty based on a standard normal distribution.}
#' \item{Entropy}{Item Entropy is an indicator of the variability or randomness of the responses.
#' Defined as \eqn{e_j=-p_j \log_2 p_j - (1-p_j)\log_2(1-p_j)}}
#' \item{ITCrr}{Item-total Correlation is a Pearson's correlation fo an item with the number of Number-Right score.}
#' }
#' @examples
#' # using sample dataset
#' ItemStatistics(J15S500)
#' @export

ItemStatistics <- function(U, na = NULL, Z = NULL, w = NULL) {
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
