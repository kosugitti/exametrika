#' @title StudentAnalysis
#' @description
#' The StudentAnalysis function returns descriptive statistics for each individual student.
#' Specifically, it provides the number of responses, the number of correct answers,
#' the passage rate, the standardized score, the percentile, and the stanine.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#'   \itemize{
#'     \item ID: Student identifier
#'     \item NR: Number of responses
#'     \item NRS: Number-right score (total correct answers)
#'     \item PR: Passage rate (proportion correct)
#'     \item SS: Standardized score (z-score)
#'     \item Percentile: Student's percentile rank
#'     \item Stanine: Student's stanine score (1-9)
#'   }
#' @return Returns a data frame containing the following columns for each student:
#'   \itemize{
#'     \item ID: Student identifier
#'     \item NR: Number of responses
#'     \item NRS: Number-right score (total correct answers)
#'     \item PR: Passage rate (proportion correct)
#'     \item SS: Standardized score (z-score)
#'     \item Percentile: Student's percentile rank
#'     \item Stanine: Student's stanine score (1-9)
#'   }
#' @examples
#' # using sample dataset
#' StudentAnalysis(J15S500)
#' @export
#'

StudentAnalysis <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  NRS <- nrs(tmp)
  NR <- NCOL(tmp$U) - rowSums(is.na(tmp$U))
  PR <- passage(tmp)
  SS <- sscore(tmp)
  Ptile <- percentile(tmp)
  ST <- stanine(tmp)
  ret <- data.frame(
    ID = tmp$ID,
    NR = NR,
    NRS = NRS,
    PR = PR,
    SS = SS,
    Percentile = Ptile,
    Stanine = ST$stanineScore
  )
  return(ret)
}
