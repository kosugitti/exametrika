#' @title Two-Parameter Logistic Model
#' @description
#' The two-parameter logistic model is a classic model that defines
#' the probability of a student with ability theta successfully
#' answering item j, using both a slope parameter and a
#' location parameter.
#' @param a slope parameter
#' @param b location parameter
#' @param theta ability parameter
#' @export

TwoPLM <- function(a, b, theta) {
  p <- 1 / (1 + exp(-a * (theta - b)))
  return(p)
}

#' @title Three-Parameter Logistic Model
#' @description
#' The three-parameter logistic model is a model where the lower
#' asymptote parameter c is added to the 2PLM
#' @param a slope parameter
#' @param b location parameter
#' @param c lower asymptote parameter
#' @param theta ability parameter
#' @export

ThreePLM <- function(a, b, c, theta) {
  p <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  return(p)
}

#' @title Four-Parameter Logistic Model
#' @description
#' The four-parameter logistic model is a model where one additional
#' parameter d, called the upper asymptote parameter, is added to the
#' 3PLM.
#' @param a slope parameter
#' @param b location parameter
#' @param c lower asymptote parameter
#' @param d upper asymptote parameter
#' @param theta ability parameter
#' @export

LogisticModel <- function(a = 1, b, c = 0, d = 1, theta) {
  p <- c + ((d - c) / (1 + exp(-a * (theta - b))))
  return(p)
}

#' @title Rasch Model
#' @description
#' The one-parameter logistic model is a model with only one parameter b.
#' This model is a 2PLM model in which a is constarind to 1.
#' This model is also called the Rasch model.
#' @param b slope parameter
#' @param theta ability parameter
#' @export

RaschModel <- function(b, theta) {
  p <- 1 / (1 + exp(-1 * (theta - b)))
  return(p)
}

#' @title IIF for 2PLM
#' @description
#' Item Information Function for 2PLM
#' @param a slope parameter
#' @param b location parameter
#' @param theta ability parameter
#' @export

IIF2PLM <- function(a, b, theta) {
  a^2 * TwoPLM(a, b, theta) * (1 - TwoPLM(a, b, theta))
}

#' @title IIF for 3PLM
#' @description
#' Item Information Function for 3PLM
#' @param a slope parameter
#' @param b location parameter
#' @param c lower asymptote parameter
#' @param theta ability parameter
#' @export

IIF3PLM <- function(a, b, c, theta) {
  numerator <- a^2 * (1 - ThreePLM(a, b, c, theta)) * (ThreePLM(a, b, c, theta) - c)^2
  denominator <- (1 - c)^2 * ThreePLM(a, b, c, theta)
  tmp <- numerator / denominator
  return(tmp)
}

#' @title IIF for 4PLM
#' @description
#' Item Information Function for 4PLM
#' @param a slope parameter
#' @param b location parameter
#' @param c lower asymptote parameter
#' @param d upper asymptote parameter
#' @param theta ability parameter
#' @export

ItemInformationFunc <- function(a = 1, b, c = 0, d = 1, theta) {
  numerator <- a^2 * (LogisticModel(a, b, c, d, theta) - c) * (d - LogisticModel(a, b, c, d, theta)) *
    (LogisticModel(a, b, c, d, theta) * (c + d - LogisticModel(a, b, c, d, theta)) - c * d)
  denominator <- (d - c)^2 * LogisticModel(a, b, c, d, theta) * (1 - LogisticModel(a, b, c, d, theta))
  tmp <- numerator / denominator
  return(tmp)
}


#' @title TIF for IRT
#' @description
#' Test Information Function for 4PLM
#' @param params parameter matrix
#' @param theta ability parameter
#' @export

TestInformationFunc <- function(params, theta) {
  tl <- nrow(params)
  tmp <- 0
  for (i in 1:tl) {
    a <- params[i, 1]
    b <- params[i, 2]
    if (ncol(params) > 2) {
      c <- params[i, 3]
    } else {
      c <- 0
    }
    if (ncol(params) > 3) {
      d <- params[i, 4]
    } else {
      d <- 1
    }
    tmp <- tmp + ItemInformationFunc(a = a, b = b, c = c, d = d, theta)
  }
  return(tmp)
}
