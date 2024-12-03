#' @title log-likelihood function for ability parameter
#' @description An internal function to calculate the log-likelihood
#' @param theta target unknown parameter
#' @param Lambda Item parameters matrix
#' @param U data matrix
#' @param Z missing indicator
#' @noRd
LLtheta_mat <- function(theta, Lambda, U, Z) {
  f <- function(x) {
    p <- Lambda[, 3] + (Lambda[, 4] - Lambda[, 3]) / (1 + exp(-Lambda[, 1] * (x - Lambda[, 2])))
    ll <- sum(U * log(p) + (1 - U) * log(1 - p)) - x^2 / 2
    ll <- U %*% log(p) + (1 - U) %*% log(1 - p) - x^2 / 2
    return(exp(ll))
  }
  f_vec <- Vectorize(f)
  return(f_vec(theta))
}


#' @title Ability Parameter Estimation Function
#' @description
#' A function that returns the Expected A Posteriori (EAP) of the
#' ability parameter and its posterior standard deviation.
#' @param Lambda Item parameters matrix
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @noRd

EAP_PSD <- function(Lambda, U, Z) {
  dev <- 0.01
  xdev <- seq(-5, 5, dev)
  N <- length(xdev)

  tmp1 <- LLtheta_mat(xdev[1:N - 1], Lambda, U, Z)
  tmp2 <- LLtheta_mat(xdev[2:N], Lambda, U, Z)

  denominator <- rowSums((tmp1 + tmp2) * dev / 2)
  numerator <- rowSums((tmp1 %*% diag(xdev[1:N - 1]) + tmp2 %*% diag(xdev[2:N])) * dev / 2)
  EAP <- numerator / denominator

  weight <- ((EAP %*% t(rep(1, length(xdev)))) - ((rep(1, length(EAP))) %*% t(xdev)))^2
  numerator <- rowSums((tmp1 * weight[, 1:N - 1] + tmp2 * weight[, 2:N]) * dev / 2)
  PSDs <- sqrt(numerator / denominator)

  ret <- list(EAP = EAP, PSDs = PSDs)
  ret <- structure(ret, class = c("exametrika", "IRT_EAP_PSD"))
  return(ret)
}
