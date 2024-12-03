#' @title IRP index
#' @description
#' IRP indices(Kumagai,2007) that help us understand the IRP shape.
#' @param IRP IRP table
#' @noRd

IRPindex <- function(IRP) {
  Beta <- apply(abs(IRP - 0.5), 1, which.min)
  NR <- NROW(IRP)
  NC <- NCOL(IRP)
  B <- IRP[cbind(1:NR, Beta)]
  A <- Alpha <- rep(NA, NR)
  C <- Gamma <- rep(0, NR)
  for (i in 1:NR) {
    vec <- IRP[i, ]
    lags <- vec - c(NA, vec[1:(NC - 1)])
    A[i] <- max(lags, na.rm = T)
    Alpha[i] <- which.max(lags) - 1
    C[i] <- sum(lags[lags < 0], na.rm = T)
    if (C[i] != 0) {
      Gamma[i] <- (length(lags[lags < 0]) - 1) / (NC - 1)
    }
  }
  ret <- as.data.frame(cbind(Alpha, A, Beta, B, Gamma, C))
  return(ret)
}
