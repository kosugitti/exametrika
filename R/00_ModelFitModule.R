#' @title Model Fit Functions for Items
#' @description
#' A general function that returns the model fit indices.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param ell_A log likelihood of this model
#' @param nparam number of parameters for this model
#' @return
#' \describe{
#' \item{model_log_like}{log likelihood of analysis model}
#' \item{bench_log_like}{log likelihood of benchmark model}
#' \item{null_log_like}{log likelihood of null model}
#' \item{model_Chi_sq}{Chi-Square statistics for analysis model}
#' \item{null_Chi_sq}{Chi-Square statistics for null model}
#' \item{model_df}{degrees of freedom of analysis model}
#' \item{null_df}{degrees of freedom of null model}
#' \item{NFI}{Normed Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{RFI}{Relative Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{IFI}{Incremental Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{TLI}{Tucker-Lewis Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{CFI}{Comparative Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{RMSEA}{Root Mean Square Error of Approximation. Smaller values closer to 0.0 indicate a better fit.}
#' \item{AIC}{Akaike Information Criterion. A lower value indicates a better fit.}
#' \item{CAIC}{Consistent AIC.A lower value indicates a better fit.}
#' \item{BIC}{Bayesian Information Criterion. A lower value indicates a better fit.}
#' }
#' @export

ItemFit <- function(U, Z, ell_A, nparam) {
  ## Item Fit Indices
  testlength <- ncol(U)
  nobs <- NROW(U)
  nrs <- colSums(Z)
  crr <- colSums(Z * U) / nrs
  const <- exp(-testlength)
  # Null model
  ell_N <- nobs * crr * log(crr + const) + nobs * (1 - crr) * log(1 - crr + const)
  # Benchmark model
  total <- rowSums(Z * U)
  totalList <- sort(unique(total))
  totalDist <- as.vector(table(total))
  ntotal <- length(totalList)
  ## Group Membership Profile Matrix
  MsG <- matrix(0, ncol = ntotal, nrow = nobs)
  for (i in 1:nobs) {
    MsG[i, which(totalList == total[i])] <- 1
  }
  ## PjG
  U1gj <- t(MsG) %*% (Z * U)
  U0gj <- replicate(testlength, totalDist, simplify = "matrix") - U1gj
  PjG <- U1gj / totalDist
  ell_B <- colSums(U1gj * log(PjG + const) + U0gj * log(1 - PjG + const))

  # chisquares
  chi_A <- 2 * (ell_B - ell_A)
  chi_B <- 2 * (ell_B - ell_N)

  # dfs
  df_B <- rep(ntotal - 1, testlength)
  df_A <- rep(ntotal - nparam, testlength)

  ItemFitIndices <- calcFitIndices(
    chi_A,
    chi_B,
    df_A,
    df_B,
    nobs
  )
  ItemFitIndices <- structure(
    c(list(
      model_log_like = ell_A,
      bench_log_like = ell_B,
      null_log_like = ell_N,
      model_Chi_sq = chi_A,
      null_Chi_sq = chi_B,
      model_df = df_A,
      null_df = df_B
    ), ItemFitIndices),
    class = c("exametrika", "ModelFit")
  )

  TestFitIndices <- calcFitIndices(
    sum(chi_A),
    sum(chi_B),
    sum(df_A),
    sum(df_B),
    nobs
  )
  TestFitIndices <- structure(
    c(list(
      model_log_like = sum(ell_A),
      bench_log_like = sum(ell_B),
      null_log_like = sum(ell_N),
      model_Chi_sq = sum(chi_A),
      null_Chi_sq = sum(chi_B),
      model_df = sum(df_A),
      null_df = sum(df_B)
    ), TestFitIndices),
    class = c("exametrika", "ModelFit")
  )

  ret <- list(
    item = ItemFitIndices,
    test = TestFitIndices
  )
  return(ret)
}

#' @title Model Fit Functions for test whole
#' @description
#' A general function that returns the model fit indices.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param ell_A log likelihood of this model
#' @param nparam number of parameters for this model
#' @return
#' \describe{
#' \item{model_log_like}{log likelihood of analysis model}
#' \item{bench_log_like}{log likelihood of benchmark model}
#' \item{null_log_like}{log likelihood of null model}
#' \item{model_Chi_sq}{Chi-Square statistics for analysis model}
#' \item{null_Chi_sq}{Chi-Square statistics for null model}
#' \item{model_df}{degrees of freedom of analysis model}
#' \item{null_df}{degrees of freedom of null model}
#' \item{NFI}{Normed Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{RFI}{Relative Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{IFI}{Incremental Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{TLI}{Tucker-Lewis Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{CFI}{Comparative Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{RMSEA}{Root Mean Square Error of Approximation. Smaller values closer to 0.0 indicate a better fit.}
#' \item{AIC}{Akaike Information Criterion. A lower value indicates a better fit.}
#' \item{CAIC}{Consistent AIC.A lower value indicates a better fit.}
#' \item{BIC}{Bayesian Information Criterion. A lower value indicates a better fit.}
#' }
#' @export

TestFit <- function(U, Z, ell_A, nparam) {
  nres <- colSums(Z)
  nitem <- NCOL(U)
  nobs <- NROW(U)
  crr <- colSums(Z * U) / nres
  const <- exp(-nitem)
  # Benchmark model
  total <- rowSums(Z * U)
  totalList <- sort(unique(total))
  totalDist <- as.vector(table(total))
  ntotal <- length(totalList)
  ## Group Membership Profile Matrix
  MsG <- matrix(0, ncol = nobs, nrow = ntotal)
  for (i in 1:nobs) {
    MsG[which(total[i] == totalList), i] <- 1
  }
  ## PjG
  U1gj <- MsG %*% (Z * U)
  U0gj <- replicate(nitem, totalDist, simplify = "matrix") - U1gj
  PjG <- U1gj / totalDist
  ell_B <- colSums(U1gj * log(PjG + const) + U0gj * log(1 - PjG + const))
  ell_B <- sum(ell_B)
  bench_nparm <- ntotal * nitem
  # Null model
  ell_N <- nobs * crr * log(crr + const) + nobs * (1 - crr) * log(1 - crr + const)
  ell_N <- sum(ell_N)
  null_nparam <- nitem
  df_B <- bench_nparm - null_nparam
  chi_B <- 2 * (ell_B - ell_N)
  # Analysis model
  chi_A <- 2 * (ell_B - ell_A)
  df_A <- bench_nparm - nparam

  indices <- calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)
  TestFitIndices <- structure(
    c(list(
      model_log_like = ell_A,
      bench_log_like = ell_B,
      null_log_like = ell_N,
      model_Chi_sq = chi_A,
      null_Chi_sq = chi_B,
      model_df = df_A,
      null_df = df_B
    ), indices),
    class = c("exametrika", "ModelFit")
  )
}

#' @title Model Fit Functions for saturated model
#' @description
#' A general function that returns the model fit indices.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param ell_A log likelihood of this model
#' @param nparam number of parameters for this model
#' @return
#' \describe{
#' \item{model_log_like}{log likelihood of analysis model}
#' \item{bench_log_like}{log likelihood of benchmark model}
#' \item{null_log_like}{log likelihood of null model}
#' \item{model_Chi_sq}{Chi-Square statistics for analysis model}
#' \item{null_Chi_sq}{Chi-Square statistics for null model}
#' \item{model_df}{degrees of freedom of analysis model}
#' \item{null_df}{degrees of freedom of null model}
#' \item{NFI}{Normed Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{RFI}{Relative Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{IFI}{Incremental Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{TLI}{Tucker-Lewis Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{CFI}{Comparative Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{RMSEA}{Root Mean Square Error of Approximation. Smaller values closer to 0.0 indicate a better fit.}
#' \item{AIC}{Akaike Information Criterion. A lower value indicates a better fit.}
#' \item{CAIC}{Consistent AIC.A lower value indicates a better fit.}
#' \item{BIC}{Bayesian Information Criterion. A lower value indicates a better fit.}
#' }
#' @export

TestFitSaturated <- function(U, Z, ell_A, nparam) {
  nres <- colSums(Z)
  nitem <- NCOL(U)
  nobs <- NROW(U)
  crr <- colSums(U) / nres
  const <- exp(-nitem)
  # Benchmark model
  ell_B <- 0
  npattern <- length(as.numeric(table(apply(U, 1, paste, collapse = ""))))
  bench_nparm <- npattern * nitem
  # Null model
  ell_N <- nobs * crr * log(crr + const) + nobs * (1 - crr) * log(1 - crr + const)
  ell_N <- sum(ell_N)
  null_nparam <- nitem
  df_B <- bench_nparm - null_nparam
  chi_B <- 2 * (ell_B - ell_N)
  # Analysis model
  chi_A <- 2 * (ell_B - ell_A)
  df_A <- bench_nparm - nparam

  indices <- calcFitIndices(chi_A, chi_B, df_A, df_B, nobs)
  TestFitIndices <- structure(
    c(list(
      model_log_like = ell_A,
      bench_log_like = ell_B,
      null_log_like = ell_N,
      model_Chi_sq = chi_A,
      null_Chi_sq = chi_B,
      model_df = df_A,
      null_df = df_B
    ), indices),
    class = c("exametrika", "ModelFit")
  )
}

#' @title calc Fit Indices
#' @description
#' A general function that returns the model fit indices.
#' @param chi_A chi-squares for this model
#' @param chi_B chi-squares for compared model
#' @param df_A degrees of freedom for this model
#' @param df_B degrees of freedom for compared model
#' @param nobs number of observations for Information criteria
#' @return
#' \describe{
#' \item{NFI}{Normed Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{RFI}{Relative Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{IFI}{Incremental Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{TLI}{Tucker-Lewis Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{CFI}{Comparative Fit Index. Lager values closer to 1.0 indicate a better fit.}
#' \item{RMSEA}{Root Mean Square Error of Approximation. Smaller values closer to 0.0 indicate a better fit.}
#' \item{AIC}{Akaike Information Criterion. A lower value indicates a better fit.}
#' \item{CAIC}{Consistent AIC.A lower value indicates a better fit.}
#' \item{BIC}{Bayesian Information Criterion. A lower value indicates a better fit.}
#' }
#' @export
#'

calcFitIndices <- function(chi_A, chi_B, df_A, df_B, nobs) {
  corrected_values <- pmax(chi_A - df_A, 0)
  RMSEA <- sqrt(corrected_values / (df_A * (nobs - 1)))

  AIC <- chi_A - 2 * df_A
  CAIC <- chi_A - df_A * log(nobs + 1)
  BIC <- chi_A - df_A * log(nobs)

  NFI <- (chi_A / chi_B)
  RFI <- ((chi_A / df_A) / (chi_B / df_B))
  IFI <- ((chi_A - df_A) / (chi_B - df_A))
  TLI <- ((chi_A / df_A) - 1) / ((chi_B / df_B) - 1)
  CFI <- ((chi_A - df_A) / (chi_B - df_B))

  ## Clip Function
  vars <- list(NFI, RFI, IFI, TLI, CFI)
  names <- c("NFI", "RFI", "IFI", "TLI", "CFI")

  for (i in 1:length(vars)) {
    assign(names[i], 1 - pmin(pmax(vars[[i]], 0), 1))
  }

  for (i in 1:length(chi_A)) {
    if (df_A[i] <= 0) {
      RFI[i] <- NaN
      TLI[i] <- NaN
      RMSEA[i] <- NaN
    }
  }

  return(list(
    NFI = NFI, RFI = RFI, IFI = IFI, TLI = TLI, CFI = CFI,
    RMSEA = RMSEA, AIC = AIC, CAIC = CAIC, BIC = BIC
  ))
}
