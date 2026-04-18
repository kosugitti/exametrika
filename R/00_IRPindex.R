#' @title IRP/FRP Profile Shape Indices (Kumagai, 2007)
#' @description
#' Computes six indices that quantitatively describe the shape of an
#' IRP (Item Reference Profile) or FRP (Field Reference Profile).
#' Used for both binary and polytomous Biclustering.
#'
#' ## Input matrix specification
#' - Rows: fields (or items), Columns: latent classes/ranks
#' - Cell values in \[0, 1\] (probabilities or normalized expected scores)
#'
#' ## Six indices
#'
#' ### Location parameters
#' - **Beta**: Class number whose value is closest to 0.5; the "center" of the profile.
#'   `Beta_f = argmin_c |FRP(f,c) - 0.5|`
#' - **B**: Profile value at the Beta position. B close to 0.5 indicates ideal location.
#'   `B_f = FRP(f, Beta_f)`
#'
#' ### Slope parameters
#' - **Alpha**: Class transition where the lag (adjacent-class difference) is largest;
#'   the steepest ascent point. `Alpha_f = argmax_c lag(f,c) - 1`
#'   (Alpha=1 means the steepest rise is from class 1 to class 2.)
#' - **A**: Maximum lag value; magnitude of the steepest slope.
#'   `A_f = max_c lag(f,c)` where `lag(f,c) = FRP(f,c) - FRP(f,c-1)`
#'
#' ### Monotonicity indices
#' - **C**: Sum of negative lags; amount of monotonicity violation.
#'   C = 0 means the profile is perfectly monotone increasing.
#'   `C_f = sum of lag(f,c)` (only terms where lag < 0)
#' - **Gamma**: Proportion of non-monotone transitions. Positive only when C != 0.
#'   `Gamma_f = (number of negative lags - 1) / (ncls - 1)`
#'
#' ## Relationship to Ordinal Alignment Conditions
#' - **SOAC** (Strongly Ordinal Alignment Condition): C = 0 for all fields
#'   and TRP is monotone increasing; all profiles are perfectly monotone.
#' - **WOAC** (Weakly Ordinal Alignment Condition): TRP is monotone increasing
#'   but some fields have C != 0; monotone overall with local reversals.
#'
#' ## Use with polytomous Biclustering
#' For polytomous data (ordinal Biclustering), the 3D category probability
#' array BCRM is converted to expected scores and normalized to \[0,1\].
#' See the FRPIndex computation in \code{Biclustering.ordinal} for details.
#'
#' @param IRP Numeric matrix. Rows = fields (or items), Columns = classes/ranks.
#'   Values in \[0,1\]. For binary Biclustering, pass correct-response probabilities;
#'   for polytomous Biclustering, pass normalized expected scores
#'   `(E\[score\] - 1) / (maxQ - 1)`.
#' @return data.frame with rows for fields and columns Alpha, A, Beta, B, Gamma, C.
#'
#' @references
#' Kumagai, K. (2007). IRP indices for understanding profile shapes.
#'
#' @noRd

IRPindex <- function(IRP) {
  # Beta: class position closest to 0.5
  Beta <- apply(abs(IRP - 0.5), 1, which.min)
  NR <- NROW(IRP)
  NC <- NCOL(IRP)
  # B: profile value at the Beta position
  B <- IRP[cbind(1:NR, Beta)]
  A <- Alpha <- rep(NA, NR)
  C <- Gamma <- rep(0, NR)
  for (i in 1:NR) {
    vec <- IRP[i, ]
    # lag: difference between adjacent classes (c minus c-1)
    lags <- vec - c(NA, vec[1:(NC - 1)])
    # A: maximum lag (steepest ascent)
    A[i] <- max(lags, na.rm = T)
    # Alpha: position of maximum lag (source class number)
    Alpha[i] <- which.max(lags) - 1
    # C: sum of negative lags (monotonicity violation; 0 = perfectly monotone)
    C[i] <- sum(lags[lags < 0], na.rm = T)
    if (C[i] != 0) {
      # Gamma: proportion of non-monotone transitions
      Gamma[i] <- (length(lags[lags < 0]) - 1) / (NC - 1)
    }
  }
  ret <- as.data.frame(cbind(Alpha, A, Beta, B, Gamma, C))
  return(ret)
}
