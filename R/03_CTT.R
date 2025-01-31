#' @title Alpha Coefficient
#' @description
#' This function computes Tau-Equivalent Measurement, also known as Cronbach's alpha coefficient, for a given data set.
#' @param x This should be a data matrix or a Covariance/Phi/Tetrachoric matrix.
#' @param Z This parameter represents a missing indicator matrix. It is only needed if 'x' is a data matrix.
#' @param w This parameter is an item weight vector. It is only required if 'x' is a data matrix.
#' @param na This parameter identifies the numbers or characters that should be treated as missing values when 'x' is a data matrix.
#' @return For a correlation/covariance matrix input, returns a single numeric value
#'   representing the alpha coefficient. For a data matrix input, returns a list with
#'   three components:
#'   \describe{
#'     \item{AlphaCov}{Alpha coefficient calculated from covariance matrix}
#'     \item{AlphaPhi}{Alpha coefficient calculated from phi coefficient matrix}
#'     \item{AlphaTetrachoric}{Alpha coefficient calculated from tetrachoric correlation matrix}
#'   }
#' @references Cronbach, L. J. (1951). Coefficient alpha and the internal structure of a test. Psychometrika, 16,297–334.
#' @importFrom stats cov
#' @export

AlphaCoefficient <- function(x, na = NULL, Z = NULL, w = NULL) {
  calcs <- function(V) {
    J <- NCOL(V)
    alphaConst <- J / (J - 1)
    tr <- sum(diag(V))
    alpha <- (sum(V) - tr) / (sum(V)) * alphaConst
    return(alpha)
  }

  if (NROW(x) != NCOL(x)) {
    if (class(x)[1] != "exametrika") {
      tmp <- dataFormat(data = x, na = na, Z = Z, w = w)
    } else {
      tmp <- x
    }
    V2 <- exametrika::PhiCoefficient(tmp)
    V3 <- exametrika::TetrachoricCorrelationMatrix(tmp)
    tmp$U[tmp$Z == 0] <- NA
    V1 <- stats::cov(tmp$U, use = "pairwise")
    alpha1 <- calcs(V1)
    alpha2 <- calcs(V2)
    alpha3 <- calcs(V3)
    return(list(
      AlphaCov = alpha1,
      AlphaPhi = alpha2,
      AlphaTetrachoric = alpha3
    ))
  } else {
    return(calcs(x))
  }
}

#' @title Alpha Coefficient if Item removed
#' @description
#' This function returns the alpha coefficient when the specified item is excluded.
#' @param x This should be a data matrix or a Covariance/Phi/Tetrachoric matrix.
#' @param Z This parameter represents a missing indicator matrix. It is only needed if 'x' is a data matrix.
#' @param w This parameter is an item weight vector. It is only required if 'x' is a data matrix.
#' @param na This parameter identifies the numbers or characters that should be treated as missing values when 'x' is a data matrix.
#' @param delItem Specify the item to be deleted. If NULL, calculations are performed for all cases.
#' @importFrom stats cov

AlphaIfDel <- function(x, delItem = NULL, na = NULL, Z = NULL, w = NULL) {
  calcs <- function(V, delItem) {
    J <- NCOL(V)
    if (is.null(delItem)) {
      alphaIfDel <- vector(length = J)
      for (j in 1:J) {
        mat <- V[-j, -j]
        alphaIfDel[j] <- AlphaCoefficient(mat)
      }
      return(alphaIfDel)
    } else {
      mat <- V[-delItem, -delItem]
      ret <- AlphaCoefficient(mat)
      return(ret)
    }
  }

  if (class(x)[1] != "exametrika") {
    tmp <- dataFormat(data = x, na = na, Z = Z, w = w)
  } else {
    tmp <- x
  }

  if (NROW(x) != NCOL(x)) {
    V2 <- exametrika::PhiCoefficient(tmp)
    V3 <- exametrika::TetrachoricCorrelationMatrix(tmp)
    tmp$U[tmp$Z == 0] <- NA
    V1 <- stats::cov(tmp$U, use = "pairwise")
    alpha1 <- calcs(V1, delItem)
    alpha2 <- calcs(V2, delItem)
    alpha3 <- calcs(V3, delItem)
    return(list(
      AlphaCov = alpha1,
      AlphaPhi = alpha2,
      AlphaTetrachoric = alpha3
    ))
  }
}

#' @title Omega Coefficient
#' @description
#' This function computes Tau-Congeneric Measurement, also known as McDonald's tau coefficient, for a given data set.
#' @references McDonald, R. P. (1999). Test theory: A unified treatment. Erlbaum.
#' @param x This should be a data matrix or a Covariance/Phi/Tetrachoric matrix.
#' @param Z This parameter represents a missing indicator matrix. It is only needed if 'x' is a data matrix.
#' @param w This parameter is an item weight vector. It is only required if 'x' is a data matrix.
#' @param na This parameter identifies the numbers or characters that should be treated as missing values when 'x' is a data matrix.
#' @return For a correlation/covariance matrix input, returns a single numeric value
#'   representing the omega coefficient. For a data matrix input, returns a list with
#'   three components:
#'   \describe{
#'     \item{OmegaCov}{Omega coefficient calculated from covariance matrix}
#'     \item{OmegaPhi}{Omega coefficient calculated from phi coefficient matrix}
#'     \item{OmegaTetrachoric}{Omega coefficient calculated from tetrachoric correlation matrix}
#'   }
#' @importFrom stats cov runif
#' @export

OmegaCoefficient <- function(x, na = NULL, Z = NULL, w = NULL) {
  calcs <- function(V) {
    J <- NCOL(V)
    offdiagonal <- matrix(1, ncol = ncol(V), nrow = nrow(V)) - diag(nrow = nrow(V))
    lam <- runif(J)
    # Define the function to minimize
    objective_function <- function(lam) {
      lammat <- outer(lam, lam)
      sum(offdiagonal * (lammat - V)^2)
    }
    # optimize
    result <- optim(lam, objective_function, method = "BFGS")
    # Error check
    if (result$convergence != 0) {
      stop(result$message)
    }
    lamest <- result$par
    Numerator <- sum(lamest)^2
    Denominator <- Numerator + sum(diag(V - outer(lamest, lamest)))
    omega <- Numerator / Denominator
    return(omega)
  }

  if (NROW(x) != NCOL(x)) {
    if (class(x)[1] != "exametrika") {
      tmp <- dataFormat(data = x, na = na, Z = Z, w = w)
    } else {
      tmp <- x
    }
    V2 <- exametrika::PhiCoefficient(tmp)
    V3 <- exametrika::TetrachoricCorrelationMatrix(tmp)
    tmp$U[tmp$Z == 0] <- NA
    V1 <- stats::cov(tmp$U, use = "pairwise")
    omega1 <- calcs(V1)
    omega2 <- calcs(V2)
    omega3 <- calcs(V3)
    return(list(
      OmegaCov = omega1,
      OmegaPhi = omega2,
      OmegaTetrachoric = omega3
    ))
  } else {
    return(calcs(x))
  }
}


#' @title Classical Test Theory
#' @description
#' This function calculates the overall alpha and omega coefficients for
#' the given data matrix. It also computes the alpha coefficient for
#' each item, assuming that item is excluded.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return Returns a list of class c("exametrika", "CTT") containing two data frames:
#'   \describe{
#'     \item{Reliability}{A data frame with overall reliability coefficients
#'       (Alpha and Omega) calculated using different correlation matrices
#'       (Covariance, Phi, and Tetrachoric)}
#'     \item{ReliabilityExcludingItem}{A data frame showing alpha coefficients
#'       when each item is excluded, calculated using different correlation matrices}
#'   }
#' @examples
#' \donttest{
#' # using sample dataset
#' CTT(J15S500)
#' }
#' @export

CTT <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  alphaAll <- AlphaCoefficient(tmp)
  omegaAll <- OmegaCoefficient(tmp)
  eachAlpha <- AlphaIfDel(tmp)

  vec1 <- as.vector(unlist(alphaAll))
  vec2 <- as.vector(unlist(omegaAll))
  df <- data.frame(list(
    name = c(
      "Alpha(Covariance)",
      "Alpha(Phi)",
      "Alpha(Tetrachoric)",
      "Omega(Covariance)",
      "Omega(Phi)",
      "Omega(Tetrachoric)"
    ),
    value = c(vec1, vec2)
  ))
  df2 <- data.frame(list(
    IfDeleted = tmp$ItemLabel,
    "Alpha.Covariance" = eachAlpha$AlphaCov,
    "Alpha.Phi" = eachAlpha$AlphaPhi,
    "Alpha.Tetrachoric" = eachAlpha$AlphaTetrachoric
  ))
  ret <- list(Reliability = df, ReliabilityExcludingItem = df2)
  structure(ret, class = c("exametrika", "CTT"))
}
