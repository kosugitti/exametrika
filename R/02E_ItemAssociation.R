# Item association / joint-response measures (JointSampleSize, JCRR, JSR, CSR,
# CCRR, ItemLift, MutualInformation, PhiCoefficient), correlation-matrix
# helpers (tetrachoric/polychoric/polyserial), and Dimensionality.
# Split out of R/02_TestItemFunctions.R (2026-07-01) for maintainability;
# no logic changed, only moved.

#' @title Joint Sample Size
#' @description
#' The joint sample size is a matrix whose elements are the number of
#' individuals who responded to each pair of items.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return Returns a matrix of class c("exametrika", "matrix") where each element (i,j)
#'   represents the number of students who responded to both item i and item j. The
#'   diagonal elements represent the total number of responses for each item.
#' @export
JointSampleSize <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("JointSampleSize")
}
#' @rdname JointSampleSize
#' @export
JointSampleSize.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  JointSampleSize.binary(U, na, Z, w)
}

#' @rdname JointSampleSize
#' @export
JointSampleSize.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  S_jk <- t(U$Z) %*% U$Z
  structure(S_jk, class = c("exametrika", "matrix"))
}

#' @title Joint Correct Response Rate
#' @description
#' The joint correct response rate (JCRR) is the rate of students who passed
#' both items. This function is applicable only to binary response data.
#' For non-binary data, it will automatically redirect to the JSR function
#' with an appropriate message.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A matrix of joint correct response rates with exametrika class.
#' Each element (i,j) represents the proportion of students who correctly
#' answered both items i and j.
#' @examples
#' # example code
#' # Calculate JCRR using sample dataset J5S10
#' JCRR(J5S10)
#' @export
JCRR <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("JCRR")
}

#' @rdname JCRR
#' @export
JCRR.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = JCRR.binary(U, na = NULL, Z = NULL, w = NULL),
      "rated" = JCRR.nominal(U, na = NULL, Z = NULL, w = NULL),
      "ordinal" = JCRR.nominal(U, na = NULL, Z = NULL, w = NULL),
      "nominal" = JCRR.nominal(U, na = NULL, Z = NULL, w = NULL)
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    JCRR(U)
  }
}

#' @rdname JCRR
#' @export
JCRR.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  P_J <- t(U$Z * U$U) %*% (U$Z * U$U) / (t(U$Z) %*% U$Z)
  structure(P_J, class = c("exametrika", "matrix"))
}

#' @rdname JCRR
#' @export
JCRR.nominal <- function(U, na = NULL, Z = NULL, w = NULL) {
  message("JCRR is for binary data only. Using Joint Selection Rate for your polytomous data instead.")
  JSR(U)
}

#' @title Joint Selection Rate
#' @description Calculate the Joint Selection Rate (JSR) for polytomous data.
#'   JSR measures the proportion of respondents who selected specific category
#'   combinations between pairs of items. For each pair of items (j,k),
#'   it returns a contingency table showing the joint probability of selecting
#'   each category combination.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @return A list of Joint Selection Rate matrices for each item pair.
#' @examples
#' # example code
#' # Calculate JCRR using sample dataset J5S1000
#' JSR(J5S1000)
#' @export
JSR <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type == "binary") {
    message("JSR is for non-binary data only. Using Joint Correct Response Rate for your binary data instead.")
    return(JCRR(U))
  }
  nitems <- NCOL(U$Q)
  ncat <- U$categories
  U$Q[U$Z == 0] <- NA
  JSR <- vector("list", nitems)
  for (j in 1:nitems) {
    JSR[[j]] <- vector("list", nitems)
    for (k in 1:nitems) {
      tbl <- table(U$Q[, j], U$Q[, k], useNA = "no") / sum(table(U$Q[, j], U$Q[, k], useNA = "no"))
      colnames(tbl) <- unlist(U$CategoryLabel[k])
      rownames(tbl) <- unlist(U$CategoryLabel[j])
      JSR[[j]][[k]] <- tbl
    }
  }
  return(JSR)
}

#' @title Conditional Selection Rate
#' @description Calculate the Conditional Selection Rate (CSR) for polytomous data.
#'   CSR measures the proportion of respondents who selected a specific category
#'   in item K, given that they selected a particular category in item J.
#' @details
#'   The function returns a nested list structure CSR, where \code{CSR[[j]][[k]]} contains
#'   a matrix of conditional probabilities. In this matrix, the element at row l and
#'   column m represents P(K=m|J=l), which is the probability of selecting category m
#'   for item K, given that category l was selected for item J.
#'
#'   Mathematically, for each cell (l,m) in the \code{CSR[[j]][[k]]} matrix:
#'   \code{CSR[[j]][[k]][l,m] = P(Item K = category m | Item J = category l)}
#'
#'   This is calculated as the number of respondents who selected both category l for
#'   item J and category m for item K, divided by the total number of respondents who
#'   selected category l for item J.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @return A list of Joint Selection Rate matrices for each item pair.
#' @examples
#' # example code
#' # Calculate CSR using sample dataset J5S1000
#' CSR(J5S1000)
#'
#' # Extract the conditional selection rates from item 1 to item 2
#' csr_1_2 <- CSR(J5S1000)[[1]][[2]]
#' # This shows the probability of selecting each category in item 2
#' # given that a specific category was selected in item 1
#' @export
CSR <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type == "binary") {
    message("CSR is for non-binary data only. Using Conditional Correct Response Rate for your binary data instead.")
    return(CCRR(U))
  }
  nitems <- NCOL(U$Q)
  ncat <- U$categories
  U$Q[U$Z == 0] <- NA
  CSR <- vector("list", nitems)
  for (j in 1:nitems) {
    CSR[[j]] <- vector("list", nitems)
    for (k in 1:nitems) {
      tbl <- table(U$Q[, j], U$Q[, k], useNA = "no")
      ccsr <- matrix(NA, nrow = ncat[j], ncol = ncat[k])
      for (l in 1:ncat[j]) {
        ccsr[l, ] <- tbl[l, ] / rowSums(tbl)[l]
      }
      rownames(ccsr) <- U$CategoryLabel[[j]]
      colnames(ccsr) <- U$CategoryLabel[[k]]
      CSR[[j]][[k]] <- ccsr
    }
  }
  return(CSR)
}

#' @title Conditional Correct Response Rate
#' @description
#' The conditional correct response rate (CCRR) represents the ratio of the students
#' who passed Item C (consequent item) to those who passed Item A (antecedent item).
#' This function is applicable only to binary response data.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A matrix of conditional correct response rates with exametrika class.
#' Each element (i,j) represents the probability of correctly answering item j
#' given that item i was answered correctly.
#' @examples
#' # example code
#' # Calculate CCRR using sample dataset J5S10
#' CCRR(J5S10)
#' @export

CCRR <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("CCRR")
}

#' @rdname CCRR
#' @export
CCRR.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = CCRR.binary(U, na = NULL, Z = NULL, w = NULL),
      "rated" = CCRR.nominal(U, na = NULL, Z = NULL, w = NULL),
      "ordinal" = CCRR.nominal(U, na = NULL, Z = NULL, w = NULL),
      "nominal" = CCRR.nominal(U, na = NULL, Z = NULL, w = NULL)
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    CCRR(U)
  }
}

#' @rdname CCRR
#' @export
CCRR.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  Z <- U$Z
  OneJ <- rep(1, ncol(U$U))
  Pj <- JCRR(U)
  p <- crr(U)
  P_C <- Pj / (p %*% t(OneJ))
  structure(P_C, class = c("exametrika", "matrix"))
}

#' @rdname CCRR
#' @export
CCRR.nominal <- function(U, na = NULL, Z = NULL, w = NULL) {
  message("CCRR is for binary data only. Using Conditional Selection Rate for your polytomous data instead.")
  CSR(U)
}
#' @title Item Lift
#' @description
#' The lift is a commonly used index in a POS data analysis.
#' The item lift of Item k to Item j is defined as follow:
#' \eqn{ l_{jk} = \frac{p_{k\mid j}}{p_k} }
#' This function is applicable only to binary response data.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A matrix of item lift values with exametrika class.
#' Each element (j,k) represents the lift value of item k given item j,
#' which indicates how much more likely item k is to be correct given that
#' item j was answered correctly.
#' @references Brin, S., Motwani, R., Ullman, J., & Tsur, S. (1997). Dynamic itemset counting and
#' implication rules for market basket data. In Proceedings of ACM SIGMOD International Conference
#' on Management of Data (pp. 255–264). https://dl.acm.org/doi/10.1145/253262.253325
#' @examples
#' # example code
#' # Calculate ItemLift using sample dataset J5S10
#' ItemLift(J5S10)
#' @export
ItemLift <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("ItemLift")
}

#' @rdname ItemLift
#' @export
ItemLift.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "ItemLift")
  }
  ItemLift.binary(U, na, Z, w)
}

#' @rdname ItemLift
#' @export
ItemLift.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  OneJ <- rep(1, ncol(U$U))
  Pc <- CCRR(U)
  p <- crr(U)
  P_L <- Pc / (OneJ %*% t(p))
  structure(P_L, class = c("exametrika", "matrix"))
}

#' @title Mutual Information
#' @description
#' Mutual Information is a measure that represents the degree of interdependence
#' between two items. This function is applicable to both binary and polytomous response data.
#' The measure is calculated using the joint probability distribution of responses
#' between item pairs and their marginal probabilities.
#' @details
#' For binary data, the following formula is used:
#' \deqn{
#' MI_{jk} = p_{00} \log_2 \frac{p_{00}}{(1-p_j)(1-p_k)} + p_{01} \log_2 \frac{p_{01}}{(1-p_j)p_k}
#'  + p_{10} \log_2 \frac{p_{10}}{p_j(1-p_k)} + p_{11} \log_2 \frac{p_{11}}{p_jp_k}
#' }
#' Where:
#' \itemize{
#'   \item \eqn{p_{00}} is the joint probability of incorrect responses to both items j and k
#'   \item \eqn{p_{01}} is the joint probability of incorrect response to item j and correct to item k
#'   \item \eqn{p_{10}} is the joint probability of correct response to item j and incorrect to item k
#'   \item \eqn{p_{11}} is the joint probability of correct responses to both items j and k
#' }
#'
#' For polytomous data, the following formula is used:
#' \deqn{MI_{jk} = \sum_{j=1}^{C_j}\sum_{k=1}^{C_k}p_{jk}\log \frac{p_{jk}}{p_{j.}p_{.k}}}
#'
#' The base of the logarithm can be the number of rows, number of columns, min(rows, columns),
#' base-10 logarithm, natural logarithm (e), etc.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @param base The base for the logarithm. Default is 2. For polytomous data,
#'   you can use "V" to set the base to min(rows, columns), "e" for natural logarithm (base e),
#'   or any other number to use that specific base.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A matrix of mutual information values with exametrika class.
#' Each element (i,j) represents the mutual information between items i and j,
#' measured in bits. Higher values indicate stronger interdependence between items.
#' @examples
#' # example code
#' # Calculate Mutual Information using sample dataset J15S500
#' MutualInformation(J15S500)
#' @export
MutualInformation <- function(U, na = NULL, Z = NULL, w = NULL, base = 2) {
  UseMethod("MutualInformation")
}

#' @rdname MutualInformation
#' @export
MutualInformation.default <- function(U, na = NULL, Z = NULL, w = NULL, base = 2) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = MutualInformation.binary(U, na, Z, w, base = base),
      "rated" = MutualInformation.ordinal(U, na, Z, w, base = base),
      "ordinal" = MutualInformation.ordinal(U, na, Z, w, base = base),
      "nominal" = MutualInformation.ordinal(U, na, Z, w, base = base)
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    MutualInformation(U, na, Z, w, base = base)
  }
}

#' @rdname MutualInformation
#' @export
MutualInformation.binary <- function(U, na = NULL, Z = NULL, w = NULL, base = 2) {
  p <- crr(U)
  # Calculate joint response matrix
  S <- list()
  S$S_11 <- t(U$Z * U$U) %*% (U$Z * U$U)
  S$S_10 <- t(U$Z * U$U) %*% (U$Z * (1 - U$U))
  S$S_01 <- t(U$Z * (1 - U$U)) %*% (U$Z * U$U)
  S$S_00 <- t(U$Z * (1 - U$U)) %*% (U$Z * (1 - U$U))

  # Calculate joint probability matrix
  P <- lapply(S, function(x) x / (t(U$Z) %*% U$Z))

  # Calculate lift matrix
  L <- list()
  L$L_11 <- P$S_11 / (p %*% t(p))
  L$L_10 <- P$S_10 / (p %*% t(1 - p))
  L$L_01 <- P$S_01 / ((1 - p) %*% t(p))
  L$L_00 <- P$S_00 / ((1 - p) %*% t(1 - p))

  # Calculate mutual information
  MI <- P$S_00 * log(L$L_00, base = base) +
    P$S_01 * log(L$L_01, base = base) +
    P$S_10 * log(L$L_10, base = base) +
    P$S_11 * log(L$L_11, base = base)

  # Adjust diagonal elements
  diag(MI) <- diag(P$S_00 * log(L$L_00, base = base) +
    P$S_11 * log(L$L_11, base = base))

  structure(MI, class = c("exametrika", "matrix"))
}

#' @rdname MutualInformation
#' @export
MutualInformation.ordinal <- function(U, na = NULL, Z = NULL, w = NULL, base = 2) {
  if (is.character(base)) {
    if (base != "V" & base != "v" & base != "e") {
      stop("The base of the logarithm must be a number, 'V', or 'e'.")
    }
  }
  nitems <- NCOL(U$Z)
  ncat <- apply(U$Q, 2, max)
  mat <- matrix(ncol = nitems, nrow = nitems)
  for (i in 1:nitems) {
    for (j in 1:nitems) {
      x <- U$Q[, i]
      y <- U$Q[, j]
      x[x == -1] <- NA
      y[y == -1] <- NA
      pairwise <- !is.na(x + y)
      x <- x[pairwise]
      y <- y[pairwise]
      tbl <- table(x, y) / sum(length(x))

      if (base == "V" | base == "v") {
        tei <- min(ncat[i], ncat[j])
      } else if (base == "e") {
        tei <- exp(1)
      } else {
        tei <- as.numeric(base)
      }
      mi <- 0
      cSum <- colSums(tbl)
      rSum <- rowSums(tbl)
      for (k in 1:NROW(tbl)) {
        for (m in 1:NCOL(tbl)) {
          mi <- mi + (tbl[k, m] * (log(tbl[k, m] / rSum[k] / cSum[m], base = tei)))
        }
      }
      mat[i, j] <- mi
    }
  }
  return(mat)
}

#' @title Phi-Coefficient
#' @description
#' The phi coefficient is the Pearson's product moment correlation coefficient
#' between two binary items. This function is applicable only to binary response data.
#' The coefficient ranges from -1 to 1, where 1 indicates perfect positive correlation,
#' -1 indicates perfect negative correlation, and 0 indicates no correlation.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A matrix of phi coefficients with exametrika class.
#' Each element (i,j) represents the phi coefficient between items i and j.
#' The matrix is symmetric with ones on the diagonal.
#' @examples
#' # example code
#' # Calculate Phi-Coefficient using sample dataset J15S500
#' PhiCoefficient(J15S500)
#' @export
PhiCoefficient <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("PhiCoefficient")
}

#' @rdname PhiCoefficient
#' @export
PhiCoefficient.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "PhiCoefficient")
  }
  PhiCoefficient.binary(U, na, Z, w)
}

#' @rdname PhiCoefficient
#' @export
PhiCoefficient.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  p <- crr(U)
  OneS <- rep(1, nrow(U$U))
  OneJ <- rep(1, ncol(U$U))

  # Calculate centered cross-product matrix
  C <- t(U$Z * (U$U - OneS %*% t(p))) %*%
    (U$Z * (U$U - OneS %*% t(p))) /
    (t(U$Z) %*% U$Z - OneJ %*% t(OneJ))

  # Calculate standard deviations
  v <- diag(C)

  # Calculate correlation matrix
  phi <- C / sqrt(v) %*% t(sqrt(v))

  structure(phi, class = c("exametrika", "matrix"))
}

#' @title Tetrachoric Correlation
#' @description
#' Tetrachoric Correlation is superior to the phi coefficient as a measure of the
#' relation of an item pair. See Divgi, 1979; Olsson, 1979;Harris, 1988.
#' @references Divgi, D. R. (1979). Calculation of the tetrachoric correlation coefficient.
#' Psychometrika, 44, 169–172.
#' @references Olsson, U. (1979). Maximum likelihood estimation of the polychoric correlation
#'  coefficient. Psychometrika,44, 443–460.
#' @references Harris, B. (1988). Tetrachoric correlation coefficient. In L. Kotz, & N. L. Johnson
#'  (Eds.), Encyclopedia of statistical sciences (Vol. 9, pp. 223–225). Wiley.
#' @param x binary vector x
#' @param y binary vector y
#' @importFrom mvtnorm pmvnorm
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats optimize
#' @return Returns a single numeric value of class "exametrika" representing the
#'   tetrachoric correlation coefficient between the two binary variables. The value
#'   ranges from -1 to 1, where:
#'   \itemize{
#'     \item 1 indicates perfect positive correlation
#'     \item -1 indicates perfect negative correlation
#'     \item 0 indicates no correlation
#'   }
#' @export

tetrachoric <- function(x, y) {
  pairwise <- !is.na(x + y)
  # count 2x2 cells
  x.fac <- factor(x[pairwise], levels = 0:1)
  y.fac <- factor(y[pairwise], levels = 0:1)
  tbl <- table(x.fac, y.fac)
  S00 <- tbl[1, 1]
  S10 <- tbl[2, 1]
  S01 <- tbl[1, 2]
  S11 <- tbl[2, 2]
  if (S00 == 0) {
    S00 <- 0.5
  }
  if (S10 == 0) {
    S10 <- 0.5
  }
  if (S01 == 0) {
    S01 <- 0.5
  }
  if (S11 == 0) {
    S11 <- 0.5
  }
  # calcs tau
  tau_j <- qnorm(1 - mean(x, na.rm = TRUE))
  tau_k <- qnorm(1 - mean(y, na.rm = TRUE))
  ## BVN funcs
  BVN11 <- function(rho, tau_j, tau_k) {
    pmvnorm(upper = c(-tau_j, -tau_k), corr = matrix(c(1, rho, rho, 1), ncol = 2))
  }
  BVN01 <- function(rho, tau_j, tau_k) {
    pnorm(tau_j) - pmvnorm(upper = c(tau_j, tau_k), corr = matrix(c(1, rho, rho, 1), ncol = 2))
  }
  BVN10 <- function(rho, tau_j, tau_k) {
    pnorm(tau_k) - pmvnorm(upper = c(tau_j, tau_k), corr = matrix(c(1, rho, rho, 1), ncol = 2))
  }
  BVN00 <- function(rho, tau_j, tau_k) {
    pmvnorm(upper = c(tau_j, tau_k), corr = matrix(c(1, rho, rho, 1), ncol = 2))
  }
  ## LL
  log_likelihood_phi <- function(rho, tau_j, tau_k, S00, S11, S10, S01) {
    S00 * log(BVN00(rho, tau_j, tau_k)) + S01 * log(BVN01(rho, tau_j, tau_k)) +
      S10 * log(BVN10(rho, tau_j, tau_k)) + S11 * log(BVN11(rho, tau_j, tau_k))
  }
  ret <- optim(
    par = 0, # initial value
    fn = function(x) {
      -log_likelihood_phi(rho = x, tau_j, tau_k, S00, S11, S10, S01)
    },
    lower = -1, # lower limit
    upper = 1, # upper limit
    method = "Brent" # one-dimensional optimization method
  )
  ret <- structure(ret$par, class = c("exametrika"))
  return(ret)
}

#' @title Polychoric Correlation
#' @description
#' Calculate the polychoric correlation coefficient between two polytomous (categorical ordinal) variables.
#' Polychoric correlation estimates the correlation between two theorized normally distributed
#' continuous latent variables from two observed ordinal variables.
#'
#' @param x A polytomous vector (categorical ordinal variable)
#' @param y A polytomous vector (categorical ordinal variable)
#' @return The polychoric correlation coefficient between x and y
#' @details
#' This function handles missing values (coded as -1 or NA) using pairwise deletion.
#' The estimation uses maximum likelihood approach with Brent's method for optimization.
#' @examples
#' # Example with simulated data
#' set.seed(123)
#' x <- sample(1:5, 100, replace = TRUE)
#' y <- sample(1:4, 100, replace = TRUE)
#' polychoric(x, y)
#'
#' @export
polychoric <- function(x, y) {
  # Use fast C++ implementation
  result <- polychoric_cpp(as.integer(x), as.integer(y))

  # Check for convergence issues
  if (is.na(result)) {
    stop("Failed to converge when calculating polychoric correlation. Try with different initial values or check your data.")
  }

  return(result)
}


#' @title Polyserial Correlation
#' @description Calculates the polyserial correlation coefficient between a continuous variable and an ordinal variable.
#' @details This function implements Olsson et al.'s ad hoc method for estimating the polyserial correlation
#' coefficient. The method assumes that the continuous variable follows a normal distribution and
#' that the ordinal variable is derived from an underlying continuous normal variable through
#' thresholds.
#' @param x A numeric vector representing the continuous variable.
#' @param y A numeric vector representing the ordinal variable (must be integer values).
#' @return A numeric value representing the estimated polyserial correlation coefficient.
#' @references U.Olsson, F.Drasgow, and N.Dorans (1982).
#' The polyserial correlation coefficient. Psychometrika, 47,337-347.
#' @examples
#' n <- 300
#' x <- rnorm(n)
#' y <- sample(1:5, size = n, replace = TRUE)
#' polyserial(x, y)
#' @importFrom stats dnorm
#' @export
#'
polyserial <- function(x, y) {
  x[x == -1] <- NA
  y[y == -1] <- NA
  pairwise <- !is.na(x + y)
  x <- x[pairwise]
  y <- y[pairwise]
  nobs <- length(y)
  ncat <- max(as.numeric(as.factor(y)))

  tbl <- tabulate(y)
  freq <- tbl / sum(tbl)
  cum_freq <- cumsum(freq)

  thresholds <- qnorm(cum_freq)[-ncat]
  tau <- dnorm(thresholds)
  sum_tau <- sum(tau)

  rxy <- cor(x, y)
  sd_y <- sqrt(var(y) * (nobs - 1) / nobs)
  rho <- rxy * sd_y / sum_tau
  rho <- pmin(pmax(rho, -1), 1)
  return(rho)
}

#' @title Polychoric Correlation Matrix
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @return A matrix of polychoric correlations with exametrika class.
#' Each element (i,j) represents the polychoric correlation between items i and j.
#' The matrix is symmetric with ones on the diagonal.
#' @examples
#' \donttest{
#' # example code
#' PolychoricCorrelationMatrix(J5S1000)
#' }
#' @export
#'
PolychoricCorrelationMatrix <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("PolychoricCorrelationMatrix")
}

#' @rdname PolychoricCorrelationMatrix
#' @export
PolychoricCorrelationMatrix.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  if (tmp$response.type != "ordinal") {
    response_type_error(tmp$response.type, "PolychoricCorrelationMatrix")
  }
  PolychoricCorrelationMatrix.ordinal(tmp, na, Z, w)
}

#' @rdname PolychoricCorrelationMatrix
#' @export
PolychoricCorrelationMatrix.ordinal <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  tmp$Q[tmp$Z == 0] <- NA

  # Use fast C++ matrix implementation
  ret.mat <- polychoric_matrix_cpp(tmp$Q)
  rownames(ret.mat) <- tmp$ItemLabel
  colnames(ret.mat) <- tmp$ItemLabel

  return(ret.mat)
}

#' @title Tetrachoric Correlation Matrix
#' @description
#' Calculates the matrix of tetrachoric correlations between all pairs of items.
#' Tetrachoric Correlation is superior to the phi coefficient as a measure of the
#' relation of an item pair. This function is applicable only to binary response data.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A matrix of tetrachoric correlations with exametrika class.
#' Each element (i,j) represents the tetrachoric correlation between items i and j.
#' The matrix is symmetric with ones on the diagonal.
#' @examples
#' \donttest{
#' # example code
#' TetrachoricCorrelationMatrix(J15S500)
#' }
#' @export
TetrachoricCorrelationMatrix <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("TetrachoricCorrelationMatrix")
}

#' @rdname TetrachoricCorrelationMatrix
#' @export
TetrachoricCorrelationMatrix.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    U <- dataFormat(U, na = na, Z = Z, w = w)
  }
  if (U$response.type != "binary") {
    response_type_error(U$response.type, "TetrachoricCorrelationMatrix")
  }
  TetrachoricCorrelationMatrix.binary(U, na, Z, w)
}

#' @rdname TetrachoricCorrelationMatrix
#' @export
TetrachoricCorrelationMatrix.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  tmp$U[tmp$Z == 0] <- NA
  Una <- tmp$U
  m <- ncol(Una)
  mat <- matrix(NA, ncol = m, nrow = m)
  colnames(mat) <- tmp$ItemLabel
  rownames(mat) <- tmp$ItemLabel
  for (i in 1:(m - 1)) {
    for (j in (i + 1):m) {
      x <- Una[, i]
      y <- Una[, j]
      mat[i, j] <- tetrachoric(x, y)
      mat[j, i] <- mat[i, j]
    }
  }
  diag(mat) <- 1
  structure(mat, class = c("exametrika", "matrix"))
}


#' @title Dimensionality
#' @description
#' The dimensionality is the number of components
#' the test is measuring.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#'   it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#'   observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @return Returns a list of class c("exametrika", "Dimensionality") containing:
#'   \describe{
#'     \item{Component}{Sequence of component numbers}
#'     \item{Eigenvalue}{Eigenvalues of the tetrachoric correlation matrix}
#'     \item{PerOfVar}{Percentage of variance explained by each component}
#'     \item{CumOfPer}{Cumulative percentage of variance explained}
#'   }
#'
#' @export
Dimensionality <- function(U, na = NULL, Z = NULL, w = NULL) {
  UseMethod("Dimensionality")
}

#' @rdname Dimensionality
#' @export
Dimensionality.default <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (inherits(U, "exametrika")) {
    switch(U$response.type,
      "binary" = Dimensionality.binary(U, na = NULL, Z = NULL, w = NULL),
      "rated" = Dimensionality.rated(U, na = NULL, Z = NULL, w = NULL),
      "ordinal" = Dimensionality.ordinal(U, na = NULL, Z = NULL, w = NULL),
      "nominal" = response_type_error(U$response.type, "Dimensionality")
    )
  } else {
    U <- dataFormat(U, na = na, Z = Z, w = w)
    Dimensionality(U)
  }
}

#' @rdname Dimensionality
#' @export
Dimensionality.binary <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  R <- TetrachoricCorrelationMatrix(tmp)
  Esystem <- eigen(R)
  Eval <- Esystem$values
  EvalVariance <- Esystem$values / length(Eval) * 100
  CumVari <- cumsum(EvalVariance)
  ret <-
    structure(
      list(
        Component = seq(1:length(Eval)),
        Eigenvalue = Eval,
        PerOfVar = EvalVariance,
        CumOfPer = CumVari
      ),
      class = c("exametrika", "Dimensionality")
    )

  return(ret)
}

#' @rdname Dimensionality
#' @export
Dimensionality.rated <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  R <- TetrachoricCorrelationMatrix(tmp$U)
  Esystem <- eigen(R)
  Eval <- Esystem$values
  EvalVariance <- Esystem$values / length(Eval) * 100
  CumVari <- cumsum(EvalVariance)
  ret <-
    structure(
      list(
        Component = seq(1:length(Eval)),
        Eigenvalue = Eval,
        PerOfVar = EvalVariance,
        CumOfPer = CumVari
      ),
      class = c("exametrika", "Dimensionality")
    )

  return(ret)
}

#' @rdname Dimensionality
#' @export
Dimensionality.ordinal <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  R <- PolychoricCorrelationMatrix(tmp)
  Esystem <- eigen(R)
  Eval <- Esystem$values
  EvalVariance <- Esystem$values / length(Eval) * 100
  CumVari <- cumsum(EvalVariance)
  ret <-
    structure(
      list(
        Component = seq(1:length(Eval)),
        Eigenvalue = Eval,
        PerOfVar = EvalVariance,
        CumOfPer = CumVari
      ),
      class = c("exametrika", "Dimensionality")
    )

  return(ret)
}
