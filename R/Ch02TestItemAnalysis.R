#' @title Joint Sample Size
#' @description
#' The joint sample size is a matrix whose elements are the number of
#' individuals who responded to each pair of items.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return Returns a matrix of class c("exametrika", "matrix") where each element (i,j)
#'   represents the number of students who responded to both item i and item j. The
#'   diagonal elements represent the total number of responses for each item.
#' @export

JointSampleSize <- createBinaryFunction(
  function(U, ...) {
    S_jk <- t(U$Z) %*% U$Z
    structure(S_jk, class = c("exametrika", "matrix"))
  },
  "JointSmapleSize"
)

#' @title Joint Correct Response Rate
#' @description
#' The joint correct response rate (JCRR) is the rate of students who passed
#' both items. This function is applicable only to binary response data.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
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
JCRR <- createBinaryFunction(
  function(U, ...) {
    P_J <- t(U$Z * U$U) %*% (U$Z * U$U) / (t(U$Z) %*% U$Z)
    structure(P_J, class = c("exametrika", "matrix"))
  },
  "JCRR"
)

#' @title Conditional Correct Response Rate
#' @description
#' The conditional correct response rate (CCRR) represents the ratio of the students
#' who passed Item C (consequent item) to those who passed Item A (antecedent item).
#' This function is applicable only to binary response data.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
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
CCRR <- createBinaryFunction(
  function(U, ...) {
    Z <- U$Z
    OneJ <- rep(1, ncol(U$U))
    Pj <- JCRR(U)
    p <- crr(U)
    P_C <- Pj / (p %*% t(OneJ))
    structure(P_C, class = c("exametrika", "matrix"))
  },
  "CCRR"
)

#' @title Item Lift
#' @description
#' The lift is a commonly used index in a POS data analysis.
#' The item lift of Item k to Item j is defined as follow:
#' \eqn{ l_{jk} = \frac{p_{k\mid j}}{p_k} }
#' This function is applicable only to binary response data.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
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
ItemLift <- createBinaryFunction(
  function(U, ...) {
    OneJ <- rep(1, ncol(U$U))
    Pc <- CCRR(U)
    p <- crr(U)
    P_L <- Pc / (OneJ %*% t(p))
    structure(P_L, class = c("exametrika", "matrix"))
  },
  "ItemLift"
)

#' @title Mutual Information
#' @description
#' Mutual Information is a measure that represents the degree of interdependence
#' between two items. This function is applicable only to binary response data.
#' The measure is calculated using the joint probability distribution of responses
#' between item pairs and their marginal probabilities.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
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
MutualInformation <- createBinaryFunction(
  function(U, ...) {
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
    MI <- P$S_00 * log(L$L_00, base = 2) +
      P$S_01 * log(L$L_01, base = 2) +
      P$S_10 * log(L$L_10, base = 2) +
      P$S_11 * log(L$L_11, base = 2)

    # Adjust diagonal elements
    diag(MI) <- diag(P$S_00 * log(L$L_00, base = 2) +
      P$S_11 * log(L$L_11, base = 2))

    structure(MI, class = c("exametrika", "matrix"))
  },
  "MutualInformation"
)

#' @title Phi-Coefficient
#' @description
#' The phi coefficient is the Pearson's product moment correlation coefficient
#' between two binary items. This function is applicable only to binary response data.
#' The coefficient ranges from -1 to 1, where 1 indicates perfect positive correlation,
#' -1 indicates perfect negative correlation, and 0 indicates no correlation.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
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
PhiCoefficient <- createBinaryFunction(
  function(U, ...) {
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
  },
  "PhiCoefficient"
)

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


#' @title Tetrachoric Correlation Matrix
#' @description
#' Calculates the matrix of tetrachoric correlations between all pairs of items.
#' Tetrachoric Correlation is superior to the phi coefficient as a measure of the
#' relation of an item pair. This function is applicable only to binary response data.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
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
TetrachoricCorrelationMatrix <- createBinaryFunction(
  function(U, ...) {
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
  },
  "TetrachoricCorrelationMatrix"
)

#' @title Inter-Item Analysis
#' @description
#' Inter-Item Analysis returns various metrics for analyzing relationships between pairs of items.
#' This function is applicable only to binary response data. The following metrics are calculated:
#' \itemize{
#'   \item JSS: Joint Sample Size
#'   \item JCRR: Joint Correct Response Rate
#'   \item IL: Item Lift
#'   \item MI: Mutual Information
#'   \item Phi: Phi Coefficient
#'   \item Tetrachoric: Tetrachoric Correlation
#' }
#' Each metric is returned in matrix form where element (i,j) represents the relationship
#' between items i and j.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A list of class "exametrika" and "IIAnalysis" containing the following matrices:
#' \describe{
#'   \item{JSS}{Joint Sample Size matrix}
#'   \item{JCRR}{Joint Correct Response Rate matrix}
#'   \item{IL}{Item Lift matrix}
#'   \item{MI}{Mutual Information matrix}
#'   \item{Phi}{Phi Coefficient matrix}
#'   \item{Tetrachoric}{Tetrachoric Correlation matrix}
#' }
#' @examples
#' \donttest{
#' # example code
#' InterItemAnalysis(J15S500)
#' }
#' @export
InterItemAnalysis <- createBinaryFunction(
  function(U, ...) {
    # Calculate all matrices
    JSS <- JointSampleSize(U)
    JCRR <- JCRR(U)
    IL <- ItemLift(U)
    MI <- MutualInformation(U)
    Phi <- PhiCoefficient(U)
    Tet <- TetrachoricCorrelationMatrix(U)

    # Create return structure
    structure(
      list(
        JSS = JSS,
        JCRR = JCRR,
        IL = IL,
        MI = MI,
        Phi = Phi,
        Tetrachoric = Tet
      ),
      class = c("exametrika", "IIAnalysis")
    )
  },
  "InterItemAnalysis"
)

#' @title Correct Response Rate
#' @description
#' The correct response rate (CRR) is one of the most basic and important
#' statistics for item analysis. This is an index of item difficulty and
#' a measure of how many students out of those who tried an item correctly
#' responded to it. This function is applicable only to binary response data.
#'
#' The CRR for each item is calculated as:
#' \deqn{p_j = \frac{\sum_{i=1}^n z_{ij}u_{ij}}{\sum_{i=1}^n z_{ij}}}
#' where \eqn{z_{ij}} is the missing indicator and \eqn{u_{ij}} is the response.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of weighted correct response rates for each item.
#' Values range from 0 to 1, where higher values indicate easier items
#' (more students answered correctly).
#' @examples
#' # Simple binary data
#' U <- matrix(c(1, 0, 1, 1, 0, 1), ncol = 2)
#' crr(U)
#'
#' # using sample datasaet
#' crr(J15S500)
#' @export
crr <- createBinaryFunction(
  function(U, ...) {
    # Create unit vector for summation
    OneS <- rep(1, length = nrow(U$U))

    # Calculate correct response rate
    # (sum of correct responses) / (sum of non-missing responses)
    p <- t(U$Z * U$U) %*% OneS / t(U$Z) %*% OneS

    # Apply item weights
    pW <- U$w * p

    return(pW)
  },
  "crr"
)

#' @title Item Odds
#' @description
#' Item Odds are defined as the ratio of Correct Response Rate to
#' Incorrect Response Rate:
#' \deqn{O_j = \frac{p_j}{1-p_j}}
#' where \eqn{p_j} is the correct response rate for item j.
#' This function is applicable only to binary response data.
#'
#' The odds value represents how many times more likely a correct response is
#' compared to an incorrect response. For example, an odds of 2 means students
#' are twice as likely to answer correctly as incorrectly.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of odds values for each item. Values range from 0 to infinity,
#' where:
#' \itemize{
#'   \item odds > 1: correct response more likely than incorrect
#'   \item odds = 1: equally likely
#'   \item odds < 1: incorrect response more likely than correct
#' }
#' @examples
#' # using sample dataset
#' ItemOdds(J5S10)
#' @export
ItemOdds <- createBinaryFunction(
  function(U, ...) {
    # Calculate correct response rates
    p <- crr(U)

    # Calculate odds
    o <- p / (1 - p)

    return(o)
  },
  "ItemOdds"
)

#' @title Item Threshold
#' @description
#' Item threshold is a measure of difficulty based on a standard normal distribution.
#' This function is applicable only to binary response data.
#'
#' The threshold is calculated as:
#' \deqn{\tau_j = \Phi^{-1}(1-p_j)}
#' where \eqn{\Phi^{-1}} is the inverse standard normal distribution function
#' and \eqn{p_j} is the correct response rate for item j.
#'
#' Higher threshold values indicate more difficult items, as they represent the
#' point on the standard normal scale above which examinees tend to answer incorrectly.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of threshold values for each item on the standard normal scale.
#' Typical values range from about -3 to 3, where:
#' \itemize{
#'   \item Positive values indicate difficult items
#'   \item Zero indicates items of medium difficulty (50% correct)
#'   \item Negative values indicate easy items
#' }
#' @importFrom stats qnorm
#' @examples
#' # using sample dataset
#' ItemThreshold(J5S10)
#' @export
ItemThreshold <- createBinaryFunction(
  function(U, ...) {
    # Calculate correct response rates
    p <- crr(U)

    # Calculate thresholds using inverse normal distribution
    tau <- qnorm(1 - p)

    return(tau)
  },
  "ItemThreshold"
)

#' @title Item Entropy
#' @description
#' The item entropy is an indicator of the variability or randomness
#' of the responses. This function is applicable only to binary response data.
#'
#' The entropy value represents the uncertainty or information content of the
#' response pattern for each item, measured in bits. Maximum entropy (1 bit)
#' occurs when correct and incorrect responses are equally likely (p = 0.5).
#' @details
#' The item entropy is calculated as:
#' \deqn{e_j = -p_j\log_2p_j-(1-p_j)\log_2(1-p_j)}
#' where \eqn{p_j} is the correct response rate for item j.
#'
#' The entropy value has the following properties:
#' \itemize{
#'   \item Maximum value of 1 bit when p = 0.5 (most uncertainty)
#'   \item Minimum value of 0 bits when p = 0 or 1 (no uncertainty)
#'   \item Higher values indicate more balanced response patterns
#'   \item Lower values indicate more predictable response patterns
#' }
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of entropy values for each item, measured in bits.
#' Values range from 0 to 1, where:
#' \itemize{
#'   \item 1: maximum uncertainty (p = 0.5)
#'   \item 0: complete certainty (p = 0 or 1)
#'   \item Values near 1 indicate items with balanced response patterns
#'   \item Values near 0 indicate items with extreme response patterns
#' }
#' @examples
#' # using sample dataset
#' ItemEntropy(J5S10)
#'
#' @export
ItemEntropy <- createBinaryFunction(
  function(U, ...) {
    # Calculate correct response rates
    p <- crr(U)

    # Calculate entropy in bits
    # Using log base 2 for information content in bits
    itemE <- -p * log(p, base = 2) - (1 - p) * log(1 - p, base = 2)

    return(itemE)
  },
  "ItemEntropy"
)


#' @title Item-Total Correlation
#' @description
#' Item-Total correlation (ITC) is a Pearson's correlation of an item with
#' the Number-Right Score (NRS) or total score. This function is applicable
#' only to binary response data.
#'
#' The ITC is a measure of item discrimination, indicating how well an item
#' distinguishes between high and low performing examinees.
#' @details
#' The correlation is calculated between:
#' \itemize{
#'   \item Each item's responses (0 or 1)
#'   \item The total test score (sum of correct responses)
#' }
#' Higher positive correlations indicate items that better discriminate between
#' high and low ability examinees.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of item-total correlations. Values typically range
#' from -1 to 1, where:
#' \itemize{
#'   \item Values near 1: Strong positive discrimination
#'   \item Values near 0: No discrimination
#'   \item Negative values: Potential item problems (lower ability students
#'         performing better than higher ability students)
#' }
#' @note
#' Values below 0.2 might indicate problematic items that should be reviewed.
#' Values above 0.3 are generally considered acceptable.
#' @examples
#' # using sample dataset
#' ItemTotalCorr(J15S500)
#'
#' @export
ItemTotalCorr <- createBinaryFunction(
  function(U, ...) {
    # Calculate item probabilities
    p <- crr(U)

    # Calculate total scores
    Zeta <- sscore(U)

    # Create probability matrix (repeating p for each student)
    TBL <- matrix(rep(p, each = NROW(U$U)),
      nrow = NROW(U$U),
      byrow = FALSE
    )

    # Handle missing values in response matrix
    Una <- ifelse(is.na(U$U), 0, U$U)

    # Calculate deviations from expected values
    dev <- U$Z * (Una - TBL)

    # Calculate item variances
    V <- colSums(dev^2) / (colSums(U$Z) - 1)
    SD <- sqrt(V)

    # Calculate correlations
    rho_Zi <- t(dev) %*% Zeta / SD / colSums(U$Z)

    return(rho_Zi)
  },
  "ItemTotalCorr"
)



#' @title Biserial Correlation
#' @description
#' A biserial correlation is a correlation between dichotomous-ordinal and
#' continuous variables.
#' @param i i is a dichotomous-ordinal variable (0/1). x and y can also be the other way around.
#' @param t t is a continuous variable. x and y can also be the other way around.
#' @return The biserial correlation coefficient between the two variables.
#' @importFrom stats na.omit qnorm pnorm optim
#' @export
Biserial_Correlation <- function(i, t) {
  # Original function remains unchanged
  # Count unique values
  unique_i <- length(unique(na.omit(i)))
  unique_t <- length(unique(na.omit(t)))
  # Check if one is binary and the other is continuous
  if (!((unique_i == 2 && unique_t > 2) | (unique_t == 2 && unique_i > 2))) {
    stop("One argument must be binary and the other must be continuous.")
  }
  ## if switched...
  if (unique_i > 2) {
    tmp <- i
    i <- t
    t <- tmp
  }
  ## calcs correlation
  tau_j <- qnorm(1 - mean(i, na.rm = TRUE))
  ll <- function(rho, tau_j, i, t) {
    tmp <- (1 - i) %*% (log(pnorm(tau_j, mean = rho * t, sd = sqrt(1 - rho^2)))) +
      i %*% (log(1 - pnorm(tau_j, mean = rho * t, sd = sqrt(1 - rho^2))))
  }
  pairwise <- !is.na(i + t)
  ret <- optim(
    par = 0, # initial value
    fn = function(x) {
      -ll(rho = x, tau_j, i[pairwise], t[pairwise])
    },
    lower = -1, # lower limit
    upper = 1, # upper limit
    method = "Brent" # one-dimensional optimization method
  )
  return(ret$par)
}

#' @title Item-Total Biserial Correlation
#' @description
#' The Item-Total Biserial Correlation computes the biserial correlation
#' between each item and the total score. This function is applicable only
#' to binary response data.
#'
#' This correlation provides a measure of item discrimination, indicating how well
#' each item distinguishes between high and low performing examinees.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of item-total biserial correlations. Values range
#' from -1 to 1, where:
#' \itemize{
#'   \item Values near 1: Strong positive discrimination
#'   \item Values near 0: No discrimination
#'   \item Negative values: Potential item problems
#' }
#' @note
#' The biserial correlation is generally preferred over the point-biserial
#' correlation when the dichotomization is artificial (i.e., when the underlying
#' trait is continuous).
#' @examples
#' # using sample dataset
#' ITBiserial(J15S500)
#' @export
ITBiserial <- createBinaryFunction(
  function(U, ...) {
    # Calculate total scores
    Zeta <- sscore(U)

    # Handle missing values
    U_data <- U$U
    U_data[U$Z == 0] <- NA

    # Calculate biserial correlation for each item
    ITB <- vapply(seq_len(ncol(U_data)), function(i) {
      Biserial_Correlation(U_data[, i], Zeta)
    }, numeric(1))

    return(ITB)
  },
  "ITBiserial"
)

#' @title Number Right Score
#' @description
#' The Number-Right Score (NRS) function calculates the weighted sum of correct
#' responses for each examinee. This function is applicable only to binary
#' response data.
#'
#' For each examinee, the score is computed as:
#' \deqn{NRS_i = \sum_{j=1}^J z_{ij}u_{ij}w_j}
#' where:
#' \itemize{
#'   \item \eqn{z_{ij}} is the missing response indicator (0/1)
#'   \item \eqn{u_{ij}} is the response (0/1)
#'   \item \eqn{w_j} is the item weight
#' }
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector containing the Number-Right Score for each examinee.
#' The score represents the weighted sum of correct answers, where:
#' \itemize{
#'   \item Maximum score is the sum of all item weights
#'   \item Minimum score is 0
#'   \item Missing responses do not contribute to the score
#' }
#' @examples
#' # using sample dataset
#' nrs(J15S500)
#' @export
nrs <- createBinaryFunction(
  function(U, ...) {
    # Calculate weighted sum of correct responses
    # Z * U gives correct answers accounting for missing data
    # Multiply by weights and sum across items
    total_weighted_score <- (U$Z * U$U) %*% U$w

    return(total_weighted_score)
  },
  "nrs"
)

#' @title Passage Rate of Student
#' @description
#' The Passage Rate for each student is calculated as their Number-Right Score (NRS)
#' divided by the number of items presented to them. This function is applicable
#' only to binary response data.
#'
#' The passage rate is calculated as:
#' \deqn{P_i = \frac{\sum_{j=1}^J z_{ij}u_{ij}w_j}{\sum_{j=1}^J z_{ij}}}
#' where:
#' \itemize{
#'   \item \eqn{z_{ij}} is the missing response indicator (0/1)
#'   \item \eqn{u_{ij}} is the response (0/1)
#'   \item \eqn{w_j} is the item weight
#' }
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector containing the passage rate for each student.
#' Values range from 0 to 1 (or maximum weight) where:
#' \itemize{
#'   \item 1: Perfect score on all attempted items
#'   \item 0: No correct answers
#'   \item NA: No items attempted
#' }
#' @note
#' The passage rate accounts for missing responses by only considering items that
#' were actually presented to each student. This provides a fair comparison
#' between students who attempted different numbers of items.
#' @examples
#' # using sample dataset
#' passage(J15S500)
#'
#' @export
passage <- createBinaryFunction(
  function(U, ...) {
    # Calculate Number-Right Score
    total_score <- nrs(U)

    # Calculate number of items presented to each student
    items_attempted <- NCOL(U$U) - rowSums(1 - U$Z)

    # Calculate passage rate
    # Divide total score by number of items attempted
    passage_rate <- total_score / items_attempted

    return(passage_rate)
  },
  "passage"
)

#' @title Standardized Score
#' @description
#' The standardized score (z-score) indicates how far a student's performance
#' deviates from the mean in units of standard deviation. This function is
#' applicable only to binary response data.
#'
#' The score is calculated by standardizing the passage rates:
#' \deqn{Z_i = \frac{r_i - \bar{r}}{\sigma_r}}
#' where:
#' \itemize{
#'   \item \eqn{r_i} is student i's passage rate
#'   \item \eqn{\bar{r}} is the mean passage rate
#'   \item \eqn{\sigma_r} is the standard deviation of passage rates
#' }
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of standardized scores for each student. The scores follow
#' a standard normal distribution with:
#' \itemize{
#'   \item Mean = 0
#'   \item Standard deviation = 1
#'   \item Approximately 68% of scores between -1 and 1
#'   \item Approximately 95% of scores between -2 and 2
#'   \item Approximately 99% of scores between -3 and 3
#' }
#' @note
#' The standardization allows for comparing student performance across different
#' tests or groups. A positive score indicates above-average performance, while
#' a negative score indicates below-average performance.
#' @examples
#' # using sample dataset
#' sscore(J5S10)
#' @export
sscore <- createBinaryFunction(
  function(U, ...) {
    # Get number of students
    S <- nrow(U$U)

    # Create unit vector
    OneS <- rep(1, length = S)

    # Calculate passage rates
    passage_rates <- passage(U)

    # Calculate mean passage rate
    mean_rate <- mean(passage_rates, na.rm = TRUE)

    # Calculate variance of passage rates
    centered_rates <- passage_rates - mean_rate
    var_rates <- sum(centered_rates^2, na.rm = TRUE) / (S - 1)

    # Calculate standardized scores
    z_scores <- centered_rates / sqrt(var_rates)

    return(z_scores)
  },
  "sscore"
)


#' @title Student Percentile Ranks
#' @description
#' The percentile function calculates each student's relative standing in the group,
#' expressed as a percentile rank (1-100). This function is applicable only to
#' binary response data.
#'
#' The percentile rank indicates the percentage of scores in the distribution
#' that fall below a given score. For example, a percentile rank of 75 means
#' the student performed better than 75% of the group.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A numeric vector of percentile ranks (1-100) for each student, where:
#' \itemize{
#'   \item 100: Highest performing student(s)
#'   \item 50: Median performance
#'   \item 1: Lowest performing student(s)
#' }
#' @note
#' Percentile ranks are calculated using the empirical cumulative distribution
#' function of standardized scores. Tied scores receive the same percentile rank.
#' The values are rounded up to the nearest integer to provide ranks from 1 to 100.
#' @examples
#' # using sample dataset
#' percentile(J5S10)
#'
#' @importFrom stats ecdf
#' @export
percentile <- createBinaryFunction(
  function(U, ...) {
    # Calculate standardized scores
    standardized_scores <- sscore(U)

    # Calculate empirical cumulative distribution function
    empirical_dist <- ecdf(standardized_scores)

    # Convert to percentiles (1-100)
    # Ceiling function ensures minimum of 1 and handles rounding
    percentile_ranks <- ceiling(empirical_dist(standardized_scores) * 100)

    return(percentile_ranks)
  },
  "percentile"
)

#' @title Stanine Scores
#' @description
#' The Stanine (Standard Nine) scoring system divides students into nine groups
#' based on a normalized distribution. This function is applicable only to
#' binary response data.
#'
#' These groups correspond to the following percentile ranges:
#' \itemize{
#'   \item Stanine 1: lowest 4% (percentiles 1-4)
#'   \item Stanine 2: next 7% (percentiles 5-11)
#'   \item Stanine 3: next 12% (percentiles 12-23)
#'   \item Stanine 4: next 17% (percentiles 24-40)
#'   \item Stanine 5: middle 20% (percentiles 41-60)
#'   \item Stanine 6: next 17% (percentiles 61-77)
#'   \item Stanine 7: next 12% (percentiles 78-89)
#'   \item Stanine 8: next 7% (percentiles 90-96)
#'   \item Stanine 9: highest 4% (percentiles 97-100)
#' }
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ... Internal parameters for maintaining compatibility with the binary data
#'            processing system. Not intended for direct use.
#' @note This function is implemented using a binary data compatibility wrapper and
#'       will raise an error if used with polytomous data.
#' @return A list containing two elements:
#' \describe{
#'   \item{stanine}{The score boundaries for each stanine level}
#'   \item{stanineScore}{The stanine score (1-9) for each student}
#' }
#' @note
#' Stanine scores provide a normalized scale with:
#' \itemize{
#'   \item Mean = 5
#'   \item Standard deviation = 2
#'   \item Scores range from 1 to 9
#'   \item Score of 5 represents average performance
#' }
#' @references
#' Angoff, W. H. (1984). Scales, norms, and equivalent scores. Educational Testing Service.
#' (Reprint of chapter in R. L. Thorndike (Ed.) (1971) Educational Measurement (2nd Ed.).
#' American Council on Education.
#' @importFrom stats quantile
#' @examples
#' result <- stanine(J15S500)
#' # View score boundaries
#' result$stanine
#' # View individual scores
#' result$stanineScore
#' @export
stanine <- createBinaryFunction(
  function(U, ...) {
    # Define stanine boundaries (cumulative proportions)
    stanine_bounds <- cumsum(c(0.04, 0.07, 0.12, 0.17, 0.20, 0.17, 0.12, 0.07))

    # Calculate raw scores
    raw_scores <- nrs(U)

    # Calculate score boundaries using raw scores
    stanine_boundaries <- quantile(raw_scores,
      probs = stanine_bounds,
      na.rm = TRUE
    )

    # Calculate percentile scores
    percentile_scores <- percentile(U)

    # Calculate stanine boundaries using percentile scores
    stanine_percentile_bounds <- quantile(percentile_scores,
      probs = stanine_bounds
    )

    # Assign stanine scores
    stanine_scores <- cut(percentile_scores,
      breaks = c(-Inf, stanine_percentile_bounds, Inf),
      right = FALSE,
      labels = 1:9
    )

    # Return results
    list(
      stanine = stanine_boundaries,
      stanineScore = stanine_scores
    )
  },
  "stanine"
)


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
  NRS <- nrs(U = tmp$U, Z = tmp$Z, w = tmp$w)
  NR <- NCOL(tmp$U) - rowSums(is.na(tmp$U))
  PR <- passage(U = tmp$U, Z = tmp$Z, w = tmp$w)
  SS <- sscore(U = tmp$U, Z = tmp$Z, w = tmp$w)
  Ptile <- percentile(U = tmp$U, Z = tmp$Z, w = tmp$w)
  ST <- stanine(U = tmp$U, Z = tmp$Z, w = tmp$w)
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

#' @title Simple Test Statistics
#' @description
#' Statistics regarding the total score.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return
#' \describe{
#' \item{TestLength}{Length of the test. The number of items included in the test.}
#' \item{SampleSize}{Sample size. The number of rows in the dataset.}
#' \item{Mean}{Average number of correct answers.}
#' \item{SEofMean}{Standard error of mean}
#' \item{Variance}{Variance}
#' \item{SD}{Standard Deviation}
#' \item{Skewness}{Skewness}
#' \item{Kurtosis}{Kurtosis}
#' \item{Min}{Minimum score}
#' \item{Max}{Max score}
#' \item{Range}{Range of score}
#' \item{Q1}{First quartile. Same as the 25th percentile.}
#' \item{Median}{Median.Same as the 50th percentile.}
#' \item{Q3}{Third quartile. Same as the 75th percentile.}
#' \item{IQR}{Interquartile range. It is calculated by subtracting the first quartile from the third quartile.}
#' \item{Stanine}{see [stanine]}
#' }
#' @importFrom stats sd var
#' @examples
#' # using sample dataset
#' TestStatistics(J15S500)
#' @export

TestStatistics <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  tW <- nrs(tmp)
  TestLength <- NCOL(tmp$Z)
  SampleSize <- NROW(tmp$Z)
  Mean <- mean(tW)
  SEofMean <- sd(tW) / sqrt(SampleSize)
  Variance <- var(tW)
  SD <- sd(tW)
  SDs <- sqrt(mean((tW - Mean)^2))
  tmpZ <- (tW - Mean) / SDs
  Skewness <- mean(tmpZ^3)
  Kurtosis <- mean(tmpZ^4) - 3
  Min <- min(tW)
  Max <- max(tW)
  Range <- Max - Min
  Q1 <- quantile(tW, probs = 0.25, na.rm = TRUE)
  Median <- quantile(tW, probs = 0.5, na.rm = TRUE)
  Q3 <- quantile(tW, probs = 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  Stanine <- stanine(tmp)
  ret <-
    structure(list(
      TestLength = TestLength,
      SampleSize = SampleSize,
      Mean = Mean,
      SEofMean = SEofMean,
      Variance = Variance,
      SD = SD,
      Skewness = Skewness,
      Kurtosis = Kurtosis,
      Min = Min,
      Max = Max,
      Range = Range,
      Q1 = Q1,
      Median = Median,
      Q3 = Q3,
      IQR = IQR,
      Stanine = Stanine$stanine
    ), class = c("exametrika", "TestStatistics"))
  return(ret)
}

#' @title Dimensionality
#' @description
#' The dimensionality is the number of components
#' the test is measuring.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return Returns a list of class c("exametrika", "Dimensionality") containing:
#'   \itemize{
#'     \item Component: Sequence of component numbers
#'     \item Eigenvalue: Eigenvalues of the tetrachoric correlation matrix
#'     \item PerOfVar: Percentage of variance explained by each component
#'     \item CumOfPer: Cumulative percentage of variance explained
#'   }
#' @export

Dimensionality <- function(U, na = NULL, Z = NULL, w = NULL) {
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
