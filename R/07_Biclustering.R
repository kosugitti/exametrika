#' @title softmax function
#' @description
#' to avoid overflow
#' @param x numeric vector
#'

softmax <- function(x) {
  x_max <- max(x)
  x <- x - x_max
  return(exp(x) / sum(exp(x)))
}

#' @title Biclustering and Ranklustering Analysis
#' @description
#' Performs biclustering, ranklustering, or their confirmatory variants on binary response data.
#' These methods simultaneously cluster both examinees and items into homogeneous groups
#' (or ordered ranks for ranklustering). The analysis reveals latent structures and patterns
#' in the data by creating a matrix with rows and columns arranged to highlight block structures.
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#' it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return An object of class "exametrika" and "Biclustering" containing:
#' \describe{
#'  \item{model}{Model type indicator (1 for biclustering, 2 for ranklustering)}
#'  \item{msg}{A character string indicating the model type. }
#'  \item{mic}{Logical value indicating whether monotonicity constraint was applied}
#'  \item{testlength}{Number of items in the test}
#'  \item{nobs}{Number of examinees in the dataset}
#'  \item{Nclass}{Number of latent classes/ranks specified}
#'  \item{Nfield}{Number of latent fields specified}
#'  \item{N_Cycle}{Number of EM iterations performed}
#'  \item{converge}{Logical value indicating wheter the algorithm converged within maxiter iterasions}
#'  \item{LFD}{Latent Field Distribution - counts of items assigned to each field}
#'  \item{LRD/LCD}{Latent Rank/Class Distribution - counts of examinees assigned to each class/rank}
#'  \item{FRP}{Field Reference Profile matrix - probability of correct response for each field-class combination}
#'  \item{FRPIndex}{Field Reference Profile indices including location parameters, slope parameters, and monotonicity indices}
#'  \item{TRP}{Test Reference Profile - expected score for examinees in each class/rank}
#'  \item{CMD/RMD}{Class/Rank Membership Distribution - sum of membership probabilities across examinees}
#'  \item{FieldMembership}{Matrix showing the probabilities of each item belonging to each field}
#'  \item{ClassMembership}{Matrix showing the probabilities of each examinee belonging to each class/rank}
#'  \item{SmoothedMembership}{Matrix of smoothed class membership probabilities after filtering}
#'  \item{FieldEstimated}{Vector of the most likely field assignments for each item}
#'  \item{ClassEstimated}{Vector of the most likely class/rank assignments for each examinee}
#'  \item{Students}{Data frame containing membership probabilities and classification information for each examinee}
#'  \item{FieldAnalysis}{Matrix showing field analysis results with item-level information}
#'  \item{TestFitIndices}{Model fit indices for evaluating the quality of the clustering solution}
#'  \item{SOACflg}{Logical flag indicating whether Strongly Ordinal Alignment Condition is satisfied}
#'  \item{WOACflg}{Logical flag indicating whether Weakly Ordinal Alignment Condition is satisfied}
#' }
#'
#' @details
#' Biclustering simultaneously clusters both rows (examinees) and columns (items) of a data matrix.
#' Unlike traditional clustering that groups either rows or columns, biclustering identifies
#' submatrices with similar patterns. Ranklustering is a variant that imposes an ordinal
#' structure on the classes, making it suitable for proficiency scaling.
#'
#' The algorithm uses an Expectation-Maximization approach to iteratively estimate:
#' 1. Field membership of items (which items belong to which fields)
#' 2. Class/rank membership of examinees (which examinees belong to which classes)
#' 3. Field Reference Profiles (probability patterns for each field-class combination)
#'
#' The confirmatory option allows for pre-specified field assignments, which is useful
#' when there is prior knowledge about item groupings or for testing hypothesized structures.
#'
#' @references
#' Shojima, K. (2012). Biclustering of binary data matrices using bilinear models.
#' Behaviormetrika, 39(2), 161-178.
#'
#' @export

Biclustering <- function(U, ...) {
  UseMethod("Biclustering")
}

#' @rdname Biclustering
#' @param na Values to be treated as missing values.
#' @param Z Missing indicator matrix of type matrix or data.frame. 1 indicates observed values, 0 indicates missing values.
#' @param w Item weight vector.
#' @param ... Additional arguments passed to specific methods.
#'
#' @export
#'
Biclustering.default <- function(U, na = NULL, Z = NULL, w = NULL, ...) {
  if (inherits(U, "exametrika")) {
    if (U$response.type == "binary") {
      return(Biclustering.binary(U, ...))
    } else if (U$response.type == "ordinal") {
      return(Biclustering.ordinal(U, ...))
    } else if (U$response.type == "rated") {
      stop("Biclustering.rated is not implemented yet")
    } else if (U$response.type == "nominal") {
      return(Biclustering.nominal(U, ...))
    }
  }

  U <- dataFormat(U, na = na, Z = Z, w = w)
  Biclustering(U, ...)
}


#' @rdname Biclustering
#' @param ncls Number of latent classes/ranks to identify (between 2 and 20).
#' @param nfld Number of latent fields (item clusters) to identify.
#' @param method Analysis method to use (character string):
#'   * "B" or "Biclustering": Standard biclustering (default)
#'   * "R" or "Ranklustering": Ranklustering with ordered class structure
#' @param conf Confirmatory parameter for pre-specified field assignments. Can be either:
#'   * A vector with items and corresponding fields in sequence
#'   * A field membership profile matrix (items × fields) with 0/1 values
#'   * NULL (default) for exploratory analysis where field memberships are estimated
#' @param mic Logical; if TRUE, forces Field Reference Profiles to be monotonically
#' increasing. Default is FALSE.
#' @param maxiter Maximum number of EM algorithm iterations. Default is 100.
#' @param verbose Logical; if TRUE, displays progress during estimation. Default is TRUE.
#' @param beta1 Beta distribution parameter 1 for prior density of field reference matrix. Default is 1.
#' @param beta2 Beta distribution parameter 2 for prior density of field reference matrix. Default is 1.
#' @param ... Additional arguments passed to specific methods.
#'
#' @examples
#' \donttest{
#' # Perform Biclustering with Binary method (B)
#' # Analyze data with 5 fields and 6 classes
#' result.Bi <- Biclustering(J35S515, nfld = 5, ncls = 6, method = "B")
#'
#' # Perform Biclustering with Rank method (R)
#' # Store results for further analysis and visualization
#' result.Rank <- Biclustering(J35S515, nfld = 5, ncls = 6, method = "R")
#'
#' # Display the Bicluster Reference Matrix (BRM) as a heatmap
#' plot(result.Rank, type = "Array")
#'
#' # Plot Field Reference Profiles (FRP) in a 2x3 grid
#' # Shows the probability patterns for each field
#' plot(result.Rank, type = "FRP", nc = 2, nr = 3)
#'
#' # Plot Rank Membership Profiles (RMP) for students 1-9 in a 3x3 grid
#' # Shows posterior probability distribution of rank membership
#' plot(result.Rank, type = "RMP", students = 1:9, nc = 3, nr = 3)
#'
#' # Example of confirmatory analysis with pre-specified fields
#' # Assign items 1-10 to field 1, 11-20 to field 2, etc.
#' field_assignments <- c(rep(1, 10), rep(2, 10), rep(3, 15))
#' result.Conf <- Biclustering(J35S515, nfld = 3, ncls = 5, conf = field_assignments)
#' }
#' @export
Biclustering.binary <- function(U,
                                ncls = 2, nfld = 2,
                                method = "B",
                                conf = NULL,
                                mic = FALSE,
                                maxiter = 100,
                                verbose = TRUE,
                                beta1 = 1,
                                beta2 = 1, ...) {
  tmp <- U
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)
  const <- .Machine$double.eps
  ret.TS <- TestStatistics(tmp)

  if (method == "B" | method == "Biclustering") {
    if (verbose) {
      message("Biclustering is chosen.")
    }
    model <- 1
  } else if (method == "R" | method == "Ranklustering") {
    if (verbose) {
      message("Ranklustering is chosen.")
    }
    model <- 2
  } else if (method == "BINET") {
    if (verbose) {
      message("BINET is chosen.")
    }
    model <- 3
  } else {
    stop("The method must be selected as either Biclustering or Ranklustering.")
  }

  # set conf_mat for confirmatory clustering
  if (!is.null(conf)) {
    if (verbose) {
      message("Confirmatory Clustering is chosen.")
    }
    if (is.vector(conf)) {
      # check size
      if (length(conf) != NCOL(U)) {
        stop("conf vector size does NOT match with data.")
      }
      conf_mat <- matrix(0, nrow = NCOL(U), ncol = max(conf))
      for (i in 1:NROW(conf_mat)) {
        conf_mat[i, conf[i]] <- 1
      }
    } else if (is.matrix(conf) | is.data.frame(conf)) {
      if (NROW(conf) != NCOL(U)) {
        stop("conf matrix size does NOT match with data.")
      }
      if (any(!conf %in% c(0, 1))) {
        stop("The conf matrix should only contain 0s and 1s.")
      }
      if (any(rowSums(conf) > 1)) {
        stop("The row sums of the conf matrix must be equal to 1.")
      }
    } else {
      stop("conf matrix is not set properly.")
    }
    ###
    nfld <- NCOL(conf_mat)
  } else {
    conf_mat <- NULL
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  if (model == 3) {
    zero_scorer <- ifelse(ret.TS$Min == 0, 1, 0)
    full_scorer <- ifelse(ret.TS$Max == testlength, 1, 0)
    if (ncls < zero_scorer + full_scorer + 1) {
      stop(paste(
        "The number of class must be more than ",
        zero_scorer + full_scorer + 1, "."
      ))
    }
  }

  ### Algorithm
  testell <- -1 / const
  oldtestell <- -2 / const
  emt <- 0
  maxemt <- 100

  fld0 <- pmin(ceiling(1:testlength / (testlength / nfld)), nfld)
  crr_order <- order(crr(tmp), decreasing = TRUE)
  fld <- fld0[match(1:testlength, crr_order)]
  fldmemb <- matrix(0, nrow = testlength, ncol = nfld)
  for (i in 1:testlength) {
    fldmemb[i, fld[i]] <- 1
  }
  ## Confirmatory Biclustering
  if (!any(is.null(conf_mat))) {
    fldmemb <- conf_mat
  }

  PiFR <- matrix(NA, nrow = nfld, ncol = ncls)
  for (i in 1:nfld) {
    for (j in 1:ncls) {
      PiFR[i, j] <- (nfld - i + j) / (nfld + ncls)
    }
  }
  # For BINET
  if (model == 3) {
    if (zero_scorer == 1) {
      PiFR[, 1] <- 0
    }
    if (full_scorer == 1) {
      PiFR[, ncls] <- 1
    }
  }

  if (model != 2) {
    Fil <- diag(rep(1, ncls))
  } else {
    Fil <- create_filter_matrix(ncls)
  }

  ## Algorithm
  FLG <- TRUE
  converge <- TRUE
  while (FLG) {
    if (testell - oldtestell < 1e-4 * abs(oldtestell)) {
      FLG <- FALSE
      break
    }
    if (emt == maxemt) {
      message("\nReached ten times the maximum number of iterations.")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
      FLG <- FALSE
      converge <- FALSE
    }
    emt <- emt + 1
    oldtestell <- testell
    csr <- (tmp$Z * tmp$U) %*% fldmemb
    fsr <- (tmp$Z * (1 - tmp$U)) %*% fldmemb
    # Apply pmax only when necessary to avoid NaN/Inf
    if (any(is.nan(csr)) || any(is.infinite(csr)) || any(csr <= 0)) {
      csr <- pmax(csr, const)
    }
    if (any(is.nan(fsr)) || any(is.infinite(fsr)) || any(fsr <= 0)) {
      fsr <- pmax(fsr, const)
    }
    llsr <- csr %*% log(PiFR + const) + fsr %*% log(1 - PiFR + const)
    # minllsr <- apply(llsr, 1, min)
    # expllsr <- exp(llsr - minllsr)
    # clsmemb <- round(expllsr / rowSums(expllsr), 1e8)
    clsmemb <- t(apply(llsr, 1, softmax))

    smoothed_memb <- clsmemb %*% Fil

    cjr <- t(tmp$Z * tmp$U) %*% smoothed_memb
    fjr <- t(tmp$Z * (1 - tmp$U)) %*% smoothed_memb
    # Apply pmax only when necessary to avoid NaN/Inf
    if (any(is.nan(cjr)) || any(is.infinite(cjr)) || any(cjr <= 0)) {
      cjr <- pmax(cjr, const)
    }
    if (any(is.nan(fjr)) || any(is.infinite(fjr)) || any(fjr <= 0)) {
      fjr <- pmax(fjr, const)
    }
    lljf <- cjr %*% log(t(PiFR) + const) + fjr %*% log(t(1 - PiFR) + const)

    # max_log_lljf <- apply(lljf, 1, max)
    # log_lljf_adj <- lljf - max_log_lljf
    # log_fldmemb <- log_lljf_adj - log(rowSums(exp(log_lljf_adj)))
    fldmemb <- t(apply(lljf, 1, softmax))

    if (!any(is.null(conf_mat))) {
      fldmemb <- conf_mat
    }

    cfr <- t(fldmemb) %*% t(tmp$Z * tmp$U) %*% smoothed_memb
    ffr <- t(fldmemb) %*% t(tmp$Z * (1 - tmp$U)) %*% smoothed_memb
    # Apply pmax only when necessary to avoid NaN/Inf
    if (any(is.nan(cfr)) || any(is.infinite(cfr)) || any(cfr <= 0)) {
      cfr <- pmax(cfr, const)
    }
    if (any(is.nan(ffr)) || any(is.infinite(ffr)) || any(ffr <= 0)) {
      ffr <- pmax(ffr, const)
    }
    oldPiFR <- PiFR
    PiFR <- (cfr + beta1 - 1) / (cfr + ffr + beta1 + beta2 - 2)
    PiFR[is.nan(PiFR)] <- 1e-16
    PiFR[is.infinite(PiFR)] <- 1e-16
    PiFR <- pmax(pmin(PiFR, 1 - 1e-16), 1e-16)
    if (model == 3) {
      if (zero_scorer == 1) {
        PiFR[, 1] <- 0
      }
      if (full_scorer == 1) {
        PiFR[, ncls] <- 1
      }
    }
    if (mic) {
      PiFR <- t(apply(PiFR, 1, sort))
    }
    if (any(is.nan(cfr))) {
      stop("The calculation diverged during the process. Please adjust your settings appropriately")
    }

    testell <- sum(cfr * log(PiFR + const) + ffr * log(1 - PiFR + const))
    if (verbose) {
      message(
        sprintf(
          "\r%-80s",
          paste0(
            "iter ", emt, " logLik ", format(testell, digits = 6)
          )
        ),
        appendLF = FALSE
      )
    }
    if (testell - oldtestell <= 0) {
      PiFR <- oldPiFR
      break
    }
  }

  # Ensure proper line termination after verbose output
  if (verbose) {
    message("")
  }

  #### OUTPUT

  cls <- apply(clsmemb, 1, which.max)
  fld <- apply(fldmemb, 1, which.max)
  fldmemb01 <- sign(fldmemb - apply(fldmemb, 1, max)) + 1
  flddist <- colSums(fldmemb01)
  clsmemb01 <- sign(clsmemb - apply(clsmemb, 1, max)) + 1
  clsdist <- colSums(clsmemb01)
  TRP <- colSums(PiFR * flddist)
  StudentRank <- clsmemb
  rownames(StudentRank) <- tmp$ID
  if (model == 2) {
    RU <- ifelse(cls + 1 > ncls, NA, cls + 1)
    RD <- ifelse(cls - 1 < 1, NA, cls - 1)
    RUO <- StudentRank[cbind(1:nobs, RU)] / StudentRank[cbind(1:nobs, cls)]
    RDO <- StudentRank[cbind(1:nobs, RD)] / StudentRank[cbind(1:nobs, cls)]
    StudentRank <- cbind(StudentRank, cls, RUO, RDO)
    colnames(StudentRank) <- c(
      paste("Membership", 1:ncls), "Estimate",
      "Rank-Up Odds", "Rank-Down Odds"
    )
  } else {
    StudentRank <- cbind(StudentRank, cls)
    colnames(StudentRank) <- c(
      paste("Membership", 1:ncls), "Estimate"
    )
  }

  if (model == 2) {
    msg1 <- "Rank"
  } else {
    msg1 <- "Class"
  }
  FRP <- PiFR
  colnames(FRP) <- paste0(msg1, 1:ncls)
  rownames(FRP) <- paste0("Field", 1:nfld)
  colnames(fldmemb) <- paste0("Field", 1:nfld)
  rownames(clsmemb) <- tmp$ID
  colnames(clsmemb) <- paste0(msg1, 1:ncls)

  FRPIndex <- IRPindex(FRP)

  TRPlag <- TRP[2:ncls]
  TRPmic <- sum(TRPlag[1:(ncls - 1)] - TRP[1:(ncls - 1)] < 0, na.rm = TRUE)
  FRPmic <- sum(abs(FRPIndex$C))
  SOACflg <- WOACflg <- FALSE
  if (TRPmic == 0) {
    WOACflg <- TRUE
    if (FRPmic == 0) {
      SOACflg <- TRUE
    }
  }
  if (verbose) {
    if (SOACflg & WOACflg) {
      message("\nStrongly ordinal alignment condition was satisfied.")
    }
    if (!SOACflg & WOACflg) {
      message("\nWeakly ordinal alignment condition was satisfied.")
    }
  }

  ### Model Fit
  cfr <- t(fldmemb) %*% t(tmp$Z * tmp$U) %*% clsmemb
  ffr <- t(fldmemb) %*% t(tmp$Z * (1 - tmp$U)) %*% clsmemb
  testell <- sum(cfr * log(PiFR + const) + ffr * log(1 - PiFR + const))
  nparam <- ifelse(model == 1, ncls * nfld, sum(diag(Fil)) * nfld)
  FitIndices <- TestFit(tmp$U, tmp$Z, testell, nparam)

  ### Field Analysis
  crr <- crr(tmp$Z * tmp$U)
  fieldAnalysis <- as.data.frame(fldmemb)
  fieldAnalysis <- cbind(crr, fld, fieldAnalysis)
  colnames(fieldAnalysis) <- c("CRR", "LFE", paste0("Field", 1:nfld))
  fieldAnalysis <- fieldAnalysis[order(fieldAnalysis$CRR, decreasing = TRUE), ]
  fieldAnalysis <- fieldAnalysis[order(fieldAnalysis$LFE), ]
  rownames_tmp <- rownames(fieldAnalysis)
  fieldAnalysis <- matrix(as.numeric(as.matrix(fieldAnalysis)), ncol = NCOL(fieldAnalysis), nrow = NROW(fieldAnalysis))
  colnames(fieldAnalysis) <- c("CRR", "LFE", paste0("Field", 1:nfld))
  rownames(fieldAnalysis) <- rownames_tmp

  msg <- ifelse(model == 1, "Class", "Rank")
  ret <- structure(list(
    model = model,
    mic = mic,
    msg = msg,
    converge = converge,
    U = U,
    testlength = testlength,
    nobs = nobs,
    Nclass = ncls,
    Nfield = nfld,
    N_Cycle = emt,
    LFD = flddist,
    LRD = clsdist,
    LCD = clsdist,
    FRP = FRP,
    FRPIndex = FRPIndex,
    TRP = TRP,
    CMD = colSums(clsmemb),
    RMD = colSums(clsmemb),
    FieldMembership = fldmemb,
    ClassMembership = clsmemb,
    SmoothedMembership = smoothed_memb,
    FieldEstimated = fld,
    ClassEstimated = cls,
    Students = StudentRank,
    FieldAnalysis = fieldAnalysis,
    TestFitIndices = FitIndices,
    SOACflg = SOACflg,
    WOACflg = WOACflg
  ), class = c("exametrika", "Biclustering"))
  return(ret)
}
