#' @title softmax function
#' @description
#' to avoid overflow
#' @param x numericvector
#'

softmax <- function(x) {
  x_max <- max(x)
  x <- x - x_max
  return(exp(x) / sum(exp(x)))
}

#' @title Biclustering and Ranklustering
#' @description
#' performs biclustering, rankclustering, and their confirmatory models.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param ncls number of classes
#' @param nfld number of fields
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param conf For the confirmatory parameter, you can input either a vector with
#' items and corresponding fields in sequence, or a field membership profile
#' matrix. In the case of the former, the field membership profile matrix will be generated internally.
#' When providing a membership profile matrix, it needs to be either matrix or data.frame.
#' The number of fields(nfld) will be overwrite to the number of columns of this matrix.
#' The default is NULL, and the field membership
#' matrix will be estimated according to the specified number of classes(ncls) and fields(nfld).
#' @param mic Monotonic increasing IRP option. The default is FALSE.
#' @param method One of:
#'   * "B": Biclustering
#'   * "R": Ranklustering
#' @param maxiter Maximum number of iterations. default is 100.
#' @param verbose verbose output Flag. default is TRUE
#'
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{Nclass}{number of classes you set}
#'  \item{BRM}{Bicluster Reference Matrix}
#'  \item{FRP}{Field Reference Profile}
#'  \item{FRPIndex}{Index of FFP includes the item location parameters B and Beta,
#'  the slope parameters A and Alpha, and the monotonicity indices C and Gamma.}
#'  \item{TRP}{Test Reference Profile}
#'  \item{FMP}{Field Membership Profile}
#'  \item{Students}{Class/Rank Membership Profile matrix.The s-th row vector of \eqn{\hat{M}_R}, \eqn{\hat{m}_R}, is the
#' rank membership profile of Student s, namely the posterior probability distribution representing the student's
#' belonging to the respective latent classes. It also includes the rank with the maximum estimated membership probability,
#' as well as the rank-up odds and rank-down odds.}
#'  \item{LRD}{Latent Rank Distribution. see also [plot.exametrika]}
#'  \item{LCD}{Latent Class Distribution. see also [plot.exametrika]}
#'  \item{LFD}{Latent Field Distribution. see also [plot.exametrika]}
#'  \item{RMD}{Rank Membership Distribution.}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#' }
#' @examples
#' \donttest{
#' # Perform Biclustering with Binary method (B)
#' # Analyze data with 5 fields and 6 classes
#' Biclustering(J35S515, nfld = 5, ncls = 6, method = "B")
#'
#' # Perform Biclustering with Rank method (R)
#' # Store results for further analysis and visualization
#' result.Ranklusteing <- Biclustering(J35S515, nfld = 5, ncls = 6, method = "R")
#'
#' # Display the Bicluster Reference Matrix (BRM) as a heatmap
#' plot(result.Ranklusteing, type = "Array")
#'
#' # Plot Field Reference Profiles (FRP) in a 2x3 grid
#' # Shows the probability patterns for each field
#' plot(result.Ranklusteing, type = "FRP", nc = 2, nr = 3)
#'
#' # Plot Rank Membership Profiles (RMP) for students 1-9 in a 3x3 grid
#' # Shows the posterior probability distribution of rank membership for each student
#' plot(result.Ranklusteing, type = "RMP", students = 1:9, nc = 3, nr = 3)
#' }

#' @export

Biclustering <- function(U, ncls = 2, nfld = 2,
                         Z = NULL, w = NULL, na = NULL,
                         method = "B",
                         conf = NULL,
                         mic = FALSE,
                         maxiter = 100,
                         verbose = TRUE) {
  # data format
  if (class(U)[1] != "exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)
  const <- exp(-testlength)
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
  beta1 <- 1
  beta2 <- 1
  testell <- -1 / const
  oldtestell <- -2 / const
  emt <- 0
  maxemt <- 100

  fld0 <- ceiling(1:testlength / (testlength / nfld))
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
    f0 <- ifelse(ncls < 5, 1.05 - 0.05 * ncls,
      ifelse(ncls < 10, 1.00 - 0.04 * ncls,
        0.80 - 0.02 * ncls
      )
    )
    f1 <- diag(0, ncls)
    f1[row(f1) == col(f1) - 1] <- (1 - f0) / 2
    Fil <- diag(rep(f0, ncls)) + t(f1) + f1
    Fil[, 1] <- Fil[, 1] / sum(Fil[, 1])
    Fil[, ncls] <- Fil[, ncls] / sum(Fil[, ncls])
  }

  ## Algorithm
  FLG <- TRUE
  while (FLG) {
    if (testell - oldtestell < 1e-4 * abs(oldtestell)) {
      FLG <- FALSE
      break
    }
    if (emt == maxemt) {
      message("\nReached ten times the maximum number of iterations.")
      FLG <- FALSE
    }
    emt <- emt + 1
    oldtestell <- testell
    csr <- tmp$U %*% fldmemb
    fsr <- (tmp$Z * (1 - tmp$U)) %*% fldmemb
    llsr <- csr %*% log(PiFR + const) + fsr %*% log(1 - PiFR + const)
    # minllsr <- apply(llsr, 1, min)
    # expllsr <- exp(llsr - minllsr)
    # clsmemb <- round(expllsr / rowSums(expllsr), 1e8)
    clsmemb <- t(apply(llsr, 1, softmax))

    smoothed_memb <- clsmemb %*% Fil

    cjr <- t(tmp$U) %*% smoothed_memb
    fjr <- t(tmp$Z * (1 - tmp$U)) %*% smoothed_memb
    lljf <- cjr %*% log(t(PiFR) + const) + fjr %*% log(t(1 - PiFR) + const)

    # max_log_lljf <- apply(lljf, 1, max)
    # log_lljf_adj <- lljf - max_log_lljf
    # log_fldmemb <- log_lljf_adj - log(rowSums(exp(log_lljf_adj)))
    fldmemb <- t(apply(lljf, 1, softmax))

    if (!any(is.null(conf_mat))) {
      fldmemb <- conf_mat
    }

    cfr <- t(fldmemb) %*% t(tmp$U) %*% smoothed_memb
    ffr <- t(fldmemb) %*% t(tmp$Z * (1 - tmp$U)) %*% smoothed_memb
    oldPiFR <- PiFR
    PiFR <- (cfr + beta1 - 1) / (cfr + ffr + beta1 + beta2 - 2)
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
  if (verbose) {
    message("iter ", emt, " logLik ", format(testell, digits = 6))
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
      message("Strongly ordinal alignment condition was satisfied.")
    }
    if (!SOACflg & WOACflg) {
      message("Weakly ordinal alignment condition was satisfied.")
    }
  }

  ### Model Fit
  cfr <- t(fldmemb) %*% t(tmp$U) %*% clsmemb
  ffr <- t(fldmemb) %*% t(tmp$Z * (1 - tmp$U)) %*% clsmemb
  testell <- sum(cfr * log(PiFR + const) + ffr * log(1 - PiFR + const))
  nparam <- ifelse(model == 1, ncls * nfld, sum(diag(Fil)) * nfld)
  FitIndices <- TestFit(tmp$U, tmp$Z, testell, nparam)

  ret <- structure(list(
    model = model,
    mic = mic,
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
    TestFitIndices = FitIndices,
    SOACflg = SOACflg,
    WOACflg = WOACflg
  ), class = c("exametrika", "Biclustering"))
  return(ret)
}



#' @title Field Analysis
#' @description Output for Field Analysis
#' @param x Biclustering Objects yielded by Biclustering Function
#' @param digits printed digits
#' @return Returns a list of class c("exametrika", "Biclustering", "FieldAnalysis") containing:
#'   \describe{
#'     \item{FieldAnalysisMatrix}{A matrix showing field analysis results with rows
#'       representing items and columns showing:
#'       \describe{
#'         \item{CRR}{Correct Response Rate}
#'         \item{LFE}{Latent Field Estimation}
#'         \item{Field1...FieldN}{Field membership values}
#'       }
#'     }
#'   }
#' @export

FieldAnalysis <- function(x, digits = 4) {
  # data format
  if (class(x)[1] != "exametrika") {
    stop("Field Analysis needs exametrika Output.")
  }
  if (class(x)[2] != "Biclustering") {
    stop("Field Analysis needs Biclustering Output.")
  }
  y <- x$FieldMembership
  crr <- crr(x$U)
  yy <- as.data.frame(y)
  yy <- cbind(crr, x$FieldEstimated, yy)
  colnames(yy) <- c("CRR", "LFE", paste0("Field", 1:x$Nfield))
  yy <- yy[order(yy$CRR, decreasing = TRUE), ]
  yy <- yy[order(yy$LFE), ]
  nr <- NROW(yy)
  nc <- NCOL(yy)
  rownames_tmp <- rownames(yy)
  yy <- matrix(as.numeric(as.matrix(yy)), ncol = nc, nrow = nr)
  colnames(yy) <- c("CRR", "LFE", paste0("Field", 1:x$Nfield))
  rownames(yy) <- rownames_tmp
  return(structure(list(
    FieldAnalysisMatrix = yy
  ), class = c("exametrika", "Biclustering", "FieldAnalysis")))
}
