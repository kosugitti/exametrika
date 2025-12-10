#' @title Biclustering with Infinite Relational Model
#' @description
#'  This function performs Biclustering structure learning using the Infinite Relational Model (IRM)
#'  to automatically determine the optimal number of classes C and optimal number of
#'  fields F. It can be found in a single run of the analysis, but
#'  it takes a long computation time when the sample size S is large.
#'  This method incorporates the Chinese restaurant process
#'  and Gibbs sampling. In detail, See Section 7.8 in Shojima(2022).
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param gamma_c \eqn{\gamma_C} is the hyperparameter of the CRP and represents the
#' attractiveness of a new Class. As \eqn{\gamma_C} increases, the student is more likely
#' to be seated at a vacant class. The default is 1.
#' @param gamma_f \eqn{\gamma_F} is the hyperparameter of the CRP and represents the
#' attractiveness of a new Field. The greater this value it more likely to be classified
#' in the new field. The default is 1.
#' @param max_iter A maximum iteration number of IRM process. The default is 100.
#' @param stable_limit The IRM process exits the loop when the FRM stabilizes and no longer
#'  changes significantly. This option sets the maximum number of stable iterations,
#'  with a default of 5.
#' @param minSize A value used for readjusting the number of classes.If the size of each
#' class is less than \code{minSize}, the number of classes will be reduced. Note that this
#' under limit of size is not used for either all correct or all incorrect class.
#' @param EM_limit After IRM process, resizing the number of classes process will starts.
#' This process using EM algorithm,\code{EM_limit} is the maximum number of iteration with
#' default of 20.
#' @param seed seed value for random numbers.
#' @param verbose verbose output Flag. default is TRUE
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{msg}{A character string indicating the model type. }
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{n_class}{Optimal number of classes (new naming convention).}
#'  \item{n_field}{Optimal number of fields (new naming convention).}
#'  \item{em_cycle}{Number of EM algorithm iterations (new naming convention).}
#'  \item{Nclass}{Optimal number of classes (deprecated, use n_class).}
#'  \item{Nfield}{Optimal number of fields (deprecated, use n_field).}
#'  \item{EM_Cycle}{Number of EM algorithm iterations (deprecated, use em_cycle).}
#'  \item{BRM}{Bicluster Reference Matrix}
#'  \item{FRP}{Field Reference Profile}
#'  \item{FRPIndex}{Index of FFP includes the item location parameters B and Beta,
#'  the slope parameters A and Alpha, and the monotonicity indices C and Gamma.}
#'  \item{TRP}{Test Reference Profile}
#'  \item{FMP}{Field Membership Profile}
#'  \item{Students}{Rank Membership Profile matrix.The s-th row vector of \eqn{\hat{M}_R}, \eqn{\hat{m}_R}, is the
#' rank membership profile of Student s, namely the posterior probability distribution representing the student's
#' belonging to the respective latent classes. It also includes the rank with the maximum estimated membership probability,
#' as well as the rank-up odds and rank-down odds.}
#'  \item{LRD}{Latent Rank Distribution. see also [plot.exametrika]}
#'  \item{LFD}{Latent Field Distribution. see also [plot.exametrika]}
#'  \item{RMD}{Rank Membership Distribution.}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#' }
#' @importFrom stats rmultinom
#' @examples
#' \donttest{
#' # Fit a Biclustering model with automatic structure learning using IRM
#' # gamma_c and gamma_f are concentration parameters for the Chinese Restaurant Process
#' result <- Biclustering_IRM(J35S515, gamma_c = 1, gamma_f = 1, verbose = TRUE)
#'
#' # Display the Bicluster Reference Matrix (BRM) as a heatmap
#' # Shows the discovered clustering structure of items and students
#' plot(result, type = "Array")
#'
#' # Plot Field Reference Profiles (FRP) in a 3-column grid
#' # Shows the probability patterns for each automatically determined field
#' plot(result, type = "FRP", nc = 3)
#'
#' # Plot Test Reference Profile (TRP)
#' # Shows the overall response pattern across all fields
#' plot(result, type = "TRP")
#' }
#'
#' @export

Biclustering_IRM <- function(U, Z = NULL, w = NULL, na = NULL,
                             gamma_c = 1, gamma_f = 1,
                             max_iter = 100, stable_limit = 5, minSize = 20, EM_limit = 20,
                             seed = 123, verbose = TRUE) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)
  const <- 1e-100
  gamp <- 1

  # Initialize
  set.seed(seed)
  limit_count <- 0
  iter <- 1
  pattern <- sort(unique(nrs(tmp)))
  zeroG <- ifelse(pattern[1] == 0, 1, 0)
  fullG <- ifelse(pattern[length(pattern)] == testlength, 1, 0)

  ## Initial Class
  ncls <- length(pattern)
  cls01 <- matrix(0, ncol = length(pattern), nrow = nobs)
  for (i in 1:nobs) {
    cls01[i, which((pattern == nrs(tmp)[i]))] <- 1
  }
  colnames(cls01) <- paste("Class", 1:ncls)
  rownames(cls01) <- tmp$ID
  cls <- cls01 %*% (1:ncls)
  Nc <- colSums(cls01)

  ## Initial Field
  nfld <- testlength
  field <- 1:testlength
  crr <- crr(tmp)
  df_tmp <- data.frame(field, crr)
  df_tmp <- df_tmp[order(crr, decreasing = T), ]
  sorted_list <- df_tmp$field
  field <- order(sorted_list)
  fld01 <- matrix(0, nrow = testlength, ncol = nfld)
  for (i in 1:testlength) {
    fld01[i, field[i]] <- 1
  }
  colnames(fld01) <- paste("Field", 1:nfld)
  rownames(fld01) <- tmp$ItemLabel
  Nf <- colSums(fld01)

  ## Inner Function for Ccf/Fcf/Ncf/Pcf
  Ccf_Fcf_Pcf <- function(U, Z, cls01, fld01, gamp, zeroG, fullG, type = 1) {
    Ccf <- t(cls01) %*% tmp$U %*% fld01
    Fcf <- t(cls01) %*% (tmp$Z * (1 - tmp$U)) %*% fld01
    Ncf <- Ccf + Fcf
    Pcf <- (Ccf + gamp - 1) / (Ncf + 2 * gamp - (type * 2))
    if (zeroG == 1) {
      Pcf[1, ] <- 0
    }
    if (fullG == 1) {
      Pcf[nrow(Pcf), ] <- 1
    }
    return(list(Ccf = Ccf, Fcf = Fcf, Ncf = Ncf, Pcf = Pcf))
  }
  ## Inner Function for Sorting PCF
  Pcf_sort <- function(Pcf, cls01, fld01, ncls, nfld) {
    sort_list <- order(colSums(Pcf), decreasing = TRUE)
    Pcf <- Pcf[, sort_list]
    fld01 <- fld01[, sort_list]
    colnames(fld01) <- paste("Field", 1:nfld)
    field <- t((1:nfld) %*% t(fld01))

    sort_list2 <- order(rowSums(Pcf), decreasing = FALSE)
    Pcf <- Pcf[sort_list2, ]
    cls01 <- cls01[, sort_list2]
    colnames(cls01) <- paste("Class", 1:ncls)
    cls <- cls01 %*% (1:ncls)
    return(list(Pcf = Pcf, cls01 = cls01, fld01 = fld01, cls = cls, field = field))
  }

  ### Initial value for parameter set and the model fit
  ret <- Ccf_Fcf_Pcf(tmp$U, tmp$Z, cls01, fld01, gamp, zeroG, fullG, type = 1)
  Ccf <- ret$Ccf
  Fcf <- ret$Fcf
  Ncf <- ret$Ncf
  Pcf <- ret$Pcf

  # IRM Iteration ------------------------------------------------------
  IRM_FLG <- TRUE
  while (IRM_FLG) {
    ### Gibbs sampler i
    Cif <- tmp$U %*% fld01 # SxF. number of correct response Studet S in Field f
    Fif <- (tmp$Z * (1 - tmp$U)) %*% fld01 # SxF. number of incorrect response Studet S in Field f
    iRand <- sample(1:nobs, nobs, replace = F)

    for (loop in 1:nobs) {
      target <- iRand[loop]

      if (cls[target] >= 2 && cls[target] <= ncls - 1) {
        # delete selected member
        Nc[cls[target]] <- Nc[cls[target]] - 1
        Ncf[cls[target], ] <- Ncf[cls[target], ] - Nf
        Ccf[cls[target], ] <- Ccf[cls[target], ] - Cif[target, ]
        Fcf[cls[target], ] <- Fcf[cls[target], ] - Fif[target, ]
        CcfPlus <- Ccf + matrix(rep(Cif[target, ], ncls), nrow = ncls, byrow = T)
        FcfPlus <- Fcf + matrix(rep(Fif[target, ], ncls), nrow = ncls, byrow = T)

        if (Nc[cls[target]] == 0) {
          ## if the class disappeared...
          ncls <- ncls - 1
          delCls <- cls[target]
          cls01 <- cls01[, -delCls]
          cls <- cls01 %*% (1:ncls)
          Nc <- Nc[-delCls]
          Ncf <- Ncf[-delCls, ]
          Ccf <- Ccf[-delCls, ]
          Fcf <- Fcf[-delCls, ]
          CcfPlus <- CcfPlus[-delCls, ]
          FcfPlus <- FcfPlus[-delCls, ]
        }

        # Table Choice!
        ## Likelihood for Existing Table
        Nc_tmp <- Nc[-1]
        Nc_tmp <- Nc_tmp[-(length(Nc_tmp))]
        exist_tab_tmp <- log((Nc_tmp / (nobs - 1 + gamma_c)) + const)
        vec1 <- numeric(ncls - 2)
        vec2 <- numeric(ncls - 2)
        vec3 <- numeric(ncls - 2)
        for (i in 2:(ncls - 1)) {
          Ctmp <- 0
          Ftmp <- 0
          den <- 0
          for (j in 1:nfld) {
            ct1 <- (CcfPlus[i, j] - Ccf[i, j] - 1)
            if (ct1 >= 0) {
              for (s in 0:ct1) {
                Ctmp <- Ctmp + log(CcfPlus[i, j] + gamp - 1 - s + const)
              }
            }
            ct2 <- (FcfPlus[i, j] - Fcf[i, j] - 1)
            if (ct2 >= 0) {
              for (s in 0:ct2) {
                Ftmp <- Ftmp + log(FcfPlus[i, j] + gamp - 1 - s + const)
              }
            }
            ct3 <- CcfPlus[i, j] + FcfPlus[i, j] - Ccf[i, j] - Fcf[i, j] - 1
            if (ct3 >= 0) {
              for (s in 0:ct3) {
                den <- den +
                  (log(CcfPlus[i, j] + FcfPlus[i, j] + 2 * gamp - 1 - s + const))
              }
            }
          }
          vec1[i - 1] <- Ctmp
          vec2[i - 1] <- Ftmp
          vec3[i - 1] <- den
        }
        exist_tab <- exist_tab_tmp + vec1 + vec2 - vec3

        ## Likelihood for New Table
        new_tab_tmp <- log((gamma_c / (nobs - 1 + gamma_c)) + const)
        num <- 0
        den <- 0
        for (j in 1:nfld) {
          maxS <- Cif[target, j] - 1
          if (maxS >= 0) {
            for (s in 0:maxS) {
              num <- num + log(Cif[target, j] + gamp - 1 - s + const)
            }
          }
          maxS <- Nf[j] - Cif[target, j] - 1
          if (maxS >= 0) {
            for (s in 0:maxS) {
              num <- num + log(Nf[j] - Cif[target, j] + gamp - 1 - s + const)
            }
          }
          maxS <- Nf[j] - 1
          if (maxS >= 0) {
            for (s in 0:maxS) {
              den <- den + log(Nf[j] + 2 * gamp - 1 - s + const)
            }
          }
        }
        new_tab <- num - den + new_tab_tmp

        # Likelihood to Probability
        ptab <- c(exist_tab, new_tab)
        mintab <- min(ptab)
        exptab <- exp(ptab - mintab)
        ptab <- exptab / sum(exptab)
        ptab <- round(ptab + const, digits = -log10(const))

        sampled_value <- rmultinom(1, 1, ptab)
        newclass01 <- c(0, sampled_value, 0)
        delpos <- length(newclass01) - 1
        if (newclass01[delpos] == 0) {
          # Student s belongs an other class
          cls01[target, ] <- newclass01[-delpos]
          cls[target] <- which.max(cls01[target, ])
        } else {
          # Student s belongs to a new class
          ncls <- ncls + 1
          new_class <- which.max(newclass01)
          cls01_left <- cls01[, 1:(ncls - 2)]
          cls01_right <- cls01[, (ncls - 1)]
          zeros <- rep(0, nrow(cls01))
          cls01 <- cbind(cls01_left, zeros, cls01_right)
          cls01[target, ] <- newclass01

          cls <- cls01 %*% (1:ncls)
          Ccf <- t(cls01) %*% tmp$U %*% fld01
          Fcf <- t(cls01) %*% (tmp$Z * (1 - tmp$U)) %*% fld01
          Ncf <- Ccf + Fcf
        }

        # Update Number of Correct/Incorrect response
        Nc <- colSums(cls01)
        Ccf[cls[target], ] <- Ccf[cls[target], ] + Cif[target, ]
        Fcf[cls[target], ] <- Fcf[cls[target], ] + Fif[target, ]
        Ncf[cls[target], ] <- Ccf[cls[target], ] + Fcf[cls[target], ]
      }
    }

    # Gibbs sampler j
    oldField <- field # J x C
    Cjc <- t(tmp$U) %*% cls01
    Fjc <- t(tmp$Z * (1 - tmp$U)) %*% cls01
    jRand <- sample(1:testlength, testlength, replace = F)

    for (loop in 1:testlength) {
      target <- jRand[loop]

      # delete selected item
      Nf[field[target]] <- Nf[field[target]] - 1
      Ccf[, field[target]] <- Ccf[, field[target]] - t(Cjc[target, ])
      Fcf[, field[target]] <- Fcf[, field[target]] - t(Fjc[target, ])
      Ncf <- Ccf + Fcf
      CcfPlus <- Ccf + matrix(rep(Cjc[target, ], nfld), nrow = ncls, byrow = F)
      FcfPlus <- Fcf + matrix(rep(Fjc[target, ], nfld), nrow = ncls, byrow = F)

      ## ## if the field disappeared...
      if (Nf[field[target]] == 0) {
        nfld <- nfld - 1
        delFld <- field[target]
        fld01 <- fld01[, -delFld]
        field <- fld01 %*% (1:nfld)
        Nf <- Nf[-delFld]
        Ncf <- Ncf[, -delFld]
        Ccf <- Ccf[, -delFld]
        Fcf <- Fcf[, -delFld]
        CcfPlus <- CcfPlus[, -delFld]
        FcfPlus <- FcfPlus[, -delFld]
      }

      # Table Choice!
      ## Likelihood for each field
      log_tab_tmp <- log((Nf / (testlength - 1 + gamma_f)) + const)
      vec1 <- rep(0, nfld)
      vec2 <- rep(0, nfld)
      vec3 <- rep(0, nfld)
      for (j in 1:nfld) {
        Ctmp <- 0
        Ftmp <- 0
        den <- 0
        for (i in 1:ncls) {
          ct1 <- CcfPlus[i, j] - Ccf[i, j] - 1
          if (ct1 >= 0) {
            for (s in 0:ct1) {
              Ctmp <- Ctmp + log(CcfPlus[i, j] + gamp - 1 - s + const)
            }
          }
          ct2 <- FcfPlus[i, j] - Fcf[i, j] - 1
          if (ct2 >= 0) {
            for (s in 0:ct2) {
              Ftmp <- Ftmp + log(FcfPlus[i, j] + gamp - 1 - s + const)
            }
          }
          ct3 <- CcfPlus[i, j] + FcfPlus[i, j] - Ccf[i, j] - Fcf[i, j] - 1
          if (ct3 >= 0) {
            for (s in 0:ct3) {
              den <- den + (log(CcfPlus[i, j] + FcfPlus[i, j] + 2 * gamp - 1 - s + const))
            }
          }
        }
        vec1[j] <- Ctmp
        vec2[j] <- Ftmp
        vec3[j] <- den
      }
      log_tab <- log_tab_tmp + vec1 + vec2 - vec3
      minLog <- min(log_tab)
      log_exp_tab <- log_tab - minLog
      maxLogExp <- max(log_exp_tab)
      exptab <- exp(log_exp_tab - maxLogExp)
      ptab <- exptab / sum(exptab)
      ptab <- round(ptab + const, digits = -log10(const))

      selected_fld <- rmultinom(1, 1, ptab)
      fld01[target, ] <- selected_fld
      field[target] <- which.max(selected_fld)
      Nf[field[target]] <- Nf[field[target]] + 1
      Ccf[, field[target]] <- Ccf[, field[target]] + t(Cjc[target, ])
      Fcf[, field[target]] <- Fcf[, field[target]] + t(Fjc[target, ])
      Ncf[, field[target]] <- Ccf[, field[target]] + Fcf[, field[target]]
    }

    limit_count <- if (sum(abs(oldField - field)) == 0) {
      limit_count <- limit_count + 1
    } else {
      limit_count <- 0
    }
    if (verbose) {
      message(
        sprintf(
          "iter %d: match=%d nfld=%d ncls=%d",
          iter, limit_count, nfld, ncls
        )
      )
    }
    if (limit_count == stable_limit || iter == max_iter) {
      IRM_FLG <- FALSE
    } else {
      iter <- iter + 1
    }
  }

  # Pcf update
  ret <- Ccf_Fcf_Pcf(tmp$U, tmp$Z, cls01, fld01, gamp, zeroG, fullG, type = 1)
  Ccf <- ret$Ccf
  Fcf <- ret$Fcf
  Ncf <- ret$Ncf
  Pcf <- ret$Pcf

  # model Fit
  llm <- sum(Ccf * log(Pcf + const) + Fcf * log(1 - Pcf + const))
  df <- nobs * testlength - ncls * nfld
  bic <- -2 * llm - df * log(nobs)
  indices <- c(llm, df, bic)

  # sort
  ret <- Pcf_sort(Pcf, cls01, fld01, ncls, nfld)
  Pcf <- ret$Pcf
  cls01 <- ret$cls01
  fld01 <- ret$fld01
  cls <- ret$cls
  field <- ret$field

  ### Pcf Update
  Cif <- tmp$U %*% fld01
  Fif <- (tmp$Z * (1 - tmp$U)) %*% fld01
  ret <- Ccf_Fcf_Pcf(tmp$U, tmp$Z, cls01, fld01, gamp, zeroG, fullG, type = 0)
  Ccf <- ret$Ccf
  Fcf <- ret$Fcf
  Ncf <- ret$Ncf
  Pcf <- ret$Pcf


  # Reorganizing small-sized classses -------------------------------
  zero_position <- which(rowSums(tmp$Z * tmp$U, na.rm = T) == 0)
  full_position <- which(rowSums(tmp$Z * tmp$U, na.rm = T) == testlength)
  delt <- 0
  delrep <- ncls

  DelRepFLG <- TRUE
  while (DelRepFLG) {
    bestfit <- 10^10
    Nc <- colSums(cls01)
    NcTable <- matrix(Nc)
    NcTable <- cbind(NcTable, 1:ncls)
    if (zeroG == 1) {
      NcTable <- NcTable[-1, ]
    }
    if (fullG == 1) {
      NcTable <- NcTable[-nrow(NcTable), ]
    }
    minclass <- NcTable[order(NcTable[, 1]), ][1, ]
    if (minclass[1] < minSize) {
      delt <- delt + 1
      if (verbose) {
        message(
          sprintf(
            "Adjusting classes: BIC=%.1f ncls=%d (min size < %d)",
            bic, ncls, minSize
          )
        )
      }
    } else {
      DelRepFLG <- FALSE
      break
    }

    ncls <- ncls - 1
    delc <- minclass[2]
    Pcf <- Pcf[-delc, ]

    EMt <- 1
    zeta <- log(rep(1, ncls) / ncls)

    EMrepFLG <- TRUE
    while (EMrepFLG) {
      log_num_Zic <- Cif %*% t(log(Pcf + const)) + Fif %*% t(log(1 - Pcf + const)) + matrix(rep(zeta, nobs), nrow = nobs)
      numeZic <- exp(log_num_Zic)
      if (zeroG == 1) {
        numeZic[, 1] <- rep(0, nobs)
      }
      if (fullG == 1) {
        numeZic[, ncol(numeZic)] <- rep(0, nobs)
      }
      denomZi <- rowSums(numeZic)

      Zic <- numeZic / denomZi
      Zic_max_list <- apply(Zic, 1, max)

      cls01 <- sign(Zic - Zic_max_list) + 1
      zero_scorer <- rep(0, ncls)
      zero_scorer[1] <- 1
      full_scorer <- rep(0, ncls)
      full_scorer[ncls] <- 1
      cls01[zero_position, ] <- matrix(rep(zero_scorer, length(zero_position)), nrow = length(zero_position), byrow = T)
      cls01[full_position, ] <- matrix(rep(full_scorer, length(full_position)), nrow = length(full_position), byrow = T)

      cls <- cls01 %*% (1:ncls)

      ret <- Ccf_Fcf_Pcf(tmp$U, tmp$Z, cls01, fld01, gamp, zeroG, fullG, type = 0)
      Ccf <- ret$Ccf
      Fcf <- ret$Fcf
      Ncf <- ret$Ncf
      Pcf <- ret$Pcf

      llm <- sum(Ccf * log(Pcf + const) + Fcf * log(1 - Pcf + const))
      df <- nobs * testlength - ncls * nfld
      bic <- -2 * llm - df * log(nobs)
      indices <- c(llm, df, bic)
      fit <- bic
      if (fit < bestfit) {
        EMt <- EMt + 1
        bestfit <- fit
        best_index <- indices
        bestPcf <- Pcf
        bestclass <- class
      } else {
        Pcf <- bestPcf
        class <- bestclass
        EMrepFLG <- FALSE
        break
      }
      if (EMt >= EM_limit) {
        stop("The EM algorithm has reached its limit. It may not have converged.")
      }
    }
  }


  # Model Fit Finally -----------------------------------------------
  ncls <- max(cls)
  nfld <- max(field)
  cls01 <- matrix(0, ncol = ncls, nrow = nobs)
  for (i in 1:nobs) {
    cls01[i, cls[i]] <- 1
  }
  colnames(cls01) <- paste("Class", 1:ncls)
  rownames(cls01) <- tmp$ID

  fld01 <- matrix(0, nrow = testlength, ncol = nfld)
  for (i in 1:testlength) {
    fld01[i, field[i]] <- 1
  }
  colnames(fld01) <- paste("Field", 1:nfld)
  rownames(fld01) <- tmp$ItemLabel

  ret <- Ccf_Fcf_Pcf(tmp$U, tmp$Z, cls01, fld01, gamp, zeroG, fullG, type = 1)
  Ccf <- ret$Ccf
  Fcf <- ret$Fcf
  Ncf <- ret$Ncf
  Pcf <- ret$Pcf

  llm <- sum(Ccf * log(Pcf + const) + Fcf * log(1 - Pcf + const))
  FitIndices <- TestFit(tmp$U, tmp$Z, llm, ncls * nfld)

  # Sort ------------------------------------------------------------
  ret <- Pcf_sort(Pcf, cls01, fld01, ncls, nfld)
  Pcf <- ret$Pcf
  cls01 <- ret$cls01
  fld01 <- ret$fld01
  cls <- ret$cls
  field <- ret$field


  # Output ---------------------------------------------------------
  pifr <- t(Pcf)
  flddist <- colSums(fld01)
  clsdist <- colSums(cls01)
  TRP <- colSums(pifr * flddist)

  ret <- structure(list(
    U = U,
    testlength = testlength,
    msg = "Class",
    nobs = nobs,
    n_class = ncls, # New naming convention
    n_field = nfld, # New naming convention
    em_cycle = EMt, # New naming convention
    LFD = flddist,
    LCD = clsdist,
    FRP = pifr,
    TRP = TRP,
    FieldEstimated = field,
    ClassEstimated = cls,
    TestFitIndices = FitIndices,
    # Deprecated fields (for backward compatibility)
    Nclass = ncls,
    Nfield = nfld,
    EM_Cycle = EMt
  ), class = c("exametrika", "IRM"))
  return(ret)
}

#' @title IRM (Deprecated)
#' @description
#' This function has been renamed to \code{\link{Biclustering_IRM}}.
#' Please use \code{Biclustering_IRM} instead.
#' @param ... All arguments passed to \code{\link{Biclustering_IRM}}
#' @export
IRM <- function(...) {
  .Deprecated("Biclustering_IRM")
  Biclustering_IRM(...)
}
