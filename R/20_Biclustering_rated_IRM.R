#' @rdname Biclustering_IRM
#' @param alpha Dirichlet distribution concentration parameter for the prior density of
#' field reference probabilities (rated/nominal IRM). Must be positive. The default is 1.
#' @return
#' For rated data, the returned list includes:
#' \describe{
#'  \item{Q}{Response matrix.}
#'  \item{U}{Binary correct/incorrect matrix.}
#'  \item{Z}{Missing indicator matrix.}
#'  \item{testlength}{Number of items.}
#'  \item{nobs}{Sample size.}
#'  \item{n_class}{Optimal number of classes.}
#'  \item{n_field}{Optimal number of fields.}
#'  \item{n_cycle}{Number of EM algorithm iterations.}
#'  \item{FRP}{Field Reference Profile (BCRM), a 3D array (nfld x ncls x maxQ).}
#'  \item{FieldFRP}{Field Reference Profile based on binary correct rates (nfld x ncls).}
#'  \item{quasiFRP}{Item-level correct response rate per class (nitems x ncls).}
#'  \item{FRPIndex}{Index of FRP includes the item location parameters B and Beta,
#'  the slope parameters A and Alpha, and the monotonicity indices C and Gamma.}
#'  \item{TRP}{Test Reference Profile.}
#'  \item{LFD}{Latent Field Distribution.}
#'  \item{LCD}{Latent Class Distribution.}
#'  \item{FieldMembership}{Field membership probability matrix.}
#'  \item{ClassMembership}{Class membership probability matrix.}
#'  \item{FieldEstimated}{Estimated field assignment for each item.}
#'  \item{ClassEstimated}{Estimated class assignment for each student.}
#'  \item{Students}{Rank Membership Profile matrix with estimated class.}
#'  \item{FieldAnalysis}{Field analysis with CRR and field memberships.}
#'  \item{TestFitIndices}{Overall fit index for the test (binary layer, default).}
#'  \item{TestFitIndices_nominal}{Fit indices for the nominal layer (AIC/BIC/CAIC only).}
#'  \item{log_lik}{Log-likelihood of the binary layer model.}
#'  \item{log_lik_nominal}{Log-likelihood of the nominal layer model.}
#'  \item{SOACflg}{Logical; TRUE if Strongly Ordinal Alignment Condition is satisfied.}
#'  \item{WOACflg}{Logical; TRUE if Weakly Ordinal Alignment Condition is satisfied.}
#' }
#' @examples
#' \donttest{
#' # Fit a rated Biclustering IRM model
#' result <- Biclustering_IRM(J21S300, gamma_c = 1, gamma_f = 1, verbose = TRUE)
#' plot(result, type = "Array")
#' }
#' @export
Biclustering_IRM.rated <- function(U,
                                   gamma_c = 1, gamma_f = 1, alpha = 1,
                                   max_iter = 100, stable_limit = 5,
                                   minSize = 20, EM_limit = 20,
                                   seed = 123, verbose = TRUE, ...) {
  tmp <- U
  nobs <- NROW(tmp$Q)
  nitems <- NCOL(tmp$Q)
  const <- exp(-nitems)

  # --- Step 1: Run nominal IRM internally ---
  dat_nom <- tmp
  dat_nom$response.type <- "nominal"

  ret_nom <- Biclustering_IRM.nominal(
    dat_nom,
    gamma_c = gamma_c, gamma_f = gamma_f, alpha = alpha,
    max_iter = max_iter, stable_limit = stable_limit,
    minSize = minSize, EM_limit = EM_limit,
    seed = seed, verbose = verbose,
    ...
  )

  # --- Step 2: Retrieve nominal results ---
  ncls <- ret_nom$n_class
  nfld <- ret_nom$n_field
  fld <- ret_nom$FieldEstimated
  fldmemb <- ret_nom$FieldMembership
  fldmemb01 <- sign(fldmemb - apply(fldmemb, 1, max)) + 1
  flddist <- colSums(fldmemb01)
  BCRM <- ret_nom$FRP

  # --- Step 3: Sort classes by correct response rate (Ranklustering) ---
  clsmemb <- ret_nom$ClassMembership
  ord <- order(colSums(t(tmp$U) %*% clsmemb) / colSums(clsmemb))
  clsmemb <- clsmemb[, ord]
  BCRM <- BCRM[, ord, ]
  cls <- apply(clsmemb, 1, which.max)
  clsmemb01 <- sign(clsmemb - apply(clsmemb, 1, max)) + 1
  clsdist <- colSums(clsmemb01)

  # --- Step 4: Student rank membership ---
  StudentRank <- clsmemb
  rownames(StudentRank) <- tmp$ID
  RU <- ifelse(cls + 1 > ncls, NA, cls + 1)
  RD <- ifelse(cls - 1 < 1, NA, cls - 1)
  RUO <- StudentRank[cbind(1:nobs, RU)] / StudentRank[cbind(1:nobs, cls)]
  RDO <- StudentRank[cbind(1:nobs, RD)] / StudentRank[cbind(1:nobs, cls)]
  StudentRank <- cbind(StudentRank, cls, RUO, RDO)
  colnames(StudentRank) <- c(
    paste("Membership", 1:ncls), "Estimate",
    "Rank-Up Odds", "Rank-Down Odds"
  )

  # --- Step 5: Binary fit indices (Layer 1) ---
  PiFR <- matrix(0, nrow = nitems, ncol = ncls)
  for (c in 1:ncls) {
    students_c <- which(cls == c)
    if (length(students_c) == 1) {
      denom <- tmp$Z[students_c, ]
      PiFR[, c] <- (tmp$U[students_c, ] * tmp$Z[students_c, ]) /
        pmax(denom, const)
    } else if (length(students_c) > 1) {
      denom <- colSums(tmp$Z[students_c, ])
      PiFR[, c] <- colSums(tmp$U[students_c, ] * tmp$Z[students_c, ]) /
        pmax(denom, const)
    }
  }

  P_correct <- clsmemb %*% t(PiFR)
  test_log_lik_binary <- sum(tmp$Z * (tmp$U * log(pmax(P_correct, const)) +
    (1 - tmp$U) * log(pmax(1 - P_correct, const))))
  nparam_binary <- nitems * ncls
  FitIndices_binary <- TestFit(tmp$U, tmp$Z, test_log_lik_binary, nparam_binary)

  # --- Step 6: Nominal fit indices (Layer 2) ---
  FitIndices_nominal <- ret_nom$TestFitIndices

  # --- Step 7: FRP labels ---
  msg <- "Rank"
  FRP <- BCRM
  colnames(FRP) <- paste0(msg, 1:ncls)
  rownames(FRP) <- paste0("Field", 1:nfld)
  colnames(fldmemb) <- paste0("Field", 1:nfld)
  rownames(clsmemb) <- tmp$ID
  colnames(clsmemb) <- paste0(msg, 1:ncls)

  # --- Step 8: TRP and FRPIndex (binary-based) ---
  quasiFRP <- PiFR
  rownames(quasiFRP) <- colnames(tmp$U)
  colnames(quasiFRP) <- paste0(msg, 1:ncls)

  FieldFRP <- matrix(0, nrow = nfld, ncol = ncls)
  for (f in 1:nfld) {
    items_f <- which(fld == f)
    if (length(items_f) == 0) {
      FieldFRP[f, ] <- NA
    } else if (length(items_f) == 1) {
      FieldFRP[f, ] <- PiFR[items_f, ]
    } else {
      FieldFRP[f, ] <- colMeans(PiFR[items_f, ])
    }
  }
  rownames(FieldFRP) <- paste0("Field", 1:nfld)
  colnames(FieldFRP) <- paste0(msg, 1:ncls)

  TRP <- colSums(FieldFRP * flddist, na.rm = TRUE)
  valid_fields <- which(!is.na(FieldFRP[, 1]))
  if (length(valid_fields) > 0) {
    FRPIndex <- IRPindex(FieldFRP[valid_fields, , drop = FALSE])
  } else {
    FRPIndex <- data.frame(
      Alpha = numeric(0), A = numeric(0),
      Beta = numeric(0), B = numeric(0),
      Gamma = numeric(0), C = numeric(0)
    )
  }

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

  # --- Step 9: Field Analysis ---
  tmp_binary <- tmp
  tmp_binary$response.type <- "binary"
  crr_vals <- crr(tmp_binary)
  fieldAnalysis <- as.data.frame(fldmemb)
  fieldAnalysis <- cbind(crr_vals, fld, fieldAnalysis)
  colnames(fieldAnalysis) <- c("CRR", "LFE", paste0("Field", 1:nfld))
  fieldAnalysis <- fieldAnalysis[order(fieldAnalysis$CRR, decreasing = TRUE), ]
  fieldAnalysis <- fieldAnalysis[order(fieldAnalysis$LFE), ]
  rownames_tmp <- rownames(fieldAnalysis)
  fieldAnalysis <- matrix(
    as.numeric(as.matrix(fieldAnalysis)),
    ncol = NCOL(fieldAnalysis), nrow = NROW(fieldAnalysis)
  )
  colnames(fieldAnalysis) <- c("CRR", "LFE", paste0("Field", 1:nfld))
  rownames(fieldAnalysis) <- rownames_tmp

  # --- Return ---
  ret <- structure(list(
    model = 2,
    mic = TRUE,
    msg = msg,
    Q = tmp$Q,
    U = tmp$U,
    Z = tmp$Z,
    testlength = nitems,
    nobs = nobs,
    n_class = ncls,
    n_field = nfld,
    n_cycle = ret_nom$n_cycle,
    LFD = flddist,
    LRD = clsdist,
    LCD = clsdist,
    FRP = FRP,
    FieldFRP = FieldFRP,
    quasiFRP = quasiFRP,
    FRPIndex = FRPIndex,
    TRP = TRP,
    CMD = colSums(clsmemb),
    RMD = colSums(clsmemb),
    FieldMembership = fldmemb,
    ClassMembership = clsmemb,
    FieldEstimated = fld,
    ClassEstimated = cls,
    Students = StudentRank,
    FieldAnalysis = fieldAnalysis,
    TestFitIndices = FitIndices_binary,
    TestFitIndices_nominal = FitIndices_nominal,
    log_lik = test_log_lik_binary,
    log_lik_nominal = ret_nom$log_lik,
    SOACflg = SOACflg,
    WOACflg = WOACflg,
    # Deprecated fields (for backward compatibility)
    Nclass = ncls,
    Nfield = nfld,
    N_Cycle = ret_nom$n_cycle,
    LogLik = test_log_lik_binary
  ), class = c("exametrika", "ratedBiclustering"))

  return(ret)
}
