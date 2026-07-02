#' @rdname Biclustering
#' @param ncls Number of latent classes/ranks to identify (between 2 and 20).
#' @param nfld Number of latent fields (item clusters) to identify.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#' observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @param method Analysis method to use (character string):
#'   * "B" or "Biclustering": Standard biclustering (default for binary and
#'     ordinal data)
#'   * "R" or "Ranklustering": Ranklustering with ordered class structure
#'     (default for rated data, where classes are sorted by correct response
#'     rate)
#' @param conf Confirmatory parameter for pre-specified field assignments. Can be either:
#'   * A vector with items and corresponding fields in sequence
#'   * A field membership profile matrix (items x fields) with 0/1 values
#'   * NULL (default) for exploratory analysis where field memberships are estimated
#' @param maxiter Maximum number of EM algorithm iterations. Default is 100.
#' @param verbose Logical; if TRUE, displays progress during estimation. Default is FALSE.
#' @param alpha Dirichlet distribution concentration parameter for prior density of field reference probabilities. Default is 1.
#' @param ... Additional arguments passed to specific methods.
#'
#' @examples
#' \donttest{
#' # Perform Biclustering for rated sample data
#' # Analyze data with 5 fields and 6 classes
#' result.Bi <- Biclustering(J35S5000, nfld = 5, ncls = 6, method = "R")
#' }
#' @export
Biclustering.rated <- function(U,
                               ncls = 2, nfld = 2,
                               method = "R",
                               conf = NULL,
                               conf_class = NULL,
                               maxiter = 100,
                               verbose = FALSE,
                               alpha = 1, ...) {
  tmp <- U
  nobs <- NROW(tmp$Q)
  nitems <- NCOL(tmp$Q)
  const <- exp(-nitems)

  # --- Step 1: Run nominal Biclustering internally ---
  # Reuse the already-formatted exametrika object as nominal (avoid re-calling dataFormat)
  dat_nom <- tmp
  dat_nom$response.type <- "nominal"

  ret_nom <- Biclustering.nominal(
    dat_nom,
    ncls = ncls,
    nfld = nfld,
    conf = conf,
    conf_class = conf_class,
    maxiter = maxiter,
    verbose = verbose,
    alpha = alpha,
    ...
  )

  # --- Step 2: Retrieve nominal results ---
  fld <- ret_nom$FieldEstimated
  fldmemb <- ret_nom$FieldMembership
  fldmemb01 <- sign(fldmemb - apply(fldmemb, 1, max)) + 1
  flddist <- colSums(fldmemb01)
  BCRM <- ret_nom$FRP

  # --- Step 3: Sort classes by correct response rate (Ranklustering) ---
  if (method == "R" || method == "Ranklustering") {
    model <- 2
    mic <- TRUE
    msg <- "Rank"
  } else {
    model <- 1
    mic <- FALSE
    msg <- "Class"
  }

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
    StudentRank <- cbind(StudentRank, Estimate = cls)
    colnames(StudentRank) <- c(paste("Membership", 1:ncls), "Estimate")
  }

  # --- Step 5: Binary fit indices (Layer 1) ---
  # Compute item correct response rate per class (no field constraint)
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

  # --- Step 6: Nominal fit indices (Layer 2) --- from ret_nom directly
  FitIndices_nominal <- ret_nom$TestFitIndices

  # --- Step 7: FRP labels ---
  FRP <- BCRM
  colnames(FRP) <- paste0(msg, 1:ncls)
  rownames(FRP) <- paste0("Field", 1:nfld)
  colnames(fldmemb) <- paste0("Field", 1:nfld)
  rownames(clsmemb) <- tmp$ID
  colnames(clsmemb) <- paste0(msg, 1:ncls)

  # --- Step 8: TRP and FRPIndex (binary-based) ---
  # quasiFRP: item-level correct response rate per class (J x C, no field constraint)
  # "quasi" because field assignments come from nominal analysis, not binary
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
  # IRPindex only on non-empty fields
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
    model = model,
    mic = mic,
    msg = msg,
    converge = ret_nom$converge,
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
