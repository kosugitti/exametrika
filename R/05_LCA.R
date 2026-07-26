#' @title Latent Class Analysis
#' @description
#' Performs Latent Class Analysis (LCA) on binary response data using the Expectation-Maximization (EM) algorithm.
#' LCA identifies unobserved (latent) subgroups of examinees with similar response patterns,
#' and estimates both the class characteristics and individual membership probabilities.
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data is given,
#' it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param ncls Number of latent classes to identify (between 2 and 20). Default is 2.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1 indicate
#' observed responses, while 0 indicates missing data.
#' @param w Item weight vector specifying the relative importance of each item.
#' @param na Values to be treated as missing values.
#' @param maxiter Maximum number of EM algorithm iterations. Default is 1000.
#' @param verbose Logical; if TRUE, displays progress during estimation. Default is FALSE.
#' @param beta1 Beta distribution parameter 1 for prior density of class reference matrix. Default is 1.
#' @param beta2 Beta distribution parameter 2 for prior density of class reference matrix. Default is 1.
#' @param conf Confirmatory IRP matrix (items x ncls) for test equating.
#'   Same format as the IRP output. Non-NA values are fixed throughout estimation,
#'   NA values are freely estimated. Fixed values must be in the open interval (0, 1).
#'   When row names are present, items are matched by label; otherwise by position.
#'   Default is NULL (fully exploratory).
#'
#' @return
#' An object of class "exametrika" and "LCA" containing:
#' \describe{
#'  \item{msg}{A character string indicating the model type. }
#'  \item{testlength}{Length of the test (number of items).}
#'  \item{nobs}{Sample size (number of rows in the dataset).}
#'  \item{Nclass}{Number of latent classes specified.}
#'  \item{N_Cycle}{Number of EM algorithm iterations performed.}
#'  \item{converge}{Logical value indicating whether the algorithm converged within maxiter iterations}
#'  \item{TRP}{Test Reference Profile vector showing expected scores for each latent class.
#'    Calculated as the column sum of the estimated class reference matrix.}
#'  \item{LCD}{Latent Class Distribution vector showing the number of examinees assigned to each latent class.}
#'  \item{CMD}{Class Membership Distribution vector showing the sum of membership probabilities for each latent class.}
#'  \item{Students}{Class Membership Profile matrix showing the posterior probability of each examinee
#'    belonging to each latent class. The last column ("Estimate") indicates the most likely class assignment.}
#'  \item{IRP}{Item Reference Profile matrix where each row represents an item and each column
#'    represents a latent class. Values indicate the probability of a correct response
#'    for members of that class.}
#'  \item{ItemFitIndices}{Fit indices for each item. See also \code{\link{ItemFit}}.}
#'  \item{TestFitIndices}{Overall fit indices for the test. See also \code{\link{TestFit}}.}
#' }
#'
#' @details
#' Latent Class Analysis is a statistical method for identifying unobserved subgroups within
#' a population based on observed response patterns. It assumes that examinees belong to one
#' of several distinct latent classes, and that the probability of a correct response to each
#' item depends on class membership.
#'
#' The algorithm proceeds by:
#' 1. Initializing class reference probabilities
#' 2. Computing posterior class membership probabilities for each examinee (E-step)
#' 3. Re-estimating class reference probabilities based on these memberships (M-step)
#' 4. Iterating until convergence or reaching the maximum number of iterations
#'
#' Unlike Item Response Theory (IRT), LCA treats latent variables as categorical rather than
#' continuous, identifying distinct profiles rather than positions on a continuum.
#'
#' @references
#' Goodman, L. A. (1974). Exploratory latent structure analysis using both identifiable and
#' unidentifiable models. Biometrika, 61(2), 215-231.
#'
#' Lazarsfeld, P. F., & Henry, N. W. (1968). Latent structure analysis.
#' Boston: Houghton Mifflin.
#'
#' @examples
#' \donttest{
#' # Fit a Latent Class Analysis model with 5 classes to the sample dataset
#' result.LCA <- LCA(J15S500, ncls = 5)
#'
#' # Display the first few rows of student class membership probabilities
#' head(result.LCA$Students)
#'
#' # Plot Item Response Profiles (IRP) for items 1-6 in a 2x3 grid
#' # Shows probability of correct response for each item across classes
#' plot(result.LCA, type = "IRP", items = 1:6, nc = 2, nr = 3)
#'
#' # Plot Class Membership Probabilities (CMP) for students 1-9 in a 3x3 grid
#' # Shows probability distribution of class membership for each student
#' plot(result.LCA, type = "CMP", students = 1:9, nc = 3, nr = 3)
#'
#' # Plot Test Response Profile (TRP) showing expected scores for each class
#' plot(result.LCA, type = "TRP")
#'
#' # Plot Latent Class Distribution (LCD) showing class sizes
#' plot(result.LCA, type = "LCD")
#'
#' # Compare models with different numbers of classes
#' # (In practice, you might try more class counts)
#' lca2 <- LCA(J15S500, ncls = 2)
#' lca3 <- LCA(J15S500, ncls = 3)
#' lca4 <- LCA(J15S500, ncls = 4)
#' lca5 <- LCA(J15S500, ncls = 5)
#'
#' # Compare BIC values to select optimal number of classes
#' # (Lower BIC indicates better fit)
#' data.frame(
#'   Classes = 2:5,
#'   BIC = c(
#'     lca2$TestFitIndices$BIC,
#'     lca3$TestFitIndices$BIC,
#'     lca4$TestFitIndices$BIC,
#'     lca5$TestFitIndices$BIC
#'   )
#' )
#' }
#'
#' @export
LCA <- function(U, ...) {
  UseMethod("LCA")
}

#' @rdname LCA
#' @param ... Additional arguments passed to specific methods.
#'
#' @export
LCA.default <- function(U, na = NULL, Z = NULL, w = NULL, ...) {
  if (inherits(U, "exametrika")) {
    if (U$response.type == "binary") {
      return(LCA.binary(U, ...))
    } else if (U$response.type == "nominal") {
      return(LCA.nominal(U, ...))
    } else if (U$response.type == "rated") {
      return(LCA.rated(U, ...))
    } else if (U$response.type == "ordinal") {
      # Latent classes carry no order, so the category order cannot enter the
      # model: estimation is the nominal one. Say so rather than silently
      # treating ordered ratings as unordered labels.
      message(
        "Latent classes are unordered, so the category order is not used in ",
        "estimation; the model fitted is the nominal one. ",
        "Use LRA() if the ordering should be respected."
      )
      return(LCA.nominal(U, ...))
    }
    response_type_error(U$response.type, "LCA")
  }

  U <- dataFormat(U, na = na, Z = Z, w = w)
  return(LCA(U, ...))
}

#' @rdname LCA
#' @export
LCA.binary <- function(U, ncls = 2, na = NULL, Z = NULL, w = NULL, maxiter = 1000,
                       verbose = FALSE, beta1 = 1, beta2 = 1, conf = NULL, ...) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (tmp$response.type != "binary") {
    response_type_error(tmp$response.type, "LCA")
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  # Validate and align confirmatory IRP matrix
  if (!is.null(conf)) {
    conf <- validate_conf(conf, ncls, colnames(tmp$U))
  }

  fit <- emclus(tmp$U, tmp$Z, ncls,
    Fil = diag(rep(1, ncls)),
    beta1 = beta1, beta2 = beta2, maxiter = maxiter,
    mic = FALSE,
    verbose = verbose,
    conf = conf
  )

  ## Returns
  #### Class Information
  TRP <- fit$classRefMat %*% tmp$w
  bMax <- matrix(rep(apply(fit$postDist, 1, max), ncls), ncol = ncls)
  clsNum <- apply(fit$postDist, 1, which.max)
  cls01 <- sign(fit$postDist - bMax) + 1
  LCD <- colSums(cls01)
  CMD <- colSums(fit$postDist)
  StudentClass <- cbind(fit$postDist, clsNum)
  colnames(StudentClass) <- c(paste("Membership", 1:ncls), "Estimate")
  rownames(StudentClass) <- tmp$ID
  ### Item Information
  IRP <- t(fit$classRefMat)
  colnames(IRP) <- paste0("IRP", 1:ncls)

  ### Model Fit
  # each Items
  ell_A <- item_log_lik(tmp$U, tmp$Z, fit$postDist, fit$classRefMat)
  FitIndices <- ItemFit(tmp$U, tmp$Z, ell_A, ncls)

  ret <- structure(list(
    msg = "Class",
    testlength = testlength <- NCOL(tmp$U),
    nobs = NROW(tmp$U),
    n_class = ncls, # New naming convention
    n_cycle = fit$iter, # New naming convention
    converge = fit$converge,
    TRP = as.vector(TRP),
    LCD = as.vector(LCD),
    CMD = as.vector(CMD),
    Students = StudentClass,
    IRP = IRP,
    ItemFitIndices = FitIndices$item,
    TestFitIndices = FitIndices$test,
    log_lik = FitIndices$test$model_log_like,
    # Deprecated fields (for backward compatibility)
    Nclass = ncls,
    N_Cycle = fit$iter
  ), class = c("exametrika", "LCA"))
  return(ret)
}

#' @rdname LCA
#' @param alpha Dirichlet prior parameter for the category profiles
#'   (nominal data only). Default 1, which leaves the M-step at the plain
#'   multinomial MLE.
#'
#' @details
#' For nominal data the model is a finite mixture of product-multinomial
#' distributions: every latent class carries an independent category
#' distribution for each item, and no ordering is imposed on the classes or on
#' the categories. Category counts may differ across items.
#'
#' Ordered rating data is routed to this method as well, because unordered
#' latent classes give the category order nothing to attach to. Use
#' \code{\link{LRA}} when the ordering should be respected.
#'
#' No benchmark (saturated) model is fitted, for the same reason as in
#' \code{Biclustering.nominal}: with many items and categories nearly every
#' response pattern is unique, so the saturated log-likelihood is not
#' informative. Only AIC, BIC and CAIC are reported; the chi-square based
#' indices are NA.
#'
#' @export
LCA.nominal <- function(U, ncls = 2, na = NULL, Z = NULL, w = NULL, maxiter = 1000,
                        verbose = FALSE, alpha = 1, ...) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (!tmp$response.type %in% c("nominal", "ordinal")) {
    response_type_error(tmp$response.type, "LCA")
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  tmp$Q <- remap_category_codes(tmp$Q)
  nobs <- NROW(tmp$Q)
  nitems <- NCOL(tmp$Q)
  ncat <- as.vector(tmp$categories)
  const <- exp(-nitems)

  fit <- emclus_nominal(tmp$Q, tmp$Z, ncls,
    ncat = ncat, alpha = alpha,
    maxiter = maxiter, verbose = verbose
  )

  ## Returns
  #### Class Information
  clsNum <- apply(fit$clsmemb, 1, which.max)
  bMax <- matrix(rep(apply(fit$clsmemb, 1, max), ncls), ncol = ncls)
  cls01 <- sign(fit$clsmemb - bMax) + 1
  LCD <- colSums(cls01)
  CMD <- colSums(fit$clsmemb)
  StudentClass <- cbind(fit$clsmemb, clsNum)
  colnames(StudentClass) <- c(paste("Membership", 1:ncls), "Estimate")
  rownames(StudentClass) <- tmp$ID

  ### Item Information
  # profile is items x classes x max(ncat) with the slots beyond an item's own
  # category count held at zero; flatten to the ragged sum(ncat) layout so that
  # items with different category counts line up with their labels.
  idx <- do.call(rbind, lapply(seq_len(nitems), function(j) {
    cbind(j, seq_len(ncat[j]))
  }))
  cat_probs <- t(vapply(
    seq_len(nrow(idx)),
    function(r) fit$profile[idx[r, 1], , idx[r, 2]],
    numeric(ncls)
  ))
  ICRP <- as.data.frame(cat_probs)
  colnames(ICRP) <- paste0("class", 1:ncls)
  ICRP <- cbind(
    ItemLabel = rep(tmp$ItemLabel, ncat),
    CategoryLabel = unlist(tmp$CategoryLabel),
    ICRP
  )

  ### Model Fit
  # Null model: each item's marginal category distribution, ignoring classes.
  maxQ <- max(ncat)
  Uq <- array(0, dim = c(nobs, nitems, maxQ))
  valid <- as.vector(tmp$Z) == 1
  Uq[cbind(
    rep(seq_len(nobs), times = nitems)[valid],
    rep(seq_len(nitems), each = nobs)[valid],
    as.vector(tmp$Q)[valid]
  )] <- 1
  ZU <- Uq * as.vector(tmp$Z)
  ZU_col_sums <- colSums(ZU, dims = 1)
  NullFRQ <- ZU_col_sums / colSums(tmp$Z)
  ell_N <- sum(ZU_col_sums * log(NullFRQ + const))

  # Class sizes are not free parameters in this formulation (the E-step gives
  # every class the same implicit prior, as in emclus() for binary data), so
  # the count is the category profiles alone.
  nparam <- ncls * sum(ncat - 1)
  testell <- fit$log_lik
  AIC <- -2 * testell + 2 * nparam
  CAIC <- -2 * testell + nparam * (log(nobs) + 1)
  BIC <- -2 * testell + nparam * log(nobs)

  FitIndices <- structure(
    list(
      model_log_like = testell,
      bench_log_like = NA,
      null_log_like = ell_N,
      model_Chi_sq = NA,
      null_Chi_sq = NA,
      model_df = NA,
      null_df = NA,
      NFI = NA,
      RFI = NA,
      IFI = NA,
      TLI = NA,
      CFI = NA,
      RMSEA = NA,
      AIC = AIC,
      CAIC = CAIC,
      BIC = BIC
    ),
    class = c("exametrika", "ModelFit")
  )

  ret <- structure(list(
    msg = "Class",
    testlength = nitems,
    nobs = nobs,
    n_class = ncls,
    n_cycle = fit$iter,
    converge = fit$converge,
    categories = ncat,
    ItemLabel = tmp$ItemLabel,
    ICRP = ICRP,
    LCD = as.vector(LCD),
    CMD = as.vector(CMD),
    Students = StudentClass,
    TestFitIndices = FitIndices,
    log_lik = testell,
    # Deprecated fields (for backward compatibility)
    Nclass = ncls,
    N_Cycle = fit$iter,
    LogLik = testell
  ), class = c("exametrika", "nominalLCA"))
  return(ret)
}

#' @rdname LCA
#' @details
#' For rated data (multiple-choice items with a key) the estimation is the
#' nominal one — \code{LCA.rated} calls \code{LCA.nominal} internally — and the
#' key is used afterwards to recover the quantities that need a notion of a
#' correct answer. The Item Reference Profile is the model-implied probability
#' of the keyed category, \code{IRP[j, c] = rho[j, CA[j] | c]}, and the Test
#' Reference Profile is its weighted item sum. Unlike
#' \code{\link{Biclustering}} on rated data, the classes are not sorted by
#' correct response rate: latent classes carry no order, and sorting them would
#' suggest one.
#'
#' Two layers of fit are reported. \code{TestFitIndices} is the binary layer,
#' built from correct/incorrect responses under the class-membership-weighted
#' correct probabilities, so it carries the usual chi-square based indices and
#' is comparable with binary \code{LCA}. \code{TestFitIndicesNominal} is the
#' nominal layer taken from the internal fit, with AIC/BIC/CAIC only. The full
#' category probabilities stay in \code{ICRP} for distractor analysis.
#'
#' @export
LCA.rated <- function(U, ncls = 2, na = NULL, Z = NULL, w = NULL, maxiter = 1000,
                      verbose = FALSE, alpha = 1, ...) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (tmp$response.type != "rated") {
    response_type_error(tmp$response.type, "LCA")
  }

  # --- Step 1: estimate as nominal ---------------------------------------
  # Reuse the already-formatted object instead of re-running dataFormat, as
  # Biclustering.rated does.
  dat_nom <- tmp
  dat_nom$response.type <- "nominal"
  ret_nom <- LCA.nominal(dat_nom,
    ncls = ncls, maxiter = maxiter,
    verbose = verbose, alpha = alpha, ...
  )

  nobs <- NROW(tmp$Q)
  nitems <- NCOL(tmp$Q)
  const <- exp(-nitems)
  ncat <- as.vector(tmp$categories)
  CA <- as.vector(tmp$CA)

  # --- Step 2: correct-category probabilities ----------------------------
  # ICRP holds one row per (item, category) in item-major order, so the row of
  # item j's keyed category is the offset of item j plus CA[j].
  offset <- c(0, cumsum(ncat)[-nitems])
  key_rows <- offset + CA
  IRP <- as.matrix(ret_nom$ICRP[key_rows, paste0("class", 1:ncls), drop = FALSE])
  rownames(IRP) <- tmp$ItemLabel
  colnames(IRP) <- paste0("IRP", 1:ncls)

  TRP <- as.vector(t(IRP) %*% tmp$w)

  # --- Step 3: binary layer of fit ---------------------------------------
  clsmemb <- ret_nom$Students[, 1:ncls, drop = FALSE]
  P_correct <- clsmemb %*% t(IRP)
  ell_binary <- sum(tmp$Z * (tmp$U * log(pmax(P_correct, const)) +
    (1 - tmp$U) * log(pmax(1 - P_correct, const))))
  FitIndices <- TestFit(tmp$U, tmp$Z, ell_binary, nitems * ncls)

  ret <- structure(list(
    msg = "Class",
    testlength = nitems,
    nobs = nobs,
    n_class = ncls,
    n_cycle = ret_nom$n_cycle,
    converge = ret_nom$converge,
    categories = ncat,
    ItemLabel = tmp$ItemLabel,
    CA = CA,
    IRP = IRP,
    TRP = TRP,
    ICRP = ret_nom$ICRP,
    LCD = ret_nom$LCD,
    CMD = ret_nom$CMD,
    Students = ret_nom$Students,
    TestFitIndices = FitIndices,
    TestFitIndicesNominal = ret_nom$TestFitIndices,
    log_lik = ell_binary,
    log_lik_nominal = ret_nom$log_lik,
    # Deprecated fields (for backward compatibility)
    Nclass = ncls,
    N_Cycle = ret_nom$n_cycle,
    LogLik = ell_binary
  ), class = c("exametrika", "ratedLCA"))
  return(ret)
}
