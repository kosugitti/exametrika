# Equating (form A): new items and new respondents on the scale of a fitted
# model.
#
# The parameters of the fitted model are held fixed, so the latent classes or
# ranks of the new respondents are the ones of the fit: the anchor items
# (the items the new data share with the fit) carry the scale. What is
# estimated is only what the fit does not know, which is
#   - for the biclustering family, the field each new item belongs to, and
#   - for the one-mode models (LCA, LRA), the reference profile of each new
#     item.
# The two unknowns are updated alternately: the membership of the respondents
# given the current item parameters (the E-step of predict()), then the item
# parameters given the membership (the item E-step of biclustering, or the
# M-step of the one-mode model, restricted to the new items). The field
# reference profiles themselves are never updated; this is the discrete
# analogue of fixed-parameter calibration in IRT, and no linking
# transformation is needed because the scale is the (ordinal) rank itself.
#
# A new item that fits none of the existing fields is reported, not forced:
# for each new item the marginal likelihood under "a field of its own" (the
# block parameters integrated out under the prior of the fit, as in
# ICL_exact) is compared with the marginal likelihood under "one of the
# existing fields" (uniform over the fields), and the log Bayes factor is
# returned. Estimating new field profiles is left to a later version.

#' @title Equate new items and respondents to a fitted model
#' @description
#' Places a new test form on the scale of a fitted model through the items
#' the two share (the anchor items). The parameters of the fit are held
#' fixed; the new respondents are assigned to its latent classes or ranks,
#' and the items the fit has not seen are placed on the same scale. For the
#' biclustering family (`Biclustering()`, Ranklustering, and the reference
#' objects of [as_biclustering()]) a new item is assigned to one of the
#' existing fields, with a diagnostic that tells when none of them fits. For
#' `LCA()` and `LRA()` (binary, nominal and ordinal) the reference profile of
#' each new item is estimated, under the order restriction of the fit when
#' the fit is isotonic, or with its filter when the fit is a GTM.
#'
#' The two unknowns, the membership of the respondents and the parameters of
#' the new items, are updated alternately until the membership settles. The
#' first step uses the anchor items alone, so the scale is the fit's from the
#' start. Anchor items missing from `newdata` are simply left out; at least
#' one anchor item is required.
#'
#' @details
#' For a biclustering fit the field reference profiles are never changed, so
#' a new item can only be placed in a field the fit already has. To tell
#' whether that is appropriate, `NewFieldLogBF` compares, for each new item,
#' the marginal likelihood of its responses under a field of its own (the
#' block probabilities integrated out under the prior of the fit, as in
#' `ICL_exact`; the Beta or Dirichlet with parameter `alpha`, restricted to
#' the ordered region when the fit is isotonic) with the marginal likelihood
#' under the existing fields (equal prior weight on each field). The
#' respondents enter with their hard assignment, as in `ICL_exact`. A
#' positive value says that the item behaves unlike every field of the fit;
#' in that case the field it was given is the least bad one, and a new field
#' should be considered when the fit is revised. Because a field profile is
#' an average over its items, a single item departs from it more and more
#' measurably as the respondents grow in number, and even the fitted items
#' come out positive in a large sample. The same quantity is therefore
#' computed for the anchor items of `newdata` (`AnchorLogBF`), and a new
#' item is read against that range rather than against zero.
#'
#' @param fit A fitted model: `LCA()` (binary, nominal, rated), `LRA()`
#'   (binary, ordinal, rated), `Biclustering()` (binary, nominal, ordinal,
#'   rated) or a reference object from [as_biclustering()]. The SOM, the IRM
#'   and the network models are not supported.
#' @param newdata New responses: a matrix or data frame with the items in
#'   columns, or an `exametrikaData` object from [dataFormat()]. Column names
#'   are required: the items whose names match the fit are the anchor items,
#'   every other column is a new item. Missing responses (`NA`, or the codes
#'   given in `na`) are left out of the likelihood. Responses must use the
#'   coding of the fitted data.
#' @param na Values to treat as missing, besides `NA`.
#' @param maxiter Maximum number of alternating cycles.
#' @param tol The cycles stop when no membership probability moves by more
#'   than this.
#' @param alpha Concentration of the Beta or Dirichlet prior of the new-field
#'   diagnostic (biclustering) and of the reference profiles of the new items
#'   (nominal and ordinal one-mode models). Default 1, the flat prior of the
#'   estimation.
#' @param M Number of particles of the Monte Carlo behind the order
#'   restriction in the diagnostic of an isotonic fit; see `ICL_exact`.
#' @param seed Seed of that Monte Carlo (the global stream is left untouched).
#' @param CA Rated LCA / LRA only: the correct answer of each new item, named
#'   by item or in the order of the new items in `newdata`. Not needed when
#'   `newdata` is an `exametrikaData` object made with `response.type =
#'   "rated"`, which carries them. Without it the category profiles (`ICRP`)
#'   of the new items are still estimated, but not their correct-response
#'   profiles (`IRP`).
#' @param ... Not used.
#' @return An object of class `exametrikaEquate`, a list with
#' \describe{
#'   \item{Membership, Estimate, log_lik}{As in [predict.exametrika()]:
#'     the posterior membership of the new respondents, their class or rank,
#'     and the log-likelihood of each respondent under the model.}
#'   \item{AnchorItems, NewItems}{The item labels of the two groups.}
#'   \item{FieldMembership, FieldEstimated}{Biclustering: the posterior field
#'     membership of each new item and the field with the largest
#'     probability.}
#'   \item{NewFieldLogBF}{Biclustering: the log Bayes factor of "a field of
#'     its own" against "one of the existing fields" for each new item (see
#'     Details). `NA` when the Monte Carlo could not resolve the order
#'     probability; the value reported is then a bound, with a warning.}
#'   \item{AnchorLogBF}{Biclustering: the same quantity for the anchor items
#'     of `newdata`, the yardstick for the new items. With many respondents
#'     every item departs measurably from its field profile, so a new item
#'     stands out only when it scores well above the anchor items.}
#'   \item{NewItems}{Biclustering: the three columns above as a data frame.}
#'   \item{IRP}{Binary LCA / LRA: the item reference profile of each new
#'     item (items x classes). Rated LCA / LRA with `CA`: the row of `ICRP`
#'     at the correct answer of each new item.}
#'   \item{ICRP}{Nominal, rated LCA / ordinal, rated LRA: the category
#'     reference profile of each new item, one row per category, in the
#'     layout of the fit.}
#'   \item{CA}{Rated LCA / LRA: the correct answers used for `IRP`.}
#'   \item{n_cycle, converge, total_log_lik}{Cycles run, whether `tol` was
#'     met, and the sum of `log_lik`.}
#' }
#' @examples
#' \donttest{
#' # A new form: ten items of the fitted test as anchors, two new items
#' fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
#' X <- J15S500$U[1:100, ]
#' colnames(X)[11:12] <- c("new1", "new2")
#' eq <- equate(fit, X[, 1:12])
#' eq$NewItems
#' head(eq$Estimate)
#' }
#' @export
equate <- function(fit, newdata, ...) {
  UseMethod("equate")
}

#' @rdname equate
#' @export
equate.exametrika <- function(fit, newdata, na = NULL, maxiter = 100, tol = 1e-6,
                              alpha = 1, M = 10000, seed = 1, CA = NULL, ...) {
  model <- class(fit)[2]
  bicl_models <- c(
    "Biclustering", "nominalBiclustering", "ordinalBiclustering",
    "ratedBiclustering", "BiclusteringReference"
  )
  one_mode <- c("LCA", "nominalLCA", "ratedLCA", "LRA", "LRAordinal", "LRArated")
  if (!model %in% c(bicl_models, one_mode)) {
    stop("equate() is not available for ", model, " objects.", call. = FALSE)
  }
  if (model == "LRA" && identical(fit$method, "SOM")) {
    stop("equate() is not available for an LRA fitted with the SOM method.", call. = FALSE)
  }
  bicl <- model %in% bicl_models
  binary <- model %in% c("LCA", "LRA", "Biclustering") ||
    (model == "BiclusteringReference" && is.matrix(fit$FRP))
  nd <- eq_newdata(fit, newdata, na, binary)

  # The anchor items: one E-step with the fitted parameters, as predict() does.
  # Anchor items absent from newdata are all-missing columns and drop out.
  ndA <- list(X = nd$XA, ID = nd$ID, items = nd$items)
  L_A <- switch(model,
    LCA = ,
    LRA = pred_ll_irp(fit, ndA),
    nominalLCA = ,
    ratedLCA = pred_ll_icrp_lca(fit, ndA),
    LRAordinal = ,
    LRArated = pred_ll_icrp_lra(fit, ndA),
    Biclustering = pred_ll_bicl_binary(fit, ndA),
    BiclusteringReference = if (binary) pred_ll_bicl_binary(fit, ndA) else pred_ll_bicl_poly(fit, ndA),
    pred_ll_bicl_poly(fit, ndA)
  )
  ncls <- NCOL(L_A)
  const <- exp(-(NCOL(nd$XA) + NCOL(nd$XN)))

  # Smoothing of the membership on the item side (GTM only) and the order
  # restriction of the fit
  method <- eq_method(fit, model)
  Fil <- if (method == "GTM") create_filter_matrix(ncls) else NULL

  # The new items as a one-hot array N x J_new x Q (missing cells all zero)
  items <- eq_new_items(fit, model, nd, binary)
  A <- items$A
  Q <- dim(A)[3]

  if (bicl) {
    P <- eq_block_array(fit)
    logP <- log(pmax(P, 0) + const)
  }

  m <- row_softmax(L_A)
  converge <- FALSE
  n_cycle <- 0
  for (it in seq_len(maxiter)) {
    n_cycle <- it
    ms <- if (is.null(Fil)) m else m %*% Fil
    # Expected counts of the new items: J_new x C x Q
    counts <- eq_counts(A, ms)
    if (bicl) {
      # Field posterior of each new item (uniform prior over the fields)
      llf <- matrix(0, nrow = dim(A)[2], ncol = dim(P)[1])
      for (q in seq_len(Q)) {
        llf <- llf + matrix(counts[, , q], ncol = ncls) %*% t(matrix(logP[, , q], ncol = ncls))
      }
      G <- row_softmax(llf)
      L_N <- matrix(0, nrow = NROW(A), ncol = ncls)
      for (q in seq_len(Q)) {
        L_N <- L_N + A[, , q] %*% G %*% matrix(logP[, , q], ncol = ncls)
      }
    } else {
      profile <- eq_item_profiles(counts, items$ncat, model, method, alpha, const)
      L_N <- matrix(0, nrow = NROW(A), ncol = ncls)
      for (q in seq_len(Q)) {
        L_N <- L_N + A[, , q] %*% log(matrix(profile[, , q], ncol = ncls) + const)
      }
    }
    m_new <- row_softmax(L_A + L_N)
    delta <- max(abs(m_new - m))
    m <- m_new
    if (delta < tol) {
      converge <- TRUE
      break
    }
  }

  L <- L_A + L_N
  mx <- apply(L, 1, max)
  log_lik <- mx + log(rowSums(exp(L - mx))) - log(ncls)
  msg <- if (!is.null(fit$msg)) fit$msg else "Class"
  colnames(m) <- paste0(msg, seq_len(ncls))
  rownames(m) <- nd$ID
  est <- max.col(m, ties.method = "first")
  names(est) <- nd$ID
  names(log_lik) <- nd$ID

  ret <- list(
    model = model,
    msg = msg,
    Membership = m,
    Estimate = est,
    log_lik = log_lik,
    AnchorItems = nd$anchors,
    NewItems = nd$new_items
  )
  if (bicl) {
    rownames(G) <- nd$new_items
    colnames(G) <- paste0("Field", seq_len(NCOL(G)))
    fld <- max.col(G, ties.method = "first")
    names(fld) <- nd$new_items
    ordered <- method == "isotonic"
    logbf <- eq_new_field_logbf(A, m, P, logP, alpha, ordered, M, seed)
    names(logbf) <- nd$new_items
    # The same diagnostic for the anchor items of this data, as the yardstick:
    # with many respondents every item departs from its field profile, so
    # a new item stands out only against what the fitted items score here.
    ndB <- list(XN = nd$XA[, nd$anchors, drop = FALSE], new_items = nd$anchors)
    A_anchor <- eq_new_items(fit, model, ndB, binary)$A
    logbf_anchor <- eq_new_field_logbf(A_anchor, m, P, logP, alpha, ordered, M, seed)
    names(logbf_anchor) <- nd$anchors
    ret$FieldMembership <- G
    ret$FieldEstimated <- fld
    ret$NewFieldLogBF <- logbf
    ret$AnchorLogBF <- logbf_anchor
    ret$NewItems <- data.frame(
      ItemLabel = nd$new_items,
      Field = fld,
      Membership = G[cbind(seq_along(fld), fld)],
      logBF = logbf,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  } else if (binary) {
    IRP <- matrix(profile[, , 2], ncol = ncls)
    dimnames(IRP) <- list(nd$new_items, paste0("IRP", seq_len(ncls)))
    ret$IRP <- IRP
  } else {
    ret$ICRP <- eq_icrp_frame(profile, items, nd$new_items, msg, ncls)
    if (model %in% c("ratedLCA", "LRArated")) {
      # The correct answer of each new item picks its row of the ICRP, as
      # LCA.rated builds IRP from CA
      ca <- eq_new_ca(nd, CA)
      if (is.null(ca)) {
        message(
          "The correct answers of the new items were not given (CA), so only ",
          "their category profiles (ICRP) are returned."
        )
      } else {
        ret$CA <- ca
        ret$IRP <- eq_correct_profiles(profile, items, nd$new_items, ca, ncls)
      }
    }
  }
  ret$n_cycle <- n_cycle
  ret$converge <- converge
  ret$total_log_lik <- sum(log_lik)
  return(structure(ret, class = c("exametrikaEquate", "exametrika")))
}

#' @export
print.exametrikaEquate <- function(x, digits = 3, ...) {
  cat(sprintf(
    "Equating with a %s fit: %d anchor items, %d new items, %d respondents\n",
    x$model, length(x$AnchorItems), NROW(x$NewItems), NROW(x$Membership)
  ))
  cat(sprintf(
    "%s in %d cycles\n\n", if (x$converge) "Converged" else "Not converged", x$n_cycle
  ))
  cat(sprintf("Latent %s distribution of the estimates:\n", tolower(x$msg)))
  tab <- table(factor(x$Estimate, levels = seq_len(NCOL(x$Membership))))
  names(tab) <- colnames(x$Membership)
  print(tab)
  if (!is.null(x$FieldMembership)) {
    cat("\nNew items:\n")
    df <- x$NewItems
    df$Membership <- round(df$Membership, digits)
    df$logBF <- round(df$logBF, digits)
    print(df, row.names = FALSE)
    cat("\n  logBF: a field of its own against the fields of the fit (> 0 favours its own).\n")
    ab <- x$AnchorLogBF[is.finite(x$AnchorLogBF)]
    if (length(ab) > 0) {
      cat(sprintf(
        "  The anchor items of this data score from %s to %s (median %s).\n",
        format(round(min(ab), digits), nsmall = 0), format(round(max(ab), digits), nsmall = 0),
        format(round(stats::median(ab), digits), nsmall = 0)
      ))
    }
  } else {
    if (!is.null(x$ICRP)) {
      cat("\nCategory reference profiles of the new items:\n")
      df <- x$ICRP
      num <- vapply(df, is.numeric, logical(1))
      df[num] <- lapply(df[num], round, digits)
      print(df, row.names = FALSE)
    }
    if (!is.null(x$IRP)) {
      cat(if (is.null(x$ICRP)) {
        "\nReference profiles of the new items:\n"
      } else {
        "\nCorrect-response profiles of the new items (the rows of their correct answers):\n"
      })
      print(round(x$IRP, digits))
    }
  }
  return(invisible(x))
}

#' @title Build a biclustering reference from published field profiles
#' @description
#' Assembles the minimal object that [equate()] and [predict()] need from a
#' field reference profile matrix and the field of each item, so that a new
#' form can be equated, or new respondents scored, from published values
#' alone, without the data the model was fitted to.
#'
#' @param FRP The field reference profiles: a fields x classes matrix of
#'   correct-response probabilities for binary data, or a
#'   fields x classes x categories array of category probabilities for
#'   polytomous data (each row summing to one over the categories).
#' @param fields The field of each item, as an integer vector named by the
#'   item labels (a factor or a character vector of field numbers is also
#'   accepted).
#' @param category_shift Polytomous data only: the amount the response codes
#'   must be moved up to start at 1 (0 when the codes are already 1, 2,
#'   ...; 1 when they are 0-based). The `CategoryShift` of a fit.
#' @param ordered Whether the profiles are order-restricted across the
#'   classes (an isotonic Ranklustering). Only the new-field diagnostic of
#'   [equate()] reads it.
#' @param msg "Class" or "Rank": how the latent groups are labelled.
#' @return An object of class `BiclusteringReference`, accepted by
#'   [equate()] and [predict()].
#' @examples
#' fit <- Biclustering(J15S500, ncls = 3, nfld = 2, method = "B", verbose = FALSE)
#' ref <- as_biclustering(fit$FRP, fit$FieldEstimated)
#' p <- predict(ref, J15S500$U[1:5, ])
#' p$Estimate
#' @export
as_biclustering <- function(FRP, fields, category_shift = 0, ordered = FALSE,
                            msg = c("Class", "Rank")) {
  msg <- match.arg(msg)
  if (is.data.frame(FRP)) FRP <- as.matrix(FRP)
  if (!is.numeric(FRP) || !(is.matrix(FRP) || length(dim(FRP)) == 3)) {
    stop("FRP must be a numeric fields x classes matrix or a fields x classes x categories array.",
      call. = FALSE
    )
  }
  if (any(!is.finite(FRP)) || any(FRP < 0) || any(FRP > 1)) {
    stop("FRP must hold probabilities.", call. = FALSE)
  }
  binary <- is.matrix(FRP)
  if (!binary && any(abs(rowSums(FRP, dims = 2) - 1) > 1e-6)) {
    stop("Each row of FRP must sum to one over the categories.", call. = FALSE)
  }
  nfld <- dim(FRP)[1]
  ncls <- dim(FRP)[2]
  labels <- names(fields)
  if (is.null(labels) || anyDuplicated(labels) > 0 || any(labels == "")) {
    stop("fields must be named by the item labels, without duplicates.", call. = FALSE)
  }
  f <- if (is.factor(fields)) as.integer(as.character(fields)) else suppressWarnings(as.integer(fields))
  if (any(is.na(f)) || any(f < 1 | f > nfld)) {
    stop("fields must give each item a field number between 1 and ", nfld, ".", call. = FALSE)
  }
  nitems <- length(f)
  fldmemb <- matrix(0, nrow = nitems, ncol = nfld)
  fldmemb[cbind(seq_len(nitems), f)] <- 1
  dimnames(fldmemb) <- list(labels, paste0("Field", seq_len(nfld)))
  names(f) <- labels
  if (binary) {
    dimnames(FRP) <- list(paste0("Field", seq_len(nfld)), paste0(msg, seq_len(ncls)))
  } else {
    dimnames(FRP) <- list(
      paste0("Field", seq_len(nfld)), paste0(msg, seq_len(ncls)),
      paste0("Category", seq_len(dim(FRP)[3]))
    )
  }
  return(structure(list(
    msg = msg,
    testlength = nitems,
    n_class = ncls,
    n_field = nfld,
    ItemLabel = labels,
    FRP = FRP,
    FieldMembership = fldmemb,
    FieldEstimated = f,
    CategoryShift = category_shift,
    Ordered = isTRUE(ordered)
  ), class = c("exametrika", "BiclusteringReference")))
}

print_biclustering_reference_case <- function(x, digits) {
  binary <- is.matrix(x$FRP)
  cat(sprintf(
    "Biclustering reference: %d fields, %d %s, %d items (%s)\n\n",
    x$n_field, x$n_class, if (x$msg == "Rank") "ranks" else "classes",
    x$testlength, if (binary) "binary" else sprintf("%d categories", dim(x$FRP)[3])
  ))
  cat("Field Reference Profile\n")
  if (binary) {
    print(round(x$FRP, digits))
  } else {
    for (q in seq_len(dim(x$FRP)[3])) {
      cat(sprintf("Category %d\n", q))
      print(round(matrix(x$FRP[, , q], nrow = x$n_field, dimnames = dimnames(x$FRP)[1:2]), digits))
    }
  }
  cat("\nField of each item\n")
  print(x$FieldEstimated)
  return(invisible(x))
}

# ---- internals --------------------------------------------------------------

# Split newdata into the anchor items (in the order of the fit, all-missing
# columns for the fit's items that newdata lacks) and the new items
eq_newdata <- function(fit, newdata, na, binary) {
  items <- pred_item_labels(fit)
  if (inherits(newdata, "exametrikaData")) {
    X <- if (binary) newdata$U else newdata$Q
    if (is.null(X)) {
      stop("newdata does not hold ", if (binary) "binary (U)" else "polytomous (Q)",
        " responses.",
        call. = FALSE
      )
    }
    X <- as.matrix(X) * 1
    X[newdata$Z == 0] <- NA
    colnames(X) <- newdata$ItemLabel
    id <- newdata$ID
    CA_data <- if (is.null(newdata$CA)) NULL else stats::setNames(as.vector(newdata$CA), newdata$ItemLabel)
  } else {
    CA_data <- NULL
    # as.matrix() keeps a bare matrix bare; as.data.frame() would name its
    # columns V1, V2, ... and defeat the check below
    X <- as.matrix(newdata)
    if (!is.numeric(X)) {
      stop("newdata must hold numeric response codes.", call. = FALSE)
    }
    id <- rownames(newdata)
  }
  if (!is.null(na)) {
    X[X %in% na] <- NA
  }
  if (is.null(colnames(X)) || any(colnames(X) == "")) {
    stop("newdata needs item names: the anchor items are matched to the fit by name.",
      call. = FALSE
    )
  }
  anchors <- items[items %in% colnames(X)]
  if (length(anchors) == 0) {
    stop("newdata shares no item with the fitted model.", call. = FALSE)
  }
  new_items <- setdiff(colnames(X), items)
  if (length(new_items) == 0) {
    stop("newdata holds no new item; use predict() to score respondents on the fitted items.",
      call. = FALSE
    )
  }
  if (binary && any(!is.na(X) & !X %in% c(0, 1))) {
    stop("A binary model needs responses coded 0/1.", call. = FALSE)
  }
  XA <- matrix(NA_real_, nrow = NROW(X), ncol = length(items), dimnames = list(NULL, items))
  XA[, anchors] <- X[, anchors]
  XN <- X[, new_items, drop = FALSE]
  if (is.null(id) || length(id) != NROW(X)) {
    id <- paste0("Student", seq_len(NROW(X)))
  }
  return(list(
    XA = XA, XN = XN, ID = as.character(id), items = items,
    anchors = anchors, new_items = new_items,
    CA_data = if (is.null(CA_data)) NULL else CA_data[new_items]
  ))
}

#' The correct answers of the new items of a rated fit
#'
#' Taken from the `CA` argument when given (named by item, or in the order
#' of the new items in `newdata`), else from the `CA` of an `exametrikaData`
#' newdata; NULL when neither has them.
#' @noRd
eq_new_ca <- function(nd, CA) {
  if (is.null(CA)) {
    ca <- nd$CA_data
    if (is.null(ca) || all(is.na(ca))) {
      return(NULL)
    }
    return(ca)
  }
  # unlist() keeps the names a list or data frame column carries; as.vector()
  # would drop them and the match by item with them
  CA <- unlist(CA)
  if (!is.null(names(CA)) && all(nzchar(names(CA)))) {
    miss <- setdiff(nd$new_items, names(CA))
    if (length(miss) > 0) {
      stop("CA has no entry for the new item(s): ", paste(miss, collapse = ", "), ".",
        call. = FALSE
      )
    }
    return(stats::setNames(CA[nd$new_items], nd$new_items))
  }
  if (length(CA) != length(nd$new_items)) {
    stop("CA must have one correct answer per new item (", length(nd$new_items),
      "), named by item or in the order of the new items in newdata.",
      call. = FALSE
    )
  }
  return(stats::setNames(CA, nd$new_items))
}

#' Correct-response profiles of the new items of a rated fit
#'
#' The row of each item's category profile at its correct answer, as
#' `LCA.rated()` builds `IRP` from `CA`. A correct answer that nobody chose
#' has no row and gives NA, with a warning.
#' @noRd
eq_correct_profiles <- function(profile, items, new_items, ca, ncls) {
  IRP <- matrix(NA_real_, nrow = length(new_items), ncol = ncls)
  dimnames(IRP) <- list(new_items, paste0("IRP", seq_len(ncls)))
  unseen <- character()
  for (j in seq_along(new_items)) {
    pos <- match(ca[j], items$codes[[j]])
    if (is.na(pos)) {
      unseen <- c(unseen, sprintf("%s (CA = %s)", new_items[j], ca[j]))
    } else {
      IRP[j, ] <- profile[j, , pos]
    }
  }
  if (length(unseen) > 0) {
    warning(
      "The correct answer of ", paste(unseen, collapse = ", "),
      " was never chosen in newdata; its IRP is NA.",
      call. = FALSE
    )
  }
  return(IRP)
}

# "isotonic", "GTM" or "none": what the fit imposes across the classes
eq_method <- function(fit, model) {
  if (model %in% c("LRA", "LRAordinal", "LRArated")) {
    # LRA.rated is a GTM fit and stores no method
    return(if (is.null(fit$method)) "GTM" else fit$method)
  }
  if (model %in% c("Biclustering", "ordinalBiclustering", "ratedBiclustering")) {
    if (isTRUE(fit$model == 2) && !is.null(fit$estimation) && !is.na(fit$estimation)) {
      return(fit$estimation)
    }
    return("none")
  }
  if (model == "BiclusteringReference" && isTRUE(fit$Ordered)) {
    return("isotonic")
  }
  return("none")
}

# Block probabilities of a biclustering fit as a fields x classes x Q array
# (binary: slice 1 incorrect, slice 2 correct, as in binary_indicator_array)
eq_block_array <- function(fit) {
  FRP <- fit$FRP
  if (is.data.frame(FRP)) FRP <- as.matrix(FRP)
  if (length(dim(FRP)) == 3) {
    return(FRP)
  }
  FRP <- as.matrix(FRP)
  P <- array(0, dim = c(dim(FRP), 2))
  P[, , 1] <- 1 - FRP
  P[, , 2] <- FRP
  return(P)
}

# One-hot array of the new items. Binary: Q = 2 with slice 2 = correct.
# Biclustering polytomous: the common shift of the fit. One-mode polytomous:
# each item's own observed codes, in increasing order.
eq_new_items <- function(fit, model, nd, binary) {
  XN <- nd$XN
  if (binary) {
    Z <- 1 * !is.na(XN)
    U <- XN
    U[is.na(U)] <- 0
    A <- array(0, dim = c(NROW(XN), NCOL(XN), 2))
    A[, , 1] <- Z * (1 - U)
    A[, , 2] <- Z * U
    return(list(A = A, ncat = rep(2L, NCOL(XN)), codes = NULL))
  }
  if (model %in% c("nominalBiclustering", "ordinalBiclustering", "ratedBiclustering", "BiclusteringReference")) {
    maxQ <- dim(fit$FRP)[3]
    shift <- fit$CategoryShift
    if (is.null(shift)) shift <- 0
    P <- XN + shift
    bad <- !is.na(P) & (P < 1 | P > maxQ | P != round(P))
    if (any(bad)) {
      pred_warn_unseen(vapply(which(colSums(bad) > 0), function(j) {
        sprintf("%s: %s", nd$new_items[j], paste(sort(unique(XN[bad[, j], j])), collapse = ", "))
      }, ""))
      P[bad] <- NA
    }
    return(list(A = pred_onehot(P, maxQ), ncat = rep(maxQ, NCOL(XN)), codes = NULL))
  }
  codes <- lapply(seq_len(NCOL(XN)), function(j) sort(unique(XN[!is.na(XN[, j]), j])))
  ncat <- lengths(codes)
  if (any(ncat == 0)) {
    stop("New item(s) without any response: ",
      paste(nd$new_items[ncat == 0], collapse = ", "), ".",
      call. = FALSE
    )
  }
  P <- pred_recode_items(XN, codes, nd$new_items)
  return(list(A = pred_onehot(P, max(ncat)), ncat = ncat, codes = codes))
}

# Expected counts of the new items: J_new x C x Q, from a one-hot array and a
# (possibly smoothed) membership
eq_counts <- function(A, ms) {
  counts <- array(0, dim = c(dim(A)[2], NCOL(ms), dim(A)[3]))
  for (q in seq_len(dim(A)[3])) {
    counts[, , q] <- t(A[, , q]) %*% ms
  }
  return(counts)
}

# Reference profiles of the new items of a one-mode model from their expected
# counts: J_new x C x Q category probabilities. Binary: MLE of the
# correct-response probability, with weighted PAVA across the ranks for an
# isotonic LRA. Nominal: Dirichlet(alpha) MAP. Ordinal: the order-restricted
# MAP of the isotonic LRA, or the MAP of the (filter-smoothed) counts for a
# GTM.
eq_item_profiles <- function(counts, ncat, model, method, alpha, const) {
  J <- dim(counts)[1]
  C <- dim(counts)[2]
  Q <- dim(counts)[3]
  profile <- array(0, dim = c(J, C, Q))
  if (model %in% c("LCA", "LRA")) {
    n <- counts[, , 1] + counts[, , 2]
    p <- counts[, , 2] / n
    p[!is.finite(p)] <- 0.5
    p <- matrix(pmax(pmin(p, 1 - const), const), nrow = J)
    if (method == "isotonic") {
      for (j in seq_len(J)) {
        p[j, ] <- pava_up(p[j, ], matrix(n, nrow = J)[j, ])$fitted
      }
    }
    profile[, , 2] <- p
    profile[, , 1] <- 1 - p
    return(profile)
  }
  for (j in seq_len(J)) {
    Qj <- ncat[j]
    n_j <- matrix(counts[j, , seq_len(Qj)], nrow = C) # C x Qj
    if (model == "LRAordinal" && method == "isotonic" && Qj >= 2) {
      P_j <- iso_dual_map(n_j + (alpha - 1), tol = 1e-6)
    } else {
      numer <- pmax(n_j + alpha - 1, 0)
      tot <- rowSums(numer)
      P_j <- numer / tot
      P_j[tot <= 0, ] <- 1 / Qj
    }
    profile[j, , seq_len(Qj)] <- P_j
  }
  return(profile)
}

# Category reference profiles of the new items as a data frame in the layout
# of the fit (one row per category)
eq_icrp_frame <- function(profile, items, new_items, msg, ncls) {
  rows <- do.call(rbind, lapply(seq_along(new_items), function(j) {
    Qj <- items$ncat[j]
    data.frame(
      ItemLabel = new_items[j],
      CategoryLabel = items$codes[[j]],
      matrix(profile[j, , seq_len(Qj)], nrow = Qj, byrow = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  colnames(rows)[-(1:2)] <- paste0(tolower(msg), seq_len(ncls))
  rownames(rows) <- NULL
  return(rows)
}

# New-field diagnostic of each new item: log p(responses | a field of its own)
# - log p(responses | one of the existing fields), with the respondents at
# their hard assignment. The first integrates the block probabilities out
# under the Dirichlet(alpha) prior (the order-restricted one when ordered,
# with the Monte Carlo of ICL_exact); the second averages the likelihood over
# the existing fields with equal prior weight.
eq_new_field_logbf <- function(A, m, P, logP, alpha, ordered, M, seed) {
  cls01 <- hard_membership(m)
  counts <- eq_counts(A, cls01)
  J <- dim(counts)[1]
  C <- dim(counts)[2]
  Q <- dim(counts)[3]
  nfld <- dim(P)[1]
  a <- rep(alpha, length.out = Q)
  log_exist <- numeric(J)
  log_own <- numeric(J)
  for (j in seq_len(J)) {
    n_j <- matrix(counts[j, , ], nrow = C) # C x Q
    llf <- vapply(seq_len(nfld), function(f) {
      sum(n_j * matrix(logP[f, , ], nrow = C))
    }, numeric(1))
    mx <- max(llf)
    log_exist[j] <- mx + log(sum(exp(llf - mx))) - log(nfld)
    log_own[j] <- log_dirmult_blocks(array(n_j, dim = c(1, C, Q)), alpha, Q)
  }
  if (ordered && C > 1) {
    res <- with_private_seed(seed, {
      pr <- log_order_prior(C, a, M)
      po <- lapply(seq_len(J), function(j) {
        log_order_posterior(matrix(counts[j, , ], nrow = C), a, M)
      })
      list(
        prior = pr$log_p, post = vapply(po, `[[`, numeric(1), "log_p"),
        capped = vapply(po, `[[`, logical(1), "capped")
      )
    })
    log_own <- log_own + res$post - res$prior
    if (any(res$capped)) {
      warning(
        "equate(): the posterior probability of the rank order fell below the ",
        "Monte Carlo resolution for ", sum(res$capped), " new item(s); ",
        "NewFieldLogBF is a bound there."
      )
    }
  }
  return(log_own - log_exist)
}
