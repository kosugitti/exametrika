#' @title Classify new respondents with a fitted model
#' @description
#' Assigns new respondents to the latent classes or ranks of a fitted model.
#' The estimated parameters (and, for biclustering, the field memberships of
#' the items) are held fixed and one E-step is run on the new responses, with
#' the uniform class prior the estimation itself uses (or the rank prior given
#' by `trapezoidal` in the ordinal and rated LRA). On the data the model was
#' fitted to, the result reproduces the membership stored in the fit.
#'
#' Supported fits: `LCA()` (binary, nominal, ordinal, rated), `LRA()` (binary,
#' ordinal, rated) and `Biclustering()` / Ranklustering (binary, nominal,
#' ordinal, rated). The IRM and network models are not supported.
#'
#' @param object A fitted model.
#' @param newdata New responses: a matrix or data frame with the items in
#'   columns (named as in the fitted data; extra columns are ignored, and the
#'   item names are required unless the column count already matches), or an
#'   `exametrikaData` object from [dataFormat()]. Missing responses (`NA`, or
#'   the codes given in `na`) are allowed and simply left out of the
#'   likelihood. Responses must use the coding of the fitted data.
#' @param na Values to treat as missing, besides `NA`.
#' @param ... Not used.
#' @return An object of class `exametrikaPrediction`, a list with
#' \describe{
#'   \item{Membership}{Matrix of posterior membership probabilities
#'     (respondents x classes or ranks).}
#'   \item{Estimate}{The class or rank with the largest membership.}
#'   \item{log_lik}{Log-likelihood of each respondent's responses under the
#'     model (the class or rank mixed out).}
#'   \item{model}{The class of the fitted object.}
#'   \item{msg}{"Class" or "Rank".}
#' }
#' A response with a category the fitted data never used for that item cannot
#' be scored by the model; it is treated as missing, with a warning.
#' @examples
#' \donttest{
#' fit <- LCA(J15S500, ncls = 5)
#' pred <- predict(fit, J15S500$U[1:10, ])
#' pred$Estimate
#' }
#' @importFrom stats predict
#' @export
predict.exametrika <- function(object, newdata, na = NULL, ...) {
  model <- class(object)[2]
  supported <- c(
    "LCA", "nominalLCA", "ratedLCA", "LRA", "LRAordinal", "LRArated",
    "Biclustering", "nominalBiclustering", "ordinalBiclustering",
    "ratedBiclustering"
  )
  if (!model %in% supported) {
    stop("predict() is not available for ", model, " objects.", call. = FALSE)
  }
  binary <- model %in% c("LCA", "LRA", "Biclustering")
  nd <- pred_newdata(object, newdata, na, binary)
  L <- switch(model,
    LCA = ,
    LRA = pred_ll_irp(object, nd),
    nominalLCA = ,
    ratedLCA = pred_ll_icrp_lca(object, nd),
    LRAordinal = ,
    LRArated = pred_ll_icrp_lra(object, nd),
    Biclustering = pred_ll_bicl_binary(object, nd),
    nominalBiclustering = ,
    ordinalBiclustering = ,
    ratedBiclustering = pred_ll_bicl_poly(object, nd)
  )
  ncls <- NCOL(L)
  m <- apply(L, 1, max)
  log_lik <- m + log(rowSums(exp(L - m))) - log(ncls)
  memb <- row_softmax(L)
  msg <- if (!is.null(object$msg)) object$msg else "Class"
  colnames(memb) <- paste0(msg, seq_len(ncls))
  rownames(memb) <- nd$ID
  est <- max.col(memb, ties.method = "first")
  names(est) <- nd$ID
  names(log_lik) <- nd$ID
  return(structure(list(
    Membership = memb,
    Estimate = est,
    log_lik = log_lik,
    model = model,
    msg = msg
  ), class = c("exametrikaPrediction", "exametrika")))
}

#' @export
print.exametrikaPrediction <- function(x, ...) {
  cat(sprintf(
    "Prediction from a %s fit: %d respondents, %d %ss\n\n",
    x$model, NROW(x$Membership), NCOL(x$Membership), tolower(x$msg)
  ))
  cat(sprintf("Latent %s distribution of the estimates:\n", tolower(x$msg)))
  tab <- table(factor(x$Estimate, levels = seq_len(NCOL(x$Membership))))
  names(tab) <- colnames(x$Membership)
  print(tab)
  return(invisible(x))
}

# Item labels of a fitted model, in the order of its parameters
pred_item_labels <- function(object) {
  if (!is.null(object$ItemLabel)) {
    return(as.character(object$ItemLabel))
  }
  if (is.list(object$U) && !is.null(object$U$ItemLabel)) {
    # LRA (ordinal, rated) keeps the whole formatted data
    return(as.character(object$U$ItemLabel))
  }
  if (is.matrix(object$IRP) && !is.null(rownames(object$IRP))) {
    return(rownames(object$IRP))
  }
  if (!is.null(names(object$FieldEstimated))) {
    return(names(object$FieldEstimated))
  }
  for (nm in c("Q", "U")) {
    if (!is.null(colnames(object[[nm]]))) {
      return(colnames(object[[nm]]))
    }
  }
  stop("The fitted object does not record its item labels.", call. = FALSE)
}

# New responses as a raw code matrix X (NA = missing) in the item order of the
# fit, with respondent IDs
pred_newdata <- function(object, newdata, na, binary) {
  items <- pred_item_labels(object)
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
  } else {
    X <- as.matrix(as.data.frame(newdata))
    if (!is.numeric(X)) {
      stop("newdata must hold numeric response codes.", call. = FALSE)
    }
    id <- rownames(newdata)
  }
  if (!is.null(na)) {
    X[X %in% na] <- NA
  }
  if (is.null(colnames(X))) {
    if (NCOL(X) != length(items)) {
      stop("newdata has no item names and ", NCOL(X), " columns; the model has ",
        length(items), " items.",
        call. = FALSE
      )
    }
    colnames(X) <- items
  }
  miss <- setdiff(items, colnames(X))
  if (length(miss) > 0) {
    stop("newdata lacks the item(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  X <- X[, items, drop = FALSE]
  if (is.null(id) || length(id) != NROW(X)) {
    id <- paste0("Student", seq_len(NROW(X)))
  }
  if (binary && any(!is.na(X) & !X %in% c(0, 1))) {
    stop("A binary model needs responses coded 0/1.", call. = FALSE)
  }
  return(list(X = X, ID = as.character(id), items = items))
}

# Recode raw codes to positions 1..k per item; unseen codes become missing
pred_recode_items <- function(X, codes, items, collapse = NULL) {
  P <- matrix(NA_integer_, nrow = NROW(X), ncol = NCOL(X))
  unseen <- character(0)
  for (j in seq_len(NCOL(X))) {
    x <- X[, j]
    p <- match(x, codes[[j]])
    if (!is.null(collapse) && collapse %in% codes[[j]]) {
      # Rare distractors were pooled into one category at fit time
      p[!is.na(x) & is.na(p)] <- match(collapse, codes[[j]])
    }
    bad <- !is.na(x) & is.na(p)
    if (any(bad)) {
      unseen <- c(unseen, sprintf("%s: %s", items[j], paste(sort(unique(x[bad])), collapse = ", ")))
    }
    P[, j] <- p
  }
  pred_warn_unseen(unseen)
  return(P)
}

pred_warn_unseen <- function(unseen) {
  if (length(unseen) > 0) {
    warning("predict(): responses in categories the fitted data never used for that item ",
      "were treated as missing.\n  ", paste(unseen, collapse = "\n  "),
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# Respondents x items x categories one-hot array of positions P (NA = missing)
pred_onehot <- function(P, ncat) {
  A <- array(0, dim = c(NROW(P), NCOL(P), ncat))
  ok <- which(!is.na(P), arr.ind = TRUE)
  A[cbind(ok, P[ok])] <- 1
  return(A)
}

# Binary LCA / LRA: IRP is items x classes
pred_ll_irp <- function(object, nd) {
  IRP <- as.matrix(object$IRP)
  const <- exp(-NROW(IRP))
  Z <- 1 * !is.na(nd$X)
  U <- nd$X
  U[is.na(U)] <- 0
  return(U %*% log(IRP + const) + (Z * (1 - U)) %*% log(1 - IRP + const))
}

# Nominal / ordinal / rated LCA: ICRP rows follow CategoryMap
pred_ll_icrp_lca <- function(object, nd) {
  codes <- pred_category_map(object)
  ncat <- lengths(codes)
  ncls <- object$n_class
  if (NROW(object$ICRP) != sum(ncat)) {
    stop("The category profile does not match the stored category map.", call. = FALSE)
  }
  profile <- m2_profile_from_icrp(object$ICRP, ncat, ncls, "class")
  const <- exp(-length(ncat))
  A <- pred_onehot(pred_recode_items(nd$X, codes, nd$items), max(ncat))
  L <- matrix(0, nrow = NROW(nd$X), ncol = ncls)
  for (q in seq_len(max(ncat))) {
    L <- L + A[, , q] %*% log(matrix(profile[, , q], ncol = ncls) + const)
  }
  return(L)
}

# Ordinal / rated LRA: ICRP rows follow CategoryMap, with a rank prior
pred_ll_icrp_lra <- function(object, nd) {
  codes <- pred_category_map(object)
  ncat <- lengths(codes)
  nrank <- object$n_rank
  if (is.null(nrank)) nrank <- object$n_class
  catRef <- as.matrix(object$ICRP[, paste0("rank", seq_len(nrank))])
  if (NROW(catRef) != sum(ncat)) {
    stop("The category profile does not match the stored category map.", call. = FALSE)
  }
  P <- pred_recode_items(nd$X, codes, nd$items, collapse = object$CollapseCode)
  offset <- c(0, cumsum(ncat)[-length(ncat)])
  M <- matrix(0, nrow = NROW(P), ncol = sum(ncat))
  ok <- which(!is.na(P), arr.ind = TRUE)
  M[cbind(ok[, 1], offset[ok[, 2]] + P[ok])] <- 1
  log_prior <- if (is.null(object$LogPrior)) rep(0, nrank) else object$LogPrior
  L <- M %*% log(catRef + 1e-6)
  return(sweep(L, 2, log_prior, "+"))
}

pred_category_map <- function(object) {
  if (!is.null(object$CategoryMap)) {
    return(object$CategoryMap)
  }
  # Fits made before the map was stored: the rated models keep the raw codes
  if (class(object)[2] %in% c("ratedLCA") && !is.null(object$Q)) {
    Q <- object$Q
    Q[object$Z == 0] <- -1
    return(category_map(Q))
  }
  stop("This fit does not store the category map that predict() needs. ",
    "Refit it with exametrika 2.2.0 or later.",
    call. = FALSE
  )
}

# Binary biclustering: soft field memberships, as in the class E-step
pred_ll_bicl_binary <- function(object, nd) {
  FRP <- as.matrix(object$FRP)
  fldmemb <- as.matrix(object$FieldMembership)
  const <- .Machine$double.eps
  Z <- 1 * !is.na(nd$X)
  U <- nd$X
  U[is.na(U)] <- 0
  csr <- (Z * U) %*% fldmemb
  fsr <- (Z * (1 - U)) %*% fldmemb
  if (any(csr <= 0)) csr <- pmax(csr, const)
  if (any(fsr <= 0)) fsr <- pmax(fsr, const)
  return(csr %*% log(FRP + const) + fsr %*% log(1 - FRP + const))
}

# Nominal / ordinal / rated biclustering: codes are shifted, never renumbered
pred_ll_bicl_poly <- function(object, nd) {
  FRP <- object$FRP
  if (length(dim(FRP)) == 2) {
    FRP <- array(FRP, dim = c(1, dim(FRP)))
  }
  nfld <- dim(FRP)[1]
  ncls <- dim(FRP)[2]
  maxQ <- dim(FRP)[3]
  shift <- object$CategoryShift
  if (is.null(shift)) {
    if (class(object)[2] == "ratedBiclustering" && !is.null(object$Q)) {
      Q <- object$Q
      Q[object$Z == 0] <- -1
      shift <- category_shift(Q)
    } else {
      stop("This fit does not store the category coding that predict() needs. ",
        "Refit it with exametrika 2.2.0 or later.",
        call. = FALSE
      )
    }
  }
  P <- nd$X + shift
  bad <- !is.na(P) & (P < 1 | P > maxQ | P != round(P))
  if (any(bad)) {
    pred_warn_unseen(vapply(which(colSums(bad) > 0), function(j) {
      sprintf("%s: %s", nd$items[j], paste(sort(unique(nd$X[bad[, j], j])), collapse = ", "))
    }, ""))
    P[bad] <- NA
  }
  A <- pred_onehot(P, maxQ)
  fldmemb <- as.matrix(object$FieldMembership)
  const <- exp(-NCOL(nd$X))
  L <- matrix(0, nrow = NROW(P), ncol = ncls)
  for (q in seq_len(maxQ)) {
    lp <- log(pmax(matrix(FRP[, , q], nrow = nfld, ncol = ncls), 0) + const)
    L <- L + A[, , q] %*% fldmemb %*% lp
  }
  return(L)
}
