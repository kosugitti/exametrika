# Limited-information goodness-of-fit statistic M2
# (Maydeu-Olivares & Joe, 2005, 2006)
#
# What the statistic tests: whether the model reproduces the univariate and
# bivariate margins -- the item-pair cross tables. The reference point is the
# saturated model of the 2nd-order margins, not of the full response-pattern
# table, so no benchmark log-likelihood is needed. That is what makes it usable
# for nominal data, where the full table is almost all empty cells and the
# saturated model is not informative.
#
# Formulation: exametrika's LCA does not estimate the class proportions; every
# class carries the same implicit prior 1/C (Shojima 2022, p. 160). Hence
#   pi_j(q)      = (1/C) sum_c rho_{jq|c}
#   pi_jj'(q,q') = (1/C) sum_c rho_{jq|c} rho_{j'q'|c}
# and the parameters are the category profiles alone, t = C * sum_j (Q_j - 1).

#' @title Margin bookkeeping for M2
#' @description
#' A margin is one or two (item, category) pairs. The last category of each item
#' is dropped as the baseline; M2 is invariant to that choice. Singles come
#' first, then pairs, and every other matrix in the computation follows this
#' ordering.
#' @param ncat integer vector of category counts, one per item
#' @return list with `single`, `pair`, padded `items`/`cats`, and the total `m`
#' @noRd
m2_margin_index <- function(ncat) {
  nitems <- length(ncat)
  free <- lapply(seq_len(nitems), function(j) seq_len(ncat[j] - 1L))

  single <- do.call(rbind, lapply(seq_len(nitems), function(j) {
    cbind(item = j, cat = free[[j]])
  }))

  pair <- list()
  for (j in seq_len(nitems - 1L)) {
    for (jp in seq(j + 1L, nitems)) {
      grid <- expand.grid(q = free[[j]], qp = free[[jp]])
      pair[[length(pair) + 1L]] <- cbind(
        item1 = j, cat1 = grid$q, item2 = jp, cat2 = grid$qp
      )
    }
  }
  pair <- do.call(rbind, pair)

  items <- rbind(cbind(single[, "item"], NA_integer_), pair[, c("item1", "item2")])
  cats <- rbind(cbind(single[, "cat"], NA_integer_), pair[, c("cat1", "cat2")])

  return(list(
    single = single, pair = pair, items = items, cats = cats,
    m = nrow(single) + nrow(pair)
  ))
}

#' @title Per-margin, per-class product of category probabilities
#' @param profile items x classes x categories array
#' @param idx output of \code{m2_margin_index()}
#' @return m x ncls matrix, `A[k, c]` = product over the margin's pairs of rho
#' @noRd
m2_class_products <- function(profile, idx) {
  ncls <- dim(profile)[2]
  pick <- function(j, q) {
    return(matrix(profile[cbind(
      rep(j, ncls), rep(seq_len(ncls), each = length(j)), rep(q, ncls)
    )], nrow = length(j), ncol = ncls))
  }
  A_single <- pick(idx$single[, "item"], idx$single[, "cat"])
  A_pair <- pick(idx$pair[, "item1"], idx$pair[, "cat1"]) *
    pick(idx$pair[, "item2"], idx$pair[, "cat2"])
  return(rbind(A_single, A_pair))
}

#' @title Model-implied margins
#' @param A output of \code{m2_class_products()}
#' @noRd
m2_pi <- function(A) {
  return(rowMeans(A)) # (1/C) sum_c
}

#' @title Asymptotic covariance of the margin proportions
#' @description
#' `Xi[a, b] = pi_{a union b} - pi_a pi_b`. For disjoint item sets the union
#' probability is `(1/C) sum_c A[a,c] A[b,c]`, i.e. one matrix product for the
#' whole table. Overlapping item sets need two corrections, and both are
#' necessary: two categories of the same item cannot co-occur (0) and a repeated
#' category must not be squared (divide the duplicated factor out). Margins built
#' on the *same* item pair share two items, which the per-item pass cannot fix
#' on its own -- such a block is diagonal.
#' @param profile items x classes x categories array
#' @param A output of \code{m2_class_products()}
#' @param idx output of \code{m2_margin_index()}
#' @noRd
m2_xi <- function(profile, A, idx) {
  ncls <- ncol(A)
  joint <- tcrossprod(A) / ncls
  items <- idx$items
  cats <- idx$cats
  nitems <- dim(profile)[1]

  for (j in seq_len(nitems)) {
    ks <- which(items[, 1] == j | items[, 2] == j)
    if (length(ks) < 2) next
    catj <- ifelse(items[ks, 1] == j, cats[ks, 1], cats[ks, 2])
    rho_j <- matrix(profile[cbind(
      j, rep(seq_len(ncls), each = length(ks)), rep(catj, ncls)
    )], nrow = length(ks), ncol = ncls)
    Aks <- A[ks, , drop = FALSE]
    val <- ((Aks / rho_j) %*% t(Aks)) / ncls
    val[!outer(catj, catj, "==")] <- 0
    joint[ks, ks] <- val
  }

  pi_vec <- m2_pi(A)

  n1 <- nrow(idx$single)
  block_key <- paste(idx$pair[, "item1"], idx$pair[, "item2"])
  for (b in unique(block_key)) {
    ks <- n1 + which(block_key == b)
    joint[ks, ks] <- diag(pi_vec[ks], nrow = length(ks))
  }

  # Subtract the outer product one column at a time and in place. Writing
  # `joint - outer(pi_vec, pi_vec)` would hold three m x m matrices at once
  # (joint, the outer product, the result); at m = 19,800 each of those is
  # 2.9 GB, and the peak decides how many of these can run in parallel.
  for (k in seq_along(pi_vec)) {
    joint[, k] <- joint[, k] - pi_vec * pi_vec[k]
  }
  return(joint)
}

#' @title Parameter index for M2
#' @description Order is (item, category, class) with class fastest.
#' @noRd
m2_par_index <- function(ncat, ncls) {
  return(do.call(rbind, lapply(seq_along(ncat), function(j) {
    g <- expand.grid(cls = seq_len(ncls), cat = seq_len(ncat[j] - 1L))
    data.frame(item = j, cat = g$cat, cls = g$cls)
  })))
}

#' @title Jacobian of the margins with respect to the parameters
#' @description
#' With the class proportions fixed at 1/C the parameters are the category
#' profiles alone, so two derivatives cover every margin. The sum-to-one
#' constraint does not appear because the baseline category is dropped.
#' @noRd
m2_delta <- function(profile, idx, ncat) {
  ncls <- dim(profile)[2]
  par_index <- m2_par_index(ncat, ncls)
  Delta <- matrix(0, nrow = idx$m, ncol = nrow(par_index))
  key <- paste(par_index$item, par_index$cat)

  n1 <- nrow(idx$single)
  for (k in seq_len(n1)) {
    cols <- which(key == paste(idx$single[k, "item"], idx$single[k, "cat"]))
    Delta[k, cols] <- 1 / ncls
  }
  for (k in seq_len(nrow(idx$pair))) {
    j <- idx$pair[k, "item1"]
    q <- idx$pair[k, "cat1"]
    jp <- idx$pair[k, "item2"]
    qp <- idx$pair[k, "cat2"]
    Delta[n1 + k, which(key == paste(j, q))] <- profile[jp, , qp] / ncls
    Delta[n1 + k, which(key == paste(jp, qp))] <- profile[j, , q] / ncls
  }
  return(Delta)
}

#' @title Observed margins
#' @noRd
m2_p_obs <- function(Q, Z, idx) {
  nobs <- nrow(Q)
  QZ <- Q
  QZ[Z == 0] <- NA
  p_single <- vapply(seq_len(nrow(idx$single)), function(k) {
    j <- idx$single[k, "item"]
    return(sum(QZ[, j] == idx$single[k, "cat"], na.rm = TRUE) / nobs)
  }, numeric(1))
  p_pair <- vapply(seq_len(nrow(idx$pair)), function(k) {
    j <- idx$pair[k, "item1"]
    jp <- idx$pair[k, "item2"]
    return(sum(QZ[, j] == idx$pair[k, "cat1"] & QZ[, jp] == idx$pair[k, "cat2"],
      na.rm = TRUE
    ) / nobs)
  }, numeric(1))
  return(c(p_single, p_pair))
}

#' @title Core of the M2 computation
#' @description
#' Works in the \eqn{\Xi^{-1/2}} coordinates, where the parameter-estimation
#' correction is an orthogonal projection onto the column space of the Jacobian.
#' Two reasons not to invert \eqn{\Delta' \Xi^{-1} \Delta} directly: it is a
#' Cholesky solve plus an SVD rather than an explicit inverse, and it survives a
#' rank-deficient Jacobian. The latter is not hypothetical -- with the class
#' proportions fixed, the second-order margins see the class deviations only
#' through their Gram matrix, so any rotation of the (C-1)-dimensional class
#' space leaves them unchanged and rank(Delta) = t - (C-1)(C-2)/2. The
#' parameters are then not identified from bivariate margins once C >= 3, while
#' the margins themselves, and hence the projection, still are.
#' @noRd
m2_core <- function(profile, ncat, nobs, Q = NULL, Z = NULL, p_vec = NULL) {
  idx <- m2_margin_index(ncat)
  A <- m2_class_products(profile, idx)
  pi_vec <- m2_pi(A)
  if (is.null(p_vec)) {
    p_vec <- m2_p_obs(Q, Z, idx)
  }
  e <- p_vec - pi_vec
  Xi <- m2_xi(profile, A, idx)
  Delta <- m2_delta(profile, idx, ncat)

  L <- chol(Xi) # Xi = L'L, L upper triangular
  # Xi is not needed once the factor exists; dropping the reference lets R
  # reuse the block. An explicit gc() here was tried and removed: it cost ~30%
  # of the runtime and saved nothing, because the peak is set by Xi and its
  # Cholesky factor being alive at the same time, which R cannot avoid without
  # an in-place factorisation.
  rm(Xi)
  e_tilde <- backsolve(L, e, transpose = TRUE)
  B <- backsolve(L, Delta, transpose = TRUE)
  sv <- svd(B)
  rank_delta <- sum(sv$d > max(dim(B)) * .Machine$double.eps * sv$d[1])
  U_r <- sv$u[, seq_len(rank_delta), drop = FALSE]
  resid <- e_tilde - U_r %*% crossprod(U_r, e_tilde)

  stat <- nobs * sum(resid^2)
  df <- idx$m - rank_delta
  return(list(
    M2 = stat, df = df, p = stats::pchisq(stat, df, lower.tail = FALSE),
    m = idx$m, n_param = ncol(Delta), rank_delta = rank_delta
  ))
}

#' @title Limited-information goodness-of-fit statistic (M2)
#' @description
#' Tests whether a fitted model reproduces the item-pair cross tables. The
#' reference point is the saturated model of the first- and second-order
#' margins, not of the full response-pattern table, so no benchmark
#' log-likelihood is required. That is what makes the statistic usable for
#' nominal data, where nearly every response pattern is unique and the
#' full-information chi-square does not follow its nominal distribution
#' (Collins et al., 1993).
#'
#' The analogy that usually lands: the chi-square of a structural equation model
#' does not test the whole multivariate distribution, only whether the model
#' reproduces the covariance matrix. \eqn{M_2} is the categorical counterpart,
#' with the cross tables in place of the covariance matrix.
#'
#' @param x A fitted model object of class "exametrika".
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class "exametrika" and "M2" containing:
#' \describe{
#'  \item{M2}{The statistic.}
#'  \item{df}{Degrees of freedom, \code{m - rank(Delta)}. Note that this is not
#'    \code{m - n_param}: see \code{rank_delta}.}
#'  \item{p}{Upper tail probability of the chi-square distribution with \code{df}
#'    degrees of freedom.}
#'  \item{m}{Number of margins used: the free categories of every item, plus one
#'    entry for each combination of a free category of one item with a free
#'    category of another.}
#'  \item{n_param}{Number of model parameters, \code{ncls * sum(ncat - 1)}. The
#'    class proportions are not estimated in this formulation, so they are not
#'    counted.}
#'  \item{rank_delta}{Rank of the Jacobian. For \code{ncls >= 3} it falls short
#'    of \code{n_param} by \code{(ncls - 1)(ncls - 2) / 2}, because the
#'    second-order margins determine the class deviations only through their
#'    Gram matrix, which is invariant to rotations of the class space.}
#' }
#'
#' @details
#' The statistic is interpretable only when the estimator is the maximum
#' likelihood estimator of the fitted model, since the asymptotics require the
#' margin residual to be orthogonal to the Jacobian. That holds for
#' \code{LCA.nominal} and \code{LCA.rated}, which are fitted by EM. It does not
#' hold for filter-based estimation (GTM), which is a regularisation rather than
#' a maximum likelihood estimator, nor for order-restricted estimation, whose
#' limiting distribution is a mixture of chi-squares rather than a single one.
#'
#' The degrees of freedom here are orders of magnitude smaller than in a
#' full-information test, so an RMSEA computed from \eqn{M_2} is not comparable
#' with a full-information RMSEA and the conventional cutoffs do not carry over.
#'
#' Cost: the margin covariance is a dense \code{m x m} matrix and the Cholesky
#' factorisation dominates. With 20 items and 5 categories \code{m} is 3,120
#' (74 MB, well under a second); with 50 items it is 19,800 (2.9 GB, around 20
#' seconds). A message reports the size before the work starts when \code{m} is
#' large.
#'
#' @references
#' Maydeu-Olivares, A., & Joe, H. (2005). Limited- and full-information
#' estimation and goodness-of-fit testing in 2^n contingency tables: A unified
#' framework. Journal of the American Statistical Association, 100(471),
#' 1009-1020.
#'
#' Maydeu-Olivares, A., & Joe, H. (2006). Limited information goodness-of-fit
#' testing in multidimensional contingency tables. Psychometrika, 71(4),
#' 713-732.
#'
#' Collins, L. M., Fidler, P. L., Wugalter, S. E., & Long, J. D. (1993). Goodness-of-fit
#' testing for latent class models. Multivariate Behavioral Research, 28(3), 375-389.
#'
#' @examples
#' \donttest{
#' dat <- dataFormat(J20S600, response.type = "nominal")
#' fit <- LCA(dat, ncls = 3)
#' M2(fit)
#' }
#'
#' @export
M2 <- function(x, ...) {
  UseMethod("M2")
}

#' @rdname M2
#' @export
M2.default <- function(x, ...) {
  stop(
    "M2() is available for models fitted by maximum likelihood on polytomous ",
    "data: currently LCA() on nominal or rated data."
  )
}

#' @rdname M2
#' @param verbose Logical; if TRUE (default), reports the size of the margin
#'   covariance matrix before computing it when that matrix is large.
#' @param gc Logical; if TRUE (default), releases the workspace back to the
#'   operating system before returning. The margin covariance and its Cholesky
#'   factor are the largest objects the package ever allocates -- gigabytes for a
#'   long test -- and R holds on to that block otherwise. Interactive use, where
#'   this is called once per model, wants it. A loop over many fits does not:
#'   the collection costs a noticeable fraction of the computation and buys
#'   nothing, since the next call allocates the same block again. Pass FALSE
#'   there.
#' @export
M2.nominalLCA <- function(x, verbose = TRUE, gc = TRUE, ...) {
  return(m2_from_lca(x, verbose = verbose, gc = gc))
}

#' @rdname M2
#' @export
M2.ratedLCA <- function(x, verbose = TRUE, gc = TRUE, ...) {
  return(m2_from_lca(x, verbose = verbose, gc = gc))
}

#' @title Shared body of the LCA M2 methods
#' @noRd
m2_from_lca <- function(x, verbose = TRUE, gc = TRUE) {
  if (is.null(x$Q) || is.null(x$Z)) {
    stop(
      "The fitted object does not carry the response data needed by M2(). ",
      "Refit with the current version of the package."
    )
  }
  ncat <- as.vector(x$categories)
  ncls <- x$n_class
  nitems <- length(ncat)

  # rebuild the profile array from ICRP (items x classes x categories)
  profile <- array(0, dim = c(nitems, ncls, max(ncat)))
  offset <- c(0, cumsum(ncat)[-nitems])
  cols <- paste0("class", seq_len(ncls))
  for (j in seq_len(nitems)) {
    profile[j, , seq_len(ncat[j])] <- t(as.matrix(
      x$ICRP[offset[j] + seq_len(ncat[j]), cols]
    ))
  }

  m <- sum(ncat - 1) + sum(outer(ncat - 1, ncat - 1)[upper.tri(diag(nitems))])
  if (verbose && m > 5000) {
    message(sprintf(
      "M2: %d margins; the covariance matrix is %.1f GB and its factorisation dominates the cost.",
      m, m^2 * 8 / 1024^3
    ))
  }

  out <- m2_core(profile, ncat, nobs = x$nobs, Q = x$Q, Z = x$Z)
  out$n_class <- ncls
  m2_release(gc)
  return(structure(out, class = c("exametrika", "M2")))
}

#' @title Margin-based fit indices from M2
#' @description
#' Builds the usual incremental fit indices from two margin-based chi-squares:
#' the fitted model's M2 and the independence model's. They must not be mixed
#' with the response-pattern chi-squares that `TestFitIndices` carries -- the two
#' live in different worlds and a ratio of one to the other is not defensible
#' (Shojima, personal communication, 2026-07-26). Hence a separate object.
#'
#' The baseline is the independence model, which reproduces the observed
#' first-order margins and throws away every association, so all of its misfit
#' lands in the second-order margins and the indices read as "the share of the
#' inter-item association that the model accounts for". A stricter baseline (say
#' uniform first-order margins) is possible but changes what the indices mean.
#'
#' AIC/BIC/CAIC are deliberately left out. `calcFitIndices()` would happily
#' produce chi-square based ones, but the package already reports
#' likelihood-based information criteria for these models, and two different
#' AICs under one name invites exactly the confusion this separation is meant to
#' avoid.
#' @noRd
m2_fit_indices <- function(m2_model, m2_null, nobs) {
  idx <- calcFitIndices(
    chi_A = m2_model$M2, chi_B = m2_null$M2,
    df_A = m2_model$df, df_B = m2_null$df, nobs = nobs
  )
  return(structure(
    list(
      M2 = m2_model$M2, df = m2_model$df, p = m2_model$p,
      M2_null = m2_null$M2, df_null = m2_null$df,
      n_margin = m2_model$m,
      NFI = idx$NFI, RFI = idx$RFI, IFI = idx$IFI,
      TLI = idx$TLI, CFI = idx$CFI, RMSEA = idx$RMSEA
    ),
    class = c("exametrika", "ModelFitM2")
  ))
}

#' @title The independence model in the form m2_core() expects
#' @description
#' One class whose category probabilities are the observed marginal ones. With a
#' single class there is no rotational indeterminacy, so its Jacobian has full
#' column rank and df_null = m - sum(ncat - 1), which is exactly the number of
#' second-order margin cells.
#' @noRd
m2_null_profile <- function(Q, Z, ncat) {
  nitems <- length(ncat)
  profile <- array(0, dim = c(nitems, 1, max(ncat)))
  for (j in seq_len(nitems)) {
    obs <- Q[Z[, j] == 1, j]
    for (q in seq_len(ncat[j])) {
      profile[j, 1, q] <- sum(obs == q) / length(obs)
    }
  }
  return(profile)
}

#' @title Attach the margin-based fit indices to a fitted model
#' @description
#' Computes \code{\link{M2}} for the model and for the independence baseline, and
#' returns the fitted object with a \code{TestFitIndicesM2} component added. The
#' print method then shows the response-pattern indices and the margin-based ones
#' side by side.
#'
#' This is a separate step rather than part of the fit because it is expensive:
#' the cost is the Cholesky factorisation of a dense matrix whose size grows with
#' the square of the item count (see \code{\link{M2}}).
#'
#' @param x A fitted model object of class "exametrika".
#' @param ... Additional arguments passed to methods.
#'
#' @return The fitted object with \code{TestFitIndicesM2} added.
#'
#' @examples
#' \donttest{
#' dat <- dataFormat(J20S600, response.type = "nominal")
#' fit <- LCA(dat, ncls = 3)
#' fit <- add_M2(fit)
#' fit
#' print(fit, fit_indices = "margin")
#' }
#'
#' @export
add_M2 <- function(x, ...) {
  UseMethod("add_M2")
}

#' @rdname add_M2
#' @export
add_M2.default <- function(x, ...) {
  stop(
    "add_M2() is available for models fitted by maximum likelihood on ",
    "polytomous data: currently LCA() on nominal or rated data."
  )
}

#' @rdname add_M2
#' @param verbose Logical; if TRUE (default), reports the size of the margin
#'   covariance matrix before computing it when that matrix is large.
#' @param gc Logical; if TRUE (default), releases the workspace before
#'   returning. See \code{\link{M2}}.
#' @export
add_M2.nominalLCA <- function(x, verbose = TRUE, gc = TRUE, ...) {
  return(add_m2_to_lca(x, verbose = verbose, gc = gc))
}

#' @rdname add_M2
#' @export
add_M2.ratedLCA <- function(x, verbose = TRUE, gc = TRUE, ...) {
  return(add_m2_to_lca(x, verbose = verbose, gc = gc))
}

#' @title Shared body of the add_M2 methods
#' @noRd
add_m2_to_lca <- function(x, verbose = TRUE, gc = TRUE) {
  ncat <- as.vector(x$categories)
  # Two statistics are needed, the model's and the baseline's, and they cannot
  # share a covariance matrix -- each is computed under its own model. On a long
  # test that is a wait worth narrating.
  talk <- verbose && m2_report_size(ncat, verbose = FALSE) > 5000

  if (talk) message("M2: computing the statistic for the fitted model ...")
  fitted <- m2_from_lca(x, verbose = verbose, gc = FALSE)

  if (talk) message("M2: computing the statistic for the independence baseline ...")
  null_profile <- m2_null_profile(x$Q, x$Z, ncat)
  null_fit <- m2_core(null_profile, ncat, nobs = x$nobs, Q = x$Q, Z = x$Z)
  if (talk) message("M2: done.")
  x$TestFitIndicesM2 <- m2_fit_indices(fitted, null_fit, x$nobs)
  m2_release(gc)
  return(x)
}

#' @title Jacobian when several items share one set of parameters
#' @description
#' Biclustering estimates one category profile per (field, class), and every item
#' in a field uses it. The margins are the same functions as before -- responses
#' are independent given the class, whether or not two items sit in the same
#' field -- but the derivative of a margin now lands on the field's column, and
#' several margins land on the same column.
#'
#' Contributions are accumulated rather than assigned, which matters for one
#' case: a pair margin whose two items belong to the *same* field and name the
#' *same* category is `(1/C) sum_c rho^2`, so the derivative picks up a factor of
#' two. Assigning would silently halve it.
#' @param profile items x classes x categories, the field profile expanded to items
#' @param field integer vector: which field each item belongs to
#' @noRd
m2_delta_shared <- function(profile, idx, ncat, field) {
  ncls <- dim(profile)[2]
  nfld <- max(field)
  ncat_fld <- vapply(seq_len(nfld), function(f) ncat[which(field == f)[1]], numeric(1))
  par_index <- do.call(rbind, lapply(seq_len(nfld), function(f) {
    g <- expand.grid(cls = seq_len(ncls), cat = seq_len(ncat_fld[f] - 1L))
    data.frame(fld = f, cat = g$cat, cls = g$cls)
  }))
  key <- paste(par_index$fld, par_index$cat)
  Delta <- matrix(0, nrow = idx$m, ncol = nrow(par_index))

  n1 <- nrow(idx$single)
  for (k in seq_len(n1)) {
    j <- idx$single[k, "item"]
    cols <- which(key == paste(field[j], idx$single[k, "cat"]))
    Delta[k, cols] <- Delta[k, cols] + 1 / ncls
  }
  for (k in seq_len(nrow(idx$pair))) {
    row <- n1 + k
    j <- idx$pair[k, "item1"]
    q <- idx$pair[k, "cat1"]
    jp <- idx$pair[k, "item2"]
    qp <- idx$pair[k, "cat2"]
    cols_j <- which(key == paste(field[j], q))
    cols_jp <- which(key == paste(field[jp], qp))
    Delta[row, cols_j] <- Delta[row, cols_j] + profile[jp, , qp] / ncls
    Delta[row, cols_jp] <- Delta[row, cols_jp] + profile[j, , q] / ncls
  }
  return(Delta)
}

#' @title Core of the M2 computation, with an optional shared-parameter map
#' @param field integer vector of field memberships, or NULL when every item has
#'   its own parameters (LCA, LRA)
#' @noRd
m2_core_general <- function(profile, ncat, nobs, Q, Z, field = NULL) {
  idx <- m2_margin_index(ncat)
  A <- m2_class_products(profile, idx)
  pi_vec <- m2_pi(A)
  e <- m2_p_obs(Q, Z, idx) - pi_vec
  Xi <- m2_xi(profile, A, idx)
  Delta <- if (is.null(field)) {
    m2_delta(profile, idx, ncat)
  } else {
    m2_delta_shared(profile, idx, ncat, field)
  }

  L <- chol(Xi)
  rm(Xi)
  e_tilde <- backsolve(L, e, transpose = TRUE)
  B <- backsolve(L, Delta, transpose = TRUE)
  sv <- svd(B)
  rank_delta <- sum(sv$d > max(dim(B)) * .Machine$double.eps * sv$d[1])
  U_r <- sv$u[, seq_len(rank_delta), drop = FALSE]
  resid <- e_tilde - U_r %*% crossprod(U_r, e_tilde)

  stat <- nobs * sum(resid^2)
  df <- idx$m - rank_delta
  return(list(
    M2 = stat, df = df, p = stats::pchisq(stat, df, lower.tail = FALSE),
    m = idx$m, n_param = ncol(Delta), rank_delta = rank_delta
  ))
}

#' @title Recover the item x class x category profile from an ICRP data frame
#' @noRd
m2_profile_from_icrp <- function(icrp, ncat, ncls, prefix) {
  nitems <- length(ncat)
  cols <- paste0(prefix, seq_len(ncls))
  profile <- array(0, dim = c(nitems, ncls, max(ncat)))
  offset <- c(0, cumsum(ncat)[-nitems])
  for (j in seq_len(nitems)) {
    profile[j, , seq_len(ncat[j])] <- t(as.matrix(icrp[offset[j] + seq_len(ncat[j]), cols]))
  }
  return(profile)
}

#' @title Give the workspace back to the operating system
#' @noRd
m2_release <- function(gc) {
  if (isTRUE(gc)) {
    gc(verbose = FALSE, full = TRUE)
  }
  return(invisible(NULL))
}

#' @title Report the size of the margin covariance before building it
#' @noRd
m2_report_size <- function(ncat, verbose) {
  nitems <- length(ncat)
  m <- sum(ncat - 1) + sum(outer(ncat - 1, ncat - 1)[upper.tri(diag(nitems))])
  if (verbose && m > 5000) {
    message(sprintf(
      "M2: %d margins; the covariance matrix is %.1f GB and its factorisation dominates the cost.",
      m, m^2 * 8 / 1024^3
    ))
  }
  return(invisible(m))
}

#' @rdname M2
#' @export
M2.ordinalBiclustering <- function(x, verbose = TRUE, gc = TRUE, ...) {
  return(m2_from_biclustering(x, verbose = verbose, gc = gc))
}

#' @rdname M2
#' @export
M2.nominalBiclustering <- function(x, verbose = TRUE, gc = TRUE, ...) {
  return(m2_from_biclustering(x, verbose = verbose, gc = gc))
}

#' @title M2 for a biclustering fit
#' @description
#' The field profile is expanded to the items that carry it, so the margins are
#' computed exactly as for LCA; only the Jacobian knows about the sharing. The
#' field partition is treated as given -- its uncertainty is not propagated,
#' which is worth remembering when reading the degrees of freedom.
#' @noRd
m2_from_biclustering <- function(x, verbose = TRUE, gc = TRUE) {
  if (is.null(x$Q) || is.null(x$Z)) {
    stop("The fitted object does not carry the response data needed by M2().")
  }
  ncls <- x$n_class
  field <- as.vector(x$FieldEstimated)
  nitems <- length(field)
  maxQ <- dim(x$FRP)[3]
  # Category counts come from the data, not from the array's width. A field
  # profile has maxQ slots whatever its items look like, so an item with fewer
  # categories would otherwise be given a margin that can never be observed --
  # zero variance, and the covariance matrix stops being positive definite.
  ncat <- apply(x$Q * (x$Z == 1), 2, max)
  if (any(ncat != maxQ)) {
    stop(
      "M2() on a biclustering fit needs every item to have the same number of ",
      "categories: a field profile is shared across its items, so items with ",
      "fewer categories would be assigned margins that cannot occur."
    )
  }
  profile <- array(0, dim = c(nitems, ncls, maxQ))
  for (j in seq_len(nitems)) {
    profile[j, , ] <- x$FRP[field[j], , ]
  }

  m2_report_size(ncat, verbose)
  out <- m2_core_general(profile, ncat, nobs = x$nobs, Q = x$Q, Z = x$Z, field = field)
  out$n_class <- ncls
  m2_release(gc)
  return(structure(out, class = c("exametrika", "M2")))
}

#' @rdname M2
#' @export
M2.LRAordinal <- function(x, verbose = TRUE, gc = TRUE, ...) {
  dat <- x$U
  if (is.null(dat$Q) || is.null(dat$Z)) {
    stop("The fitted object does not carry the response data needed by M2().")
  }
  ncat <- as.vector(dat$categories)
  nrank <- x$n_rank
  profile <- m2_profile_from_icrp(x$ICRP, ncat, nrank, prefix = "rank")

  m2_report_size(ncat, verbose)
  out <- m2_core_general(profile, ncat, nobs = x$nobs, Q = dat$Q, Z = dat$Z)
  out$n_class <- nrank
  m2_release(gc)
  return(structure(out, class = c("exametrika", "M2")))
}
