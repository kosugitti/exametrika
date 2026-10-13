# Deterministic starting values for polytomous biclustering ----------------
#
# The EM of Biclustering.ordinal() is deterministic but not global: from the
# start used up to 2.2.0 ("legacy": fields = items split by their median,
# block probabilities = a ladder j/C common to every field) it stopped at a
# local optimum in over 80% of simulated data sets. Two starts built from the
# data reach the level of a start at the true partition:
#
#   quantile  classes = equal-count bins of the respondent score,
#   ward      classes = Ward clustering of the respondent score (does not
#             assume equal class sizes),
#
# and in both the fields are a Ward clustering of the items' class-mean
# profiles, centred per item, so that items are grouped by how they rise
# across the classes rather than by their level. The block probabilities are
# then the category frequencies of each block plus one. Neither start wins
# everywhere (quantile under equal class sizes, ward under unequal ones), so
# init = "auto" runs both and keeps the larger ELBO.
#
# All functions take Q after remap_category_codes() (codes 1..maxQ, missing
# -1) and the missing indicator Z (1 = observed), and are invariant to the
# order of the rows.

#' Respondent score: mean of the observed category codes
#' @noRd
bicl_init_score <- function(Q, Z) {
  nobs_j <- rowSums(Z)
  s <- rowSums(Q * Z) / nobs_j
  s[nobs_j == 0] <- mean(s[nobs_j > 0])
  return(s)
}

#' Classes from equal-count bins of the score
#'
#' Ties in the score are broken by the response pattern, so the bins do not
#' depend on the order of the rows (only identical rows remain tied, and they
#' give the same block counts whichever bin each lands in).
#' @noRd
bicl_init_class_quantile <- function(Q, Z, ncls) {
  s <- bicl_init_score(Q, Z)
  key <- as.data.frame(Q * Z)
  ord <- do.call(order, c(list(s), unname(as.list(key))))
  rk <- integer(length(s))
  rk[ord] <- seq_along(s)
  return(as.integer(pmin(ceiling(rk * ncls / length(s)), ncls)))
}

#' Classes from Ward clustering of the score
#'
#' Computed on the distinct score values weighted by their counts, which
#' gives the same tree as the respondent-level clustering (identical scores
#' merge first at height zero) without its dependence on the row order and at
#' a fraction of the cost. Returns NULL when there are fewer distinct scores
#' than classes.
#' @noRd
bicl_init_class_ward <- function(Q, Z, ncls) {
  s <- bicl_init_score(Q, Z)
  v <- sort(unique(s))
  if (length(v) < ncls) {
    return(NULL)
  }
  n <- as.vector(table(factor(s, levels = v)))
  # Ward distance between two groups of identical scores
  d <- abs(outer(v, v, "-")) * sqrt(2 * outer(n, n) / outer(n, n, "+"))
  h <- stats::hclust(stats::as.dist(d), method = "ward.D2", members = n)
  cls <- as.integer(stats::cutree(h, k = ncls))[match(s, v)]
  return(bicl_init_relabel(cls, s))
}

#' Number the classes in increasing order of their mean score
#' @noRd
bicl_init_relabel <- function(cls, s) {
  m <- tapply(s, cls, mean)
  return(as.integer(match(cls, as.integer(names(m))[order(m)])))
}

#' Fields from Ward clustering of the items' class-mean profiles
#' @noRd
bicl_init_field_profile <- function(Q, Z, cls, nfld) {
  ncls <- max(cls)
  G <- matrix(0, nrow = length(cls), ncol = ncls)
  G[cbind(seq_along(cls), cls)] <- 1
  prof <- crossprod(Q * Z, G) / crossprod(Z, G)
  item_mean <- colSums(Q * Z) / colSums(Z)
  miss <- which(!is.finite(prof), arr.ind = TRUE)
  prof[miss] <- item_mean[miss[, 1]]
  prof <- prof - rowMeans(prof)
  h <- stats::hclust(stats::dist(prof), method = "ward.D2")
  return(as.integer(stats::cutree(h, k = nfld)))
}

#' Block probabilities and field membership from a hard partition
#'
#' @return list(fldmemb, BCRM, BBRM)
#' @noRd
bicl_init_from_partition <- function(Q, Z, cls, fld, ncls, nfld, maxQ) {
  nitems <- NCOL(Q)
  BCRM <- array(0, dim = c(nfld, ncls, maxQ))
  for (f in seq_len(nfld)) {
    items <- which(fld == f)
    for (cc in seq_len(ncls)) {
      x <- Q[cls == cc, items, drop = FALSE]
      cnt <- tabulate(x[Z[cls == cc, items, drop = FALSE] == 1], nbins = maxQ) + 1
      BCRM[f, cc, ] <- cnt / sum(cnt)
    }
  }
  BBRM <- array(NA, dim = c(nfld, ncls, maxQ + 1))
  BBRM[, , 1] <- 1
  BBRM[, , maxQ + 1] <- 0
  for (q in maxQ:2) {
    BBRM[, , q] <- BCRM[, , q] + BBRM[, , q + 1]
  }
  fldmemb <- matrix(0, nrow = nitems, ncol = nfld)
  fldmemb[cbind(seq_len(nitems), fld)] <- 1
  return(list(fldmemb = fldmemb, BCRM = BCRM, BBRM = BBRM))
}

#' Starting values requested by `init`
#'
#' @param init "auto", "quantile" or "ward" ("legacy" is built by the caller)
#' @param conf_mat,conf_class_mat fixed field / class memberships or NULL
#' @return named list of starts, each list(fldmemb, BCRM, BBRM)
#' @noRd
bicl_init_starts <- function(Q, Z, ncls, nfld, maxQ, init, conf_mat, conf_class_mat) {
  if (!is.null(conf_class_mat)) {
    parts <- list(conf_class = max.col(conf_class_mat, ties.method = "first"))
  } else {
    parts <- list()
    if (init %in% c("auto", "quantile")) {
      parts$quantile <- bicl_init_class_quantile(Q, Z, ncls)
    }
    if (init %in% c("auto", "ward")) {
      w <- bicl_init_class_ward(Q, Z, ncls)
      if (is.null(w)) {
        if (init == "ward") {
          message("init = \"ward\": fewer distinct scores than classes; using \"quantile\".")
          parts$quantile <- bicl_init_class_quantile(Q, Z, ncls)
        }
      } else if (is.null(parts$quantile) || !identical(w, parts$quantile)) {
        parts$ward <- w
      }
    }
  }
  starts <- lapply(parts, function(cls) {
    fld <- if (!is.null(conf_mat)) {
      max.col(conf_mat, ties.method = "first")
    } else {
      bicl_init_field_profile(Q, Z, cls, nfld)
    }
    return(bicl_init_from_partition(Q, Z, cls, fld, ncls, nfld, maxQ))
  })
  return(starts)
}
