#' @title Scalability Coefficients (Loevinger's H)
#'
#' @description
#' Computes the scalability coefficients of Mokken scale analysis: the
#' item-pair coefficients \eqn{H_{ij}}, the item coefficients \eqn{H_j}, and
#' the test coefficient \eqn{H}. For polytomous items these are the weighted
#' Loevinger coefficients of Molenaar (1991).
#'
#' @details
#' Each coefficient is a normed covariance. For a pair of items,
#' \deqn{H_{ij} = \mathrm{Cov}(X_i, X_j) / \mathrm{Cov}(X_i, X_j)_{\max},}
#' where the denominator is the largest covariance attainable given the two
#' marginal distributions. It is obtained by pairing the sorted scores of the
#' two items (the comonotone arrangement). The item and test coefficients
#' pool the pair-wise numerators and denominators (the item coefficient is
#' Mokken's; Loevinger's own item-test coefficient was defined differently),
#' \deqn{H_j = \sum_{i \ne j} \mathrm{Cov}(X_i, X_j) /
#'   \sum_{i \ne j} \mathrm{Cov}(X_i, X_j)_{\max},}
#' \deqn{H = \sum_{i < j} \mathrm{Cov}(X_i, X_j) /
#'   \sum_{i < j} \mathrm{Cov}(X_i, X_j)_{\max}.}
#'
#' Unidimensionality, local independence and latent monotonicity imply
#' \eqn{0 \le H_{ij} \le 1}, \eqn{0 \le H_j \le 1} and \eqn{0 \le H \le 1}
#' (Sijtsma & Molenaar, 2002). A negative coefficient therefore points to an
#' item scored in the opposite direction (for example, a reverse-keyed item
#' that has not been recoded) or to an item whose relation to the others is
#' not monotone. Mokken (1971) suggested the rules of thumb
#' \eqn{0.3 \le H < 0.4} (weak scale), \eqn{0.4 \le H < 0.5} (moderate) and
#' \eqn{H \ge 0.5} (strong).
#'
#' Missing responses are handled pair-wise: each pair of items uses the
#' examinees who answered both. With complete data the results agree with
#' \code{coefH()} of the \pkg{mokken} package. Standard errors are not
#' computed. Item weights \code{w} are ignored.
#'
#' The function only reports; it never recodes or drops items. Whether a
#' reverse-keyed item should be recoded is a decision for the person who
#' prepares the data.
#'
#' @param U Either an object of class "exametrika" or raw data. When raw data
#'   is given, it is converted to the exametrika class with the
#'   \code{\link{dataFormat}} function.
#' @param na Values to be treated as missing values.
#' @param Z Missing indicator matrix of type matrix or data.frame. Values of 1
#'   indicate observed responses, while 0 indicates missing data.
#' @param w Item weight vector. Not used.
#'
#' @return An object of class \code{c("exametrika", "Scalability")} with
#' \describe{
#'   \item{ItemLabel}{Item labels.}
#'   \item{Hij}{Matrix of item-pair coefficients (diagonal is \code{NA}).}
#'   \item{Hj}{Vector of item coefficients.}
#'   \item{H}{Test coefficient.}
#' }
#'
#' @references
#' Loevinger, J. (1948). The technic of homogeneous tests compared with some
#' aspects of "scale analysis" and factor analysis. \emph{Psychological
#' Bulletin}, 45, 507--529.
#'
#' Mokken, R. J. (1971). \emph{A theory and procedure of scale analysis}.
#' De Gruyter.
#'
#' Molenaar, I. W. (1991). A weighted Loevinger H-coefficient extending
#' Mokken scaling to multicategory items. \emph{Kwantitatieve Methoden},
#' 12(37), 97--117.
#'
#' Sijtsma, K., & Molenaar, I. W. (2002). \emph{Introduction to nonparametric
#' item response theory}. Sage.
#'
#' van der Ark, L. A. (2007). Mokken scale analysis in R. \emph{Journal of
#' Statistical Software}, 20(11), 1--19.
#'
#' @examples
#' \donttest{
#' sc <- Scalability(J15S3810)
#' sc$H
#' sc$Hj
#' }
#'
#' @export
Scalability <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (!inherits(U, "exametrika")) {
    dat <- dataFormat(U, na = na, Z = Z, w = w)
  } else {
    dat <- U
  }
  if (dat$response.type == "nominal") {
    response_type_error(dat$response.type, "Scalability")
  }
  # Ordinal data are scored by category; binary and rated data by 0/1
  X <- if (dat$response.type == "ordinal") dat$Q else dat$U
  obs <- dat$Z == 1
  nitems <- ncol(X)
  if (nitems < 2) {
    stop("Scalability requires at least two items.")
  }

  core <- scalability_core(X, obs)
  Hij <- core$Hij
  Hj <- core$Hj
  H <- core$H

  labels <- dat$ItemLabel
  dimnames(Hij) <- list(labels, labels)
  names(Hj) <- labels

  ret <- structure(
    list(ItemLabel = labels, Hij = Hij, Hj = Hj, H = H),
    class = c("exametrika", "Scalability")
  )
  return(ret)
}

#' H coefficients from a score matrix and an observed-cell mask
#' @noRd
scalability_core <- function(X, obs) {
  nitems <- ncol(X)
  num <- matrix(0, nitems, nitems)
  den <- matrix(0, nitems, nitems)
  for (i in seq_len(nitems - 1)) {
    for (j in seq(i + 1, nitems)) {
      both <- obs[, i] & obs[, j]
      cv <- scalability_pair(X[both, i], X[both, j])
      num[i, j] <- num[j, i] <- cv[1]
      den[i, j] <- den[j, i] <- cv[2]
    }
  }
  Hij <- num / den
  diag(Hij) <- NA
  Hj <- rowSums(num) / rowSums(den)
  H <- sum(num[upper.tri(num)]) / sum(den[upper.tri(den)])
  return(list(Hij = Hij, Hj = Hj, H = H))
}

#' Observed and maximal covariance of one item pair
#'
#' The maximum covariance given the two marginals is reached when the
#' sorted scores are paired (Frechet-Hoeffding upper bound). Both are
#' computed with the same denominator, so the ratio does not depend on it.
#'
#' @param x,y Numeric vectors of equal length (complete cases).
#' @return Numeric vector c(observed, maximum).
#' @noRd
scalability_pair <- function(x, y) {
  n <- length(x)
  if (n < 2) {
    return(c(NA_real_, NA_real_))
  }
  observed <- sum((x - mean(x)) * (y - mean(y))) / n
  maximum <- sum((sort(x) - mean(x)) * (sort(y) - mean(y))) / n
  return(c(observed, maximum))
}

#' @title Manifest Monotonicity (Rest-Score Regression)
#'
#' @description
#' Checks, item by item, whether the probability of reaching each category
#' rises with the rest score, the observable counterpart of latent
#' monotonicity in Mokken scale analysis (Molenaar & Sijtsma, 2000; van der
#' Ark, 2007). A single-peaked or U-shaped item, which the item-rest
#' correlation and \code{\link{Scalability}} can show as merely weak, appears
#' here as a run of decreases.
#'
#' @details
#' For item \eqn{j} the rest score \eqn{R_{-j}} is the sum of the other
#' items. Respondents are sorted by rest score and cut into groups of at least
#' \code{minsize} people, never splitting a rest score; a remainder smaller
#' than \code{minsize} is merged into the last group. Within each group the
#' item-step proportions \eqn{P(X_j \ge x)} are computed for every category
#' \eqn{x} above the lowest. Manifest monotonicity requires
#' \deqn{P(X_j \ge x \mid R_{-j} \in \text{higher group}) \ge
#'   P(X_j \ge x \mid R_{-j} \in \text{lower group})}
#' for every pair of groups. A decrease larger than \code{minvi} counts as a
#' violation, and each violation gets the normal approximation
#' \deqn{z = \frac{2\left|\sqrt{(a+1)(d+1)} - \sqrt{bc}\right|}{\sqrt{n-1}},}
#' where \eqn{a} and \eqn{b} are the numbers at or above and below \eqn{x}
#' in the higher group, \eqn{c} and \eqn{d} the same in the lower group, and
#' \eqn{n = a + b + c + d}. A \eqn{z} above 1.6449 (one-sided 5\%, no
#' multiplicity correction) is counted as significant.
#'
#' The default \code{minsize} follows the mokken package: \eqn{N/10} for
#' \eqn{N \ge 500}, \eqn{N/5} for \eqn{250 < N < 500}, \eqn{N/3} for
#' \eqn{N \le 250} and 50 for \eqn{N < 150} (all rounded down). It is
#' advisable to repeat the check with a few values of \code{minsize}.
#'
#' Respondents with any missing response are dropped (listwise), because a
#' rest score over different item sets is not comparable; a message reports
#' how many. With complete data the results agree with
#' \code{check.monotonicity()} of the \pkg{mokken} package.
#'
#' For polytomous items latent monotonicity does not strictly imply manifest
#' monotonicity (Junker & Sijtsma, 2000); the check is used in practice as if
#' it did. The function only reports; it never recodes or drops items.
#'
#' @inheritParams Scalability
#' @param minvi Smallest decrease counted as a violation. Default 0.03.
#' @param minsize Smallest rest-score group. \code{NULL} (default) uses the
#'   rule described in Details.
#'
#' @return An object of class \code{c("exametrika", "ManifestMonotonicity")}
#' with
#' \describe{
#'   \item{ItemLabel}{Item labels.}
#'   \item{summary}{Data frame, one row per item: the item scalability
#'     coefficient \code{Hj} and the totals over item steps of active pairs,
#'     violations, violations per active pair, largest violation, sum of
#'     violations, sum per active pair, largest \eqn{z} and number of
#'     significant violations.}
#'   \item{groups}{List, one data frame per item: rest-score range, size,
#'     category frequencies, mean item score and \eqn{P(X \ge x)} for each
#'     group.}
#'   \item{violations}{List, one matrix per item: the same statistics per
#'     item step, with the two groups involved in the largest \eqn{z}.}
#'   \item{minsize, minvi}{The settings used.}
#'   \item{n_dropped}{Number of respondents dropped for missing responses.}
#' }
#'
#' @references
#' Junker, B. W., & Sijtsma, K. (2000). Latent and manifest monotonicity in
#' item response models. \emph{Applied Psychological Measurement}, 24, 65--81.
#'
#' Molenaar, I. W., & Sijtsma, K. (2000). \emph{User's manual MSP5 for
#' Windows}. IEC ProGAMMA.
#'
#' van der Ark, L. A. (2007). Mokken scale analysis in R. \emph{Journal of
#' Statistical Software}, 20(11), 1--19.
#'
#' @examples
#' \donttest{
#' mm <- ManifestMonotonicity(J5S1000)
#' mm$summary
#' }
#'
#' @export
ManifestMonotonicity <- function(U, na = NULL, Z = NULL, w = NULL,
                                 minvi = 0.03, minsize = NULL) {
  if (!inherits(U, "exametrika")) {
    dat <- dataFormat(U, na = na, Z = Z, w = w)
  } else {
    dat <- U
  }
  if (dat$response.type == "nominal") {
    response_type_error(dat$response.type, "ManifestMonotonicity")
  }
  X <- if (dat$response.type == "ordinal") dat$Q else dat$U

  # Listwise deletion: a rest score must be a sum over the same items
  complete <- rowSums(dat$Z == 0) == 0
  n_dropped <- sum(!complete)
  if (n_dropped > 0) {
    message(
      "ManifestMonotonicity: ", n_dropped,
      " respondents with missing responses were dropped."
    )
  }
  X <- X[complete, , drop = FALSE]
  N <- nrow(X)
  J <- ncol(X)
  if (J < 2) {
    stop("ManifestMonotonicity requires at least two items.")
  }
  if (is.null(minsize)) {
    minsize <- mm_default_minsize(N)
  }
  if (minsize > N / 2) {
    stop("minsize is larger than half the number of respondents.")
  }

  # Categories run from the lowest to the highest code observed in the data
  cats <- seq(min(X), max(X))
  labels <- dat$ItemLabel
  total <- rowSums(X)

  groups <- vector("list", J)
  violations <- vector("list", J)
  for (j in seq_len(J)) {
    rest <- total - X[, j]
    member <- mm_rest_groups(rest, minsize)
    groups[[j]] <- mm_group_table(X[, j], rest, member, cats)
    violations[[j]] <- mm_violations(X[, j], member, cats, minvi)
  }
  names(groups) <- labels
  names(violations) <- labels

  hj <- scalability_core(X, matrix(TRUE, nrow(X), ncol(X)))$Hj
  tot <- t(vapply(violations, function(v) v[nrow(v), ], numeric(10)))
  smry <- data.frame(
    Item = labels,
    Hj = unname(hj),
    n_active = tot[, 1],
    n_violation = tot[, 2],
    vi_per_active = tot[, 3],
    max_violation = tot[, 4],
    sum_violation = tot[, 5],
    sum_per_active = tot[, 6],
    z_max = tot[, 7],
    n_significant = tot[, 10],
    row.names = NULL
  )

  ret <- structure(
    list(
      ItemLabel = labels,
      summary = smry,
      groups = groups,
      violations = violations,
      minsize = minsize,
      minvi = minvi,
      n_dropped = n_dropped
    ),
    class = c("exametrika", "ManifestMonotonicity")
  )
  return(ret)
}

#' Default minimum rest-score group size (as in mokken)
#' @noRd
mm_default_minsize <- function(N) {
  size <- if (N >= 500) floor(N / 10) else floor(N / 5)
  if (N <= 250) size <- floor(N / 3)
  if (N < 150) size <- 50
  return(size)
}

#' Assign respondents to rest-score groups
#'
#' Walk up the sorted rest scores, closing a group once it holds at least
#' `minsize` people and only at the end of a run of tied scores; a final
#' remainder smaller than `minsize` joins the last group.
#' @return Integer vector of group numbers, one per respondent.
#' @noRd
mm_rest_groups <- function(rest, minsize) {
  sorted <- sort(rest)
  N <- length(sorted)
  last_tie <- function(pos) max(which(sorted == sorted[pos]))
  bounds <- last_tie(minsize)
  while (N - max(bounds) >= minsize) {
    bounds <- c(bounds, last_tie(minsize + max(bounds)))
  }
  bounds <- bounds[-length(bounds)]
  upper <- c(sorted[bounds], max(sorted))
  member <- vapply(rest, function(r) sum(r > upper) + 1L, integer(1))
  return(member)
}

#' Per-group frequencies and item-step proportions for one item
#' @noRd
mm_group_table <- function(x, rest, member, cats) {
  G <- max(member)
  freq <- t(vapply(seq_len(G), function(g) {
    tabulate(match(x[member == g], cats), length(cats))
  }, numeric(length(cats))))
  n <- rowSums(freq)
  cum <- t(apply(freq, 1, function(f) rev(cumsum(rev(f))))) / n
  out <- data.frame(
    Group = seq_len(G),
    Lo = vapply(seq_len(G), function(g) min(rest[member == g]), numeric(1)),
    Hi = vapply(seq_len(G), function(g) max(rest[member == g]), numeric(1)),
    N = n
  )
  colnames(freq) <- paste0("F", cats)
  out <- cbind(out, freq)
  out$Mean <- as.vector(freq %*% cats) / n
  steps <- cum[, -1, drop = FALSE]
  colnames(steps) <- paste0("P(X>=", cats[-1], ")")
  out <- cbind(out, steps)
  return(out)
}

#' Violation statistics per item step for one item
#' @return Matrix with one row per item step plus a Total row.
#' @noRd
mm_violations <- function(x, member, cats, minvi) {
  G <- max(member)
  K <- length(cats)
  out <- matrix(0, nrow = K, ncol = 10)
  dimnames(out) <- list(
    c(paste0("P(X>=", cats[-1], ")"), "Total"),
    c(
      "n_active", "n_violation", "vi_per_active", "max_violation",
      "sum_violation", "sum_per_active", "z_max", "group_low",
      "group_high", "n_significant"
    )
  )
  for (s in seq_len(K - 1)) {
    at_or_above <- vapply(seq_len(G), function(g) sum(x[member == g] >= cats[s + 1]), numeric(1))
    below <- vapply(seq_len(G), function(g) sum(x[member == g] < cats[s + 1]), numeric(1))
    p <- at_or_above / (at_or_above + below)
    # Active pair: the lower group is above 0 and the higher one below 1,
    # so that a decrease is possible at all
    active <- 0
    best <- c(z = 0, lo = 0, hi = 0)
    n_vi <- 0
    max_vi <- 0
    sum_vi <- 0
    n_sig <- 0
    for (lo in seq_len(G - 1)) {
      for (hi in seq(lo + 1, G)) {
        if (p[lo] > 1e-10 && p[hi] < 0.999999999999) active <- active + 1
        drop <- p[lo] - p[hi]
        if (drop > minvi) {
          n_vi <- n_vi + 1
          max_vi <- max(max_vi, drop)
          sum_vi <- sum_vi + drop
          a <- at_or_above[hi]
          b <- below[hi]
          c_ <- at_or_above[lo]
          d <- below[lo]
          z <- abs(2 * (sqrt((a + 1) * (d + 1)) - sqrt(b * c_))) /
            sqrt(a + b + c_ + d - 1)
          if (z > 1.6449) n_sig <- n_sig + 1
          if (z > best["z"]) best <- c(z = z, lo = lo, hi = hi)
        }
      }
    }
    out[s, ] <- c(
      active, n_vi, n_vi / active, max_vi, sum_vi, sum_vi / active,
      best["z"], best["lo"], best["hi"], n_sig
    )
  }
  out[K, 1] <- sum(out[-K, 1])
  out[K, 2] <- sum(out[-K, 2])
  out[K, 3] <- out[K, 2] / out[K, 1]
  out[K, 4] <- max(out[-K, 4])
  out[K, 5] <- sum(out[-K, 5])
  out[K, 6] <- out[K, 5] / out[K, 1]
  out[K, 7] <- max(out[-K, 7])
  out[K, 10] <- sum(out[-K, 10])
  return(out)
}

#' @title Check Unidimensionality and Monotonicity Before an Ordered Analysis
#'
#' @description
#' Runs the checks that an order-restricted (isotonic) analysis takes for
#' granted: that every item rises with the same latent order, with no
#' reverse-keyed item left unrecoded and no item that peaks in the middle.
#' For each item it reports the item-rest correlation, the loading on the
#' first principal component, the scalability coefficient \eqn{H_j}, and the
#' violations of manifest monotonicity, and flags the items that look
#' reverse-keyed, non-monotone or weak.
#'
#' @details
#' \strong{Reference set and iteration.} An item is judged against the rest
#' score, but a reverse-keyed or non-monotone item inside the rest score
#' distorts the yardstick for every other item, so that sound items also
#' show violations. The rest score is therefore built from a reference set
#' of items that is cleaned step by step. It starts from the items whose
#' first-component loading has the majority sign. In each round every item
#' is judged against the reference set (excluding itself); if an item in the
#' set is flagged as reverse-keyed or non-monotone, the worst one (most
#' significant violations, then lowest item-rest correlation) leaves the set
#' and the round is repeated. Removing one item at a time keeps a heavily
#' contaminated first round from discarding every item. The items outside
#' the final set after each round are kept in \code{rounds}.
#'
#' \strong{Subscales.} When \code{fields} is given, each subscale is checked
#' on its own. This matters: on a multidimensional scale the item-total
#' relations are dominated by the mixture of dimensions, and reverse-keyed
#' items can stay hidden when all items are pooled.
#'
#' \strong{Flags.} An item is flagged \code{"reverse?"} when its correlation
#' with the clean rest score is negative, \code{"non-monotone"} when it has at
#' least one significant violation of manifest monotonicity (see
#' \code{\link{ManifestMonotonicity}}; with large samples even a small
#' decrease becomes significant, so read the flag together with the number
#' of violations and the largest \eqn{z}), and \code{"weak"} when \eqn{H_j}
#' against the clean items is below \code{weak_h} (0.3, Mokken's rule of
#' thumb). The first-component loading is computed once on all items of the
#' subscale (tetrachoric or polychoric correlations) and reported for
#' reference; its sign is oriented to sum to a positive value, so it marks
#' items against the majority rather than against a known direction.
#'
#' Respondents with a missing response in a subscale are dropped for that
#' subscale. The function only reports; recoding or dropping items is left
#' to the person who prepares the data.
#'
#' @inheritParams ManifestMonotonicity
#' @param fields Optional vector with one entry per item giving its
#'   subscale (field). \code{NULL} (default) treats all items as one scale.
#' @param weak_h Threshold of \eqn{H_j} below which an item is flagged as
#'   weak. Default 0.3.
#' @param max_rounds Maximum number of rounds of the iteration. \code{NULL}
#'   (default) allows as many rounds as there are items.
#'
#' @return An object of class \code{c("exametrika", "MonotonicityCheck")}
#' with
#' \describe{
#'   \item{items}{Data frame, one row per item: field, item, item-rest
#'     correlation, first-component loading, \eqn{H_j}, number of violations,
#'     number of significant violations, largest \eqn{z}, and the flags (all
#'     against the clean rest score of the final round).}
#'   \item{fields}{Data frame, one row per field: number of items and
#'     respondents used, \eqn{H} over all items, \eqn{H} over the items left
#'     unflagged, number of flagged items and rounds used.}
#'   \item{rounds}{List per field of the flagged items in each round.}
#'   \item{detail}{List per field of the rest-score group tables of the final
#'     round, as in \code{\link{ManifestMonotonicity}}.}
#'   \item{minsize, minvi, weak_h}{The settings used.}
#' }
#'
#' @seealso \code{\link{Scalability}}, \code{\link{ManifestMonotonicity}},
#'   \code{\link{Dimensionality}}
#'
#' @examples
#' \donttest{
#' mc <- MonotonicityCheck(J5S1000)
#' mc
#' }
#'
#' @export
MonotonicityCheck <- function(U, na = NULL, Z = NULL, w = NULL, fields = NULL,
                              minvi = 0.03, minsize = NULL, weak_h = 0.3,
                              max_rounds = NULL) {
  if (!inherits(U, "exametrika")) {
    dat <- dataFormat(U, na = na, Z = Z, w = w)
  } else {
    dat <- U
  }
  if (dat$response.type == "nominal") {
    response_type_error(dat$response.type, "MonotonicityCheck")
  }
  X_all <- if (dat$response.type == "ordinal") dat$Q else dat$U
  labels <- dat$ItemLabel
  J <- ncol(X_all)
  if (is.null(fields)) {
    fields <- rep("All", J)
  }
  if (length(fields) != J) {
    stop("fields must have one entry per item (", J, ").")
  }
  field_ids <- unique(fields)

  item_rows <- list()
  field_rows <- list()
  rounds <- list()
  detail <- list()
  used_minsize <- integer(0)
  for (fid in field_ids) {
    cols <- which(fields == fid)
    if (length(cols) < 3) {
      warning(
        "MonotonicityCheck: field '", fid, "' has fewer than three items ",
        "and is skipped."
      )
      next
    }
    keep <- rowSums(dat$Z[, cols, drop = FALSE] == 0) == 0
    if (sum(!keep) > 0) {
      message(
        "MonotonicityCheck: ", sum(!keep), " respondents with missing ",
        "responses were dropped for field '", fid, "'."
      )
    }
    X <- X_all[keep, cols, drop = FALSE]
    res <- mc_check_field(X, labels[cols], dat$response.type, minvi, minsize,
                          weak_h, max_rounds)
    used_minsize <- c(used_minsize, res$minsize)
    item_rows[[fid]] <- cbind(Field = fid, res$items)
    field_rows[[fid]] <- data.frame(
      Field = fid, n_items = length(cols), n_used = nrow(X),
      H_all = res$H_all, H_clean = res$H_clean,
      n_flagged = sum(res$items$flag != ""), rounds = length(res$rounds),
      stringsAsFactors = FALSE
    )
    rounds[[fid]] <- res$rounds
    detail[[fid]] <- res$groups
  }
  items <- do.call(rbind, item_rows)
  rownames(items) <- NULL
  field_tab <- do.call(rbind, field_rows)
  rownames(field_tab) <- NULL

  ret <- structure(
    list(
      items = items,
      fields = field_tab,
      rounds = rounds,
      detail = detail,
      minsize = used_minsize,
      minvi = minvi,
      weak_h = weak_h
    ),
    class = c("exametrika", "MonotonicityCheck")
  )
  return(ret)
}

#' Iterated check of one field
#' @noRd
mc_check_field <- function(X, labels, response_type, minvi, minsize, weak_h,
                           max_rounds) {
  N <- nrow(X)
  J <- ncol(X)
  if (is.null(minsize)) {
    minsize <- mm_default_minsize(N)
  }
  if (minsize > N / 2) {
    stop("minsize is larger than half the number of respondents.")
  }
  cats <- seq(min(X), max(X))
  pair <- mc_pair_cov(X)
  loading <- mc_first_loading(X, response_type)

  # Start from the items that load in the majority direction, then drop the
  # worst flagged item from the reference set one at a time. Removing one at a
  # time keeps a contaminated first round from discarding every item.
  clean <- which(loading >= 0)
  if (length(clean) < 2) {
    clean <- seq_len(J)
  }
  history <- list(labels[setdiff(seq_len(J), clean)])
  if (is.null(max_rounds)) {
    max_rounds <- J
  }
  for (round in seq_len(max_rounds)) {
    stats <- mc_round(X, clean, pair, cats, minvi, minsize)
    bad <- clean[stats$r_rest[clean] < 0 | stats$n_significant[clean] > 0]
    bad <- bad[!is.na(bad)]
    if (length(bad) == 0 || length(clean) <= 2) {
      break
    }
    # Worst = most significant violations, then lowest item-rest correlation
    worst <- bad[order(-stats$n_significant[bad], stats$r_rest[bad])][1]
    clean <- setdiff(clean, worst)
    history[[length(history) + 1]] <- labels[setdiff(seq_len(J), clean)]
  }
  flagged <- labels[setdiff(seq_len(J), clean)]

  flag <- vapply(seq_len(J), function(j) {
    f <- c(
      if (stats$r_rest[j] < 0) "reverse?",
      if (stats$n_significant[j] > 0) "non-monotone",
      if (is.finite(stats$Hj[j]) && stats$Hj[j] < weak_h) "weak"
    )
    return(paste(f, collapse = ", "))
  }, character(1))

  clean_final <- setdiff(seq_len(J), match(flagged, labels))
  H_clean <- if (length(clean_final) >= 2) {
    mc_h(pair, clean_final)
  } else {
    NA_real_
  }
  items <- data.frame(
    Item = labels,
    r_rest = stats$r_rest,
    loading = unname(loading),
    Hj = stats$Hj,
    n_violation = stats$n_violation,
    n_significant = stats$n_significant,
    z_max = stats$z_max,
    flag = flag,
    stringsAsFactors = FALSE
  )
  groups <- stats$groups
  names(groups) <- labels
  ret <- list(
    items = items, H_all = mc_h(pair, seq_len(J)), H_clean = H_clean,
    rounds = history, groups = groups, minsize = minsize
  )
  return(ret)
}

#' Statistics of every item against the rest score over `clean` items
#' @noRd
mc_round <- function(X, clean, pair, cats, minvi, minsize) {
  J <- ncol(X)
  out <- list(
    r_rest = numeric(J), Hj = numeric(J), n_violation = numeric(J),
    n_significant = numeric(J), z_max = numeric(J), groups = vector("list", J)
  )
  for (j in seq_len(J)) {
    ref <- setdiff(clean, j)
    rest <- rowSums(X[, ref, drop = FALSE])
    out$r_rest[j] <- if (stats::sd(rest) > 0 && stats::sd(X[, j]) > 0) {
      stats::cor(X[, j], rest)
    } else {
      NA_real_
    }
    out$Hj[j] <- sum(pair$num[j, ref]) / sum(pair$den[j, ref])
    member <- mm_rest_groups(rest, minsize)
    out$groups[[j]] <- mm_group_table(X[, j], rest, member, cats)
    v <- mm_violations(X[, j], member, cats, minvi)
    tot <- v[nrow(v), ]
    out$n_violation[j] <- tot[2]
    out$n_significant[j] <- tot[10]
    out$z_max[j] <- tot[7]
  }
  return(out)
}

#' Observed and maximal covariances of all item pairs (complete data)
#' @noRd
mc_pair_cov <- function(X) {
  J <- ncol(X)
  num <- matrix(0, J, J)
  den <- matrix(0, J, J)
  for (i in seq_len(J - 1)) {
    for (j in seq(i + 1, J)) {
      cv <- scalability_pair(X[, i], X[, j])
      num[i, j] <- num[j, i] <- cv[1]
      den[i, j] <- den[j, i] <- cv[2]
    }
  }
  return(list(num = num, den = den))
}

#' Test H over a subset of items
#' @noRd
mc_h <- function(pair, idx) {
  num <- pair$num[idx, idx]
  den <- pair$den[idx, idx]
  return(sum(num[upper.tri(num)]) / sum(den[upper.tri(den)]))
}

#' First-component loadings of one field (tetrachoric or polychoric)
#' @noRd
mc_first_loading <- function(X, response_type) {
  type <- if (response_type == "ordinal") "ordinal" else "binary"
  tmp <- suppressMessages(dataFormat(X, response.type = type))
  R <- if (type == "ordinal") {
    PolychoricCorrelationMatrix(tmp)
  } else {
    TetrachoricCorrelationMatrix(tmp)
  }
  res <- dimensionality_result(R, colnames(X))
  return(res$first_loading)
}
