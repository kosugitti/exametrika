# Exact integrated completed likelihood (ICL_exact) --------------------------
#
# The ICL reported as `ICL` is the BIC-type approximation of Keribin, Brault,
# Celeux and Govaert (2015). This file computes the criterion itself: the
# parameters are integrated out under the prior of the fitted model and the
# memberships are the hard (MAP) assignments of the final posterior,
#
#   ICL_exact = -2 [ log p(c_hat) + log p(f_hat) + log p(Q | c_hat, f_hat) ]
#
# on the -2 log scale of AIC/BIC/ICL (smaller is better). Nothing is counted:
# the order restriction of the isotonic models, whose effective number of
# parameters is data-dependent, is handled by its prior probability instead.
#
# Terms:
#   blocks      Every block (field f, class c) -- or (item j, class c) for a
#               one-mode model -- holds a multinomial with a Dirichlet(alpha)
#               prior, so the data term is a product of Dirichlet-multinomial
#               marginals of the block counts. Categories that never occur
#               contribute nothing beyond the normalising constant; the block
#               keeps the number of categories the fitted model gave it (maxQ
#               for a biclustering block, the item's own count for a one-mode
#               model). Missing cells are absent from the counts.
#   assignment  The class term is sum_c n_c log w_c with the mixing weights of
#               the fitted model: -n log C when they are fixed at 1/C and the
#               estimated weights with mixing = "estimate". The field term is
#               -J log F (always fixed at 1/F).
#   order       For an order-restricted (isotonic) model the prior on the
#               ranks of one field (or item) is the Dirichlet restricted to
#               the region R where the upper-cumulative probabilities are
#               non-decreasing across ranks. By the encompassing-prior
#               identity (Klugkist, Laudy and Hoijtink, 2005) the restricted
#               marginal equals the unrestricted one times
#               P(R | data) / P(R). P(R | data) is estimated by a sequential
#               particle method from the independent Dirichlet posteriors of
#               the ranks (log_order_chain_mc); P(R) is exact under the flat
#               prior (see log_order_prior_uniform) and estimated by the same
#               particle method otherwise.
#
# A GTM or SOM fit is a smoothed estimate with no prior model, so ICL_exact is
# NA there.

#' Hook-length formula for the prior probability of the stochastic order
#'
#' Under the flat prior (alpha = 1) the upper-cumulative vector S of a rank
#' is uniform on the ordered simplex {1 >= S_1 >= ... >= S_d >= 0} with
#' d = Q - 1. The region R (S non-decreasing across the C ranks, and
#' non-increasing within a rank) is the order polytope of the product of
#' two chains, C by d; its volume is the number of linear extensions
#' divided by (Cd)!, and the linear extensions of a C-by-d grid are the
#' standard Young tableaux of the C-by-d rectangle, counted by the hook-length
#' formula. Dividing by the volume (1/d!)^C of the unrestricted region gives
#' P(R) = (d!)^C / prod of hook lengths, which reduces to 1/C! for Q = 2.
#' @param C number of ranks.
#' @param Q number of categories.
#' @return log P(R).
#' @noRd
log_order_prior_uniform <- function(C, Q) {
  d <- Q - 1
  if (C < 2 || d < 1) {
    return(0)
  }
  hooks <- outer(C - seq_len(C), d - seq_len(d), "+") + 1
  return(C * lfactorial(d) - sum(log(hooks)))
}

#' Rows of Dirichlet draws
#' @param M number of draws.
#' @param alpha concentration vector (length Q).
#' @return M x Q matrix.
#' @noRd
rdirichlet_rows <- function(M, alpha) {
  Q <- length(alpha)
  g <- matrix(stats::rgamma(M * Q, shape = rep(alpha, each = M)), nrow = M)
  return(g / rowSums(g))
}

#' Upper-cumulative probabilities of the rows of a probability matrix
#' @param P M x Q matrix of probability vectors.
#' @return M x (Q - 1) matrix, column b holding P(category > b).
#' @noRd
upper_cum_rows <- function(P) {
  Q <- ncol(P)
  S <- matrix(0, nrow = nrow(P), ncol = Q - 1)
  acc <- P[, Q]
  for (b in (Q - 1):1) {
    S[, b] <- acc
    if (b > 1) acc <- acc + P[, b]
  }
  return(S)
}

#' Probability that independent Dirichlet ranks are stochastically ordered
#'
#' Sequential particle estimate of P(S_1 <= S_2 <= ... <= S_C) for C
#' independent Dirichlet vectors (row r of \code{a} holds the parameters of
#' rank r), S being the upper-cumulative probabilities. The constraint
#' between ranks k and k + 1 is added one at a time: the fraction of fresh
#' draws of rank k + 1 that dominate the current particles estimates the
#' conditional probability given the constraints so far, and the accepted
#' draws are resampled to the particle count before the next step. The
#' product of the conditional probabilities estimates the joint one; it stays
#' accurate when the joint probability is far below 1/M, which a direct count
#' could not resolve. A step without any accepted draw after the retries is
#' capped at the resolution reached (1/(100 M)).
#'
#' Used for the prior (every row of \code{a} equal to alpha; exact under the
#' flat prior, see \code{log_order_prior_uniform}) and for the posterior
#' (alpha + the block counts of each rank).
#' @param a C x Q matrix of Dirichlet parameters, one row per rank.
#' @param M number of particles.
#' @return list(log_p, se, capped).
#' @noRd
log_order_chain_mc <- function(a, M) {
  C <- nrow(a)
  Q <- ncol(a)
  if (C < 2 || Q < 2) {
    return(list(log_p = 0, se = 0, capped = FALSE))
  }
  if (Q == 2) {
    return(log_order_chain_beta(a, M))
  }
  S_top <- upper_cum_rows(rdirichlet_rows(M, a[1, ]))
  log_p <- 0
  var_sum <- 0
  capped <- FALSE
  for (k in seq_len(C - 1)) {
    S_new <- upper_cum_rows(rdirichlet_rows(M, a[k + 1, ]))
    ok <- rowSums(S_new >= S_top) == (Q - 1)
    M_k <- M
    for (mult in c(10, 100)) {
      if (any(ok)) break
      # Below the resolution of M particles: retry with more draws of this
      # rank against the same particles (recycled), 10x then 100x.
      M_k <- mult * M
      S_new <- upper_cum_rows(rdirichlet_rows(M_k, a[k + 1, ]))
      ok <- rowSums(S_new >= S_top[rep(seq_len(M), mult), , drop = FALSE]) == (Q - 1)
    }
    n_ok <- sum(ok)
    if (n_ok == 0) {
      # Cap this step and carry on from unconstrained draws of this rank: the
      # remaining steps are then conditional on a looser event and the result
      # is a bound.
      log_p <- log_p - log(M_k)
      capped <- TRUE
      S_top <- S_new[seq_len(M), , drop = FALSE]
      next
    }
    p <- n_ok / M_k
    log_p <- log_p + log(p)
    var_sum <- var_sum + (1 - p) / (p * M_k)
    idx <- which(ok)
    S_top <- S_new[idx[sample.int(n_ok, M, replace = TRUE)], , drop = FALSE]
  }
  return(list(log_p = log_p, se = sqrt(var_sum), capped = capped))
}

#' Chain probability for binary data, Rao-Blackwellised
#'
#' With two categories the order is on one number per rank, the correct
#' response probability pi_r ~ Beta(a_r2, a_r1). Given the particles pi_k of
#' the current top rank, the conditional probability that the next rank
#' dominates is the mean of 1 - F_{k+1}(pi_k) over the particles, exact in
#' the Beta cdf, and the particles of the next rank are drawn from the Beta
#' truncated below at pi_k by inversion. No draw is rejected, so there is
#' nothing to cap.
#' @param a C x 2 matrix: column 1 the parameter of "incorrect", column 2
#'   of "correct".
#' @param M number of particles.
#' @noRd
log_order_chain_beta <- function(a, M) {
  C <- nrow(a)
  top <- stats::rbeta(M, a[1, 2], a[1, 1])
  log_p <- 0
  var_sum <- 0
  for (k in seq_len(C - 1)) {
    sh1 <- a[k + 1, 2]
    sh2 <- a[k + 1, 1]
    F_top <- stats::pbeta(top, sh1, sh2)
    tail <- 1 - F_top
    p <- mean(tail)
    if (p <= 0) {
      # Only when every particle sits at the upper boundary in floating point
      return(list(log_p = -Inf, se = NA_real_, capped = TRUE))
    }
    log_p <- log_p + log(p)
    var_sum <- var_sum + stats::var(tail) / (M * p^2)
    # Next particles: resample proportional to the tail mass, then draw from
    # the truncated Beta by inversion.
    idx <- sample.int(M, M, replace = TRUE, prob = tail)
    u <- stats::runif(M)
    top <- stats::qbeta(F_top[idx] + u * tail[idx], sh1, sh2)
    top <- pmin(top, 1)
  }
  return(list(log_p = log_p, se = sqrt(var_sum), capped = FALSE))
}

#' Prior probability of the stochastic order, exact when the prior is flat
#' @param C number of ranks.
#' @param alpha concentration vector of the prior (length Q).
#' @param M number of particles for a non-flat prior.
#' @noRd
log_order_prior <- function(C, alpha, M) {
  if (all(alpha == 1)) {
    return(list(log_p = log_order_prior_uniform(C, length(alpha)), se = 0, capped = FALSE))
  }
  a <- matrix(alpha, nrow = C, ncol = length(alpha), byrow = TRUE)
  res <- log_order_chain_mc(a, M)
  if (res$capped) {
    warning(
      "ICL_exact: the prior probability of the rank order under the given ",
      "alpha fell below the Monte Carlo resolution; ICL_exact is a bound."
    )
  }
  return(res)
}

#' Posterior probability that the ranks of one field are stochastically ordered
#'
#' The ranks are independent given the hard assignments, each with a
#' Dirichlet(alpha + counts) posterior, so the chain estimate applies.
#' @param counts C x Q matrix of block counts for the field.
#' @param alpha concentration vector of the prior (length Q).
#' @param M number of particles.
#' @return list(log_p, se, capped).
#' @noRd
log_order_posterior <- function(counts, alpha, M) {
  a <- sweep(counts, 2, alpha, "+")
  return(log_order_chain_mc(a, M))
}

#' Log Dirichlet-multinomial marginal of block counts
#' @param counts F x C x Q array of block counts.
#' @param alpha concentration: scalar or length-Q vector.
#' @param ncat number of categories of each block row (length F): the
#'   Dirichlet of a block has this many components; columns beyond it carry
#'   no counts.
#' @return sum over blocks of log p(counts | alpha).
#' @noRd
log_dirmult_blocks <- function(counts, alpha, ncat) {
  Fn <- dim(counts)[1]
  C <- dim(counts)[2]
  total <- 0
  for (f in seq_len(Fn)) {
    Qf <- ncat[f]
    a <- rep(alpha, length.out = Qf)
    a0 <- sum(a)
    n_fc <- counts[f, , seq_len(Qf), drop = FALSE]
    dim(n_fc) <- c(C, Qf)
    total <- total + C * (lgamma(a0) - sum(lgamma(a))) +
      sum(lgamma(sweep(n_fc, 2, a, "+"))) - sum(lgamma(a0 + rowSums(n_fc)))
  }
  return(total)
}

#' Block counts from an indicator array and hard memberships
#' @param ZU N x J x Q indicator array (missing cells all zero).
#' @param cls01 N x C hard class memberships.
#' @param fld01 J x F hard field memberships, or NULL for a one-mode model
#'   (every item is its own block row).
#' @return F x C x Q (or J x C x Q) array.
#' @noRd
icl_exact_counts <- function(ZU, cls01, fld01 = NULL) {
  Q <- dim(ZU)[3]
  C <- ncol(cls01)
  Fn <- if (is.null(fld01)) dim(ZU)[2] else ncol(fld01)
  counts <- array(0, dim = c(Fn, C, Q))
  for (q in seq_len(Q)) {
    m <- t(ZU[, , q]) %*% cls01 # J x C
    counts[, , q] <- if (is.null(fld01)) m else t(fld01) %*% m
  }
  return(counts)
}

#' Run an expression with a private random number stream
#'
#' Sets the seed for the expression and restores the caller's RNG state
#' afterwards, so that the Monte Carlo inside ICL_exact neither depends on
#' nor disturbs the global stream.
#' @noRd
with_private_seed <- function(seed, expr) {
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = genv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  })
  set.seed(seed)
  return(force(expr))
}

#' Exact ICL of a fitted latent class, rank or block model
#'
#' @param ZU N x J x Q indicator array of the responses (missing cells zero).
#'   For binary data build it from U and Z (category 1 = incorrect,
#'   2 = correct).
#' @param clsmemb N x C class (rank) memberships; the hard assignment is taken
#'   inside.
#' @param fldmemb J x F field memberships, or NULL for a one-mode model.
#' @param alpha Dirichlet concentration of the block prior: a scalar, or a
#'   length-Q vector (binary models with beta1/beta2 give c(beta2, beta1)).
#' @param ncat number of categories of each block row: length J for a
#'   one-mode model, length F (all maxQ) for a biclustering.
#' @param log_prior_cls length-C log mixing weights of the classes.
#' @param ordered TRUE for an order-restricted (isotonic) model: the order
#'   correction is applied to every field (or item).
#' @param M Monte Carlo draws for the posterior order probability.
#' @param seed seed of the private stream used for the Monte Carlo.
#' @return list(ICL_exact, ICL_exact_se, log_p_blocks, log_p_assign,
#'   log_p_order_post, log_p_order_prior). The standard error is the Monte
#'   Carlo error of the order term (zero for an unordered model), on the -2
#'   log scale.
#' @noRd
icl_exact <- function(ZU, clsmemb, fldmemb = NULL, alpha = 1, ncat,
                      log_prior_cls, ordered = FALSE, M = 10000, seed = 1) {
  cls01 <- hard_membership(clsmemb)
  fld01 <- if (is.null(fldmemb)) NULL else hard_membership(fldmemb)
  counts <- icl_exact_counts(ZU, cls01, fld01)
  C <- ncol(cls01)
  nitems <- dim(ZU)[2]

  log_p_blocks <- log_dirmult_blocks(counts, alpha, ncat)
  log_p_assign <- sum(colSums(cls01) * log_prior_cls)
  if (!is.null(fld01)) {
    log_p_assign <- log_p_assign - nitems * log(ncol(fld01))
  }

  log_post <- 0
  log_prior <- 0
  var_sum <- 0
  capped <- FALSE
  if (ordered && C > 1) {
    res <- with_private_seed(seed, {
      lp <- 0
      lq <- 0
      vs <- 0
      cp <- FALSE
      prior_cache <- list()
      for (f in seq_len(dim(counts)[1])) {
        Qf <- ncat[f]
        a <- rep(alpha, length.out = Qf)
        key <- as.character(Qf)
        if (is.null(prior_cache[[key]])) {
          prior_cache[[key]] <- log_order_prior(C, a, M)
        }
        pr <- prior_cache[[key]]
        n_f <- counts[f, , seq_len(Qf), drop = FALSE]
        dim(n_f) <- c(C, Qf)
        po <- log_order_posterior(n_f, a, M)
        lp <- lp + po$log_p
        lq <- lq + pr$log_p
        vs <- vs + po$se^2 + pr$se^2
        cp <- cp || po$capped
      }
      list(lp = lp, lq = lq, vs = vs, cp = cp)
    })
    log_post <- res$lp
    log_prior <- res$lq
    var_sum <- res$vs
    capped <- res$cp
    if (capped) {
      warning(
        "ICL_exact: the posterior probability of the rank order fell below ",
        "the Monte Carlo resolution in at least one field; ICL_exact is a ",
        "bound there."
      )
    }
  }
  total <- log_p_blocks + log_p_assign + log_post - log_prior
  return(list(
    ICL_exact = -2 * total,
    # NA marks a bound: a field's order probability fell below the resolution
    ICL_exact_se = if (capped) NA_real_ else 2 * sqrt(var_sum),
    log_p_blocks = log_p_blocks,
    log_p_assign = log_p_assign,
    log_p_order_post = log_post,
    log_p_order_prior = log_prior
  ))
}

#' Indicator array of binary responses for icl_exact()
#' @param U response matrix (0 at missing cells).
#' @param Z missing indicator.
#' @return N x J x 2 array: slice 1 incorrect, slice 2 correct.
#' @noRd
binary_indicator_array <- function(U, Z) {
  ZU <- array(0, dim = c(nrow(U), ncol(U), 2))
  ZU[, , 1] <- Z * (1 - U)
  ZU[, , 2] <- Z * U
  return(ZU)
}
