#' @title Weighted pool-adjacent-violators algorithm (PAVA)
#' @description
#' Weighted isotonic (monotone non-decreasing) regression by the
#' pool-adjacent-violators algorithm. Pools adjacent blocks whose fitted
#' values violate the ordering, using the block-mean weighted by \code{w},
#' and backtracks after each merge. Returns the fitted (monotone) vector and
#' the number of resulting blocks, the latter serving as the shape-restricted
#' degrees of freedom (Meyer & Woodroofe 2000).
#' @param y numeric vector to be monotonized (non-decreasing).
#' @param w non-negative weight vector, same length as \code{y}.
#' @return A list with \code{fitted} (monotone vector, same length as \code{y})
#'   and \code{nblock} (number of distinct level blocks).
#' @noRd
pava_up <- function(y, w = rep(1, length(y))) {
  val <- y
  wt <- w
  len <- rep(1, length(y))
  i <- 1
  while (i < length(val)) {
    if (val[i] > val[i + 1]) {
      val[i] <- (val[i] * wt[i] + val[i + 1] * wt[i + 1]) / (wt[i] + wt[i + 1])
      wt[i] <- wt[i] + wt[i + 1]
      len[i] <- len[i] + len[i + 1]
      val <- val[-(i + 1)]
      wt <- wt[-(i + 1)]
      len <- len[-(i + 1)]
      if (i > 1) {
        i <- i - 1
      }
    } else {
      i <- i + 1
    }
  }
  return(list(fitted = rep(val, len), nblock = length(val)))
}


#' @title Isotonic (order-restricted) EM for binary Latent Rank Analysis
#' @description
#' EM estimation for binary LRA under the order restriction that each item's
#' correct-response probability is monotonically non-decreasing across ranks.
#' Unlike the GTM core (\code{emclus}), no filter matrix is applied in the
#' E-step; the rank ordering is instead imposed in the M-step by a weighted
#' PAVA down each item column (weights are the per-rank expected counts). For
#' the default flat prior (\code{beta1 = beta2 = 1}) this weighted PAVA is the
#' exact order-restricted MLE (Ayer et al. 1955); with an informative prior it
#' applies the same weighted pooling to the MAP proportions.
#' @param U response matrix U of the examData class.
#' @param Z missing indicator matrix Z of the examData class.
#' @param ncls number of latent ranks.
#' @param beta1 beta distribution parameter1 (prior successes) for the rank reference matrix.
#' @param beta2 beta distribution parameter2 (prior failures) for the rank reference matrix.
#' @param maxiter Maximum number of iterations.
#' @param mic retained for interface compatibility; the isotonic core is always
#'   monotone, so this argument has no effect.
#' @param verbose verbose output flag. default is FALSE.
#' @param conf Confirmatory IRP matrix (ncls x testlength). Non-NA values are
#'   fixed (applied after the PAVA step), NA values are freely estimated.
#'   NULL means fully exploratory.
#' @return A list with \code{iter}, \code{converge}, \code{postDist},
#'   \code{classRefMat}, and \code{item_nparam} (per-item block count = the
#'   shape-restricted degrees of freedom), matching the \code{emclus} structure
#'   plus \code{item_nparam}.
#' @noRd
emclus_isotonic <- function(U, Z, ncls, beta1, beta2, maxiter = 100, mic = FALSE,
                            verbose = FALSE, conf = NULL, tol = 1e-8) {
  # Initialize
  testlength <- NCOL(U)
  const <- exp(-testlength)
  # The starting value must be below any attainable log-likelihood. The old
  # sentinel -1/const = -exp(J) is not: for a short test with many
  # respondents it sits ABOVE the real log-likelihood, so the first cycle
  # looked like a decrease and the loop exited after one iteration while
  # still reporting convergence. -Inf is unconditionally below, and the
  # comparisons are skipped on the first pass instead.
  test_log_lik <- -Inf
  old_test_log_lik <- -Inf
  classRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)

  # Prepare confirmatory constraints
  if (!is.null(conf)) {
    fixed <- !is.na(conf)
    classRefMat[fixed] <- conf[fixed]
  }

  ## EM algorithm
  emt <- 0
  converge <- TRUE
  FLG <- TRUE

  while (FLG) {
    emt <- emt + 1
    old_test_log_lik <- test_log_lik

    # E-step (no filter smoothing)
    llmat <- U %*% t(log(classRefMat + const)) + (Z * (1 - U)) %*% t(log(1 - classRefMat + const))
    postDist <- row_softmax(llmat)

    # M-step (posterior used directly; no filter)
    correct_cls <- t(postDist) %*% U
    incorrect_cls <- t(postDist) %*% (Z * (1 - U))
    old_classRefMat <- classRefMat
    classRefMat <- (correct_cls + beta1 - 1) / (correct_cls + incorrect_cls + beta1 + beta2 - 2)
    classRefMat <- pmax(pmin(classRefMat, 1 - const), const)

    # Order restriction: weighted PAVA down each item column across ranks
    nmat <- correct_cls + incorrect_cls
    for (j in 1:testlength) {
      classRefMat[, j] <- pava_up(classRefMat[, j], nmat[, j])$fitted
    }

    # Apply confirmatory constraints after PAVA (fixed cells win)
    if (!is.null(conf)) {
      classRefMat[fixed] <- conf[fixed]
    }

    item_log_lik <- colSums(correct_cls * log(classRefMat + const) + incorrect_cls * log(1 - classRefMat + const))
    # Observed-data log-likelihood, for the same reason as in emclus(): the
    # expected log-posterior is not monotone across cycles, so a decrease in it
    # is not evidence of trouble.
    llmat_new <- U %*% t(log(classRefMat + const)) +
      (Z * (1 - U)) %*% t(log(1 - classRefMat + const))
    row_max <- apply(llmat_new, 1, max)
    test_log_lik <- sum(row_max + log(rowSums(exp(llmat_new - row_max)))) -
      NROW(U) * log(ncls)
    if (verbose) {
      message(
        sprintf(
          "\n%-80s",
          paste0("iter ", emt, " log_lik ", format(test_log_lik, digits = 6))
        ),
        appendLF = FALSE
      )
    }
    if (is.finite(old_test_log_lik)) {
      if (test_log_lik - old_test_log_lik <= 0) {
        classRefMat <- old_classRefMat
        FLG <- FALSE
      }
      if ((test_log_lik - old_test_log_lik) <= tol * abs(old_test_log_lik)) {
        FLG <- FALSE
      }
    }
    if (emt == maxiter) {
      message("\nReached the maximum number of iterations.")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
      converge <- FALSE
      FLG <- FALSE
    }
  }

  # Shape-restricted df per item = number of PAVA blocks in the final matrix
  item_nparam <- apply(classRefMat, 2, function(col) length(unique(round(col, 10))))

  ret <- list(
    iter = emt,
    converge = converge,
    postDist = postDist,
    classRefMat = classRefMat,
    item_nparam = item_nparam
  )
  return(ret)
}


#' @title Category probabilities from dual multipliers (ordinal isotonic)
#' @description
#' Internal helper for the order-restricted ordinal M-step. Builds the
#' (nrank x ncat) category-probability matrix from the Fenchel dual variables
#' \code{theta} via the rational stationarity form
#' \eqn{\pi_{ck} = M_{ck} / (\lambda_c + d_{ck})}, with the per-rank normalizer
#' \eqn{\lambda_c} solved so each row sums to one.
#' @param Mcount (nrank x ncat) expected counts plus Dirichlet pseudocounts
#'   (\eqn{U_{ck} + \alpha_{ck} - 1}).
#' @param theta ((ncat-1) x (nrank-1)) non-negative dual multipliers, one per
#'   (boundary, adjacent-rank-pair).
#' @return (nrank x ncat) category-probability matrix.
#' @noRd
#' @title One rank's category probabilities from its dual offsets
#' @description
#' Solves the per-rank normalizer of the stationarity form \eqn{\pi_q = m_q /
#' (\lambda + d_q)} and returns the resulting probability row.
#'
#' Three things about the parameterisation matter more than the choice of root
#' finder, and getting them wrong costs several digits (or all of them).
#'
#' First, categories with \eqn{m_q = 0} are dropped up front. Their probability
#' is zero whatever \eqn{\lambda} is, so they carry no information about it --
#' but if one of them happens to attain \eqn{\min_q d_q}, it drags the lower end
#' of the domain to a place the root is nowhere near.
#'
#' Second, the shifted variable \eqn{u = \lambda + d_{\min}} (so denominators
#' read \eqn{u + d'_q} with \eqn{d' = d - d_{\min} \ge 0}) has a bracket in
#' closed form. From \eqn{1 = \sum_q m_q/(u + d'_q) \ge m_0/u} with \eqn{m_0 =
#' \sum_{q: d'_q = 0} m_q} we get \eqn{u \ge m_0}; from \eqn{d' \ge 0} we get
#' \eqn{u \le \sum_q m_q}. No widening search is needed.
#'
#' Third -- and this is what forces the log scale -- the two ends of that
#' bracket can sit thirteen orders of magnitude apart. On a rank holding almost
#' no weight, \eqn{m_0} is of the order of the smallest surviving count while
#' the upper end is of the order of the row total. Bisecting in \eqn{u} would
#' need roughly eighty halvings to resolve the root, and Newton's method started
#' from the right end steps clean past zero. In \eqn{t = \log u} the bracket is
#' about thirty wide and both behave.
#'
#' The probabilities are formed from \eqn{u + d'_q} rather than \eqn{\lambda +
#' d_q}. Written the second way the sum cancels the shift back out, which throws
#' away digits when \eqn{d_{\min}} is large.
#'
#' @param m one row of the count matrix
#' @param d the rank's offsets, \code{c(0, cumsum(theta_lower - theta_upper))}
#' @return the rank's category probabilities (same length as \code{m})
#' @noRd
iso_row_probs <- function(m, d) {
  pos <- m > 0
  # A rank with no weight gives an all-zero row.
  if (!any(pos)) {
    return(rep(0, length(m)))
  }
  dmin <- min(d[pos])
  dsh <- d - dmin
  total <- sum(m[pos])
  m0 <- sum(m[pos & dsh == 0])
  if (m0 >= total) {
    # every surviving category shares the same offset: u* = total exactly
    u <- total
  } else {
    f_at <- function(u) {
      return(sum(m[pos] / (u + dsh[pos])) - 1)
    }
    t_lo <- log(m0)
    t_hi <- log(total)
    t <- t_hi
    for (k in 1:200) {
      u <- exp(t)
      f <- f_at(u)
      if (f > 0) {
        t_lo <- t
      } else {
        t_hi <- t
      }
      if (abs(f) <= 1e-14) {
        break
      }
      # -df/dt, kept positive so the step reads as an addition
      den <- u + dsh[pos]
      fp <- sum(m[pos] * u / (den * den))
      t_new <- if (fp > 0) t + f / fp else (t_lo + t_hi) / 2
      if (!is.finite(t_new) || t_new <= t_lo || t_new >= t_hi) {
        t_new <- (t_lo + t_hi) / 2
      }
      if (abs(t_new - t) <= 1e-15 * max(1, abs(t))) {
        t <- t_new
        break
      }
      t <- t_new
    }
    u <- exp(t)
  }
  out <- numeric(length(m))
  out[pos] <- m[pos] / (u + dsh[pos])
  return(out)
}

iso_build_pi <- function(Mcount, theta) {
  nrank <- nrow(Mcount)
  nc <- ncol(Mcount)
  P <- matrix(0, nrank, nc)
  for (r in 1:nrank) {
    if (r <= nrank - 1) {
      theta_lower <- theta[, r]
    } else {
      theta_lower <- rep(0, nc - 1)
    }
    if (r >= 2) {
      theta_upper <- theta[, r - 1]
    } else {
      theta_upper <- rep(0, nc - 1)
    }
    d <- c(0, cumsum(theta_lower - theta_upper))
    P[r, ] <- iso_row_probs(Mcount[r, ], d)
  }
  return(P)
}


#' @title Upper-cumulative (boundary) probabilities from category probabilities
#' @description
#' Internal helper. Converts a (nrank x ncat) category-probability matrix to the
#' (nrank x (ncat-1)) boundary matrix \eqn{S_{cb} = P(\ge \text{category } b+1)}.
#' @param P (nrank x ncat) category-probability matrix.
#' @return (nrank x (ncat-1)) boundary (upper-cumulative) matrix.
#' @noRd
iso_upper_cum <- function(P) {
  nrank <- nrow(P)
  nc <- ncol(P)
  S <- matrix(0, nrank, nc - 1)
  for (r in 1:nrank) {
    cum <- rev(cumsum(rev(P[r, ])))
    S[r, ] <- cum[-1]
  }
  return(S)
}


#' @title Order-restricted MAP for one ordinal item (Fenchel dual, R reference)
#' @description
#' Pure-R reference implementation of \code{iso_dual_map()}. The package uses
#' the C++ version (\code{src/isotonic_core.cpp}), which reproduces this
#' function's arithmetic operation for operation; this one is retained so the
#' test suite can check the two against each other and so the algorithm stays
#' readable. Do not call it from model code: it is 100-500x slower.
#'
#' Solves the stochastic-order-restricted multinomial MAP for a single item's
#' expected-count matrix by dual coordinate ascent (El Barmi & Dykstra 1994).
#' Each constraint (boundary \eqn{b}, adjacent rank pair \eqn{(c,c+1)}:
#' \eqn{S_{cb} \le S_{c+1,b}}) carries a non-negative dual multiplier, cyclically
#' raised until its boundary ties (or left at zero if slack). Nesting (each row a
#' valid distribution) is automatic from the rational form when counts are
#' positive, so no separate projection is needed. For a single boundary
#' (\eqn{ncat=2}) this reduces to weighted PAVA (Ayer et al. 1955).
#'
#' Convergence is judged by the per-sweep change in the log-likelihood, NOT by
#' the residual order violation: each sweep ties the last-processed constraint,
#' so the violation looks negligible while the multipliers are still drifting
#' toward the optimum over many sweeps (fully rank-reversed inputs need dozens of
#' sweeps to pool all boundaries).
#' @param Mcount (nrank x ncat) expected counts plus Dirichlet pseudocounts.
#' @param maxiter maximum dual sweeps.
#' @param tol stop when the relative change in the log-likelihood between sweeps
#'   falls below this.
#' @return (nrank x ncat) order-restricted category-probability matrix.
#' @noRd
iso_dual_map_ref <- function(Mcount, maxiter = 100, tol = 1e-7) {
  nrank <- nrow(Mcount)
  nc <- ncol(Mcount)
  theta <- matrix(0, nc - 1, nrank - 1)
  theta_prev <- matrix(0, nc - 1, nrank - 1) # 直前のスイープで求めた値
  emt <- 0
  old_loglik <- -Inf
  FLG <- TRUE
  while (FLG) {
    emt <- emt + 1
    for (b in 1:(nc - 1)) {
      for (r in 1:(nrank - 1)) {
        theta[b, r] <- 0
        S <- iso_upper_cum(iso_build_pi(Mcount, theta))
        if (S[r, b] - S[r + 1, b] > 1e-12) {
          # 前回のスイープの値から区間を張る(C++ 側の注記参照)。theta = 0 で
          # 制約が満たされているかの判定は上に残す。相補性条件そのものなので。
          lo <- 0
          hi <- 1
          warm <- theta_prev[b, r]
          if (warm > 0) {
            theta[b, r] <- warm
            S <- iso_upper_cum(iso_build_pi(Mcount, theta))
            if (S[r, b] - S[r + 1, b] > 0) {
              lo <- warm
              hi <- warm * 2
            } else {
              hi <- warm
            }
          }
          theta[b, r] <- hi
          S <- iso_upper_cum(iso_build_pi(Mcount, theta))
          while (S[r, b] - S[r + 1, b] > 0 && hi < 1e8) {
            hi <- hi * 2
            theta[b, r] <- hi
            S <- iso_upper_cum(iso_build_pi(Mcount, theta))
          }
          # Illinois 法(挟み撃ちの改良)。g(theta) = S[r,b] - S[r+1,b] は
          # theta について単調減少で、上の倍々探索で符号の異なる2点が既に
          # 手に入っている。区間を保持したまま線形補間で詰めるので二分より
          # 速く、しかも区間外へ出ない。同じ端が2回続けて残ったらその側の
          # 関数値を半分にする —— これが素の挟み撃ちの「片側だけ動いて
          # 収束が遅くなる」欠点を消す。
          g_at <- function(x) {
            theta[b, r] <<- x
            S <- iso_upper_cum(iso_build_pi(Mcount, theta))
            return(S[r, b] - S[r + 1, b])
          }
          g_lo <- g_at(lo)
          g_hi <- g_at(hi)
          # 残差が最小だった点を覚えておき，最後にそれを採る。区間の中点を
          # 返してはいけない: 残差で打ち切ったとき根は片端にあり，中点はそこ
          # から離れている。二値のとき重み付き PAVA と一致するという理論値
          # 検査がこれを拾った。
          root <- if (abs(g_lo) <= abs(g_hi)) lo else hi
          best <- min(abs(g_lo), abs(g_hi))
          side <- 0L
          # 区間幅の停止条件は相対で取り、中点が端に丸められたら打ち切る。
          # 空ランクでは端が 1e8 に達し、その近傍の double の刻み幅(約3e-8)は
          # 絶対条件より粗いので、絶対条件のままだと無限ループになる
          # (C++ 側の注記参照)。
          while (hi - lo > 1e-12 * max(1, abs(hi)) && best > 1e-14) {
            mid <- (lo * g_hi - hi * g_lo) / (g_hi - g_lo)
            if (!is.finite(mid) || mid <= lo || mid >= hi) {
              mid <- (lo + hi) / 2
            }
            if (mid <= lo || mid >= hi) break # 表現できる中点がもう無い
            g_mid <- g_at(mid)
            if (abs(g_mid) < best) {
              best <- abs(g_mid)
              root <- mid
            }
            if (g_mid > 0) {
              lo <- mid
              g_lo <- g_mid
              if (side == 1L) g_hi <- g_hi / 2
              side <- 1L
            } else {
              hi <- mid
              g_hi <- g_mid
              if (side == -1L) g_lo <- g_lo / 2
              side <- -1L
            }
          }
          theta[b, r] <- root
          theta_prev[b, r] <- root
        }
      }
    }
    # Convergence on the log-likelihood drift, not the residual violation.
    loglik <- sum(Mcount * log(pmax(iso_build_pi(Mcount, theta), 1e-300)))
    if (abs(loglik - old_loglik) <= tol * (abs(loglik) + tol)) {
      FLG <- FALSE
    }
    old_loglik <- loglik
    if (emt >= maxiter) {
      FLG <- FALSE
    }
  }
  return(iso_build_pi(Mcount, theta))
}


#' @title Order-restricted MAP for one ordinal item (Fenchel dual)
#' @description
#' Dual coordinate ascent for the stochastic-order-restricted multinomial MAP
#' (El Barmi & Dykstra 1994). This is the entry point used by
#' \code{LRA.ordinal()} and \code{Biclustering.ordinal()}; the work is done in
#' C++ (\code{src/isotonic_core.cpp}). The C++ routine follows the same
#' arithmetic as the R reference \code{iso_dual_map_ref()} and additionally
#' exploits the fact that raising one multiplier \eqn{\theta_{cq}} only changes
#' ranks \eqn{c} and \eqn{c+1}, so the inner bisection rebuilds two rows
#' instead of all of them. Results are identical; the speedup is roughly
#' 100x (small tables) to 500x (e.g. 20 ranks x 6 categories).
#'
#' Convergence is judged by the per-sweep change in the log-likelihood, NOT by
#' the residual order violation: each sweep ties the last-processed constraint,
#' so the violation looks negligible while the multipliers are still drifting
#' toward the optimum over many sweeps (fully rank-reversed inputs need dozens of
#' sweeps to pool all boundaries).
#' @param Mcount (nrank x ncat) expected counts plus Dirichlet pseudocounts.
#' @param maxiter maximum dual sweeps.
#' @param tol stop when the relative change in the log-likelihood between sweeps
#'   falls below this.
#' @return (nrank x ncat) order-restricted category-probability matrix.
#' @noRd
iso_dual_map <- function(Mcount, maxiter = 100, tol = 1e-7) {
  fit <- iso_dual_map_cpp(Mcount, maxiter = maxiter, tol = tol, fast = TRUE)
  return(fit$P)
}
