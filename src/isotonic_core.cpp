// C++ implementation of the order-restricted M-step (Fenchel dual).
// The core of iso_dual_map in R/00_isotonic_CORE.R. It follows the same steps
// as the R version (iso_dual_map_ref) and, when a theta moves, rebuilds only
// the two rows that change.
#include <Rcpp.h>
using namespace Rcpp;

// R's sum()/cumsum() accumulate in long double (LDOUBLE in src/main/summary.c),
// and so does this code. Since the root finding uses the values themselves, a
// different accumulation shows up as a different path, so it has to match to
// keep the answers close to the R version. Quotients are formed in double, as
// in R, before accumulating.
// (Agreement to the last bit can no longer be expected: in a value-driven root
//  finder a rounding difference can become a branching difference.
//  test-isotonic-core.R compares with a tolerance.)
//
// The denominators are formed on the fly rather than stored. This function is
// called close to 20,000 times per solver call, and allocating a work vector
// eats up what the fewer iterations save (measured: 2.46s with the
// allocation, against the timing below without it).

// Build the single row of rank r from the rational form
// pi_q = M[r,q] / (lambda + d[q]).
//
// The choice of variable matters more than the root finder. The details are in
// the note on iso_row_probs() on the R side; three points:
//  (1) Drop categories with zero count. They carry no information on lambda,
//      but if one of them holds the minimum of d it drags the lower end of the
//      domain far away from the root.
//  (2) With u = lambda + dmin the bracket comes in closed form:
//      1 = sum M/(u+d') >= m0/u gives u >= m0 (m0 = total count of the
//      categories with d' = 0), and d' >= 0 gives u <= sum M.
//  (3) The two ends can be 13 orders of magnitude apart. In u, bisection takes
//      80 steps and Newton overshoots the root when it starts from the right
//      end. In t = log u the bracket is about 30 wide.
// P is also built from u + d' directly; writing lambda + d loses digits when
// adding back.
static inline void build_row(const NumericMatrix& M, int r,
                             const std::vector<double>& d,
                             NumericMatrix& P) {
  const int nc = M.ncol();
  double dmin = R_PosInf;
  long double tot = 0.0L;
  int npos = 0;
  for (int q = 0; q < nc; ++q) {
    if (M(r, q) > 0.0) {
      ++npos; tot += M(r, q);
      if (d[q] < dmin) dmin = d[q];
    }
  }
  if (npos == 0) { // a row with no weight
    for (int q = 0; q < nc; ++q) P(r, q) = 0.0;
    return;
  }
  const double total = (double)tot;
  long double m0l = 0.0L;
  for (int q = 0; q < nc; ++q)
    if (M(r, q) > 0.0 && d[q] == dmin) m0l += M(r, q);
  const double m0 = (double)m0l;

  double u;
  if (m0 >= total) {
    u = total; // every surviving category has d' = 0
  } else {
    double t_lo = std::log(m0), t_hi = std::log(total), t = t_hi;
    for (int k = 0; k < 200; ++k) {
      const double uu = std::exp(t);
      long double s1 = 0.0L;
      for (int q = 0; q < nc; ++q)
        if (M(r, q) > 0.0) s1 += (double)(M(r, q) / (uu + (d[q] - dmin)));
      const double f = (double)s1 - 1.0;
      if (f > 0.0) t_lo = t; else t_hi = t;
      if (std::fabs(f) <= 1e-14) break;
      long double s2 = 0.0L;
      for (int q = 0; q < nc; ++q) {
        if (M(r, q) <= 0.0) continue;
        const double dq = uu + (d[q] - dmin);
        s2 += (double)(M(r, q) * uu / (dq * dq));
      }
      const double fp = (double)s2; // -df/dt, kept positive
      double t_new = (fp > 0.0) ? t + f / fp : (t_lo + t_hi) / 2.0;
      if (!R_finite(t_new) || t_new <= t_lo || t_new >= t_hi) {
        t_new = (t_lo + t_hi) / 2.0;
      }
      if (std::fabs(t_new - t) <= 1e-15 * std::max(1.0, std::fabs(t))) {
        t = t_new;
        break;
      }
      t = t_new;
    }
    u = std::exp(t);
  }
  for (int q = 0; q < nc; ++q)
    P(r, q) = (M(r, q) > 0.0) ? M(r, q) / (u + (d[q] - dmin)) : 0.0;
}

// Build d for rank r from theta ((nc-1) x (nrank-1)).
// The R side uses c(0, cumsum(theta_lower - theta_upper)). cumsum accumulates
// in long double, and so does this (for the same reason as upper_at).
static inline void make_d(const NumericMatrix& theta, int r, int nrank, int nc,
                          std::vector<double>& d) {
  d[0] = 0.0;
  long double acc = 0.0L;
  for (int b = 0; b < nc - 1; ++b) {
    const double tl = (r <= nrank - 2) ? theta(b, r) : 0.0;      // pair where r is the lower rank
    const double tu = (r >= 1)         ? theta(b, r - 1) : 0.0;  // pair where r is the upper rank
    acc += (double)(tl - tu);
    d[b + 1] = (double)acc;
  }
}

// [[Rcpp::export]]
NumericMatrix iso_build_pi_cpp(NumericMatrix Mcount, NumericMatrix theta) {
  const int nrank = Mcount.nrow(), nc = Mcount.ncol();
  NumericMatrix P(nrank, nc);
  std::vector<double> d(nc);
  for (int r = 0; r < nrank; ++r) { make_d(theta, r, nrank, nc, d); build_row(Mcount, r, d, P); }
  return P;
}

// [[Rcpp::export]]
NumericMatrix iso_upper_cum_cpp(NumericMatrix P) {
  const int nrank = P.nrow(), nc = P.ncol();
  NumericMatrix S(nrank, nc - 1);
  for (int r = 0; r < nrank; ++r) {
    double cum = 0.0;
    for (int q = nc - 1; q >= 1; --q) { cum += P(r, q); S(r, q - 1) = cum; }
  }
  return S;
}

// Get a single S[r,b] (the b-th upper cumulative sum of row r).
//
// The R side uses rev(cumsum(rev(P[r, ]))). cumsum accumulates in long double
// too, so both the order of the additions (from the end down to b+1) and the
// precision of the accumulation have to match. In the bisection days only the
// sign of this value was used, so it made no difference; now that the outer
// search interpolates linearly, the value itself sets the path.
static inline double upper_at(const NumericMatrix& P, int r, int b) {
  const int nc = P.ncol();
  long double cum = 0.0L;
  for (int q = nc - 1; q >= b + 1; --q) cum += P(r, q);
  return (double)cum;
}

// [[Rcpp::export]]
List iso_dual_map_cpp(NumericMatrix Mcount, int maxiter = 100, double tol = 1e-7,
                      double viol_tol = 1e-6, bool fast = true) {
  const int nrank = Mcount.nrow(), nc = Mcount.ncol();
  NumericMatrix theta(nc - 1, nrank - 1);
  NumericMatrix theta_prev(nc - 1, nrank - 1); // values from the previous sweep
  NumericMatrix P(nrank, nc);
  std::vector<double> d(nc);

  // rebuild every row
  auto rebuild_all = [&]() {
    for (int r = 0; r < nrank; ++r) { make_d(theta, r, nrank, nc, d); build_row(Mcount, r, d, P); }
  };
  // moving theta(b,r) affects rows r and r+1 only
  auto rebuild_pair = [&](int r) {
    make_d(theta, r,     nrank, nc, d); build_row(Mcount, r,     d, P);
    make_d(theta, r + 1, nrank, nc, d); build_row(Mcount, r + 1, d, P);
  };
  auto refresh = [&](int r) { if (fast) rebuild_pair(r); else rebuild_all(); };

  rebuild_all();
  double old_loglik = R_NegInf;
  int emt = 0;
  bool converged = false;

  while (true) {
    ++emt;
    for (int b = 0; b < nc - 1; ++b) {
      for (int r = 0; r < nrank - 1; ++r) {
        theta(b, r) = 0.0;
        refresh(r);
        if (upper_at(P, r, b) - upper_at(P, r + 1, b) > 1e-12) {
          // Bracket from the previous sweep's value. From the second sweep on it lies
          // right next to the solution, so the doubling from 0 (6.4 steps per theta,
          // measured) is mostly skipped. The check that theta = 0 already satisfies
          // the constraint stays above: it is the complementary slackness condition
          // itself, not something to skip for speed.
          double lo = 0.0, hi = 1.0;
          const double warm = theta_prev(b, r);
          if (warm > 0.0) {
            theta(b, r) = warm; refresh(r);
            if (upper_at(P, r, b) - upper_at(P, r + 1, b) > 0.0) {
              lo = warm; hi = warm * 2.0; // the root is to the right
            } else {
              hi = warm;                  // the root is to the left; lo = 0 stays
            }
          }
          theta(b, r) = hi; refresh(r);
          while (upper_at(P, r, b) - upper_at(P, r + 1, b) > 0.0 && hi < 1e8) {
            hi *= 2.0; theta(b, r) = hi; refresh(r);
          }
          // Illinois method (an improved regula falsi). g(theta) = S[r,b] - S[r+1,b]
          // decreases in theta, and the doubling search above gave two points of
          // opposite sign. Linear interpolation is faster than bisection, and keeping
          // the bracket means it never leaves it. When the same end survives twice in
          // a row its function value is halved -- this removes plain regula falsi's
          // flaw of moving one side only and slowing down.
          //
          // Why speed matters here: build_row is called close to 20,000 times per
          // solver call, and the iteration count of this search sets that number.
          auto g_at = [&](double x) {
            theta(b, r) = x; refresh(r);
            return upper_at(P, r, b) - upper_at(P, r + 1, b);
          };
          double g_lo = g_at(lo);
          double g_hi = g_at(hi);
          // Remember the point with the smallest residual and take it at the end
          // (see the note on the R side).
          double root = (std::fabs(g_lo) <= std::fabs(g_hi)) ? lo : hi;
          double best = std::min(std::fabs(g_lo), std::fabs(g_hi));
          int side = 0;
          // The bracket-width test is relative. For a rank with little weight the end
          // can reach 1e8, where the spacing of doubles (about 3e-8) is coarser than
          // an absolute tolerance of 1e-12. With the absolute test the bracket stops
          // shrinking, every interpolated point rounds to an end, and the loop never
          // exits.
          while (hi - lo > 1e-12 * std::max(1.0, std::fabs(hi)) && best > 1e-14) {
            double mid = (lo * g_hi - hi * g_lo) / (g_hi - g_lo);
            if (!R_finite(mid) || mid <= lo || mid >= hi) mid = (lo + hi) / 2.0;
            if (mid <= lo || mid >= hi) break; // no representable midpoint left
            const double g_mid = g_at(mid);
            if (std::fabs(g_mid) < best) { best = std::fabs(g_mid); root = mid; }
            if (g_mid > 0.0) {
              lo = mid; g_lo = g_mid;
              if (side == 1) g_hi = g_hi / 2.0;
              side = 1;
            } else {
              hi = mid; g_hi = g_mid;
              if (side == -1) g_lo = g_lo / 2.0;
              side = -1;
            }
          }
          theta(b, r) = root; refresh(r);
          theta_prev(b, r) = root;
        }
      }
    }
    rebuild_all();
    double loglik = 0.0;
    for (int r = 0; r < nrank; ++r)
      for (int q = 0; q < nc; ++q)
        loglik += Mcount(r, q) * std::log(std::max(P(r, q), 1e-300));
    // Convergence needs both sides of the KKT conditions. Near the optimum the
    // log-likelihood is flat to second order while the order violations are
    // still shrinking geometrically, so stopping on the likelihood alone ends
    // the sweep with violations around 1e-3 left. Primal feasibility is added
    // to the test.
    double viol = 0.0;
    for (int b = 0; b < nc - 1; ++b)
      for (int r = 0; r < nrank - 1; ++r) {
        const double v = upper_at(P, r, b) - upper_at(P, r + 1, b);
        if (v > viol) viol = v;
      }
    if (std::fabs(loglik - old_loglik) <= tol * (std::fabs(loglik) + tol) &&
        viol <= viol_tol) {
      converged = true; break;
    }
    old_loglik = loglik;
    if (emt >= maxiter) break;
  }
  rebuild_all();
  return List::create(_["P"] = P, _["theta"] = theta,
                      _["iter"] = emt, _["converged"] = converged);
}
