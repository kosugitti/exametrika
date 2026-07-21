// 順序制約つきM-step(Fenchel双対)のC++実装。
// R/00_isotonic_CORE.R の iso_dual_map の中核。R版(iso_dual_map_ref)と
// 演算順序まで一致し、内側の二分探索では変化する2行だけを再計算する。
#include <Rcpp.h>
using namespace Rcpp;

// ランク r の1行だけを有理形から作る。d は長さ nc。
static inline void build_row(const NumericMatrix& M, int r,
                             const std::vector<double>& d,
                             NumericMatrix& P) {
  const int nc = M.ncol();
  double dmin = d[0];
  for (int q = 1; q < nc; ++q) if (d[q] < dmin) dmin = d[q];
  double lo = -dmin + 1e-12;
  double hi = lo + 1.0;
  // 上限を倍々に広げる
  for (;;) {
    double s = 0.0;
    for (int q = 0; q < nc; ++q) s += M(r, q) / (hi + d[q]);
    if (s > 1.0) hi = lo + (hi - lo) * 2.0; else break;
  }
  // 区間を詰める
  while (hi - lo > 1e-10) {
    double mid = (lo + hi) / 2.0;
    double s = 0.0;
    for (int q = 0; q < nc; ++q) s += M(r, q) / (mid + d[q]);
    if (s > 1.0) lo = mid; else hi = mid;
  }
  const double lam = (lo + hi) / 2.0;
  for (int q = 0; q < nc; ++q) P(r, q) = M(r, q) / (lam + d[q]);
}

// theta((nc-1) x (nrank-1)) からランク r の d を作る
static inline void make_d(const NumericMatrix& theta, int r, int nrank, int nc,
                          std::vector<double>& d) {
  d[0] = 0.0;
  double acc = 0.0;
  for (int b = 0; b < nc - 1; ++b) {
    const double tl = (r <= nrank - 2) ? theta(b, r) : 0.0;      // 自分が下側の対
    const double tu = (r >= 1)         ? theta(b, r - 1) : 0.0;  // 自分が上側の対
    acc += tl - tu;
    d[b + 1] = acc;
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

// S[r,b] を1つだけ得る（行 r の上側累積の b 番目）
static inline double upper_at(const NumericMatrix& P, int r, int b) {
  const int nc = P.ncol();
  double cum = 0.0;
  for (int q = nc - 1; q >= b + 1; --q) cum += P(r, q);
  return cum;
}

// [[Rcpp::export]]
List iso_dual_map_cpp(NumericMatrix Mcount, int maxiter = 100, double tol = 1e-7,
                      bool fast = true) {
  const int nrank = Mcount.nrow(), nc = Mcount.ncol();
  NumericMatrix theta(nc - 1, nrank - 1);
  NumericMatrix P(nrank, nc);
  std::vector<double> d(nc);

  // 全行を作り直す
  auto rebuild_all = [&]() {
    for (int r = 0; r < nrank; ++r) { make_d(theta, r, nrank, nc, d); build_row(Mcount, r, d, P); }
  };
  // theta(b,r) が動いたとき影響を受けるのは行 r と r+1 だけ
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
          double lo = 0.0, hi = 1.0;
          theta(b, r) = hi; refresh(r);
          while (upper_at(P, r, b) - upper_at(P, r + 1, b) > 0.0 && hi < 1e8) {
            hi *= 2.0; theta(b, r) = hi; refresh(r);
          }
          while (hi - lo > 1e-10) {
            const double mid = (lo + hi) / 2.0;
            theta(b, r) = mid; refresh(r);
            if (upper_at(P, r, b) - upper_at(P, r + 1, b) > 0.0) lo = mid; else hi = mid;
          }
          theta(b, r) = (lo + hi) / 2.0; refresh(r);
        }
      }
    }
    rebuild_all();
    double loglik = 0.0;
    for (int r = 0; r < nrank; ++r)
      for (int q = 0; q < nc; ++q)
        loglik += Mcount(r, q) * std::log(std::max(P(r, q), 1e-300));
    if (std::fabs(loglik - old_loglik) <= tol * (std::fabs(loglik) + tol)) {
      converged = true; break;
    }
    old_loglik = loglik;
    if (emt >= maxiter) break;
  }
  rebuild_all();
  return List::create(_["P"] = P, _["theta"] = theta,
                      _["iter"] = emt, _["converged"] = converged);
}
