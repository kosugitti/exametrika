// 順序制約つきM-step(Fenchel双対)のC++実装。
// R/00_isotonic_CORE.R の iso_dual_map の中核。R版(iso_dual_map_ref)と
// 演算順序まで一致し、内側の二分探索では変化する2行だけを再計算する。
#include <Rcpp.h>
using namespace Rcpp;

// R の sum() は長倍精度で累算する(src/main/summary.c の LDOUBLE)。二分探索は
// 「和が1を超えるか」という真偽値しか使わないので累算方式の差は結果に出な
// かったが、ニュートン法は和の値そのものを使うので、double で累算すると R 版
// と最終ビットが食い違う。iso_dual_map_ref() との expect_identical を保つため
// ここも長倍精度で足す。商は R と同じく double で作ってから累算する。
//
// 分母は保存せずその場で作る。この関数は1ソルバ呼び出しあたり2万回近く呼ば
// れるので、作業用ベクタを確保すると反復を減らして浮いた分をヒープ確保で
// 食い潰す(実測: 確保ありだと 2.46s、なしだと下の測定のとおり)。

// ランク r の1行だけを有理形から作る。d は長さ nc。
//
// sum_q M[r,q]/(lambda + d[q]) = 1 を満たす lambda を安全化ニュートン法で解く。
// f は定義域 lambda > -min(d) で狭義単調減少かつ凸(f' < 0, f'' > 0)なので、
// 凸関数の接線が関数の下側を通ることから、f < 0 の点から踏み出した1歩は根の
// 左側に着地し、以後は左から単調に近づく。行き過ぎない。
// 区間も閉じた形で取れる: 左端 -min(d) で f は +無限大、右端 sum(M) - min(d)
// では分母が全て sum(M) 以上になるので f <= 0。倍々に広げる探索は要らない。
static inline void build_row(const NumericMatrix& M, int r,
                             const std::vector<double>& d,
                             NumericMatrix& P) {
  const int nc = M.ncol();
  double dmin = d[0];
  for (int q = 1; q < nc; ++q) if (d[q] < dmin) dmin = d[q];
  long double tot = 0.0L;
  for (int q = 0; q < nc; ++q) tot += M(r, q);
  const double total = (double)tot;

  double lam;
  if (total <= 0.0) {
    // 重みのない行。lambda によらず P は全て 0 になる
    lam = -dmin + 1.0;
  } else {
    double lo = -dmin;
    double hi = total - dmin;
    lam = hi;
    for (int k = 0; k < 60; ++k) {
      long double s1 = 0.0L;
      for (int q = 0; q < nc; ++q) s1 += (double)(M(r, q) / (lam + d[q]));
      const double f = (double)s1 - 1.0;
      if (f > 0.0) lo = lam; else hi = lam;
      if (std::fabs(f) <= 1e-14) break;
      long double s2 = 0.0L;
      for (int q = 0; q < nc; ++q) {
        const double dq = lam + d[q];
        s2 += (double)(M(r, q) / (dq * dq));
      }
      const double fp = (double)s2; // -f'(lam)、正で持つ
      double lam_new = lam + f / fp;
      if (!R_finite(lam_new) || lam_new <= lo || lam_new >= hi) {
        lam_new = (lo + hi) / 2.0;
      }
      if (std::fabs(lam_new - lam) <= 1e-15 * std::max(1.0, std::fabs(lam))) {
        lam = lam_new;
        break;
      }
      lam = lam_new;
    }
  }
  for (int q = 0; q < nc; ++q) P(r, q) = M(r, q) / (lam + d[q]);
}

// theta((nc-1) x (nrank-1)) からランク r の d を作る
// R 側は c(0, cumsum(theta_lower - theta_upper)) で作る。cumsum は長倍精度で
// 累算するので、ここも合わせる(upper_at と同じ理由)。
static inline void make_d(const NumericMatrix& theta, int r, int nrank, int nc,
                          std::vector<double>& d) {
  d[0] = 0.0;
  long double acc = 0.0L;
  for (int b = 0; b < nc - 1; ++b) {
    const double tl = (r <= nrank - 2) ? theta(b, r) : 0.0;      // 自分が下側の対
    const double tu = (r >= 1)         ? theta(b, r - 1) : 0.0;  // 自分が上側の対
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

// S[r,b] を1つだけ得る（行 r の上側累積の b 番目）
//
// R 側は rev(cumsum(rev(P[r, ]))) で作る。cumsum も長倍精度で累算するので、
// 加算の順序(末尾から b+1 まで)だけでなく累算の精度も合わせる必要がある。
// 二分探索の時代はこの値の符号しか使わなかったので差が出なかったが、外側の
// 探索が線形補間になった以上、値そのものが経路を決める。
static inline double upper_at(const NumericMatrix& P, int r, int b) {
  const int nc = P.ncol();
  long double cum = 0.0L;
  for (int q = nc - 1; q >= b + 1; --q) cum += P(r, q);
  return (double)cum;
}

// [[Rcpp::export]]
List iso_dual_map_cpp(NumericMatrix Mcount, int maxiter = 100, double tol = 1e-7,
                      bool fast = true) {
  const int nrank = Mcount.nrow(), nc = Mcount.ncol();
  NumericMatrix theta(nc - 1, nrank - 1);
  NumericMatrix theta_prev(nc - 1, nrank - 1); // 直前のスイープで求めた値
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
          // 前回のスイープで求めた値から区間を張る。2周目以降はそれが解の
          // すぐ近くにあるので、0 から倍々に広げ直す段階(実測で theta 1個
          // あたり6.4回)がほとんど省ける。theta = 0 で制約が満たされている
          // かの判定は上に残してある。あれは相補性条件そのものなので、
          // 速度のために飛ばしてよい類のものではない。
          double lo = 0.0, hi = 1.0;
          const double warm = theta_prev(b, r);
          if (warm > 0.0) {
            theta(b, r) = warm; refresh(r);
            if (upper_at(P, r, b) - upper_at(P, r + 1, b) > 0.0) {
              lo = warm; hi = warm * 2.0; // 根は右側
            } else {
              hi = warm;                  // 根は左側。lo = 0 のままでよい
            }
          }
          theta(b, r) = hi; refresh(r);
          while (upper_at(P, r, b) - upper_at(P, r + 1, b) > 0.0 && hi < 1e8) {
            hi *= 2.0; theta(b, r) = hi; refresh(r);
          }
          // Illinois 法(挟み撃ちの改良)。g(theta) = S[r,b] - S[r+1,b] は theta
          // について単調減少で、上の倍々探索で符号の異なる2点が手に入って
          // いる。線形補間で詰めるので二分より速く、区間を保持するので外へ
          // 出ない。同じ端が2回続けて残ったらその側の関数値を半分にする ——
          // 素の挟み撃ちが片側だけ動いて遅くなる欠点を消すため。
          //
          // ここを速くする意味: build_row は1ソルバ呼び出しあたり2万回近く
          // 呼ばれ、その回数を決めているのがこの探索の反復数である。
          auto g_at = [&](double x) {
            theta(b, r) = x; refresh(r);
            return upper_at(P, r, b) - upper_at(P, r + 1, b);
          };
          double g_lo = g_at(lo);
          double g_hi = g_at(hi);
          // 残差が最小だった点を覚えて最後にそれを採る(R 側の注記参照)
          double root = (std::fabs(g_lo) <= std::fabs(g_hi)) ? lo : hi;
          double best = std::min(std::fabs(g_lo), std::fabs(g_hi));
          int side = 0;
          while (hi - lo > 1e-12 && best > 1e-14) {
            double mid = (lo * g_hi - hi * g_lo) / (g_hi - g_lo);
            if (!R_finite(mid) || mid <= lo || mid >= hi) mid = (lo + hi) / 2.0;
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
