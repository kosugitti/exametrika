// 順序制約つきM-step(Fenchel双対)のC++実装。
// R/00_isotonic_CORE.R の iso_dual_map の中核。R版(iso_dual_map_ref)と同じ
// 手順を踏み、theta が動いたときは変化する2行だけを再計算する。
#include <Rcpp.h>
using namespace Rcpp;

// R の sum()/cumsum() は長倍精度で累算する(src/main/summary.c の LDOUBLE)。
// ここも合わせる。求根が値そのものを使うようになって以降、累算方式の差は
// 経路の差になって表に出るので、R 版と近い答えを保つには揃えておく必要が
// ある。商は R と同じく double で作ってから累算する。
// (両者が最終ビットまで一致することはもう期待できない。値で駆動する求根は
//  丸めの違いが分岐の違いになりうるため。test-isotonic-core.R は許容誤差で
//  比べている。)
//
// 分母は保存せずその場で作る。この関数は1ソルバ呼び出しあたり2万回近く呼ば
// れるので、作業用ベクタを確保すると反復を減らして浮いた分をヒープ確保で
// 食い潰す(実測: 確保ありだと 2.46s、なしだと下の測定のとおり)。

// ランク r の1行だけを有理形 pi_q = M[r,q]/(lambda + d[q]) から作る。
//
// 求根そのものより、変数の取り方のほうが効く。詳細は R 側 iso_row_probs() の
// 注記にあるが、要点は3つ。
//  (1) 度数 0 のカテゴリを外す。lambda に情報を与えないのに、そこが d の最小を
//      取ると定義域の下端が根から遠く離れた場所へ引きずられる。
//  (2) u = lambda + dmin と置くと区間が閉じた形で出る。1 = sum M/(u+d') >= m0/u
//      より u >= m0 (m0 は d' = 0 のカテゴリの度数和)、d' >= 0 より u <= sum M。
//  (3) その両端は13桁離れることがある。u の空間では二分法に80回、ニュートン法は
//      右端から踏み出すと根を飛び越す。t = log u なら区間幅は30程度に収まる。
// P も u + d' から直に作る。lambda + d と書くと足し戻しで桁が落ちる。
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
  if (npos == 0) { // 重みのない行
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
    u = total; // 生き残ったカテゴリの d' が全て 0
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
      const double fp = (double)s2; // -df/dt、正で持つ
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
                      double viol_tol = 1e-6, bool fast = true) {
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
          // 区間幅の判定は相対で取る。重みの薄いランクでは端が 1e8 まで伸びる
          // ことがあり、その近傍の double の刻み幅(約 3e-8)は絶対条件 1e-12 より
          // 粗い。絶対条件のままだと区間がそれ以上縮まず、補間点が必ず端に丸め
          // られて抜けられなくなる。
          while (hi - lo > 1e-12 * std::max(1.0, std::fabs(hi)) && best > 1e-14) {
            double mid = (lo * g_hi - hi * g_lo) / (g_hi - g_lo);
            if (!R_finite(mid) || mid <= lo || mid >= hi) mid = (lo + hi) / 2.0;
            if (mid <= lo || mid >= hi) break; // 表現できる中点がもう無い
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
    // 収束判定には KKT の両側が要る。対数尤度は最適点の近くで2次的に平ら
    // になる一方、順序制約の違反はまだ幾何級数的に減っている途中なので、
    // 尤度だけで止めると違反が 1e-3 程度残ったところで掃引が終わる。
    // 主問題の実行可能性も条件に加える。
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
