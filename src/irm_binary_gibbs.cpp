// Collapsed Gibbs sampler for the binary Infinite Relational Model used by
// Biclustering_IRM.binary().
//
// This is a line-by-line translation of the R reference
// irm_binary_gibbs_r() in R/07_IRM.R.  The arithmetic is carried out in the
// same order as the R code (including the log-sum loops that stand in for
// lgamma and the tiny constant added inside every log), and every random
// draw is delegated to the R functions sample.int() and rmultinom() through
// the `draw` callback, so that with the same set.seed() the two
// implementations give bit-identical results.
//
// Two features of the Mathematica original are kept as they are:
//   * the first and last classes (minimum and maximum number-right score)
//     are never resampled;
//   * fields can only disappear; there is no "new field" option.

#include <Rcpp.h>
#include <vector>
#include <cmath>

using namespace Rcpp;

namespace {

inline int idx2(int i, int j, int d1) { return i + d1 * j; }

// R's sum(): accumulate in long double, return double.
inline double r_sum(const std::vector<double>& x) {
  long double s = 0.0;
  for (std::size_t i = 0; i < x.size(); ++i) s += x[i];
  return static_cast<double>(s);
}

// log-probabilities -> probabilities exactly as the R code does it:
// maxtab <- max(ptab); exptab <- exp(ptab - maxtab); ptab <- exptab / sum(exptab)
inline std::vector<double> to_prob(const std::vector<double>& logp) {
  double m = logp[0];
  for (std::size_t i = 1; i < logp.size(); ++i) if (logp[i] > m) m = logp[i];
  std::vector<double> e(logp.size());
  for (std::size_t i = 0; i < logp.size(); ++i) e[i] = std::exp(logp[i] - m);
  double s = r_sum(e);
  for (std::size_t i = 0; i < e.size(); ++i) e[i] = e[i] / s;
  return e;
}

// sum_{s=0}^{ct} log(x + gamp - 1 - s + konst), accumulated in R's order
inline void log_run(double& acc, double x, double ct, double gamp, double konst) {
  if (ct >= 0) {
    for (double s = 0; s <= ct; s += 1.0) {
      acc = acc + std::log(x + gamp - 1 - s + konst);
    }
  }
}

} // namespace

//' Gibbs phase of the binary IRM
//'
//' @param U response matrix with missing cells already zeroed (U * Z)
//' @param Z missing indicator matrix
//' @param cls01_init initial class membership (nobs x ncls)
//' @param fld01_init initial field membership (nitems x nfld)
//' @param gamma_c,gamma_f CRP concentration parameters
//' @param gamp beta prior parameter (1 in the caller)
//' @param konst the small constant added inside the logs (1e-100)
//' @param maxiter,stable_limit stopping rule
//' @param verbose print progress
//' @param draw R closure: given a probability vector returns the 1-based
//'   index drawn by rmultinom() after the caller's rounding
//' @param sample_int base::sample.int
//' @return list(cls01, fld01, ncls, nfld, iter)
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List irm_binary_gibbs_cpp(NumericMatrix U, NumericMatrix Z,
                          NumericMatrix cls01_init, NumericMatrix fld01_init,
                          double gamma_c, double gamma_f, double gamp, double konst,
                          int maxiter, int stable_limit, bool verbose,
                          Function draw, Function sample_int) {
  const int nobs = U.nrow();
  const int nitems = U.ncol();

  // Z * (1 - U): incorrect-response indicator
  std::vector<double> Umat(static_cast<std::size_t>(nobs) * nitems);
  std::vector<double> Wmat(static_cast<std::size_t>(nobs) * nitems);
  for (int j = 0; j < nitems; ++j) {
    for (int i = 0; i < nobs; ++i) {
      Umat[idx2(i, j, nobs)] = U(i, j);
      Wmat[idx2(i, j, nobs)] = Z(i, j) * (1 - U(i, j));
    }
  }

  // class assignment (1-based) and sizes
  int ncls = cls01_init.ncol();
  std::vector<int> cls(nobs, 0);
  std::vector<double> Nc(ncls, 0.0);
  for (int i = 0; i < nobs; ++i) {
    for (int c = 0; c < ncls; ++c) {
      if (cls01_init(i, c) == 1.0) { cls[i] = c + 1; Nc[c] += 1; break; }
    }
  }
  // field assignment (1-based) and sizes
  int nfld = fld01_init.ncol();
  std::vector<int> fld(nitems, 0);
  std::vector<double> Nf(nfld, 0.0);
  for (int j = 0; j < nitems; ++j) {
    for (int f = 0; f < nfld; ++f) {
      if (fld01_init(j, f) == 1.0) { fld[j] = f + 1; Nf[f] += 1; break; }
    }
  }

  // Cif = U %*% fld01, Fif = (Z*(1-U)) %*% fld01   (nobs x nfld)
  std::vector<double> Cif, Fif;
  auto compute_Cif = [&]() {
    Cif.assign(static_cast<std::size_t>(nobs) * nfld, 0.0);
    Fif.assign(static_cast<std::size_t>(nobs) * nfld, 0.0);
    for (int j = 0; j < nitems; ++j) {
      const int f = fld[j] - 1;
      for (int i = 0; i < nobs; ++i) {
        Cif[idx2(i, f, nobs)] += Umat[idx2(i, j, nobs)];
        Fif[idx2(i, f, nobs)] += Wmat[idx2(i, j, nobs)];
      }
    }
  };
  // Cjc = t(U) %*% cls01, Fjc = t(Z*(1-U)) %*% cls01   (nitems x ncls)
  std::vector<double> Cjc, Fjc;
  auto compute_Cjc = [&]() {
    Cjc.assign(static_cast<std::size_t>(nitems) * ncls, 0.0);
    Fjc.assign(static_cast<std::size_t>(nitems) * ncls, 0.0);
    for (int i = 0; i < nobs; ++i) {
      const int c = cls[i] - 1;
      for (int j = 0; j < nitems; ++j) {
        Cjc[idx2(j, c, nitems)] += Umat[idx2(i, j, nobs)];
        Fjc[idx2(j, c, nitems)] += Wmat[idx2(i, j, nobs)];
      }
    }
  };
  // Ccf = t(cls01) %*% U %*% fld01 (ncls x nfld) built from Cif, which is
  // exact because every entry is a small integer
  std::vector<double> Ccf, Fcf;
  auto compute_Ccf = [&]() {
    Ccf.assign(static_cast<std::size_t>(ncls) * nfld, 0.0);
    Fcf.assign(static_cast<std::size_t>(ncls) * nfld, 0.0);
    for (int i = 0; i < nobs; ++i) {
      const int c = cls[i] - 1;
      if (c < 0) continue;
      for (int f = 0; f < nfld; ++f) {
        Ccf[idx2(c, f, ncls)] += Cif[idx2(i, f, nobs)];
        Fcf[idx2(c, f, ncls)] += Fif[idx2(i, f, nobs)];
      }
    }
  };

  compute_Cif();
  compute_Ccf();

  int limit_count = 0;
  int iter = 1;
  std::vector<double> CcfPlus, FcfPlus, logp, prob;

  bool IRM_FLG = true;
  while (IRM_FLG) {
    // ---- Gibbs sampler i (respondents) ------------------------------
    compute_Cif();
    IntegerVector iRand = sample_int(nobs);

    for (int loop = 0; loop < nobs; ++loop) {
      const int target = iRand[loop] - 1;
      const int c0 = cls[target];
      if (!(c0 >= 2 && c0 <= ncls - 1)) continue;

      // delete selected member
      Nc[c0 - 1] -= 1;
      for (int f = 0; f < nfld; ++f) {
        Ccf[idx2(c0 - 1, f, ncls)] -= Cif[idx2(target, f, nobs)];
        Fcf[idx2(c0 - 1, f, ncls)] -= Fif[idx2(target, f, nobs)];
      }
      if (Nc[c0 - 1] == 0) {
        // the class disappeared: drop its row and renumber
        const int d = c0 - 1;
        std::vector<double> Ccf2(static_cast<std::size_t>(ncls - 1) * nfld);
        std::vector<double> Fcf2(static_cast<std::size_t>(ncls - 1) * nfld);
        for (int f = 0; f < nfld; ++f) {
          int r2 = 0;
          for (int r = 0; r < ncls; ++r) {
            if (r == d) continue;
            Ccf2[idx2(r2, f, ncls - 1)] = Ccf[idx2(r, f, ncls)];
            Fcf2[idx2(r2, f, ncls - 1)] = Fcf[idx2(r, f, ncls)];
            ++r2;
          }
        }
        Ccf.swap(Ccf2);
        Fcf.swap(Fcf2);
        Nc.erase(Nc.begin() + d);
        ncls -= 1;
        for (int i = 0; i < nobs; ++i) {
          if (cls[i] == c0) cls[i] = 0;
          else if (cls[i] > c0) cls[i] -= 1;
        }
      }
      CcfPlus.resize(Ccf.size());
      FcfPlus.resize(Fcf.size());
      for (int f = 0; f < nfld; ++f) {
        for (int r = 0; r < ncls; ++r) {
          CcfPlus[idx2(r, f, ncls)] = Ccf[idx2(r, f, ncls)] + Cif[idx2(target, f, nobs)];
          FcfPlus[idx2(r, f, ncls)] = Fcf[idx2(r, f, ncls)] + Fif[idx2(target, f, nobs)];
        }
      }

      // Table choice: existing (middle) classes 2..ncls-1, then a new one
      logp.assign(ncls - 1, 0.0);
      for (int i = 2; i <= ncls - 1; ++i) {
        const double exist_tab_tmp = std::log((Nc[i - 1] / (nobs - 1 + gamma_c)) + konst);
        double Ctmp = 0, Ftmp = 0, den = 0;
        for (int j = 0; j < nfld; ++j) {
          const double cp = CcfPlus[idx2(i - 1, j, ncls)];
          const double fp = FcfPlus[idx2(i - 1, j, ncls)];
          const double cc = Ccf[idx2(i - 1, j, ncls)];
          const double fc = Fcf[idx2(i - 1, j, ncls)];
          log_run(Ctmp, cp, cp - cc - 1, gamp, konst);
          log_run(Ftmp, fp, fp - fc - 1, gamp, konst);
          const double ct3 = cp + fp - cc - fc - 1;
          if (ct3 >= 0) {
            for (double s = 0; s <= ct3; s += 1.0) {
              den = den + std::log(cp + fp + 2 * gamp - 1 - s + konst);
            }
          }
        }
        logp[i - 2] = exist_tab_tmp + Ctmp + Ftmp - den;
      }
      {
        const double new_tab_tmp = std::log((gamma_c / (nobs - 1 + gamma_c)) + konst);
        double num = 0, den = 0;
        for (int j = 0; j < nfld; ++j) {
          const double cif = Cif[idx2(target, j, nobs)];
          log_run(num, cif, cif - 1, gamp, konst);
          log_run(num, Nf[j] - cif, Nf[j] - cif - 1, gamp, konst);
          if (Nf[j] - 1 >= 0) {
            for (double s = 0; s <= Nf[j] - 1; s += 1.0) {
              den = den + std::log(Nf[j] + 2 * gamp - 1 - s + konst);
            }
          }
        }
        logp[ncls - 2] = num - den + new_tab_tmp;
      }
      prob = to_prob(logp);
      const int k = as<int>(draw(NumericVector(prob.begin(), prob.end())));

      if (k <= ncls - 2) {
        // joins existing class k + 1
        cls[target] = k + 1;
      } else {
        // opens a new class, inserted just before the last one
        ncls += 1;
        for (int i = 0; i < nobs; ++i) {
          if (cls[i] == ncls - 1) cls[i] = ncls;
        }
        cls[target] = ncls - 1;
        // insert an empty row for the new class just before the last one;
        // the member's counts are added once below
        {
          std::vector<double> Ccf2(static_cast<std::size_t>(ncls) * nfld, 0.0);
          std::vector<double> Fcf2(static_cast<std::size_t>(ncls) * nfld, 0.0);
          for (int f = 0; f < nfld; ++f) {
            for (int r = 0; r < ncls - 2; ++r) {
              Ccf2[idx2(r, f, ncls)] = Ccf[idx2(r, f, ncls - 1)];
              Fcf2[idx2(r, f, ncls)] = Fcf[idx2(r, f, ncls - 1)];
            }
            Ccf2[idx2(ncls - 1, f, ncls)] = Ccf[idx2(ncls - 2, f, ncls - 1)];
            Fcf2[idx2(ncls - 1, f, ncls)] = Fcf[idx2(ncls - 2, f, ncls - 1)];
          }
          Ccf.swap(Ccf2);
          Fcf.swap(Fcf2);
        }
      }
      Nc.assign(ncls, 0.0);
      for (int i = 0; i < nobs; ++i) if (cls[i] >= 1) Nc[cls[i] - 1] += 1;
      {
        const int c = cls[target] - 1;
        for (int f = 0; f < nfld; ++f) {
          Ccf[idx2(c, f, ncls)] += Cif[idx2(target, f, nobs)];
          Fcf[idx2(c, f, ncls)] += Fif[idx2(target, f, nobs)];
        }
      }
    }

    // ---- Gibbs sampler j (items) ------------------------------------
    std::vector<int> oldField = fld;
    compute_Cjc();
    IntegerVector jRand = sample_int(nitems);

    for (int loop = 0; loop < nitems; ++loop) {
      const int target = jRand[loop] - 1;
      const int f0 = fld[target];

      // delete selected item
      Nf[f0 - 1] -= 1;
      for (int r = 0; r < ncls; ++r) {
        Ccf[idx2(r, f0 - 1, ncls)] -= Cjc[idx2(target, r, nitems)];
        Fcf[idx2(r, f0 - 1, ncls)] -= Fjc[idx2(target, r, nitems)];
      }
      if (Nf[f0 - 1] == 0) {
        // the field disappeared: drop its column and renumber
        const int d = f0 - 1;
        Ccf.erase(Ccf.begin() + static_cast<std::ptrdiff_t>(d) * ncls,
                  Ccf.begin() + static_cast<std::ptrdiff_t>(d + 1) * ncls);
        Fcf.erase(Fcf.begin() + static_cast<std::ptrdiff_t>(d) * ncls,
                  Fcf.begin() + static_cast<std::ptrdiff_t>(d + 1) * ncls);
        Nf.erase(Nf.begin() + d);
        nfld -= 1;
        for (int j = 0; j < nitems; ++j) {
          if (fld[j] == f0) fld[j] = 0;
          else if (fld[j] > f0) fld[j] -= 1;
        }
      }
      CcfPlus.resize(Ccf.size());
      FcfPlus.resize(Fcf.size());
      for (int f = 0; f < nfld; ++f) {
        for (int r = 0; r < ncls; ++r) {
          CcfPlus[idx2(r, f, ncls)] = Ccf[idx2(r, f, ncls)] + Cjc[idx2(target, r, nitems)];
          FcfPlus[idx2(r, f, ncls)] = Fcf[idx2(r, f, ncls)] + Fjc[idx2(target, r, nitems)];
        }
      }

      // Table choice over the existing fields
      logp.assign(nfld, 0.0);
      for (int j = 0; j < nfld; ++j) {
        const double log_tab_tmp = std::log((Nf[j] / (nitems - 1 + gamma_f)) + konst);
        double Ctmp = 0, Ftmp = 0, den = 0;
        for (int i = 0; i < ncls; ++i) {
          const double cp = CcfPlus[idx2(i, j, ncls)];
          const double fp = FcfPlus[idx2(i, j, ncls)];
          const double cc = Ccf[idx2(i, j, ncls)];
          const double fc = Fcf[idx2(i, j, ncls)];
          log_run(Ctmp, cp, cp - cc - 1, gamp, konst);
          log_run(Ftmp, fp, fp - fc - 1, gamp, konst);
          const double ct3 = cp + fp - cc - fc - 1;
          if (ct3 >= 0) {
            for (double s = 0; s <= ct3; s += 1.0) {
              den = den + std::log(cp + fp + 2 * gamp - 1 - s + konst);
            }
          }
        }
        logp[j] = log_tab_tmp + Ctmp + Ftmp - den;
      }
      prob = to_prob(logp);
      const int k = as<int>(draw(NumericVector(prob.begin(), prob.end())));

      fld[target] = k;
      Nf[k - 1] += 1;
      for (int r = 0; r < ncls; ++r) {
        Ccf[idx2(r, k - 1, ncls)] += Cjc[idx2(target, r, nitems)];
        Fcf[idx2(r, k - 1, ncls)] += Fjc[idx2(target, r, nitems)];
      }
    }

    int diff = 0;
    for (int j = 0; j < nitems; ++j) diff += std::abs(oldField[j] - fld[j]);
    limit_count = (diff == 0) ? limit_count + 1 : 0;
    if (verbose) {
      Rcpp::message(Rcpp::wrap(
        std::string("iter ") + std::to_string(iter) + ": match=" + std::to_string(limit_count) +
        " nfld=" + std::to_string(nfld) + " ncls=" + std::to_string(ncls)));
    }
    if (limit_count == stable_limit || iter == maxiter) {
      IRM_FLG = false;
    } else {
      iter += 1;
    }
    Rcpp::checkUserInterrupt();
  }

  NumericMatrix cls01(nobs, ncls);
  for (int i = 0; i < nobs; ++i) if (cls[i] >= 1) cls01(i, cls[i] - 1) = 1.0;
  NumericMatrix fld01(nitems, nfld);
  for (int j = 0; j < nitems; ++j) if (fld[j] >= 1) fld01(j, fld[j] - 1) = 1.0;

  return List::create(Named("cls01") = cls01, Named("fld01") = fld01,
                      Named("ncls") = ncls, Named("nfld") = nfld,
                      Named("iter") = iter);
}
