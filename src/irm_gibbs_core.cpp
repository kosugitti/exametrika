// Collapsed Gibbs sampler core for the Infinite Relational Model (IRM)
// used by Biclustering_IRM.nominal() and Biclustering_IRM.ordinal().
//
// This is a direct C++ translation of irm_gibbs_core() in
// R/00_IRM_Gibbs_CORE.R. The translation preserves the loop structure
// and the order of RNG consumption so that, given the same RNG state,
// it produces bit-identical output to the R reference implementation.
//
// RNG calls are routed through R's RNG (Rcpp::sample, R::rmultinom)
// so set.seed() drives both implementations identically.

#include <Rcpp.h>
#include <vector>
#include <cmath>

using namespace Rcpp;

namespace {

inline int idx3(int i, int j, int k, int d1, int d2) {
  // column-major flat index for a (d1 x d2 x d3) array: A[i, j, k]
  return i + d1 * (j + d2 * k);
}

inline int idx2(int i, int j, int d1) {
  return i + d1 * j;
}

// log multivariate beta: sum(lgamma(a)) - lgamma(sum(a))
inline double lmvbeta(const std::vector<double>& a) {
  double sum_a = 0.0;
  double sum_lg = 0.0;
  const std::size_t n = a.size();
  for (std::size_t i = 0; i < n; ++i) {
    sum_a += a[i];
    sum_lg += R::lgammafn(a[i]);
  }
  return sum_lg - R::lgammafn(sum_a);
}

// log-sum-exp
inline double log_sum_exp(const std::vector<double>& x) {
  double m = x[0];
  for (std::size_t i = 1; i < x.size(); ++i) {
    if (x[i] > m) m = x[i];
  }
  double s = 0.0;
  for (std::size_t i = 0; i < x.size(); ++i) {
    s += std::exp(x[i] - m);
  }
  return m + std::log(s);
}

// Convert log-prob vector to prob vector
inline std::vector<double> log_to_prob(const std::vector<double>& log_p) {
  double lse = log_sum_exp(log_p);
  std::vector<double> p(log_p.size());
  for (std::size_t i = 0; i < log_p.size(); ++i) {
    p[i] = std::exp(log_p[i] - lse);
  }
  return p;
}

// R-compatible sample(1:n, n, replace = FALSE) -> 0-indexed permutation.
// We delegate to base::sample.int because Rcpp::sample uses a different
// RNG-consumption order from base R, breaking bit-for-bit reproducibility
// with the R reference implementation. Calling the R function does cost
// a small amount per call but is invoked only twice per Gibbs iteration.
inline std::vector<int> sample_perm(Function& r_sample_int, int n) {
  IntegerVector v = r_sample_int(n);
  std::vector<int> out(n);
  for (int i = 0; i < n; ++i) out[i] = v[i] - 1; // 1-based -> 0-based
  return out;
}

// R-compatible rmultinom(1, 1, prob): returns the index of the selected category.
//
// We delegate to R-level rmultinom() via Rcpp::Function instead of the
// C-level R::rmultinom entry point. Both entry points consume the same
// number of unif_rand() calls in principle, but the R-level wrapper runs
// FixupProb() and possibly other normalisation/validation steps that the
// C-level form skips. Empirically the C-level path desynchronises from
// R's RNG stream after a few class-Gibbs iterations, while the
// R-level path stays bit-identical. The per-call overhead is small
// compared to the rest of the inner-loop arithmetic.
inline int rmultinom_one(Function& r_rmultinom, const std::vector<double>& prob) {
  NumericVector p(prob.begin(), prob.end());
  IntegerMatrix m = r_rmultinom(1, 1, p);
  const int K = m.nrow();
  for (int k = 0; k < K; ++k) {
    if (m(k, 0) == 1) return k;
  }
  return K - 1;
}

} // anonymous namespace

// [[Rcpp::export]]
List irm_gibbs_core_cpp(NumericVector Uq_vec,
                         IntegerVector Uq_dim,
                         NumericMatrix Z,
                         NumericMatrix cls01_init,
                         NumericMatrix fld01_init,
                         double gamma_c,
                         double gamma_f,
                         NumericVector alpha_vec_R,
                         int max_iter,
                         int stable_limit,
                         bool verbose) {
  const int nobs = Uq_dim[0];
  const int nitems = Uq_dim[1];
  const int maxQ = Uq_dim[2];

  // const = exp(-nitems) (matches R-side definition)
  const double konst = std::exp(-static_cast<double>(nitems));

  // Copy alpha_vec to std::vector
  std::vector<double> alpha_vec(maxQ);
  for (int q = 0; q < maxQ; ++q) alpha_vec[q] = alpha_vec_R[q];

  // Flat copy of Uq (nobs x nitems x maxQ, column-major)
  const std::size_t Uq_size = static_cast<std::size_t>(nobs) * nitems * maxQ;
  std::vector<double> Uq(Uq_size);
  for (std::size_t k = 0; k < Uq_size; ++k) Uq[k] = Uq_vec[k];

  // Z as flat matrix (nobs x nitems)
  std::vector<double> Zmat(static_cast<std::size_t>(nobs) * nitems);
  for (int j = 0; j < nitems; ++j)
    for (int i = 0; i < nobs; ++i)
      Zmat[idx2(i, j, nobs)] = Z(i, j);

  // Class membership (nobs x ncls), dynamic ncls
  int ncls = cls01_init.ncol();
  // We store cls01 column-major as std::vector<double> of size nobs * ncls
  std::vector<double> cls01(static_cast<std::size_t>(nobs) * ncls);
  for (int c = 0; c < ncls; ++c)
    for (int i = 0; i < nobs; ++i)
      cls01[idx2(i, c, nobs)] = cls01_init(i, c);

  // Class assignment (1-indexed cls per respondent), and class sizes Nc
  std::vector<int> cls(nobs, 0);
  std::vector<int> Nc(ncls, 0);
  for (int i = 0; i < nobs; ++i) {
    for (int c = 0; c < ncls; ++c) {
      if (cls01[idx2(i, c, nobs)] == 1.0) {
        cls[i] = c + 1; // 1-indexed
        Nc[c] += 1;
        break;
      }
    }
  }

  // Field membership (nitems x nfld)
  int nfld = fld01_init.ncol();
  std::vector<double> fld01(static_cast<std::size_t>(nitems) * nfld);
  for (int f = 0; f < nfld; ++f)
    for (int j = 0; j < nitems; ++j)
      fld01[idx2(j, f, nitems)] = fld01_init(j, f);

  std::vector<int> fld(nitems, 0);
  std::vector<int> Nf(nfld, 0);
  for (int j = 0; j < nitems; ++j) {
    for (int f = 0; f < nfld; ++f) {
      if (fld01[idx2(j, f, nitems)] == 1.0) {
        fld[j] = f + 1;
        Nf[f] += 1;
        break;
      }
    }
  }

  // Compute U_fcq once: U_fcq[f,c,q] = sum_{i,j} fld01[j,f] * Z[i,j] * Uq[i,j,q] * cls01[i,c]
  // Stored as std::vector<double> (nfld x ncls x maxQ), column-major.
  // Capacity may grow as ncls/nfld grow.
  auto compute_U_fcq = [&](std::vector<double>& U_fcq_out) {
    U_fcq_out.assign(static_cast<std::size_t>(nfld) * ncls * maxQ, 0.0);
    // For each q: U_fcq[,,q] = t(fld01) %*% (Z * Uq[,,q]) %*% cls01
    // Direct triple loop (mirrors R semantics; we trust the optimizer).
    for (int q = 0; q < maxQ; ++q) {
      // First compute T[i,f] = sum_j fld01[j,f] * Z[i,j] * Uq[i,j,q]   (nobs x nfld)
      std::vector<double> T(static_cast<std::size_t>(nobs) * nfld, 0.0);
      for (int f = 0; f < nfld; ++f) {
        for (int j = 0; j < nitems; ++j) {
          double fv = fld01[idx2(j, f, nitems)];
          if (fv == 0.0) continue;
          for (int i = 0; i < nobs; ++i) {
            double zu = Zmat[idx2(i, j, nobs)] * Uq[idx3(i, j, q, nobs, nitems)];
            T[idx2(i, f, nobs)] += fv * zu;
          }
        }
      }
      // Then U_fcq[f,c,q] = sum_i T[i,f] * cls01[i,c]
      for (int c = 0; c < ncls; ++c) {
        for (int i = 0; i < nobs; ++i) {
          double cv = cls01[idx2(i, c, nobs)];
          if (cv == 0.0) continue;
          for (int f = 0; f < nfld; ++f) {
            U_fcq_out[idx3(f, c, q, nfld, ncls)] += T[idx2(i, f, nobs)] * cv;
          }
        }
      }
    }
  };

  std::vector<double> U_fcq;
  compute_U_fcq(U_fcq);

  // R sample.int and rmultinom handles (RNG-compatible)
  Function r_sample_int("sample.int");
  Function r_rmultinom("rmultinom");

  int limit_count = 0;
  int iter = 1;
  bool IRM_FLG = true;

  std::vector<int> oldfld(nitems);

  while (IRM_FLG) {
    // sample(1:nobs, nobs)
    std::vector<int> iRand = sample_perm(r_sample_int, nobs);

    // Recompute U_fcq before class loop (mirrors R: U_fcq is recomputed each iter)
    compute_U_fcq(U_fcq);

    // ====== Class-side Gibbs ======
    for (std::size_t s = 0; s < iRand.size(); ++s) {
      int target = iRand[s]; // 0-indexed respondent

      // v_fq[f, q] = sum_j fld01[j,f] * Z[target,j] * Uq[target,j,q]
      std::vector<double> v_fq(static_cast<std::size_t>(nfld) * maxQ, 0.0);
      for (int q = 0; q < maxQ; ++q) {
        for (int f = 0; f < nfld; ++f) {
          double s_acc = 0.0;
          for (int j = 0; j < nitems; ++j) {
            double fv = fld01[idx2(j, f, nitems)];
            if (fv == 0.0) continue;
            s_acc += fv * Zmat[idx2(target, j, nobs)]
                       * Uq[idx3(target, j, q, nobs, nitems)];
          }
          v_fq[idx2(f, q, nfld)] = s_acc;
        }
      }

      // Subtract target's contribution from old class
      int old_cls = cls[target] - 1; // 0-indexed
      for (int q = 0; q < maxQ; ++q) {
        for (int f = 0; f < nfld; ++f) {
          U_fcq[idx3(f, old_cls, q, nfld, ncls)] -= v_fq[idx2(f, q, nfld)];
        }
      }
      Nc[old_cls] -= 1;

      // If the class disappeared, remove column old_cls from cls01 and U_fcq slice
      if (Nc[old_cls] == 0) {
        // Remove column old_cls from cls01 (nobs x ncls -> nobs x (ncls-1))
        std::vector<double> new_cls01(static_cast<std::size_t>(nobs) * (ncls - 1));
        int dst = 0;
        for (int c = 0; c < ncls; ++c) {
          if (c == old_cls) continue;
          for (int i = 0; i < nobs; ++i) {
            new_cls01[idx2(i, dst, nobs)] = cls01[idx2(i, c, nobs)];
          }
          ++dst;
        }
        cls01.swap(new_cls01);
        // Remove slice c=old_cls from U_fcq (nfld x ncls x maxQ -> nfld x (ncls-1) x maxQ)
        std::vector<double> new_U(static_cast<std::size_t>(nfld) * (ncls - 1) * maxQ, 0.0);
        for (int q = 0; q < maxQ; ++q) {
          int dst_c = 0;
          for (int c = 0; c < ncls; ++c) {
            if (c == old_cls) continue;
            for (int f = 0; f < nfld; ++f) {
              new_U[idx3(f, dst_c, q, nfld, ncls - 1)] =
                U_fcq[idx3(f, c, q, nfld, ncls)];
            }
            ++dst_c;
          }
        }
        U_fcq.swap(new_U);
        // Remove from Nc and re-derive cls
        Nc.erase(Nc.begin() + old_cls);
        ncls -= 1;
        // cls <- as.vector(cls01 %*% (1:ncls))
        for (int i = 0; i < nobs; ++i) {
          int v = 0;
          for (int c = 0; c < ncls; ++c) {
            if (cls01[idx2(i, c, nobs)] == 1.0) { v = c + 1; break; }
          }
          cls[i] = v;
        }
      }

      // exist_tab[c] = log(Nc[c]/(nobs-1+gamma_c) + const) + sum_f [lmvbeta(nume) - lmvbeta(deno)]
      std::vector<double> exist_tab(ncls, 0.0);
      std::vector<double> nume(maxQ), deno(maxQ);
      for (int c = 0; c < ncls; ++c) {
        double base_log = std::log(static_cast<double>(Nc[c]) /
                                   (nobs - 1.0 + gamma_c) + konst);
        double acc = base_log;
        for (int f = 0; f < nfld; ++f) {
          for (int q = 0; q < maxQ; ++q) {
            double u = U_fcq[idx3(f, c, q, nfld, ncls)];
            nume[q] = u + v_fq[idx2(f, q, nfld)] + alpha_vec[q];
            deno[q] = u + alpha_vec[q];
          }
          acc += lmvbeta(nume) - lmvbeta(deno);
        }
        exist_tab[c] = acc;
      }

      // new_tab1 + new_tab2
      double new_tab1 = std::log(gamma_c / (nobs - 1.0 + gamma_c));
      double new_tab2 = 0.0;
      std::vector<double> tmp_alpha(maxQ);
      for (int f = 0; f < nfld; ++f) {
        for (int q = 0; q < maxQ; ++q) tmp_alpha[q] = v_fq[idx2(f, q, nfld)] + alpha_vec[q];
        new_tab2 += lmvbeta(tmp_alpha) - lmvbeta(alpha_vec);
      }
      double new_tab = new_tab1 + new_tab2;

      // Sample
      std::vector<double> log_p(ncls + 1);
      for (int c = 0; c < ncls; ++c) log_p[c] = exist_tab[c];
      log_p[ncls] = new_tab;
      std::vector<double> ptab = log_to_prob(log_p);
      int chosen = rmultinom_one(r_rmultinom, ptab); // 0..ncls; ncls = new

      int new_cls;
      if (chosen < ncls) {
        // Existing class
        new_cls = chosen; // 0-indexed
        for (int c = 0; c < ncls; ++c) cls01[idx2(target, c, nobs)] = 0.0;
        cls01[idx2(target, new_cls, nobs)] = 1.0;
        cls[target] = new_cls + 1;
      } else {
        // New class. Match the R reference exactly:
        //   cls01 <- cbind(cls01, 0)
        //   cls01[target, ] <- 0
        //   cls01[target, ncls] <- 1
        // i.e. clear the target row across ALL columns (including the
        // newly appended zero column) before setting the new-class entry.
        cls01.resize(static_cast<std::size_t>(nobs) * (ncls + 1), 0.0);
        for (int c = 0; c < ncls + 1; ++c) cls01[idx2(target, c, nobs)] = 0.0;
        cls01[idx2(target, ncls, nobs)] = 1.0;
        // Append a zero slice along c-axis of U_fcq:
        // old layout has stride nfld*ncls between q-slices; new layout has stride nfld*(ncls+1)
        std::vector<double> new_U(static_cast<std::size_t>(nfld) * (ncls + 1) * maxQ, 0.0);
        for (int q = 0; q < maxQ; ++q) {
          for (int c = 0; c < ncls; ++c) {
            for (int f = 0; f < nfld; ++f) {
              new_U[idx3(f, c, q, nfld, ncls + 1)] = U_fcq[idx3(f, c, q, nfld, ncls)];
            }
          }
        }
        U_fcq.swap(new_U);
        Nc.push_back(0);
        ncls += 1;
        new_cls = ncls - 1;
        cls[target] = new_cls + 1;
      }

      // Add back contribution to new class
      for (int q = 0; q < maxQ; ++q) {
        for (int f = 0; f < nfld; ++f) {
          U_fcq[idx3(f, new_cls, q, nfld, ncls)] += v_fq[idx2(f, q, nfld)];
        }
      }

      // Nc <- colSums(cls01)
      for (int c = 0; c < ncls; ++c) {
        int s_n = 0;
        for (int i = 0; i < nobs; ++i) {
          if (cls01[idx2(i, c, nobs)] == 1.0) s_n += 1;
        }
        Nc[c] = s_n;
      }
    }

    // ====== Field-side Gibbs ======
    for (int j = 0; j < nitems; ++j) oldfld[j] = fld[j];
    std::vector<int> jRand = sample_perm(r_sample_int, nitems);

    for (std::size_t s = 0; s < jRand.size(); ++s) {
      int target = jRand[s]; // 0-indexed item

      // v_cq[c, q] = sum_i cls01[i,c] * Z[i,target] * Uq[i,target,q]
      std::vector<double> v_cq(static_cast<std::size_t>(ncls) * maxQ, 0.0);
      for (int q = 0; q < maxQ; ++q) {
        for (int c = 0; c < ncls; ++c) {
          double s_acc = 0.0;
          for (int i = 0; i < nobs; ++i) {
            double cv = cls01[idx2(i, c, nobs)];
            if (cv == 0.0) continue;
            s_acc += cv * Zmat[idx2(i, target, nobs)]
                       * Uq[idx3(i, target, q, nobs, nitems)];
          }
          v_cq[idx2(c, q, ncls)] = s_acc;
        }
      }

      int old_fld = fld[target] - 1;
      for (int q = 0; q < maxQ; ++q) {
        for (int c = 0; c < ncls; ++c) {
          U_fcq[idx3(old_fld, c, q, nfld, ncls)] -= v_cq[idx2(c, q, ncls)];
        }
      }
      Nf[old_fld] -= 1;

      if (Nf[old_fld] == 0) {
        // Remove column old_fld from fld01
        std::vector<double> new_fld01(static_cast<std::size_t>(nitems) * (nfld - 1));
        int dst = 0;
        for (int f = 0; f < nfld; ++f) {
          if (f == old_fld) continue;
          for (int j2 = 0; j2 < nitems; ++j2) {
            new_fld01[idx2(j2, dst, nitems)] = fld01[idx2(j2, f, nitems)];
          }
          ++dst;
        }
        fld01.swap(new_fld01);
        // Remove slice f=old_fld from U_fcq
        std::vector<double> new_U(static_cast<std::size_t>(nfld - 1) * ncls * maxQ, 0.0);
        for (int q = 0; q < maxQ; ++q) {
          for (int c = 0; c < ncls; ++c) {
            int dst_f = 0;
            for (int f = 0; f < nfld; ++f) {
              if (f == old_fld) continue;
              new_U[idx3(dst_f, c, q, nfld - 1, ncls)] =
                U_fcq[idx3(f, c, q, nfld, ncls)];
              ++dst_f;
            }
          }
        }
        U_fcq.swap(new_U);
        Nf.erase(Nf.begin() + old_fld);
        nfld -= 1;
        for (int j2 = 0; j2 < nitems; ++j2) {
          int v = 0;
          for (int f = 0; f < nfld; ++f) {
            if (fld01[idx2(j2, f, nitems)] == 1.0) { v = f + 1; break; }
          }
          fld[j2] = v;
        }
      }

      // exist_tab[f] = log(Nf[f]/(nitems-1+gamma_f) + const) + sum_c [lmvbeta(nume)-lmvbeta(deno)]
      std::vector<double> exist_tab(nfld, 0.0);
      std::vector<double> nume(maxQ), deno(maxQ);
      for (int f = 0; f < nfld; ++f) {
        double base_log = std::log(static_cast<double>(Nf[f]) /
                                   (nitems - 1.0 + gamma_f) + konst);
        double acc = base_log;
        for (int c = 0; c < ncls; ++c) {
          for (int q = 0; q < maxQ; ++q) {
            double u = U_fcq[idx3(f, c, q, nfld, ncls)];
            nume[q] = u + v_cq[idx2(c, q, ncls)] + alpha_vec[q];
            deno[q] = u + alpha_vec[q];
          }
          acc += lmvbeta(nume) - lmvbeta(deno);
        }
        exist_tab[f] = acc;
      }

      double new_tab1 = std::log(gamma_f / (nitems - 1.0 + gamma_f));
      double new_tab2 = 0.0;
      std::vector<double> tmp_alpha(maxQ);
      for (int c = 0; c < ncls; ++c) {
        for (int q = 0; q < maxQ; ++q) tmp_alpha[q] = v_cq[idx2(c, q, ncls)] + alpha_vec[q];
        new_tab2 += lmvbeta(tmp_alpha) - lmvbeta(alpha_vec);
      }
      double new_tab = new_tab1 + new_tab2;

      std::vector<double> log_p(nfld + 1);
      for (int f = 0; f < nfld; ++f) log_p[f] = exist_tab[f];
      log_p[nfld] = new_tab;
      std::vector<double> ptab = log_to_prob(log_p);
      int chosen = rmultinom_one(r_rmultinom, ptab);

      int new_fld;
      if (chosen < nfld) {
        new_fld = chosen;
        for (int f = 0; f < nfld; ++f) fld01[idx2(target, f, nitems)] = 0.0;
        fld01[idx2(target, new_fld, nitems)] = 1.0;
        fld[target] = new_fld + 1;
      } else {
        // New field. Match R exactly: clear target row across all columns
        // (incl. the newly appended zero column), then set the new entry.
        fld01.resize(static_cast<std::size_t>(nitems) * (nfld + 1), 0.0);
        for (int f = 0; f < nfld + 1; ++f) fld01[idx2(target, f, nitems)] = 0.0;
        fld01[idx2(target, nfld, nitems)] = 1.0;
        std::vector<double> new_U(static_cast<std::size_t>(nfld + 1) * ncls * maxQ, 0.0);
        for (int q = 0; q < maxQ; ++q) {
          for (int c = 0; c < ncls; ++c) {
            for (int f = 0; f < nfld; ++f) {
              new_U[idx3(f, c, q, nfld + 1, ncls)] = U_fcq[idx3(f, c, q, nfld, ncls)];
            }
          }
        }
        U_fcq.swap(new_U);
        Nf.push_back(0);
        nfld += 1;
        new_fld = nfld - 1;
        fld[target] = new_fld + 1;
      }

      for (int q = 0; q < maxQ; ++q) {
        for (int c = 0; c < ncls; ++c) {
          U_fcq[idx3(new_fld, c, q, nfld, ncls)] += v_cq[idx2(c, q, ncls)];
        }
      }

      for (int f = 0; f < nfld; ++f) {
        int s_n = 0;
        for (int j2 = 0; j2 < nitems; ++j2) {
          if (fld01[idx2(j2, f, nitems)] == 1.0) s_n += 1;
        }
        Nf[f] = s_n;
      }
    }

    // Convergence check: field assignment stability
    int diff = 0;
    for (int j = 0; j < nitems; ++j) diff += std::abs(oldfld[j] - fld[j]);
    if (diff == 0) limit_count += 1; else limit_count = 0;

    if (verbose) {
      Rcpp::Rcout << "iter " << iter
                  << ": match=" << limit_count
                  << " nfld=" << nfld
                  << " ncls=" << ncls << std::endl;
    }

    if (limit_count == stable_limit || iter == max_iter) {
      IRM_FLG = false;
    } else {
      iter += 1;
    }
  }

  // Return results: convert std::vector back to NumericMatrix
  NumericMatrix cls01_out(nobs, ncls);
  for (int c = 0; c < ncls; ++c)
    for (int i = 0; i < nobs; ++i)
      cls01_out(i, c) = cls01[idx2(i, c, nobs)];

  NumericMatrix fld01_out(nitems, nfld);
  for (int f = 0; f < nfld; ++f)
    for (int j = 0; j < nitems; ++j)
      fld01_out(j, f) = fld01[idx2(j, f, nitems)];

  NumericVector U_fcq_out(static_cast<std::size_t>(nfld) * ncls * maxQ);
  for (std::size_t k = 0; k < U_fcq.size(); ++k) U_fcq_out[k] = U_fcq[k];
  U_fcq_out.attr("dim") = IntegerVector::create(nfld, ncls, maxQ);

  IntegerVector cls_out(nobs), fld_out(nitems), Nc_out(ncls), Nf_out(nfld);
  for (int i = 0; i < nobs; ++i) cls_out[i] = cls[i];
  for (int j = 0; j < nitems; ++j) fld_out[j] = fld[j];
  for (int c = 0; c < ncls; ++c) Nc_out[c] = Nc[c];
  for (int f = 0; f < nfld; ++f) Nf_out[f] = Nf[f];

  return List::create(
    _["cls01"] = cls01_out,
    _["fld01"] = fld01_out,
    _["U_fcq"] = U_fcq_out,
    _["ncls"] = ncls,
    _["nfld"] = nfld,
    _["cls"] = cls_out,
    _["fld"] = fld_out,
    _["Nc"] = Nc_out,
    _["Nf"] = Nf_out,
    _["n_gibbs_iter"] = iter
  );
}
