// Bock-Aitkin marginal maximum likelihood for the graded response model.
//
// The E-step walks the respondents once and returns, per item, the expected
// number of respondents at each quadrature node who chose each category
// (an ncat x nq table).  The M-step then maximises the expected complete-data
// log-likelihood item by item from those tables alone, so its cost does not
// depend on the number of respondents.  Both pieces are driven from R
// (GRM() in R/14_grm.R).
//
// Item parameters use the same unconstrained "target" coding as the direct
// optimiser in grm_analytical.cpp: log(a), b_1, log(b_2 - b_1), ...,
// log(b_{K-1} - b_{K-2}).

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

namespace {

// Category probabilities of one item at every node: an ncat x nq matrix,
// floored at 1e-10 like the direct optimiser.
NumericMatrix grm_cat_prob(double a, const NumericVector& b, const NumericVector& nodes) {
  int nq = nodes.size();
  int K = b.size() + 1;
  NumericMatrix P(K, nq);
  std::vector<double> cum(K + 1);
  for (int q = 0; q < nq; q++) {
    cum[0] = 1.0;
    cum[K] = 0.0;
    for (int k = 0; k < K - 1; k++) {
      cum[k + 1] = 1.0 / (1.0 + std::exp(-a * (nodes[q] - b[k])));
    }
    for (int k = 0; k < K; k++) {
      P(k, q) = std::max(cum[k] - cum[k + 1], 1e-10);
    }
  }
  return P;
}

// Decode one item's target vector into (a, b).
void grm_decode_item(const NumericVector& target, double& a, std::vector<double>& b) {
  int K = target.size();
  a = std::exp(target[0]);
  b.assign(K - 1, 0.0);
  b[0] = target[1];
  for (int k = 1; k < K - 1; k++) {
    b[k] = b[k - 1] + std::exp(target[k + 1]);
  }
}

} // namespace

//' E-step of the GRM EM algorithm
//'
//' @param dat integer matrix of responses coded 1..ncat (NA for missing)
//' @param ncat number of categories per item
//' @param a_vec slopes
//' @param b_list list of threshold vectors
//' @param nodes quadrature nodes
//' @param weights quadrature weights (sum to one)
//' @return a list with the marginal log-likelihood at the current
//'   parameters and, per item, the ncat x nq matrix of expected counts
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
List grm_em_estep_cpp(IntegerMatrix dat, IntegerVector ncat, NumericVector a_vec, List b_list,
                      NumericVector nodes, NumericVector weights) {
  int nobs = dat.nrow();
  int nitems = dat.ncol();
  int nq = nodes.size();

  std::vector<NumericMatrix> P(nitems);
  std::vector<NumericMatrix> R(nitems);
  for (int j = 0; j < nitems; j++) {
    NumericVector b_j = b_list[j];
    P[j] = grm_cat_prob(a_vec[j], b_j, nodes);
    R[j] = NumericMatrix(ncat[j], nq);
  }

  std::vector<double> logw(nq);
  for (int q = 0; q < nq; q++) logw[q] = std::log(weights[q]);

  double loglik = 0.0;
  std::vector<double> lp(nq);
  for (int i = 0; i < nobs; i++) {
    double lmax = R_NegInf;
    for (int q = 0; q < nq; q++) {
      double s = logw[q];
      for (int j = 0; j < nitems; j++) {
        int r = dat(i, j);
        if (r != NA_INTEGER) s += std::log(P[j](r - 1, q));
      }
      lp[q] = s;
      if (s > lmax) lmax = s;
    }
    double tot = 0.0;
    for (int q = 0; q < nq; q++) {
      lp[q] = std::exp(lp[q] - lmax);
      tot += lp[q];
    }
    loglik += lmax + std::log(tot);
    for (int j = 0; j < nitems; j++) {
      int r = dat(i, j);
      if (r == NA_INTEGER) continue;
      for (int q = 0; q < nq; q++) {
        R[j](r - 1, q) += lp[q] / tot;
      }
    }
  }

  List counts(nitems);
  for (int j = 0; j < nitems; j++) counts[j] = R[j];
  return List::create(Named("loglik") = loglik, Named("counts") = counts);
}

//' Expected complete-data log-likelihood of one item
//'
//' @param target the item's parameters in target coding
//' @param counts ncat x nq matrix of expected counts from the E-step
//' @param nodes quadrature nodes
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
double grm_item_ll_cpp(NumericVector target, NumericMatrix counts, NumericVector nodes) {
  double a;
  std::vector<double> b;
  grm_decode_item(target, a, b);
  NumericVector bv(b.begin(), b.end());
  NumericMatrix P = grm_cat_prob(a, bv, nodes);
  int K = counts.nrow();
  int nq = counts.ncol();
  double ll = 0.0;
  for (int q = 0; q < nq; q++) {
    for (int k = 0; k < K; k++) {
      if (counts(k, q) != 0.0) ll += counts(k, q) * std::log(P(k, q));
    }
  }
  return ll;
}

//' Gradient of grm_item_ll_cpp() with respect to the target coding
//'
//' @inheritParams grm_item_ll_cpp
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
NumericVector grm_item_grad_cpp(NumericVector target, NumericMatrix counts, NumericVector nodes) {
  double a;
  std::vector<double> b;
  grm_decode_item(target, a, b);
  int K = counts.nrow();
  int nq = counts.ncol();

  // d log P_k / da and / db_m through the boundary curves C_m = logistic(a (theta - b_m)),
  // P_k = C_{k-1} - C_k with C_0 = 1 and C_K = 0.  W_m = C_m (1 - C_m).
  double g_a = 0.0;
  std::vector<double> g_b(K - 1, 0.0);
  std::vector<double> C(K + 1), W(K + 1);
  for (int q = 0; q < nq; q++) {
    double th = nodes[q];
    C[0] = 1.0; C[K] = 0.0; W[0] = 0.0; W[K] = 0.0;
    for (int m = 0; m < K - 1; m++) {
      C[m + 1] = 1.0 / (1.0 + std::exp(-a * (th - b[m])));
      W[m + 1] = C[m + 1] * (1.0 - C[m + 1]);
    }
    for (int k = 0; k < K; k++) {
      double r = counts(k, q);
      if (r == 0.0) continue;
      double P = C[k] - C[k + 1];
      // The objective floors P at 1e-10, where it is flat: contribute nothing
      // there, or the raw derivative divided by the floor would blow up.
      if (P < 1e-10) continue;
      // dP_k/da = (theta - b_{k-1}) W_{k-1} - (theta - b_k) W_k  (boundary terms vanish)
      double dPa = 0.0;
      if (k >= 1) dPa += (th - b[k - 1]) * W[k];
      if (k <= K - 2) dPa -= (th - b[k]) * W[k + 1];
      g_a += r * dPa / P;
      // dP_k/db_m: b_{k-1} enters C_{k-1} (dC/db = -a W), b_k enters C_k
      if (k >= 1) g_b[k - 1] += r * (-a * W[k]) / P;
      if (k <= K - 2) g_b[k] += r * (a * W[k + 1]) / P;
    }
  }

  // chain rule into the target coding
  NumericVector g(K);
  g[0] = g_a * a;
  double s = 0.0;
  for (int m = 0; m < K - 1; m++) s += g_b[m];
  g[1] = s;
  for (int l = 1; l < K - 1; l++) {
    double tail = 0.0;
    for (int m = l; m < K - 1; m++) tail += g_b[m];
    g[l + 1] = tail * std::exp(target[l + 1]);
  }
  return g;
}
