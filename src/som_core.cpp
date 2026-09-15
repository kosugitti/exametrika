// One epoch of the SOM (self-organizing map) update for binary LRA.
// The outer loop, the annealing schedule and the presentation order stay in R
// (R/00_EMclus.R, somclus). Keeping the order on the R side is what preserves
// reproducibility through set.seed(); this file only replays one epoch.
#include <Rcpp.h>
using namespace Rcpp;

// R sums with long double accumulators (src/main/summary.c), and the winner is
// picked from these sums, so a different accumulation order can flip a winner
// and send the run down another path. Accumulate the two dot products the same
// way R does and add them at the end.
static inline void row_insertion_sort(NumericMatrix& ref, int j, int C) {
  // The matrix is re-sorted after every student, so rows arrive almost ordered
  // and insertion sort costs O(C) in practice.
  for (int c = 1; c < C; ++c) {
    double v = ref(j, c);
    int k = c - 1;
    while (k >= 0 && ref(j, k) > v) {
      ref(j, k + 1) = ref(j, k);
      --k;
    }
    ref(j, k + 1) = v;
  }
}

// [[Rcpp::export]]
List som_epoch_cpp(NumericMatrix RefMat,
                   NumericVector prior_list,
                   const NumericMatrix& U,
                   const IntegerVector& order,
                   const NumericVector& hhh_row,
                   double kappa,
                   double cnst,
                   bool mic,
                   Nullable<NumericMatrix> conf_t_ = R_NilValue,
                   Nullable<LogicalMatrix> fixed_t_ = R_NilValue) {
  NumericMatrix ref = clone(RefMat);
  NumericVector prior = clone(prior_list);

  const int J = ref.nrow();
  const int C = ref.ncol();

  const bool use_conf = conf_t_.isNotNull() && fixed_t_.isNotNull();
  NumericMatrix conf_t;
  LogicalMatrix fixed_t;
  if (use_conf) {
    conf_t = NumericMatrix(conf_t_);
    fixed_t = LogicalMatrix(fixed_t_);
  }

  for (int idx = 0; idx < order.size(); ++idx) {
    const int s = order[idx] - 1;

    int winner = 0;
    double best = R_NegInf;
    for (int c = 0; c < C; ++c) {
      long double correct = 0.0L;
      long double incorrect = 0.0L;
      for (int j = 0; j < J; ++j) {
        const double p = ref(j, c);
        correct += U(s, j) * std::log(p + cnst);
        incorrect += (1.0 - U(s, j)) * std::log(1.0 - p + cnst);
      }
      const double value = static_cast<double>(correct) +
        static_cast<double>(incorrect) + std::log(prior[c]);
      // ">=" keeps the last maximum, i.e. the larger rank wins ties, matching
      // Sort[Transpose[{mlrank, clsnum}]][[-1]] in the original implementation.
      if (value >= best) {
        best = value;
        winner = c;
      }
    }

    const int offset = C - winner - 1;
    for (int c = 0; c < C; ++c) {
      const double h = hhh_row[offset + c];
      for (int j = 0; j < J; ++j) {
        ref(j, c) += h * (U(s, j) - ref(j, c));
      }
    }

    if (use_conf) {
      for (int c = 0; c < C; ++c) {
        for (int j = 0; j < J; ++j) {
          if (fixed_t(j, c)) {
            ref(j, c) = conf_t(j, c);
          }
        }
      }
    }

    if (mic) {
      for (int j = 0; j < J; ++j) {
        row_insertion_sort(ref, j, C);
      }
    }

    for (int c = 0; c < C; ++c) {
      prior[c] += kappa / C;
    }
    prior[winner] -= kappa;
    for (int c = 0; c < C; ++c) {
      if (prior[c] > 1.0) {
        prior[c] = 1.0;
      }
      if (prior[c] < cnst) {
        prior[c] = cnst;
      }
    }
  }

  return List::create(_["RefMat"] = ref, _["prior"] = prior);
}
