// Graded Response Model with Analytical Gradient
// Fast GRM implementation using C++ with analytical score function

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
List simple_quadrature_grm(int n_points = 51) {
  NumericVector nodes(n_points);
  NumericVector weights(n_points);

  // Quadrature nodes from -6 to 6 with 51 points
  for (int i = 0; i < n_points; i++) {
    nodes[i] = -6.0 + 12.0 * i / (n_points - 1.0);
  }

  // Calculate normal density weights
  double sum_weights = 0.0;
  for (int i = 0; i < n_points; i++) {
    weights[i] = R::dnorm(nodes[i], 0.0, 1.0, false);
    sum_weights += weights[i];
  }

  // Normalize weights to sum to 1
  for (int i = 0; i < n_points; i++) {
    weights[i] /= sum_weights;
  }

  return List::create(Named("nodes") = nodes, Named("weights") = weights);
}

// [[Rcpp::export]]
List target_to_params_grm(NumericVector target, int nitems, IntegerVector ncat) {
  NumericVector a_vec(nitems);
  List b_list(nitems);

  for (int j = 0; j < nitems; j++) {
    a_vec[j] = exp(target[j]);
  }

  int pos = nitems;
  for (int j = 0; j < nitems; j++) {
    int n_thresholds = ncat[j] - 1;
    NumericVector b_j(n_thresholds);

    if (n_thresholds == 1) {
      b_j[0] = target[pos];
      pos++;
    } else {
      double first_threshold = target[pos];
      b_j[0] = first_threshold;
      pos++;

      for (int k = 1; k < n_thresholds; k++) {
        double delta = exp(target[pos]);
        b_j[k] = b_j[k-1] + delta;
        pos++;
      }
    }

    b_list[j] = b_j;
  }

  return List::create(Named("a") = a_vec, Named("b") = b_list);
}

// Log-likelihood function for Graded Response Model
// [[Rcpp::export]]
double log_lik_grm_cpp(NumericVector target, IntegerMatrix dat, int n_quad_points = 51) {
  int nobs = dat.nrow();
  int nitems = dat.ncol();

  IntegerVector ncat(nitems);
  for (int j = 0; j < nitems; j++) {
    int max_cat = 0;
    for (int i = 0; i < nobs; i++) {
      if (!IntegerMatrix::is_na(dat(i, j))) {
        max_cat = std::max(max_cat, dat(i, j));
      }
    }
    ncat[j] = max_cat;
  }

  List params = target_to_params_grm(target, nitems, ncat);
  NumericVector a_vec = params["a"];
  List b_list = params["b"];

  // Setup quadrature for numerical integration
  List quad = simple_quadrature_grm(n_quad_points);
  NumericVector quadrature = quad["nodes"];
  NumericVector weights = quad["weights"];
  int nq = quadrature.size();

  double ll = 0.0;

  // Calculate log-likelihood using quadrature integration
  for (int i = 0; i < nobs; i++) {
    std::vector<double> log_p(nq);
    for (int q = 0; q < nq; q++) {
      double log_p_q = 0.0;
      double theta_q = quadrature[q];

      for (int j = 0; j < nitems; j++) {
        if (!IntegerMatrix::is_na(dat(i, j))) {
          int resp = dat(i, j);
          NumericVector b_j = b_list[j];

          // Calculate cumulative probabilities
          std::vector<double> cum_pr(ncat[j] + 1);
          cum_pr[0] = 1.0;
          cum_pr[ncat[j]] = 0.0;

          for (int k = 0; k < b_j.size(); k++) {
            cum_pr[k + 1] = 1.0 / (1.0 + exp(-a_vec[j] * (theta_q - b_j[k])));
          }

          // Category probability
          double cat_pr = std::max(cum_pr[resp - 1] - cum_pr[resp], 1e-10);
          log_p_q += log(cat_pr);
        }
      }
      log_p[q] = log_p_q;
    }

    // Log-sum-exp for numerical stability
    double max_log_p = *std::max_element(log_p.begin(), log_p.end());
    double sum_exp = 0.0;
    for (int q = 0; q < nq; q++) {
      sum_exp += exp(log_p[q] - max_log_p) * weights[q];
    }
    ll += max_log_p + log(sum_exp);
  }

  return ll;
}

// Analytical score function with gradient computation
// [[Rcpp::export]]
NumericVector score_function_analytical_grm(NumericVector target, IntegerMatrix dat, int n_quad_points = 51) {
  int nobs = dat.nrow();
  int nitems = dat.ncol();

  IntegerVector ncat(nitems);
  for (int j = 0; j < nitems; j++) {
    int max_cat = 0;
    for (int i = 0; i < nobs; i++) {
      if (!IntegerMatrix::is_na(dat(i, j))) {
        max_cat = std::max(max_cat, dat(i, j));
      }
    }
    ncat[j] = max_cat;
  }

  List params = target_to_params_grm(target, nitems, ncat);
  NumericVector a_vec = params["a"];
  List b_list = params["b"];

  // Setup quadrature for numerical integration
  List quad = simple_quadrature_grm(n_quad_points);
  NumericVector quadrature = quad["nodes"];
  NumericVector weights = quad["weights"];
  int nq = quadrature.size();

  // Pre-compute probability matrices for all items
  std::vector<NumericMatrix> cum_pr_list(nitems);
  std::vector<NumericMatrix> cat_pr_list(nitems);
  std::vector<NumericMatrix> prod_pr_list(nitems);

  for (int j = 0; j < nitems; j++) {
    NumericVector b_j = b_list[j];
    int ncats = ncat[j];

    NumericMatrix cum_pr(ncats + 1, nq);
    NumericMatrix cat_pr(ncats, nq);
    NumericMatrix prod_pr(ncats + 1, nq);

    // Cumulative probabilities
    for (int q = 0; q < nq; q++) {
      cum_pr(0, q) = 1.0;
      cum_pr(ncats, q) = 0.0;

      for (int k = 0; k < b_j.size(); k++) {
        cum_pr(k + 1, q) = 1.0 / (1.0 + exp(-a_vec[j] * (quadrature[q] - b_j[k])));
      }
    }

    // Category probabilities
    for (int k = 0; k < ncats; k++) {
      for (int q = 0; q < nq; q++) {
        cat_pr(k, q) = std::max(cum_pr(k, q) - cum_pr(k + 1, q), 1e-10);
      }
    }

    // prod_pr = cum_pr * (1 - cum_pr) for derivatives
    for (int k = 0; k <= ncats; k++) {
      for (int q = 0; q < nq; q++) {
        prod_pr(k, q) = cum_pr(k, q) * (1.0 - cum_pr(k, q));
      }
    }

    cum_pr_list[j] = cum_pr;
    cat_pr_list[j] = cat_pr;
    prod_pr_list[j] = prod_pr;
  }

  // Calculate posterior probability p(x|z)
  NumericMatrix p_xz(nobs, nq);
  for (int i = 0; i < nobs; i++) {
    for (int q = 0; q < nq; q++) {
      double log_lik_q = 0.0;
      for (int j = 0; j < nitems; j++) {
        if (!IntegerMatrix::is_na(dat(i, j))) {
          int resp = dat(i, j);
          log_lik_q += log(cat_pr_list[j](resp - 1, q)); // 1-based to 0-based
        }
      }
      p_xz(i, q) = exp(log_lik_q);
    }
  }

  // Calculate marginal likelihood p(x)
  NumericVector p_x(nobs);
  for (int i = 0; i < nobs; i++) {
    double sum_pxz = 0.0;
    for (int q = 0; q < nq; q++) {
      sum_pxz += p_xz(i, q) * weights[q];
    }
    p_x[i] = sum_pxz;
  }

  // Calculate posterior distribution p(z|x)
  NumericMatrix p_z_x(nobs, nq);
  for (int i = 0; i < nobs; i++) {
    for (int q = 0; q < nq; q++) {
      p_z_x(i, q) = p_xz(i, q) / p_x[i];
    }
  }

  // Calculate score function derivatives
  NumericVector model_score_a(nitems);
  std::vector<NumericVector> model_score_b(nitems);

  for (int j = 0; j < nitems; j++) {
    NumericVector b_j = b_list[j];
    model_score_b[j] = NumericVector(b_j.size());
  }

  // Triple loop over observations, items, and quadrature points
  for (int i = 0; i < nobs; i++) {
    for (int j = 0; j < nitems; j++) {
      int resp = dat(i, j);
      if (!IntegerMatrix::is_na(dat(i, j))) {
        NumericVector b_j = b_list[j];

        for (int q = 0; q < nq; q++) {
          double theta_q = quadrature[q];
          double p_q = p_z_x(i, q) * weights[q];

          // Calculate discrimination parameter gradient
          double da_j = 0.0;

          // Check if response is within threshold range
          if (resp <= b_j.size()) {
            // Get probability derivative
            double prod_k1 = prod_pr_list[j](resp, q); // resp+1 in R (1-based) -> resp in C++ (0-based)
            // Update discrimination gradient
            da_j -= (theta_q - b_j[resp - 1]) * prod_k1 / cat_pr_list[j](resp - 1, q);
          }

          // Check previous threshold condition
          if (resp > 1 && resp - 1 <= b_j.size()) {
            // Get probability derivative for previous threshold
            double prod_k = prod_pr_list[j](resp - 1, q); // resp in R (1-based) -> resp-1 in C++ (0-based)
            // Add contribution from previous threshold
            da_j += (theta_q - b_j[resp - 2]) * prod_k / cat_pr_list[j](resp - 1, q);
          }

          model_score_a[j] += p_q * da_j;

          // Calculate threshold parameter gradients
          // Loop over all thresholds for this item
          for (int k = 0; k < b_j.size(); k++) {
            double db_jk = 0.0;

            // Check if this threshold affects the response
            // C++ k is 0-based, R k is 1-based, so k+1 == resp-1
            if ((k + 1) == (resp - 1) && resp > 0) {
              // Get probability derivative
              double prod_k = prod_pr_list[j](resp - 1, q); // resp in R (1-based) -> resp-1 in C++ (0-based)
              // Add positive contribution
              db_jk += a_vec[j] * prod_k / cat_pr_list[j](resp - 1, q);
            }

            // Check next threshold condition
            // C++ k is 0-based, R k is 1-based, so k+1 == resp
            if ((k + 1) == resp && resp <= b_j.size()) {
              // Get next probability derivative
              double prod_k1 = prod_pr_list[j](resp, q); // resp+1 in R (1-based) -> resp in C++ (0-based)
              // Subtract negative contribution
              db_jk -= a_vec[j] * prod_k1 / cat_pr_list[j](resp - 1, q);
            }

            model_score_b[j][k] += p_q * db_jk;
          }
        }
      }
    }
  }

  // Apply numerical stability threshold
  for (int j = 0; j < nitems; j++) {
    if (fabs(model_score_a[j]) < 1e-8) model_score_a[j] = 0.0;
    for (int k = 0; k < model_score_b[j].size(); k++) {
      if (fabs(model_score_b[j][k]) < 1e-8) model_score_b[j][k] = 0.0;
    }
  }

  // Apply Jacobian transformation for parameter conversion
  NumericVector target_score(target.size());

  // Transform discrimination parameters with Jacobian
  // jacobians[[j]]$a_jacobian = a_vec[j]
  for (int j = 0; j < nitems; j++) {
    target_score[j] = -1.0 * model_score_a[j] * a_vec[j];
  }

  // Transform threshold parameters with Jacobian matrix
  int pos = nitems;
  for (int j = 0; j < nitems; j++) {
    NumericVector b_j = b_list[j];
    int n_thresholds = b_j.size();

    if (n_thresholds == 1) {
      // Single threshold case: direct transformation
      target_score[pos] = model_score_b[j][0];
      pos++;
    } else {
      // Multiple thresholds: matrix multiplication with Jacobian
      // Jacobian matrix structure:
      // jac_matrix[1,1] = 1
      // jac_matrix[i,1] = 1 for i >= 2
      // jac_matrix[i,k] = exp(target[pos + k]) for k = 2 to i

      // First element: sum of all model_score_b elements (due to column 1 having all 1s)
      double first_elem = 0.0;
      for (int i = 0; i < n_thresholds; i++) {
        first_elem += model_score_b[j][i];
      }
      target_score[pos] = first_elem;
      pos++;

      // Other elements: weighted by exp(target[pos])
      for (int k = 1; k < n_thresholds; k++) {
        double elem_k = 0.0;
        double exp_target_k = exp(target[pos]);
        // Sum from row k to n_thresholds-1 (since jac_matrix[i,k+1] = exp_target_k for i >= k+1)
        for (int i = k; i < n_thresholds; i++) {
          elem_k += model_score_b[j][i];
        }
        target_score[pos] = elem_k * exp_target_k;
        pos++;
      }
    }
  }

  // Return negative score (for maximization)
  NumericVector result(target_score.size());
  for (int i = 0; i < target_score.size(); i++) {
    result[i] = -target_score[i];
  }

  return result;
}

// Numerical gradient for validation
// [[Rcpp::export]]
NumericVector score_function_numerical_grm(NumericVector target, IntegerMatrix dat, int n_quad_points = 51) {
  int n_params = target.size();
  NumericVector grad(n_params);
  double eps = 1e-6;

  double f0 = log_lik_grm_cpp(target, dat, n_quad_points);

  for (int i = 0; i < n_params; i++) {
    NumericVector target_plus = clone(target);
    target_plus[i] += eps;
    double f_plus = log_lik_grm_cpp(target_plus, dat, n_quad_points);
    grad[i] = (f_plus - f0) / eps;
  }

  return grad;
}

// Gradient comparison test function
// [[Rcpp::export]]
List compare_gradients_grm(NumericVector target, IntegerMatrix dat, int n_quad_points = 51) {
  NumericVector analytical = score_function_analytical_grm(target, dat, n_quad_points);
  NumericVector numerical = score_function_numerical_grm(target, dat, n_quad_points);

  NumericVector diff = analytical - numerical;
  double max_abs_diff = max(abs(diff));
  double rms_diff = sqrt(mean(diff * diff));

  return List::create(
    Named("analytical") = analytical,
    Named("numerical") = numerical,
    Named("difference") = diff,
    Named("max_abs_diff") = max_abs_diff,
    Named("rms_diff") = rms_diff
  );
}