#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// Fast bivariate normal CDF implementation using C++
// [[Rcpp::export]]
double qBiNormal_cpp(double a, double b, double rho) {
  const double pi = 3.14159265358979323846;

  // Handle NA values
  if (std::isnan(a) || std::isnan(b) || std::isnan(rho)) {
    return NA_REAL;
  }

  // Special cases for infinite bounds
  if (std::isinf(a) && std::isinf(b)) {
    if (a > 0 && b > 0) return 1.0;
    if (a < 0 && b < 0) return 0.0;
    return 0.0;
  }

  // Other infinite cases
  if (std::isinf(a) || std::isinf(b)) {
    if (a == -INFINITY || b == -INFINITY) return 0.0;
    if (std::isinf(a)) return R::pnorm(b, 0.0, 1.0, 1, 0);
    if (std::isinf(b)) return R::pnorm(a, 0.0, 1.0, 1, 0);
  }

  // Main computation case: a <= 0, b <= 0, rho <= 0
  if (a <= 0 && b <= 0 && rho <= 0) {
    double aprime = a / std::sqrt(2.0 * (1.0 - rho * rho));
    double bprime = b / std::sqrt(2.0 * (1.0 - rho * rho));

    // Gauss-Legendre quadrature weights and abscissae (15-point)
    const double A[15] = {
      5.54433663102343E-02, 1.24027738987730E-01, 1.75290943892075E-01,
      1.91488340747342E-01, 1.63473797144070E-01, 1.05937637278492E-01,
      5.00270211534535E-02, 1.64429690052673E-02, 3.57320421428311E-03,
      4.82896509305201E-04, 3.74908650266318E-05, 1.49368411589636E-06,
      2.55270496934465E-08, 1.34217679136316E-10, 9.56227446736465E-14
    };

    const double B[15] = {
      2.16869474675590E-02, 1.12684220347775E-01, 2.70492671421899E-01,
      4.86902370381935E-01, 7.53043683072978E-01, 1.06093100362236E+00,
      1.40425495820363E+00, 1.77864637941183E+00, 2.18170813144494E+00,
      2.61306084533352E+00, 3.07461811380851E+00, 3.57140815113714E+00,
      4.11373608977209E+00, 4.72351306243148E+00, 5.46048893578335E+00
    };

    double sum = 0.0;
    for (int i = 0; i < 15; i++) {
      for (int j = 0; j < 15; j++) {
        double x = B[i];
        double y = B[j];
        double f_val = std::exp(aprime * (2.0 * x - aprime) +
                               bprime * (2.0 * y - bprime) +
                               2.0 * rho * (x - aprime) * (y - bprime));
        sum += A[i] * A[j] * f_val;
      }
    }

    return sum * std::sqrt(1.0 - rho * rho) / pi;
  }

  // Recursive cases
  if (a * b * rho <= 0) {
    if (a <= 0 && b >= 0 && rho >= 0) {
      return R::pnorm(a, 0.0, 1.0, 1, 0) - qBiNormal_cpp(a, -b, -rho);
    } else if (a >= 0 && b <= 0 && rho >= 0) {
      return R::pnorm(b, 0.0, 1.0, 1, 0) - qBiNormal_cpp(-a, b, -rho);
    } else if (a >= 0 && b >= 0 && rho <= 0) {
      return R::pnorm(a, 0.0, 1.0, 1, 0) + R::pnorm(b, 0.0, 1.0, 1, 0) - 1.0 +
             qBiNormal_cpp(-a, -b, rho);
    }
  }

  if (a * b * rho >= 0) {
    double denum = std::sqrt(a * a - 2.0 * rho * a * b + b * b);
    double rho1 = ((rho * a - b) * ((a > 0) ? 1.0 : -1.0)) / denum;
    double rho2 = ((rho * b - a) * ((b > 0) ? 1.0 : -1.0)) / denum;
    double delta = (1.0 - ((a > 0) ? 1.0 : -1.0) * ((b > 0) ? 1.0 : -1.0)) / 4.0;

    return qBiNormal_cpp(a, 0.0, rho1) + qBiNormal_cpp(b, 0.0, rho2) - delta;
  }

  // Error case
  return NA_REAL;
}

// Fast polychoric likelihood calculation
// [[Rcpp::export]]
double polychoric_likelihood_cpp(double rho, IntegerMatrix mat) {
  int nr = mat.nrow();
  int nc = mat.ncol();

  // Calculate marginal proportions
  NumericVector th_r(nr), th_c(nc);
  double total = 0.0;
  for (int i = 0; i < nr; i++) {
    for (int j = 0; j < nc; j++) {
      total += mat(i, j);
    }
  }

  for (int i = 0; i < nr; i++) {
    double row_sum = 0.0;
    for (int j = 0; j < nc; j++) {
      row_sum += mat(i, j);
    }
    th_r[i] = row_sum / total;
  }

  for (int j = 0; j < nc; j++) {
    double col_sum = 0.0;
    for (int i = 0; i < nr; i++) {
      col_sum += mat(i, j);
    }
    th_c[j] = col_sum / total;
  }

  // Calculate thresholds
  NumericVector a(nr), b(nc);
  double cum_r = 0.0, cum_c = 0.0;

  for (int i = 0; i < nr; i++) {
    cum_r += th_r[i];
    a[i] = R::qnorm(std::min(cum_r, 1.0), 0.0, 1.0, 1, 0);
  }

  for (int j = 0; j < nc; j++) {
    cum_c += th_c[j];
    b[j] = R::qnorm(std::min(cum_c, 1.0), 0.0, 1.0, 1, 0);
  }

  // Calculate expected probabilities
  double log_lik = 0.0;
  for (int i = 0; i < nr; i++) {
    for (int j = 0; j < nc; j++) {
      double a_low = (i > 0) ? a[i-1] : -INFINITY;
      double a_high = a[i];
      double b_low = (j > 0) ? b[j-1] : -INFINITY;
      double b_high = b[j];

      double exp_prob = qBiNormal_cpp(a_high, b_high, rho) -
                       qBiNormal_cpp(a_high, b_low, rho) -
                       qBiNormal_cpp(a_low, b_high, rho) +
                       qBiNormal_cpp(a_low, b_low, rho);

      // Ensure numerical stability
      exp_prob = std::max(exp_prob, 1e-10);

      if (mat(i, j) > 0) {
        log_lik += mat(i, j) * std::log(exp_prob);
      }
    }
  }

  return -log_lik;
}

// Fast polychoric correlation calculation with C++ optimization
// [[Rcpp::export]]
double polychoric_cpp(IntegerVector x, IntegerVector y) {
  // Remove missing values
  int n = x.size();
  std::vector<int> x_clean, y_clean;

  for (int i = 0; i < n; i++) {
    if (!IntegerVector::is_na(x[i]) && !IntegerVector::is_na(y[i]) &&
        x[i] != -1 && y[i] != -1) {
      x_clean.push_back(x[i]);
      y_clean.push_back(y[i]);
    }
  }

  if (x_clean.size() < 4) {
    return NA_REAL;
  }

  // Create contingency table
  int min_x = *std::min_element(x_clean.begin(), x_clean.end());
  int max_x = *std::max_element(x_clean.begin(), x_clean.end());
  int min_y = *std::min_element(y_clean.begin(), y_clean.end());
  int max_y = *std::max_element(y_clean.begin(), y_clean.end());

  int nr = max_x - min_x + 1;
  int nc = max_y - min_y + 1;

  IntegerMatrix mat(nr, nc);
  for (int i = 0; i < nr; i++) {
    for (int j = 0; j < nc; j++) {
      mat(i, j) = 0;
    }
  }

  for (size_t k = 0; k < x_clean.size(); k++) {
    int row = x_clean[k] - min_x;
    int col = y_clean[k] - min_y;
    mat(row, col)++;
  }

  // Brent's method optimization
  double lower = -0.99;
  double upper = 0.99;
  double tol = 1e-6;
  int max_iter = 100;

  // Golden section search
  double phi = (1.0 + std::sqrt(5.0)) / 2.0;
  double c = upper - (upper - lower) / phi;
  double d = lower + (upper - lower) / phi;

  for (int iter = 0; iter < max_iter; iter++) {
    double fc = polychoric_likelihood_cpp(c, mat);
    double fd = polychoric_likelihood_cpp(d, mat);

    if (fc < fd) {
      upper = d;
      d = c;
      c = upper - (upper - lower) / phi;
    } else {
      lower = c;
      c = d;
      d = lower + (upper - lower) / phi;
    }

    if (std::abs(upper - lower) < tol) break;
  }

  double result = (upper + lower) / 2.0;
  return result;
}

// Vectorized polychoric correlation matrix
// [[Rcpp::export]]
NumericMatrix polychoric_matrix_cpp(IntegerMatrix data) {
  int ncol = data.ncol();
  NumericMatrix result(ncol, ncol);

  // Fill diagonal with 1s
  for (int i = 0; i < ncol; i++) {
    result(i, i) = 1.0;
  }

  // Calculate upper triangle
  for (int i = 0; i < ncol - 1; i++) {
    for (int j = i + 1; j < ncol; j++) {
      IntegerVector x = data(_, i);
      IntegerVector y = data(_, j);
      double corr = polychoric_cpp(x, y);
      result(i, j) = corr;
      result(j, i) = corr;
    }
  }

  return result;
}