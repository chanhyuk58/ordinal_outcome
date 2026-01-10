// ks_kde.cpp
// [[Rcpp::depends(Rcpp)]]

#include <Rcpp.h>

using namespace Rcpp;

// Pilot KDE with leave-one-out
// side = 1 => group Y <= j
// side = 0 => group Y > j
// [[Rcpp::export]]
NumericVector pilot_kde_cpp(const NumericVector& Y,
                            const NumericVector& V,
                            int j,
                            double h_p,
                            double sigma,
                            int side) {
  int n = V.size();
  NumericVector out(n);

  if (n == 0) return out;
  if (!R_finite(sigma) || sigma <= 0.0) return out;

  double denom_base = sigma * h_p;
  if (!R_finite(denom_base) || denom_base <= 0.0) return out;

  if (side == 1) {
    // group: Y <= j
    int n1 = 0;
    for (int i = 0; i < n; ++i) {
      if (Y[i] <= j) n1++;
    }
    if (n1 == 0) return out;

    for (int i = 0; i < n; ++i) {
      double s = 0.0;
      for (int k = 0; k < n; ++k) {
        if (k == i) continue;
        if (Y[k] > j) continue;
        double u = (V[i] - V[k]) / denom_base;
        s += R::dnorm(u, 0.0, 1.0, false) / denom_base;
      }
      out[i] = s / n1;
    }

  } else {
    // group: Y > j
    int n0 = 0;
    for (int i = 0; i < n; ++i) {
      if (Y[i] > j) n0++;
    }
    if (n0 == 0) return out;

    for (int i = 0; i < n; ++i) {
      double s = 0.0;
      for (int k = 0; k < n; ++k) {
        if (k == i) continue;
        if (Y[k] <= j) continue;
        double u = (V[i] - V[k]) / denom_base;
        s += R::dnorm(u, 0.0, 1.0, false) / denom_base;
      }
      out[i] = s / n0;
    }
  }

  return out;
}

// Final KDE with local bandwidths (no leave-one-out)
// side = 1 => group Y <= j
// side = 0 => group Y > j
// [[Rcpp::export]]
NumericVector final_kde_cpp(const NumericVector& Y,
                            const NumericVector& V,
                            int j,
                            double h,
                            const NumericVector& lambda,
                            int side) {
  int n = V.size();
  NumericVector out(n);

  if (n == 0) return out;
  if (lambda.size() != n) {
    stop("lambda must have length equal to length(V).");
  }

  int count = 0;
  if (side == 1) {
    for (int k = 0; k < n; ++k) {
      if (Y[k] <= j) count++;
    }
  } else {
    for (int k = 0; k < n; ++k) {
      if (Y[k] > j) count++;
    }
  }
  if (count == 0) {
    return out;
  }

  for (int i = 0; i < n; ++i) {
    double s = 0.0;
    for (int k = 0; k < n; ++k) {
      if (side == 1) {
        if (Y[k] > j) continue;
      } else {
        if (Y[k] <= j) continue;
      }
      double lam = lambda[k];
      if (!R_finite(lam) || lam <= 0.0) continue;
      double denom = lam * h;
      if (!R_finite(denom) || denom <= 0.0) continue;
      double u = (V[i] - V[k]) / denom;
      s += R::dnorm(u, 0.0, 1.0, false) / denom;
    }
    out[i] = s / count;
  }

  return out;
}
