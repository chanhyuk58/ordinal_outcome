#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// Pre-define the Normal PDF constant for speed
const double INV_SQRT_2PI = 0.3989422804014327;

// Helper: Vectorized Normal Kernel evaluation
inline double gaussian_kernel(double u) {
    return INV_SQRT_2PI * std::exp(-0.5 * u * u);
}

// Optimized Final KDE Logic 
// This calculates the density at points 'V_eval' using data 'V_data'
// Used for both Likelihood estimation and Post-Estimation Grid evaluation
// [[Rcpp::export]]
NumericVector ks_kde_eval_cpp(const NumericVector& V_eval,
                              const NumericVector& V_data,
                              const NumericVector& lambda,
                              double h) {
    int n_eval = V_eval.size();
    int n_data = V_data.size();
    NumericVector out(n_eval);

    for (int i = 0; i < n_eval; ++i) {
        double sum_k = 0.0;
        double val_i = V_eval[i];
        
        for (int k = 0; k < n_data; ++k) {
            double denom = lambda[k] * h;
            double u = (val_i - V_data[k]) / denom;
            sum_k += gaussian_kernel(u) / denom;
        }
        out[i] = sum_k / n_data;
    }
    return out;
}

// Optimized CDF Logic
// Essential for the link function F(v) calculation
// [[Rcpp::export]]
NumericVector ks_cdf_eval_cpp(const NumericVector& V_eval,
                              const NumericVector& V_data,
                              const NumericVector& lambda,
                              double h) {
    int n_eval = V_eval.size();
    int n_data = V_data.size();
    NumericVector out(n_eval);

    for (int i = 0; i < n_eval; ++i) {
        double sum_k = 0.0;
        double val_i = V_eval[i];
        
        for (int k = 0; k < n_data; ++k) {
            double denom = lambda[k] * h;
            double u = (val_i - V_data[k]) / denom;
            // R::pnorm is faster than manual erf implementation
            sum_k += R::pnorm(u, 0.0, 1.0, true, false);
        }
        out[i] = sum_k / n_data;
    }
    return out;
}

// Pilot KDE (Leave-one-out version) 
// Optimized for the initial bandwidth estimation
// [[Rcpp::export]]
NumericVector pilot_kde_loo_cpp(const NumericVector& V,
                                double h_p,
                                double sigma) {
    int n = V.size();
    NumericVector out(n);
    double denom_base = sigma * h_p;

    for (int i = 0; i < n; ++i) {
        double sum_k = 0.0;
        for (int k = 0; k < n; ++k) {
            if (i == k) continue;
            double u = (V[i] - V[k]) / denom_base;
            sum_k += gaussian_kernel(u);
        }
        out[i] = sum_k / ((n - 1) * denom_base);
    }
    return out;
}
