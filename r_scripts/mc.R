# -------------------------------------------------------------------
# Monte Carlo Simulation: KS (Rescaled) vs OLS (Rescaled)
# -------------------------------------------------------------------
source("./ks_functions.R") # Should contain ks_estimator and ks_rescale

run_mc_simulation <- function(pop_csv,
                              idx_csv,
                              true_beta,
                              pop_sigma = 1.0, # Actual SD of population error
                              sample_sizes = c(500, 1000),
                              n_rep = 20,
                              ks_args = list(B = 0, n_starts = 2),
                              verbose = TRUE) {
  
  pop_data <- read.csv(pop_csv)
  idx_data <- read.csv(idx_csv)
  
  p <- length(true_beta)
  x_names <- paste0("x", 0:(p - 1))
  
  # TARGET: The "Standardized Effect" (beta / sigma_error)
  # This is the gold standard for comparing different statistical models.
  true_beta_std <- true_beta / pop_sigma

  estimate_one_sample <- function(sample_idx) {
    samp <- pop_data[sample_idx, , drop = FALSE]
    
    # 1. KS Estimator (Rescaled to sigma=1)
    samp_ks <- samp
    samp_ks$y <- factor(samp_ks$y, ordered = TRUE)
    form_ks <- as.formula(paste("y ~", paste(x_names, collapse = " + ")))
    
    ks_fit <- try(ks_estimator(formula = form_ks, data = samp_ks, B = ks_args$B, 
                               n_starts = ks_args$n_starts), silent = TRUE)
    
    if (!inherits(ks_fit, "try-error")) {
      # Rescale KS to the sigma=1 "Probit-like" scale
      ks_rescaled <- try(ks_rescale(ks_fit), silent = TRUE)
      ks_coef <- if(!inherits(ks_rescaled, "try-error")) ks_rescaled$coefficients_rescaled else rep(NA, p)
    } else {
      ks_coef <- rep(NA, p)
    }

    # 2. OLS (Rescaled to sigma=1 for comparability)
    # We treat y as a number, then divide by the residual standard deviation
    ols_form <- as.formula(paste("as.numeric(y) ~", paste(x_names, collapse = " + "), "- 1"))
    ols_fit <- try(lm(ols_form, data = samp), silent = TRUE)
    
    if (!inherits(ols_fit, "try-error")) {
      ols_raw <- coef(ols_fit)
      ols_err_sigma <- summary(ols_fit)$sigma
      # Scaling OLS into the 'latent scale' equivalent
      ols_coef <- ols_raw / ols_err_sigma 
    } else {
      ols_coef <- rep(NA, p)
    }

    list(ks = ks_coef, ols = ols_coef)
  }

  results_accumulator <- list()

  for (n_samp in sample_sizes) {
    if (verbose) cat("\nSimulating n =", n_samp, "\n")
    
    # Storage for this N
    estimates <- array(NA, dim = c(2, n_rep, p), 
                       dimnames = list(c("ks", "ols"), NULL, x_names))

    for (r in 1:n_rep) {
      if (verbose) cat(r, " ")
      idx_sub <- idx_data$idx[idx_data$n == n_samp & idx_data$rep == r]
      
      est <- estimate_one_sample(idx_sub)
      estimates["ks", r, ] <- est$ks
      estimates["ols", r, ] <- est$ols
    }

    # Summary Statistics
    for (m in c("ks", "ols")) {
      for (k in 1:p) {
        vals <- estimates[m, , k]
        vals <- vals[is.finite(vals)]
        
        if(length(vals) > 0) {
          avg_est <- mean(vals)
          bias <- avg_est - true_beta_std[k]
          rmse <- sqrt(mean((vals - true_beta_std[k])^2))
          
          results_accumulator[[length(results_accumulator) + 1]] <- data.frame(
            n = n_samp, model = m, coef = x_names[k],
            true_std = true_beta_std[k], est = avg_est, 
            bias = bias, rmse = rmse, valid_reps = length(vals)
          )
        }
      }
    }
  }

  return(do.call(rbind, results_accumulator))
}

# -------------------------------------------------------------------
# Execution Logic
# -------------------------------------------------------------------
# Note: Set pop_sigma based on your population data-generating process.
pop_sigma_val <- 2.16 

mc_results <- run_mc_simulation(
  pop_csv = "../sim_data/population_lognormal.csv",
  idx_csv = "../sim_data/indices_lognormal.csv",
  true_beta = c(1.0, 0.8, -0.8, 0.5, -0.5),
  pop_sigma = pop_sigma_val,
  sample_sizes = c(500, 1000),
  n_rep = 50
)

# View results for Treatment 1
print(subset(mc_results, coef == "x0"))
