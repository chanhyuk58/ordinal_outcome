# -------------------------------------------------------------------
# MC Simulation: KS (Rescaled to sigma=1) vs OLS (Original Scale)
# -------------------------------------------------------------------
source("./ks_estimator.R") # Ensure this includes the revised ks_rescale

run_mc_simulation <- function(pop_csv, # {{{
                              idx_csv,
                              true_beta,
                              pop_sigma = 1.0, # The SD of the latent error
                              sample_sizes = c(500, 1000),
                              n_rep = 20,
                              ks_args = list(B = 0, n_starts = 3),
                              verbose = TRUE) {
  
  pop_data <- read.csv(pop_csv)
  idx_data <- read.csv(idx_csv)
  
  p <- length(true_beta)
  x_names <- paste0("x", 0:(p - 1))
  
  # Target for KS: Latent effect standardized by noise
  target_ks <- true_beta / pop_sigma
  
  # Target for OLS: The latent coefficients as if 1-5 were cardinal
  target_ols <- true_beta 

  estimate_one_sample <- function(sample_idx) {
    samp <- pop_data[sample_idx, , drop = FALSE]
    
    # --- 1. KS Estimator (Rescaled) ---
    samp_ks <- samp
    samp_ks$y <- factor(samp_ks$y, ordered = TRUE)
    form_ks <- as.formula(paste("y ~", paste(x_names, collapse = " + ")))
    
    ks_fit <- try(ks_estimator(formula = form_ks, data = samp_ks, 
                               B = ks_args$B, n_starts = ks_args$n_starts), 
                  silent = TRUE)
    
    if (!inherits(ks_fit, "try-error")) {
      # Rescale to the sigma=1 scale
      ks_rescaled <- try(ks_rescale(ks_fit), silent = TRUE)
      ks_coef <- if(!inherits(ks_rescaled, "try-error")) ks_rescaled$coefficients_rescaled else rep(NA, p)
    } else {
      ks_coef <- rep(NA, p)
    }

    # --- 2. OLS (Original Scale) ---
    # No rescaling here. Direct lm on the categorical labels.
    ols_form <- as.formula(paste("as.numeric(y) ~", paste(x_names, collapse = " + "), "- 1"))
    ols_fit <- try(lm(ols_form, data = samp), silent = TRUE)
    
    if (!inherits(ols_fit, "try-error")) {
      ols_coef <- as.numeric(coef(ols_fit))
    } else {
      ols_coef <- rep(NA, p)
    }

    list(ks = ks_coef, ols = ols_coef)
  }

  results_accumulator <- list()

  for (n_samp in sample_sizes) {
    if (verbose) cat("\nRunning N =", n_samp, "\n")
    
    estimates_ks <- matrix(NA, n_rep, p)
    estimates_ols <- matrix(NA, n_rep, p)

    for (r in 1:n_rep) {
      if (verbose) cat(r, " ")
      idx_sub <- idx_data$idx[idx_data$n == n_samp & idx_data$rep == r]
      
      est <- estimate_one_sample(idx_sub)
      estimates_ks[r, ] <- est$ks
      estimates_ols[r, ] <- est$ols
    }

    # Compute Statistics for KS
    for (k in 1:p) {
      ks_vals <- estimates_ks[, k][is.finite(estimates_ks[, k])]
      if(length(ks_vals) > 0) {
        avg_ks <- mean(ks_vals)
        results_accumulator[[length(results_accumulator) + 1]] <- data.frame(
          n = n_samp, model = "ks_rescaled", coef = x_names[k],
          target = target_ks[k], estimate = avg_ks,
          bias = avg_ks - target_ks[k], rmse = sqrt(mean((ks_vals - target_ks[k])^2))
        )
      }
      
      ols_vals <- estimates_ols[, k][is.finite(estimates_ols[, k])]
      if(length(ols_vals) > 0) {
        avg_ols <- mean(ols_vals)
        results_accumulator[[length(results_accumulator) + 1]] <- data.frame(
          n = n_samp, model = "ols_raw", coef = x_names[k],
          target = target_ols[k], estimate = avg_ols,
          bias = avg_ols - target_ols[k], rmse = sqrt(mean((ols_vals - target_ols[k])^2))
        )
      }
    }
  }

  return(do.call(rbind, results_accumulator))
}
# }}}

# -------------------------------------------------------------------
# Simulation Parameters
# -------------------------------------------------------------------
# If error ~ rlnorm(0, 1), pop_sigma is approx 2.16
# true_beta are the latent coefficients

error_type <- "lognormal"
true_sigma <- 2.1611974158950877
beta_true <- c(1.0, 0.8, -0.8, 0.5, -0.5)
# beta_true <- c(1.6, 0.7, -1.2, 1.0, -0.8)

ns <- c(200, 500, 1000, 2000)
R <- 100

outfile <- paste0("../mc_results/mc_",R,"_ols_ks_",error_type,".csv")


mc_res <- run_mc_simulation(
  pop_csv = paste0("../sim_data/population_", error_type, "_N1000000.csv"),
  idx_csv = paste0("../sim_data/indices_", error_type, "_N1000000.csv"),
  true_beta = beta_true,
  pop_sigma = true_sigma, 
  sample_sizes = ns,
  n_rep = R
)

print(mc_res[mc_res$coef == "x0", ])

write.csv(mc_res, file = outfile, row.names = FALSE)
