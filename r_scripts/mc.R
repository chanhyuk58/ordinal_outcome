# Monte Carlo simulation comparing ks_estimator and OLS
# Population data columns (at least):
#   "id" "y" "y_star" "eps" "x0" "x1" "x2" "x3" "x4" "thr1" "thr2" "thr3" "thr4"
# Index data columns:
#   "n" "rep" "idx"
#
# - Outcome: y
# - Predictors: x0, x1, x2, x3, x4 (as many as length(true_beta))
# - KS coefficients:
#     * reported as normalized (by second coefficient)
#     * bias/RMSE computed in normalized space vs true_beta_norm
# - OLS coefficients:
#     * reported unnormalized
#     * bias/RMSE computed in unnormalized space vs true_beta

# {{{
run_mc_simulation <- function(pop_csv,
                              idx_csv,
                              true_beta,
                              sample_sizes = c(200, 500, 1000, 2000),
                              n_rep = 20,
                              ks_args = list(B = 0,
                                             bandwidth_const = list(c_p = 1, c = 1),
                                             n_starts = 3,
                                             start_method = "ordered_logit"),
                              verbose = TRUE) {
  # ---------------------------------------------
  # 1. Load population and index data
  # ---------------------------------------------
  pop_data <- read.csv(pop_csv)
  idx_data <- read.csv(idx_csv)

  # Check required columns in index data: n, rep, idx
  required_idx_cols <- c("n", "rep", "idx")
  if (!all(required_idx_cols %in% colnames(idx_data))) {
    stop("idx_csv must contain columns: 'n', 'rep', 'idx'.")
  }

  # ---------------------------------------------
  # 2. Define y and X based on known column names
  # ---------------------------------------------
  # Outcome variable name
  y_name <- "y"
  if (!(y_name %in% colnames(pop_data))) {
    stop("Population data must contain column 'y' as the outcome.")
  }

  # Predictor names derived from true_beta length: x0, x1, ..., x(p-1)
  p <- length(true_beta)
  x_names <- paste0("x", 0:(p - 1))

  if (!all(x_names %in% colnames(pop_data))) {
    stop(
      "Population data must contain predictor columns: ",
      paste(x_names, collapse = ", "),
      "."
    )
  }

  # True normalized beta for KS comparison (divide by second coefficient)
  if (true_beta[2] == 0) {
    stop("true_beta[2] is zero; cannot normalize by the second coefficient.")
  }
  true_beta_norm <- true_beta / true_beta[2]

  # ---------------------------------------------
  # 3. Helper: one-sample estimation for KS and OLS
  # ---------------------------------------------
  estimate_one_sample <- function(sample_idx) {
    # Subset population
    samp <- pop_data[sample_idx, , drop = FALSE]

    # Ensure Y is treated as ordinal factor for ks_estimator
    samp[[y_name]] <- factor(samp[[y_name]], ordered = TRUE)

    # Construct formula Y ~ x0 + x1 + ... + x(p-1)
    form <- as.formula(
      paste(y_name, "~", paste(x_names, collapse = " + "))
    )

    # KS estimator
    ks_call_args <- c(
      list(formula = form, data = samp),
      ks_args
    )
    ks_fit <- try(do.call(ks_estimator, ks_call_args), silent = TRUE)
    if (inherits(ks_fit, "try-error")) {
      ks_coef <- rep(NA_real_, p)
    } else {
      ks_coef <- try(as.numeric(ks_fit$coefficients), silent = TRUE)
      if (inherits(ks_coef, "try-error") || length(ks_coef) != p) {
        ks_coef <- rep(NA_real_, p)
      }
    }

    # OLS: linear regression without intercept
    ols_form <- as.formula(
      paste(y_name, "~", paste(x_names, collapse = " + "), "- 1")
    )
    ols_fit <- try(lm(ols_form, data = samp), silent = TRUE)
    if (inherits(ols_fit, "try-error")) {
      ols_coef <- rep(NA_real_, p)
    } else {
      co <- try(coef(ols_fit), silent = TRUE)
      if (inherits(co, "try-error") || length(co) != p) {
        ols_coef <- rep(NA_real_, p)
      } else {
        ols_coef <- as.numeric(co)
      }
    }

    list(ks = ks_coef, ols = ols_coef)
  }

  # ---------------------------------------------
  # 4. Storage for raw and normalized estimates
  # ---------------------------------------------
  # mc_results: for each sample size n, store list(raw = ..., norm = ...)
  mc_results <- list()

  for (n_samp in sample_sizes) {
    if (verbose) cat("Running simulations for sample size n =", n_samp, "\n")

    # Raw coefficient estimates: model x rep x coef
    est_array <- array(NA_real_, dim = c(2, n_rep, p),
                       dimnames = list(
                         model = c("ks", "ols"),
                         rep = paste0("rep", 1:n_rep),
                         coef = paste0("beta", 1:p)
                       ))

    for (r in 1:n_rep) {
      if (verbose) cat("  Replication", r, "...\n")

      # Get indices for this sample size and replication
      idx_sub <- idx_data[idx_data$n == n_samp & idx_data$rep == r, , drop = FALSE]
      if (nrow(idx_sub) != n_samp) {
        stop(
          sprintf("For n = %d and rep = %d, expected %d indices, got %d.",
                  n_samp, r, n_samp, nrow(idx_sub))
        )
      }

      sample_idx <- idx_sub$idx

      # Estimate KS and OLS on this sample
      est <- estimate_one_sample(sample_idx)

      est_array["ks", r, ] <- est$ks
      est_array["ols", r, ] <- est$ols
    }

    # ---------------------------------------------
    # 5. Normalize by second coefficient (for KS comparison, optional for OLS)
    # ---------------------------------------------
    norm_array <- est_array  # will hold normalized values

    for (m in c("ks", "ols")) {
      for (r in 1:n_rep) {
        b <- est_array[m, r, ]
        if (any(!is.finite(b))) {
          norm_array[m, r, ] <- NA_real_
          next
        }
        b2 <- b[2]
        if (!is.finite(b2) || abs(b2) < 1e-8) {
          norm_array[m, r, ] <- NA_real_
        } else {
          norm_array[m, r, ] <- b / b2
        }
      }
    }

    # Store both raw and normalized arrays
    mc_results[[as.character(n_samp)]] <- list(raw = est_array, norm = norm_array)
  }

  # ---------------------------------------------
  # 6. Compute bias and RMSE across replications
  # ---------------------------------------------
  out_list <- list()

  for (n_samp in sample_sizes) {
    res_n <- mc_results[[as.character(n_samp)]]
    est_array <- res_n$raw
    norm_array <- res_n$norm

    for (m in c("ks", "ols")) {
      for (k in 1:p) {
        if (m == "ks") {
          # --- KS: bias/RMSE in normalized space ---
          est_norm_k <- as.numeric(norm_array[m, , k])
          est_norm_k <- est_norm_k[is.finite(est_norm_k)]

          if (length(est_norm_k) == 0L) {
            mean_norm_k <- NA_real_
            bias_k <- NA_real_
            rmse_k <- NA_real_
          } else {
            mean_norm_k <- mean(est_norm_k)
            bias_k <- mean_norm_k - true_beta_norm[k]
            rmse_k <- sqrt(mean((est_norm_k - true_beta_norm[k])^2))
          }

          coef_est_k <- mean_norm_k
          true_value_k <- true_beta_norm[k]
          target_scale <- "normalized"

        } else {
          # --- OLS: bias/RMSE in unnormalized space ---
          est_raw_k <- as.numeric(est_array[m, , k])
          est_raw_k <- est_raw_k[is.finite(est_raw_k)]

          if (length(est_raw_k) == 0L) {
            mean_raw_k <- NA_real_
            bias_k <- NA_real_
            rmse_k <- NA_real_
          } else {
            mean_raw_k <- mean(est_raw_k)
            bias_k <- mean_raw_k - true_beta[k]
            rmse_k <- sqrt(mean((est_raw_k - true_beta[k])^2))
          }

          coef_est_k <- mean_raw_k
          true_value_k <- true_beta[k]
          target_scale <- "raw"
        }

        out_list[[length(out_list) + 1L]] <- data.frame(
          sample_size = n_samp,
          model = m,
          coef_index = k,
          target_scale = target_scale,
          true_value = true_value_k,
          coef_estimate = coef_est_k,
          bias = bias_k,
          rmse = rmse_k
        )
      }
    }
  }

  summary_df <- do.call(rbind, out_list)
  rownames(summary_df) <- NULL

  list(
    summary = summary_df,
    estimates = mc_results,
    true_beta = true_beta,
    true_beta_norm = true_beta_norm
  )
}
# }}}

# -------------------------------------------------------------------
# Example of how to call (adapt paths / true_beta / ks_args)
# -------------------------------------------------------------------
source("./ks_functions.R")

pop_csv <- "../sim_data/population_lognormal_N1000000.csv"
idx_csv <- "../sim_data/indices_lognormal_N1000000.csv"
true_beta <- c(1.0, 0.8, -0.8, 0.5, -0.5)  # unnormalized true coefficients

# Make sure ks_estimator is already loaded/sourced

mc_res <- run_mc_simulation(
  pop_csv = pop_csv,
  idx_csv = idx_csv,
  true_beta = true_beta,
  sample_sizes = c(200, 500, 1000, 2000),
  # sample_sizes = c(200),
  n_rep = 20,
  ks_args = list(
    B = 0,
    bandwidth_const = list(c_p = 1, c = 1),
    n_starts = 3,
    start_method = "ordered_logit"
  ),
  verbose = TRUE
)

# Summary table with bias and RMSE
results <- mc_res$summary
results[results$coef_index == 1, ]
write.csv(results, file = "../mc_results/mc_100_lognormal_ks.csv")
