# -------------------------------------------------------------------
# R wrappers around C++ KDE functions, with R fallback
# -------------------------------------------------------------------
Rcpp::sourceCpp("ks_kde.cpp")

pilot_kde <- function(Y, V, l, h_p, sigma, j) {
  n <- length(V)
  if (n == 0L) return(numeric(0))

  # Regularize sigma if degenerate; make regularization shrink with n
  if (!is.finite(sigma) || sigma <= 0) {
    sigma <- max(stats::sd(V), n^(-1/2))
    if (!is.finite(sigma) || sigma <= 0) sigma <- n^(-1/2)
  }

  side <- if (l == 1L) 1L else 0L

  # Use C++ implementation if available
  if (exists("pilot_kde_cpp", mode = "function")) {
    res <- pilot_kde_cpp(
      Y = as.numeric(Y),
      V = as.numeric(V),
      j = as.integer(j),
      h_p = h_p,
      sigma = sigma,
      side = side
    )
    return(res)
  }

  # Pure R fallback (slower)
  out <- numeric(n)
  denom_base <- sigma * h_p
  if (!is.finite(denom_base) || denom_base <= 0) return(out)

  if (side == 1L) {
    idx <- which(Y <= j)
    n1 <- length(idx)
    if (n1 == 0L) return(out)
    for (i in seq_len(n)) {
      s <- 0
      for (k in idx) {
        if (k == i) next
        u <- (V[i] - V[k]) / denom_base
        s <- s + stats::dnorm(u) / denom_base
      }
      out[i] <- s / n1
    }
  } else {
    idx <- which(Y > j)
    n0 <- length(idx)
    if (n0 == 0L) return(out)
    for (i in seq_len(n)) {
      s <- 0
      for (k in idx) {
        if (k == i) next
        u <- (V[i] - V[k]) / denom_base
        s <- s + stats::dnorm(u) / denom_base
      }
      out[i] <- s / n0
    }
  }

  out
}

final_kde <- function(Y, V, l, lambda, h, j) {
  n <- length(V)
  if (n == 0L) return(numeric(0))
  if (length(lambda) != n) stop("lambda must have same length as V.")

  side <- if (l == 1L) 1L else 0L

  # Use C++ implementation if available
  if (exists("final_kde_cpp", mode = "function")) {
    res <- final_kde_cpp(
      Y = as.numeric(Y),
      V = as.numeric(V),
      j = as.integer(j),
      h = h,
      lambda = as.numeric(lambda),
      side = side
    )
    return(res)
  }

  # Pure R fallback (slower)
  out <- numeric(n)
  if (side == 1L) {
    idx <- which(Y <= j)
  } else {
    idx <- which(Y > j)
  }
  count <- length(idx)
  if (count == 0L) return(out)

  for (i in seq_len(n)) {
    s <- 0
    for (k in idx) {
      lam <- lambda[k]
      if (!is.finite(lam) || lam <= 0) next
      denom <- lam * h
      if (!is.finite(denom) || denom <= 0) next
      u <- (V[i] - V[k]) / denom
      s <- s + stats::dnorm(u) / denom
    }
    out[i] <- s / count
  }

  out
}

# -------------------------------------------------------------------
# Local bandwidth computation with n-dependent regularization
# -------------------------------------------------------------------

compute_local_bandwidth <- function(pilot_density, sigma, n_group, delta) {
  pd <- as.numeric(pilot_density)
  if (length(pd) == 0L) {
    return(numeric(0))
  }

  # Floor nonpositive or non-finite densities with n-dependent floor
  eps <- n_group^(-2)  # shrinks with group size
  bad <- !is.finite(pd) | (pd <= 0)
  pd[bad] <- eps

  # Geometric mean
  m <- exp(mean(log(pd)))
  l <- pd / m

  # Regularize sigma if needed, with n-dependent floor
  if (!is.finite(sigma) || sigma <= 0) {
    sigma <- max(n_group^(-1/2), 1e-8)
  }

  # Damping weights and lambda (as in original KS-style construction)
  a_n <- 1 / log(n_group)
  exponent <- n_group^((1/40 - delta * 1/20) / 2)
  d <- 1 / (1 + exp(-exponent * (l - a_n)))

  lambda <- sigma / sqrt(l * d + a_n * (1 - d))
  lambda
}

# -------------------------------------------------------------------
# Extract Link Function and Calculate Variance
# -------------------------------------------------------------------
calculate_ks_variance <- function(V_hat, Y, J, lambda_list, h, n_grid = 30000) {
  # SIGNIFICANTLY expand the grid to capture Log-normal tails
  # Instead of 3 SDs, use 10 SDs or use the min/max of the potential gaps
  v_sd <- stats::sd(V_hat)
  grid <- seq(min(V_hat) - 10 * v_sd, 
              max(V_hat) + 10 * v_sd, 
              length.out = n_grid)
  
  pdf_accum <- numeric(n_grid)
  valid_thresholds <- 0
  
  for (j in 1:J) {
    V1 <- V_hat[Y <= j]; V0 <- V_hat[Y > j]
    if (length(V1) < 5 || length(V0) < 5) next
    
    # Evaluate density using the SAME local bandwidths found at optimum
    g1 <- ks_kde_eval_cpp(grid, V1, lambda_list[[j]]$l1, h)
    g0 <- ks_kde_eval_cpp(grid, V0, lambda_list[[j]]$l0, h)
    
    p1 <- length(V1)/length(V_hat); p0 <- length(V0)/length(V_hat)
    pdf_accum <- pdf_accum + (p1 * g1 + p0 * g0)
    valid_thresholds <- valid_thresholds + 1
  }
  
  pdf_avg <- pdf_accum / valid_thresholds
  dx <- grid[2] - grid[1]
  
  # Normalize to ensure it is a valid probability distribution
  area <- sum(pdf_avg) * dx
  pdf_avg <- pdf_avg / area
  
  # Numerical Integration
  mu  <- sum(grid * pdf_avg) * dx
  var <- sum(((grid - mu)^2) * pdf_avg) * dx
  
  return(list(var = var, pdf = pdf_avg, grid = grid, mu = mu))
}

# -------------------------------------------------------------------
# Quasi-likelihood and optimization helpers
# -------------------------------------------------------------------

# Normalize beta to unit norm and fix sign (beta[1] > 0)
.normalize_beta <- function(b) {
  b <- as.numeric(b)
  if (any(!is.finite(b))) return(rep(NA_real_, length(b)))
  if (all(b == 0)) b[1] <- 1
  nb <- sqrt(sum(b^2))
  if (!is.finite(nb) || nb == 0) return(rep(NA_real_, length(b)))
  b <- b / nb
  if (b[1] < 0) b <- -b
  b
}
quasi_likelihood <- function(beta_raw, Y, X, J, h_p, h, delta, trim) {
  n <- length(Y)
  beta <- .normalize_beta(beta_raw)
  if (any(is.na(beta))) return(1e100)

  V_hat <- as.numeric(X %*% beta)
  P_est_mat <- matrix(0, nrow = J + 1, ncol = n) # Store probabilities

  for (j in 1:J) {
    V1 <- V_hat[Y <= j]; V0 <- V_hat[Y > j]
    n1 <- length(V1); n0 <- length(V0)
    
    if (n1 == 0) { P_est_mat[j, ] <- 0; next }
    if (n0 == 0) { P_est_mat[j, ] <- 1; next }

    # Pilot KDE (using optimized LOO function)
    s1 <- max(stats::sd(V1), n1^(-0.5)); s0 <- max(stats::sd(V0), n0^(-0.5))
    g1_p <- pilot_kde_loo_cpp(V1, h_p, s1)
    g0_p <- pilot_kde_loo_cpp(V0, h_p, s0)
    
    # Map back to full size for lambda calculation
    # (Easier to do in R to keep track of indices)
    g1_full <- numeric(n); g1_full[Y <= j] <- g1_p; g1_full[Y > j] <- 1e-6
    g0_full <- numeric(n); g0_full[Y > j] <- g0_p; g0_full[Y <= j] <- 1e-6

    l1 <- compute_local_bandwidth(g1_full[Y <= j], s1, n1, delta)
    l0 <- compute_local_bandwidth(g0_full[Y > j], s0, n0, delta)

    # Final KDE using data groups to evaluate ALL indices
    g1_f <- ks_kde_eval_cpp(V_hat, V1, l1, h)
    g0_f <- ks_kde_eval_cpp(V_hat, V0, l0, h)

    p1 <- n1 / n; p0 <- n0 / n
    denom <- p1 * g1_f + p0 * g0_f
    denom[denom < n^-3] <- n^-3 # Robustness floor
    
    P_est_mat[j, ] <- (p1 * g1_f) / denom
  }

  # Probability of being in specific category
  # P(Y=k) = P(Y<=k) - P(Y<=k-1)
  prob_cat <- numeric(n)
  for (i in 1:n) {
    curr_y <- Y[i]
    p_upper <- if (curr_y == (J + 1)) 1 else P_est_mat[curr_y, i]
    p_lower <- if (curr_y == 1) 0 else P_est_mat[curr_y - 1, i]
    prob_cat[i] <- p_upper - p_lower
  }

  prob_cat[prob_cat <= 0 | !is.finite(prob_cat)] <- 1e-10
  return(-sum(log(prob_cat)))
}

fit_ks_once <- function(Y, X, J, h_p, h, delta, trim, control,
                        verbose = FALSE,
                        n_starts = 3L,
                        start_method = c("ordered_logit", "lm")) {
  n <- length(Y)
  p <- ncol(X)
  start_method <- match.arg(start_method)

  # Helper to get candidate starting directions
  candidate_starts <- list()

  # Try ordered logit (MASS::polr) as starting direction if available
  if (start_method == "ordered_logit" &&
      requireNamespace("MASS", quietly = TRUE)) {
    y_factor <- factor(Y, ordered = TRUE)
    df_start <- data.frame(y = y_factor, X)
    colnames(df_start) <- c("y", colnames(X))
    form <- stats::as.formula("y ~ .")
    polr_fit <- try(MASS::polr(form, data = df_start, method = "logistic", Hessian = FALSE),
                    silent = TRUE)
    if (!inherits(polr_fit, "try-error")) {
      s_polr <- try(as.numeric(stats::coef(polr_fit)), silent = TRUE)
      if (!inherits(s_polr, "try-error") &&
          length(s_polr) == p && all(is.finite(s_polr))) {
        candidate_starts[[length(candidate_starts) + 1L]] <- .normalize_beta(s_polr)
      }
    }
  }

  # OLS start as fallback / additional candidate
  lm_fit <- try(stats::lm.fit(x = X, y = Y), silent = TRUE)
  if (!inherits(lm_fit, "try-error")) {
    s_lm <- try(as.numeric(lm_fit$coefficients), silent = TRUE)
    if (!inherits(s_lm, "try-error") &&
        length(s_lm) == p && any(is.finite(s_lm))) {
      candidate_starts[[length(candidate_starts) + 1L]] <- .normalize_beta(s_lm)
    }
  }

  # If no valid candidate so far, default to (1, 0, ..., 0)
  if (length(candidate_starts) == 0L) {
    base <- rep(0, p); base[1] <- 1
    candidate_starts[[1L]] <- base
  }

  # Deduplicate candidates (by rounding)
  key <- function(b) paste(round(b, 3), collapse = "_")
  keys <- vapply(candidate_starts, key, character(1L))
  unique_idx <- !duplicated(keys)
  candidate_starts <- candidate_starts[unique_idx]

  # Add random perturbations around base starts if we want more
  total_needed <- max(1L, as.integer(n_starts))
  base_count <- length(candidate_starts)
  if (base_count < total_needed) {
    set.seed(NULL)
    for (i in seq_len(total_needed - base_count)) {
      base <- candidate_starts[[((i - 1L) %% base_count) + 1L]]
      noise <- stats::rnorm(p, mean = 0, sd = 0.2)
      b_new <- .normalize_beta(base + noise)
      if (!any(is.na(b_new))) {
        candidate_starts[[length(candidate_starts) + 1L]] <- b_new
      }
    }
  }

  # Limit to n_starts
  if (length(candidate_starts) > total_needed) {
    candidate_starts <- candidate_starts[seq_len(total_needed)]
  }

  # Run optim from each start and keep the best
  best_val <- Inf
  best_par <- NULL
  best_optim <- NULL

  for (start in candidate_starts) {
    opt_res <- stats::optim(
      par = start,
      fn = quasi_likelihood,
      Y = Y,
      X = X,
      J = J,
      h_p = h_p,
      h = h,
      delta = delta,
      trim = trim,
      method = "BFGS",
      control = control
    )
    if (opt_res$value < best_val) {
      best_val <- opt_res$value
      best_par <- opt_res$par
      best_optim <- opt_res
    }
  }

  list(
    coef = .normalize_beta(best_par),
    value = best_val,
    convergence = best_optim$convergence,
    counts = best_optim$counts,
    message = best_optim$message
  )
}

# -------------------------------------------------------------------
# Rescaling Function
# -------------------------------------------------------------------
ks_rescale <- function(object) {
  if (!inherits(object, "ks_ordinal")) stop("Input must be a ks_ordinal object.")
  
  # 1. Recover index and settings
  V_hat <- as.numeric(object$X %*% object$coefficients)
  
  # 2. Estimate Variance
  # Note: You'll need to store lambdas during fit or recalculate here
  # For brevity, we recalculate variance using final coefficients
  var_res <- calculate_ks_variance(V_hat, object$Y, object$J, 
                                   object$lambda_list, object$bandwidth$h)
  
  sigma_ks <- sqrt(var_res$var)
  
  # 3. Rescale coefficients
  new_coefs <- object$coefficients / sigma_ks
  new_se <- object$bootstrap_se / sigma_ks
  
  object$coefficients_rescaled <- new_coefs
  object$se_rescaled <- new_se
  object$error_sigma <- sigma_ks
  object$error_dist <- var_res # contains grid and pdf for plotting
  
  return(object)
}



# -------------------------------------------------------------------
# Main KS estimator (S3)
# -------------------------------------------------------------------

ks_estimator <- function(formula,
                         data,
                         B = 100L,
                         trim = 0.95,
                         control = list(),
                         verbose = FALSE,
                         keep_boot = TRUE,
                         bandwidth_const = list(c_p = 1, c = 1),
                         n_starts = 3L,
                         start_method = c("ordered_logit", "lm")) {
  
  if (!requireNamespace("boot", quietly = TRUE)) {
    stop("Package 'boot' is required.")
  }

  # 1. Data Preparation
  data <- as.data.frame(data)
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  terms_obj <- attr(mf, "terms")
  y_raw <- stats::model.response(mf)
  X <- stats::model.matrix(terms_obj, data = mf)

  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }

  y_factor <- factor(y_raw, ordered = TRUE)
  Y <- as.integer(y_factor) # Categories 1, 2, ..., J
  levels_Y <- levels(y_factor)
  J_categories <- length(levels_Y)
  J_thresholds <- J_categories - 1 # We estimate J-1 boundaries
  n <- length(Y)

  # 2. Bandwidth Setup
  delta <- 1/15
  a <- (1/10 + (3 + delta)/3) / 5    
  b <- 3 * ((3 + delta)/20 + 1/6) / 10  
  c_p <- if (!is.null(bandwidth_const$c_p)) bandwidth_const$c_p else 1
  c_h <- if (!is.null(bandwidth_const$c)) bandwidth_const$c else 1
  h_p <- c_p * n^(-a)
  h   <- c_h * n^(-b)

  # 3. Full-sample fit (Initial Normalization: ||beta||=1)
  full_fit <- fit_ks_once(
    Y = Y, X = X, J = J_thresholds, h_p = h_p, h = h, 
    delta = delta, trim = trim, control = control,
    verbose = verbose, n_starts = n_starts, start_method = start_method
  )

  coef_hat <- as.numeric(full_fit$coef)
  names(coef_hat) <- colnames(X)

  # -----------------------------------------------------------------
  # NEW: Extract Final bandwidths (lambdas) for Post-Estimation
  # -----------------------------------------------------------------
  # We re-run the final KDE step once with the best beta to save the state
  V_final <- as.numeric(X %*% coef_hat)
  final_lambda_list <- list()

  for (j in 1:J_thresholds) {
    V1 <- V_final[Y <= j]; V0 <- V_final[Y > j]
    n1 <- length(V1); n0 <- length(V0)
    
    if (n1 > 1 && n0 > 1) {
      s1 <- max(stats::sd(V1), n1^(-0.5))
      s0 <- max(stats::sd(V0), n0^(-0.5))
      
      # Re-calculate final pilot densities for this threshold
      g1_p <- pilot_kde_loo_cpp(V1, h_p, s1)
      g0_p <- pilot_kde_loo_cpp(V0, h_p, s0)
      
      # Compute final local bandwidth multipliers (lambdas)
      l1 <- compute_local_bandwidth(g1_p, s1, n1, delta)
      l0 <- compute_local_bandwidth(g0_p, s0, n0, delta)
      
      final_lambda_list[[j]] <- list(l1 = l1, l0 = l0)
    } else {
      final_lambda_list[[j]] <- list(l1 = NULL, l0 = NULL)
    }
  }

  # 4. Bootstrap (Standard errors)
  if (B > 0L) {
    boot_stat <- function(dummy_data, indices) {
      Yb <- Y[indices]; Xb <- X[indices, , drop = FALSE]
      fit_b <- fit_ks_once(Yb, Xb, J_thresholds, h_p, h, delta, trim, control, n_starts = 1)
      as.numeric(fit_b$coef)
    }
    boot_obj <- boot::boot(data = matrix(0, n, 1), statistic = boot_stat, R = B)
    bootstrap_se <- apply(boot_obj$t, 2, stats::sd)
  } else {
    boot_obj <- NULL
    bootstrap_se <- rep(NA_real_, length(coef_hat))
  }

  # 5. Return Object (Now includes X, Y, and lambdas)
  res <- list(
    call = match.call(),
    formula = formula,
    coefficients = coef_hat,
    bootstrap_se = bootstrap_se,
    # Data stored for post-estimation
    X = X,
    Y = Y,
    lambda_list = final_lambda_list,
    # Metadata
    J = J_thresholds,
    n = n,
    levels = levels_Y,
    bandwidth = list(h_p = h_p, h = h, delta = delta),
    optim = full_fit,
    boot = if (B > 0L && keep_boot) boot_obj else NULL
  )

  class(res) <- "ks_ordinal"
  return(res)
}

# -------------------------------------------------------------------
# S3 methods
# -------------------------------------------------------------------

print.ks_ordinal <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients (normalized index parameters):\n")
  print(x$coefficients)
  cat(
    sprintf(
      "\nNumber of categories (J): %d\nSample size (n): %d\nBootstrap replications (B): %d\n",
      x$J, x$n, x$B
    )
  )
  invisible(x)
}

summary.ks_ordinal <- function(object, ...) {
  coef <- object$coefficients
  se <- object$bootstrap_se
  p <- length(coef)

  if (length(se) != p || all(is.na(se))) {
    coef_table <- cbind(Estimate = coef)
  } else {
    z <- coef / se
    pval <- 2 * stats::pnorm(-abs(z))
    coef_table <- cbind(
      Estimate = coef,
      "Std. Error" = se,
      "z value" = z,
      "Pr(>|z|)" = pval
    )
  }

  res <- list(
    call = object$call,
    coefficients = coef_table,
    J = object$J,
    n = object$n,
    B = object$B,
    optim = object$optim
  )
  class(res) <- "summary.ks_ordinal"
  res
}

print.summary.ks_ordinal <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients (normalized index parameters):\n")
  printCoefmat(x$coefficients, P.values = ncol(x$coefficients) >= 4)
  cat(
    sprintf(
      "\nNumber of categories (J): %d\nSample size (n): %d\nBootstrap replications (B): %d\n",
      x$J, x$n, x$B
    )
  )
  cat("\nOptimization:\n")
  print(x$optim)
  invisible(x)
}

coef.ks_ordinal <- function(object, ...) {
  object$coefficients
}

vcov.ks_ordinal <- function(object, ...) {
  if (!is.null(object$boot)) {
    return(stats::var(object$boot$t))
  }
  stop("Bootstrap results were not stored (keep_boot = FALSE or B = 0); vcov unavailable.")
}
