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

quasi_likelihood <- function(beta_raw, Y, X, J, h_p, h, delta, trim, verbose = FALSE) {
  n <- length(Y)

  # Enforce normalization of beta (unit norm, beta[1] > 0)
  beta <- .normalize_beta(beta_raw)
  if (any(is.na(beta))) {
    return(1e100)
  }

  V_hat <- as.numeric(X %*% beta)
  P_est_mat <- array(0, dim = c(J + 2L, n))

  for (j in 1:J) {
    n1 <- sum(Y <= j)
    n0 <- n - n1

    # Degenerate cases: all <= j or all > j
    if (n1 == 0L) {
      P_est_mat[j + 1L, ] <- 0
      next
    }
    if (n0 == 0L) {
      P_est_mat[j + 1L, ] <- 1
      next
    }

    # Pilot KDE
    sigma1 <- stats::sd(V_hat[Y <= j])
    if (!is.finite(sigma1) || sigma1 <= 0) {
      sigma1 <- max(stats::sd(V_hat), n1^(-1/2))
      if (!is.finite(sigma1) || sigma1 <= 0) sigma1 <- n1^(-1/2)
    }

    sigma0 <- stats::sd(V_hat[Y > j])
    if (!is.finite(sigma0) || sigma0 <= 0) {
      sigma0 <- max(stats::sd(V_hat), n0^(-1/2))
      if (!is.finite(sigma0) || sigma0 <= 0) sigma0 <- n0^(-1/2)
    }

    g1_pilot <- pilot_kde(Y, V_hat, l = 1L, h_p = h_p, sigma = sigma1, j = j)
    g0_pilot <- pilot_kde(Y, V_hat, l = 0L, h_p = h_p, sigma = sigma0, j = j)

    # Local bandwidths (use group sizes for n_group)
    lambda1 <- compute_local_bandwidth(g1_pilot, sigma1, n1, delta)
    lambda0 <- compute_local_bandwidth(g0_pilot, sigma0, n0, delta)

    # Final KDE
    g1_final <- final_kde(Y, V_hat, l = 1L, lambda = lambda1, h = h, j = j)
    g0_final <- final_kde(Y, V_hat, l = 0L, lambda = lambda0, h = h, j = j)

    # Conditional probabilities
    p1 <- n1 / n
    p0 <- n0 / n
    denom <- p1 * g1_final + p0 * g0_final

    # n-dependent floor for denominators (asymptotically negligible)
    denom_floor <- n^(-3)
    bad_denom <- !is.finite(denom) | (denom < denom_floor)
    denom_adj <- denom
    denom_adj[bad_denom] <- denom_floor

    P_j_est <- numeric(n)
    ok <- denom_adj > 0 & is.finite(denom_adj)
    P_j_est[ok] <- (p1 * g1_final[ok]) / denom_adj[ok]

    P_est_mat[j + 1L, ] <- P_j_est
  }

  # Boundary rows: P(Y <= 0) = 0, P(Y <= J) = 1
  P_est_mat[1L, ] <- 0
  P_est_mat[J + 2L, ] <- 1

  # Probability of actual category for each observation
  cols <- seq_len(n)
  pr <- P_est_mat[cbind(Y + 1L, cols)] - P_est_mat[cbind(Y, cols)]

  # Trimming based on X (can later be changed to index-based trimming if desired)
  if (!is.null(trim) && trim < 1) {
    Xabs <- abs(X)
    # column-wise trim quantiles
    bounds <- apply(Xabs, 2, stats::quantile, probs = trim, na.rm = TRUE)
    # compare each row to bounds
    within_bounds <- sweep(Xabs, 2, bounds, FUN = "<=")
    valid <- apply(within_bounds, 1, all)
    pr <- pr[valid]
  }

  # Likelihood penalty on invalid values
  if (length(pr) == 0L || any(!is.finite(pr)) || any(pr <= 0)) {
    return(1e100)
  }

  -sum(log(pr))
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
    stop("Package 'boot' is required. Please install it.")
  }

  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula.")
  }

  start_method <- match.arg(start_method)

  data <- as.data.frame(data)
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.omit)
  terms_obj <- attr(mf, "terms")
  y_raw <- stats::model.response(mf)
  X <- stats::model.matrix(terms_obj, data = mf)

  # Remove intercept column if present
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }

  if (ncol(X) == 0L) {
    stop("No predictors in the model matrix after removing intercept.")
  }

  # Handle ordinal response
  if (is.factor(y_raw) || is.ordered(y_raw)) {
    y_factor <- factor(y_raw, ordered = TRUE)
  } else {
    y_factor <- factor(y_raw, ordered = TRUE)
  }

  Y <- as.integer(y_factor)
  levels_Y <- levels(y_factor)
  J <- length(levels_Y)
  n <- length(Y)

  if (J < 2L) {
    stop("The response must have at least 2 distinct categories.")
  }

  # Bandwidths with tunable constants c_p and c
  delta <- 1/15
  a <- (1/10 + (3 + delta)/3) / 5    # exponent for pilot bandwidth
  b <- 3 * ((3 + delta)/20 + 1/6) / 10  # exponent for final bandwidth

  c_p <- if (!is.null(bandwidth_const$c_p)) bandwidth_const$c_p else 1
  c_h <- if (!is.null(bandwidth_const$c)) bandwidth_const$c else 1

  h_p <- c_p * n^(-a)
  h   <- c_h * n^(-b)

  # Full-sample fit
  full_fit <- fit_ks_once(
    Y = Y,
    X = X,
    J = J,
    h_p = h_p,
    h = h,
    delta = delta,
    trim = trim,
    control = control,
    verbose = verbose,
    n_starts = n_starts,
    start_method = start_method
  )

  coef_hat <- as.numeric(full_fit$coef)
  names(coef_hat) <- colnames(X)

  # Bootstrap
  B <- as.integer(B)
  if (is.na(B) || B < 0L) stop("'B' must be a non-negative integer.")

  if (B > 0L) {
    boot_stat <- function(dummy_data, indices) {
      Yb <- Y[indices]
      Xb <- X[indices, , drop = FALSE]
      fit_b <- fit_ks_once(
        Y = Yb,
        X = Xb,
        J = J,
        h_p = h_p,
        h = h,
        delta = delta,
        trim = trim,
        control = control,
        verbose = FALSE,
        n_starts = n_starts,
        start_method = start_method
      )
      as.numeric(fit_b$coef)
    }

    dummy <- matrix(0, nrow = n, ncol = 1)  # 'data' argument required by boot, but not used
    boot_obj <- boot::boot(
      data = dummy,
      statistic = boot_stat,
      R = B,
      sim = "ordinary"
    )

    boot_coefs <- boot_obj$t
    bootstrap_se <- apply(boot_coefs, 2, stats::sd)
    bootstrap_mean <- apply(boot_coefs, 2, mean)
  } else {
    boot_obj <- NULL
    bootstrap_se <- rep(NA_real_, length(coef_hat))
    bootstrap_mean <- rep(NA_real_, length(coef_hat))
  }

  res <- list(
    call = match.call(),
    formula = formula,
    data_name = deparse(substitute(data)),
    coefficients = coef_hat,
    bootstrap_se = bootstrap_se,
    bootstrap_mean = bootstrap_mean,
    B = B,
    J = J,
    n = n,
    levels = levels_Y,
    optim = list(
      value = full_fit$value,
      convergence = full_fit$convergence,
      counts = full_fit$counts,
      message = full_fit$message
    ),
    bandwidth = list(
      h_p = h_p,
      h = h,
      delta = delta,
      c_p = c_p,
      c = c_h
    ),
    boot = if (B > 0L && keep_boot) boot_obj else NULL,
    x_names = colnames(X)
  )

  class(res) <- "ks_ordinal"
  res
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
