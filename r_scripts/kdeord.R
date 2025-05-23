# Required Libraries
library('MASS')  # For multivariate normal data generation
library('np')    # For Non-parametric KDE
library('data.table')


# -------------------------------
# Functions
# -------------------------------

# Generate the population {{{
generate_pop <- function(N=10e+5, xdim=2, beta=c(1, 1), dist='logistic', 
                         df=1, seed=63130, negative=FALSE, location=0) {
  set.seed(seed) 
  
  X <- matrix(rnorm(N*xdim), ncol=xdim)
  colnames(X) <- paste0('x', 1:xdim)
  e <- switch(dist,
    logistic = rlogis(N),
    normal   = rnorm(N),
    tdis     = rt(N, df=df),
    chisq    = rchisq(N, df=df) + location,
    stop('Unknown distribution'))
  
  if (negative == TRUE) {
    e <- -e
  }
  y_star <- X %*% beta + e  # Only x1 has true effect
  thresholds <- c(-1, 0.5, 1, 3)
  y_ord <- cut(y_star, breaks = c(-Inf, thresholds, Inf),
    labels = FALSE, ordered_result = TRUE)
  
  data <- as.data.frame(cbind(X, e))
  data$y_ord <- as.numeric(y_ord)
  return(data)
}
# }}}

## Pilot KDE {{{
pilot_kde <- function(V, l, h_p, sigma, j) {
  kde_values <- numeric(n)
  if (l == 1) {
    n1 <- length(V[Y <= j])
    if (n1 > 0) {
      for (i in 1:n) {
        kde_values[i] <- 1/n1 * 
          sum(
            (Y[-i] <= j) * Kernel((V[i] - V[-i]) / (sigma * h_p)) / (sigma * h_p)
          ) 

      }
    }
  }
  if (l == 0) {
    n0 <- length(V[Y > j])
    if (n0 > 0) {
      for (i in 1:n) {
        kde_values[i] <- 1/n0 * 
          sum(
            (Y[-i] > j) * Kernel((V[i] - V[-i]) / (sigma * h_p)) / (sigma * h_p)
          ) 

      }
    }
  }
  return(kde_values)
}
# }}}

## Smooth Damping {{{
compute_local_bandwidth <- function(pilot_density, sigma, n) {
  # pilot_density <- g1_pilot
  # summary(log(pilot_density))
  # summary(pilot_density)
  m <- exp(sum(log(pilot_density)) / length(pilot_density))
  l <- pilot_density / m
  a_n <- log(n)^(-1)
  d <- 1 / (1 + exp(-n^(1/80) * (l - a_n)))
  lambda <- sigma / sqrt(l * d + a_n * (1 - d))
  return(lambda)
}
# }}}

## Final KDE {{{
final_kde <- function(V, l, lambda, h, j) {
  kde_values <- numeric(n)
  if (l == 1) {
    n1 <- length(V[Y <= j])
    if (n1 > 0) {
      for (i in 1:n) {
        kde_values[i] <- 1/n1 * 
          sum(
            (Y <= j) * Kernel((V[i] - V) / (lambda * h)) / (lambda * h)
          )
      }
    }
  }
  if (l == 0) {
    n0 <- length(V[Y > j])
    if (n0 > 0) {
      for (i in 1:n) {
        kde_values[i] <- 1/n0 * 
          sum(
            (Y > j) * Kernel((V[i] - V) / (lambda * h)) / (lambda * h)
          )
      }
    }
  }
  return(kde_values)
}
# }}}

# Klien and Sherman (2002) Estimator {{{
ks_estimator <- function(Y, X) {
  J <- max(Y)
  # Quasi-likelihood {{{
  quasi_likelihood <- function(beta) {
    V_hat <- X %*% beta

    P_est_list <- list()
    for (j in 1:J) {
      n1 <- sum(Y <= j)
      n0 <- sum(Y > j)

      # Pilot KDE estimation
      sigma1 <- sd(V_hat[Y <= j])
      sigma0 <- sd(V_hat[Y > j])

      g1_pilot <- pilot_kde(V_hat, 1, h_p, sigma1, j)
      g0_pilot <- pilot_kde(V_hat, 0, h_p, sigma0, j)

      # Local bandwidths
      lambda1 <- compute_local_bandwidth(g1_pilot, sigma1, n1)
      lambda0 <- compute_local_bandwidth(g0_pilot, sigma0, n0)

      # Final KDE
      g1_final <- final_kde(V_hat, 1,  lambda1, h, j)
      g0_final <- final_kde(V_hat, 0, lambda0, h, j)

      # Conditional Probabilities
      p1 <- n1 / length(Y)
      p0 <- n0 / length(Y)
      P_j_est <- (p1 * g1_final) / (p1 * g1_final + p0 * g0_final)
      P_est_list[[1]] <- P_j_est
    }

    # Quasi-likelihood calculation
    likelihood <- 0
    for (i in 1:length(Y)) {
      y_i <- Y[i]
      if (y_i == min(Y)) {
        likelihood <- likelihood + 1/n * as.numeric(abs(X[i]) < quantile(X, 0.95)) * log(P_est_list[[1]][i])
      } else {
        likelihood <- likelihood + 1/n * (abs(X[i]) < quantile(X, 0.95)) * log(P_est_list[[y_i + 1]][i] - P_est_list[[y_i]][i])
      }
    }
    return(- likelihood)
  }
  # }}}

  # Optimization
  beta_opt <- optim(round(coef(lm(as.numeric(Y) ~ X))[-1], 2), fn=quasi_likelihood, method = 'Nelder-Mead')
  beta_hat <- beta_opt$par

  return(beta_hat)
}
# }}}

# KDE Based Estimator {{{
kde_estimator <- function(Y, X) {
  J <- max(Y)
  # Quasi-likelihood 
  quasi_likelihood <- function(beta) {
    V_hat <- X %*% beta

    P_est_list <- list()
    for (j in 0:J) {
      g_bw <- np::npcdensbw(ydat=V_hat, xdat=ordered(as.numeric(Y <= j)))
      g1 <- fitted(np::npcdens(g_bw, newdata=data.frame(y=V_hat, x=1)))
      g0 <- fitted(np::npcdens(g_bw, newdata=data.frame(y=V_hat, x=0)))

      # Conditional Probabilities
      p1 <- sum(Y <= j) / length(Y)
      p0 <- sum(Y > j) / length(Y)
      P_j_est <- (p1 * g1) / (p1 * g1 + p0 * g0)
      P_est_list[[j + 1]] <- P_j_est
    }

    # Quasi-likelihood calculation
    likelihood <- 0
    for (i in 1:length(Y)) {
      y_i <- Y[i]
      if (y_i == min(Y)) {
        likelihood <- likelihood + 1/n * log(P_est_list[[1]][i])
      } else {
        likelihood <- likelihood + 1/n * log(P_est_list[[y_i + 1]][i] - P_est_list[[y_i]][i])
      }
    }
    return(- likelihood)
  }

  # Optimization
  beta_opt <- optim(coef(lm(as.numeric(Y) ~ X))[-1], fn=quasi_likelihood, method = 'Nelder-Mead')
  beta_hat <- beta_opt$par

  return(beta_hat)
}
# }}}

# One Simulation {{{
simul <- function(n, pop) {
  # Sample
  idx = sample(N, n)
  sample_data <- pop[idx, ]

  Y <- sample_data$y_ord
  X <- as.matrix(sample_data[, c(1, 2)])

  # Ordered logit
  fit_logit <- polr(ordered(y_ord) ~ x1 + x2, data = sample_data, method = 'logistic')
  beta_hat_logit <- coef(fit_logit)
  
  # Ordered probit
  fit_probit <- polr(ordered(y_ord) ~ x1 + x2, data = sample_data, method = 'probit')
  beta_hat_probit <- coef(fit_probit)
  
  # OLS
  fit_ols <- lm(as.numeric(y_ord) ~ x1 + x2, data=sample_data)
  beta_hat_ols <- coef(fit_ols)[-1]

  # Klein and Sherman
  beta_hat_ks <- ks_estimator(Y, X)
  
  # KDE
  beta_hat_kde <- kde_estimator(Y, X)

  out <- data.table(
    rbind(
      beta_hat_logit,
      beta_hat_probit,
      beta_hat_ks,
      beta_hat_kde,
      beta_hat_ols
    )
  )
  out <- out[, ':='(
    models = list('ologit', 'oprobit', 'KS', 'KDE', 'OLS'),
    n = n,
    dist = dist
    )
  ]
}
# }}}

# -------------------------------
# Generate Population
# -------------------------------

# Parameters
n_sim <- 10         # Number of simulations
n <- 1000            # Sample size
p <- 2               # Number of covariates
# J <- 5               # Number of ordinal categories
h_p <- 0.5           # Pilot bandwidth
h <- n^(-1/6)             # Final bandwidth
Kernel <- function(x) dnorm(x) # Gaussian Kernel

set.seed(42)
distributions <- c('logistic', 'normal', 'tdis', 'chisq')
dist <- distributions[4]
df <- 3
N <- 10e+5
beta <- c(-0.1, 1)
loc <- -5
neg <- TRUE
pop <- generate_pop(dist=dist, df=df, beta=beta, negative=neg, location=loc)
# hist(as.numeric(pop$y_ord))
hist(pop$y_ord)

# -------------------------------
# Simulation
# -------------------------------

estimates <- data.table()
for (sim in 1:n_sim) {
  cat('Simulation:', sim, '\n')
  n <- 1000
  estimates <- rbindlist(list(estimates, simul(n, pop)))
}

fwrite(estimates, file='../data/estimates.csv', bom=T)

# -------------------------------
# Simulation Results
# -------------------------------
estimates <- fread('../data/estimates.csv')
estimates[, lapply(.(x1/x2, x2/x2), mean), ,by=models]
