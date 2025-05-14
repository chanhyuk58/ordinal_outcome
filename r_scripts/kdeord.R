# Required Libraries
library("MASS")  # For multivariate normal data generation
library("np")    # For Non-parametric KDE
library("data.table")

# Parameters
n_sim <- 100         # Number of simulations
n <- 1000            # Sample size
p <- 2               # Number of covariates
J <- 3               # Number of ordinal categories
h_p <- 0.5           # Pilot bandwidth
h <- n^(-1/6)             # Final bandwidth
Kernel <- function(x) dnorm(x) # Gaussian Kernel

# Generate the population {{{
generate_pop <- function(N=10e+5, xdim=2, beta=c(1, 1), dist='logistic', 
                         df=1, seed=63130, negative=FALSE, location=0) {
  set.seed(seed) 
  
  X <- matrix(rnorm(N*xdim), ncol=xdim)
  colnames(X) <- paste0("x", 1:xdim)
  e <- switch(dist,
    logistic = rlogis(N),
    normal   = rnorm(N),
    tdis     = rt(N, df=df),
    chisq    = rchisq(N, df=df) + location,
    stop("Unknown distribution"))
  
  if (negative == TRUE) {
    e <- -e
  }
  y_star <- X %*% beta + e  # Only x1 has true effect
  thresholds <- c(-1, 0.5)

  y_ord <- cut(y_star, breaks = c(-Inf, thresholds, Inf),
    labels = FALSE, ordered_result = TRUE)
  y_ord <- as.numeric(y_ord)
  
  data <- as.data.frame(cbind(X, e, y_ord))
  return(data)
}
# }}}

# -------------------------------
# Functions
# -------------------------------

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
KS_estimator <- function(Y, X) {
  J <- max(Y)
  # Quasi-likelihood {{{
  quasi_likelihood <- function(beta) {
    # beta <- c(0.1, 0.1)
    V_hat <- X %*% beta

    P_est_list <- list()
    for (j in 0:J) {
      n1 <- sum(Y <= j)
      n0 <- sum(Y > j)

      # Separate V values
      V_j1 <- V_hat[Y <= j]
      V_j0 <- V_hat[Y > j]

      # Pilot KDE estimation
      sigma1 <- sd(V_j1)
      sigma0 <- sd(V_j0)

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
      P_est_list[[j + 1]] <- P_j_est
    }

    # Quasi-likelihood calculation
    likelihood <- 0
    for (i in 1:length(Y)) {
      y_i <- Y[i]
      if (y_i == 0) {
        likelihood <- likelihood + 1/n * (abs(X[i]) < quantile(X, 0.95)) * log(P_est_list[[1]][i])
      } else {
        likelihood <- likelihood + 1/n * (abs(X[i]) < quantile(X, 0.95)) * log(P_est_list[[y_i + 1]][i] - P_est_list[[y_i]][i])
      }
    }

    return(-likelihood)
  }
  # }}}

  # Optimization
  beta_opt <- optim(coef(lm(as.numeric(Y) ~ X))[-1], fn=quasi_likelihood, method = "BFGS")
  beta_hat <- beta_opt$par

  return(beta_hat)
}
# }}}

# KDE Based Estimator {{{
kde_estimator <- function(Y, X) {
  # Quasi-likelihood 
  quasi_likelihood <- function(beta) {
    # beta <- c(0.1, 0.1)
    V_hat <- X %*% beta
    J <- max(Y)


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
      if (y_i == 0) {
        likelihood <- likelihood + 1/n * (abs(X[i]) < quantile(X, 0.95)) * log(P_est_list[[1]][i])
      } else {
        likelihood <- likelihood + 1/n * (abs(X[i]) < quantile(X, 0.95)) * log(P_est_list[[y_i + 1]][i] - P_est_list[[y_i]][i])
      }
    }

    return(- likelihood)
  }

  beta_opt <- optim(coef(lm(as.numeric(Y) ~ X))[-1], fn=quasi_likelihood, method = "BFGS")
  beta_hat <- beta_opt$par

  return(beta_hat)
}
# }}}

# -------------------------------
# Generate Population
# -------------------------------
set.seed(42)
distributions <- c("logistic", "normal", "tdis", "chisq")
dist <- distributions[4]
df <- 5
N <- 10e+5
beta <- c(1, 0.5)
loc <- -10
neg <- FALSE
pop <- generate_pop(dist=dist, df=df, beta=beta, negative=neg, location=loc)
hist(as.numeric(pop$y_ord))
hist(pop$y_ord)

# -------------------------------
# Simulation (2002)
# -------------------------------

ks_estimates <- matrix(0, nrow = n_sim, ncol = p)
kde_estimates <- matrix(0, nrow = n_sim, ncol = p)
# {{{
for (sim in 1:n_sim) {
  cat("Simulation:", sim, "\n")

  n <- 1000
  idx <- sample(N, n)
  
  sample <- pop[idx, ]

  Y <- sample$y_ord
  X <- as.matrix(sample[,c(1, 2)])
  
  ks_estimates[sim, ] <- KS_estimator(Y, X)
  kde_estimates[sim, ] <- kde_estimator(Y, X)
}
# }}}

fwrite(ks_estimates, file="../data/ks_estimates.csv", bom=T)
fwrite(ks_estimates, file="../data/kde_estimates.csv", bom=T)


# # -------------------------------
# # Simulation Results
# # -------------------------------
# cat("\n--- Simulation Results ---\n")
# cat("True beta:", true_beta, "\n")
# cat("Mean of beta estimates:", colMeans(beta_estimates2), "\n")
# cat("SD of beta estimates:", apply(beta_estimates2, 2, sd), "\n\n")
#
# # Visualization
# # par(mfrow = c(1, 2))
# boxplot(beta_estimates2, main = "Beta Estimates", names = paste0("Beta", 1:p))
# abline(h = true_beta, col = "red", lty = 2)
