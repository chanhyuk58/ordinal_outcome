library('haven')   # to read Stata files
library('dplyr')   # for data wrangling
library('texreg')
library('MASS')
library("sure")

# Load data
df <- read_dta("~/Dropbox/projects/ordinal_outcome/replications/Carlin2021a/study1_2.dta")
dim(df)

df <- df %>% filter(tasa2 != 88)
dim(df)
df <- df %>% filter(!(arm %in% c(2, 4)))
dim(df)

# Model 1: Acceptability (tasa2)
# OLS
model_acceptability <- lm(tasa2 ~ arm3 + arm5 + arm6 + arm7 + arm8, data = df)
# logit
model_acceptability <- polr(ordered(tasa2) ~ arm3 + arm5 + arm6 + arm7 + arm8, data = df, method='logistic', Hess=TRUE)
# probit
model_acceptability <- polr(ordered(tasa2) ~ arm3 + arm5 + arm6 + arm7 + arm8, data = df, method='probit', Hess=TRUE)

# SURE Check
sure::autoplot.polr(model_acceptability, nsim=100, what="qq")

# Model 2: Donation (don1)
model_donation <- lm(don1 ~ arm3 + arm5 + arm6 + arm7 + arm8, data = df)
model_donation <- polr(ordered(don1) ~ arm3 + arm5 + arm6 + arm7 + arm8, data = df,
 method='logistic', Hess=TRUE)

screenreg(list(model_acceptability, model_donation))

## Pilot KDE {{{
pilot_kde <- function(Y, V, l, h_p, sigma, j, n) {
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
final_kde <- function(Y, V, l, lambda, h, j, n) {
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
ks_estimator <- function(Y, X, gweights=1) {

  n <- length(Y)
  h_p <- n^(-0.11)           # Pilot bandwidth
  h <- n^(-0.16)             # Final bandwidth

  J <- max(Y)

  # Quasi-likelihood {{{
  quasi_likelihood <- function(beta) {
    V_hat <- X %*% beta

    P_est_mat <- matrix(0, (J + 2L), n)
    for (j in 1:(J)) {
      n1 <- sum(Y <= j)
      n0 <- sum(Y > j)

      # Pilot KDE estimation
      sigma1 <- sd(V_hat[Y <= j])
      sigma0 <- sd(V_hat[Y > j])

      g1_pilot <- pilot_kde(Y, V_hat, 1, h_p, sigma1, j, n)
      g0_pilot <- pilot_kde(Y, V_hat, 0, h_p, sigma0, j, n)

      # Local bandwidths
      lambda1 <- compute_local_bandwidth(g1_pilot, sigma1, n1)
      lambda0 <- compute_local_bandwidth(g0_pilot, sigma0, n0)

      # Final KDE
      g1_final <- final_kde(Y, V_hat, 1,  lambda1, h, j, n)
      g0_final <- final_kde(Y, V_hat, 0, lambda0, h, j, n)

      # Conditional Probabilities
      p1 <- n1 / length(Y)
      p0 <- n0 / length(Y)
      P_j_est <- (p1 * g1_final) / (p1 * g1_final + p0 * g0_final)
      P_est_mat[(j + 1L), ] <- P_j_est
    }
    P_est_mat[1, ] <- rep(0, n)
    P_est_mat[(J + 2L), ] <- rep(1, n)

    # Quasi-likelihood calculation
    pr <- diag(P_est_mat[(Y + 1L), ]) - diag(P_est_mat[(Y), ])

    if (all(pr > 0 & !is.na(pr))) {
      valid <- (apply(abs(X) <= quantile(abs(X), 0.98), function(x) all(x), MARGIN=1))
      likelihood <- -sum(
        log(gweights[valid] %*% pr[valid])
        # log(pr)
      )
      print('Yes')
    } else {
      likelihood <- 1e+100
      print('no')
    }
    return(likelihood)
  }
  # }}}

  # Quasi_gradient {{{
  quasi_gradient <- function(beta) {
    V_hat <- X %*% beta
    grad <- rep(0, length(beta))

    P_est_mat <- matrix(0, (J + 2L), n)
    dP_dV <- array(0, dim = c(J + 2L, n))  # dP_j/dV for all j, all i

    for (j in 1:(J)) {
      n1 <- sum(Y <= j)
      n0 <- sum(Y > j)

      # Pilot KDE
      sigma1 <- sd(V_hat[Y <= j])
      sigma0 <- sd(V_hat[Y > j])
      g1_pilot <- pilot_kde(Y, V_hat, 1, h_p, sigma1, j, n)
      g0_pilot <- pilot_kde(Y, V_hat, 0, h_p, sigma0, j, n)

      lambda1 <- compute_local_bandwidth(g1_pilot, sigma1, n1)
      lambda0 <- compute_local_bandwidth(g0_pilot, sigma0, n0)

      g1 <- final_kde(Y, V_hat, 1, lambda1, h, j, n)
      g0 <- final_kde(Y, V_hat, 0, lambda0, h, j, n)

      p1 <- n1 / n
      p0 <- n0 / n

      A <- p1 * g1
      B <- p0 * g0
      P_j <- A / (A + B)
      P_est_mat[j + 1L, ] <- P_j

      # Derivatives of g1 and g0 w.r.t. V
      dg1_dV <- numeric(n)
      dg0_dV <- numeric(n)
      for (i in 1:n) {
        weights1 <- (Y <= j) * Kernel_prime((V_hat[i] - V_hat) / (lambda1 * h)) / (lambda1 * h)^2
        weights0 <- (Y > j) * Kernel_prime((V_hat[i] - V_hat) / (lambda0 * h)) / (lambda0 * h)^2
        if (n1 > 0) dg1_dV[i] <- -mean(weights1)
        if (n0 > 0) dg0_dV[i] <- -mean(weights0)
      }

      A_prime <- p1 * dg1_dV
      B_prime <- p0 * dg0_dV

      # dPj_dV <- (A_prime * (A + B) - A * (A_prime + B_prime)) / (A + B)^2
      dPj_dV <- (A_prime * B) - (A * B_prime) / (A + B)^2
      dP_dV[j + 1L, ] <- dPj_dV
    }

    # Fill boundary probabilities
    P_est_mat[1, ] <- 0
    P_est_mat[J + 2L, ] <- 1
    dP_dV[1, ] <- 0
    dP_dV[J + 2L, ] <- 0

    pr <- diag(P_est_mat[Y + 1L, ]) - diag(P_est_mat[Y, ])
    dpr_dV <- diag(dP_dV[Y + 1L, ]) - diag(dP_dV[Y, ])
    # summary(dpr_dV)

    # valid <- pr > 0 & !is.na(pr) & (apply(abs(X) <= quantile(abs(X), 0.98), function(x) all(x), MARGIN=1))
    if (all(pr > 0 & !is.na(pr))) {
      valid <- (apply(abs(X) <= quantile(abs(X), 0.98), function(x) all(x), MARGIN=1))
      X_sub <- X[valid, , drop = FALSE]
      w <- (-gweights[valid] / pr[valid]) * dpr_dV[valid]
      grad <- w %*% X_sub
    }
    else {
      grad <- rep(NA_real_, dim(X)[2])
    }
    return(grad)
  }
  # }}}

  # Quasi_hessian {{{
  quasi_hessian <- function(beta) {
    V_hat <- X %*% beta
    P_est_mat <- matrix(0, J + 2L, n)
    dP_dV <- array(0, dim = c(J + 2L, n))
    d2P_dV2 <- array(0, dim = c(J + 2L, n))

    for (j in 1:J) {
      n1 <- sum(Y <= j)
      n0 <- sum(Y > j)

      sigma1 <- sd(V_hat[Y <= j])
      sigma0 <- sd(V_hat[Y > j])

      g1_pilot <- pilot_kde(Y, V_hat, 1, h_p, sigma1, j, n)
      g0_pilot <- pilot_kde(Y, V_hat, 0, h_p, sigma0, j, n)

      lambda1 <- compute_local_bandwidth(g1_pilot, sigma1, n1)
      lambda0 <- compute_local_bandwidth(g0_pilot, sigma0, n0)

      g1 <- final_kde(Y, V_hat, 1, lambda1, h, j, n)
      g0 <- final_kde(Y, V_hat, 0, lambda0, h, j, n)

      p1 <- n1 / n
      p0 <- n0 / n
      A <- p1 * g1
      B <- p0 * g0

      A_prime <- numeric(n)
      B_prime <- numeric(n)
      A_double <- numeric(n)
      B_double <- numeric(n)

      for (i in 1:n) {
        u1 <- (V_hat[i] - V_hat) / (lambda1 * h)
        u0 <- (V_hat[i] - V_hat) / (lambda0 * h)
        weights1 <- (Y <= j)
        weights0 <- (Y > j)

        if (n1 > 0) {
          A_prime[i] <- -p1 * mean(weights1 * Kernel_prime(u1) / (lambda1 * h)^2)
          A_double[i] <- p1 * mean(weights1 * Kernel_double_prime(u1) / (lambda1 * h)^3)
        }
        if (n0 > 0) {
          B_prime[i] <- -p0 * mean(weights0 * Kernel_prime(u0) / (lambda0 * h)^2)
          B_double[i] <- p0 * mean(weights0 * Kernel_double_prime(u0) / (lambda0 * h)^3)
        }
      }

      denom <- (A + B)^2
      denom2 <- (A + B)^3

      dPj_dV <- (A_prime * (A + B) - A * (A_prime + B_prime)) / denom
      d2Pj_dV2 <- (
        A_double * (A + B) - A * (A_double + B_double)
          - 2 * (A_prime * (A_prime + B_prime))
      ) / denom -
        2 * (A_prime * (A + B) - A * (A_prime + B_prime)) * (A_prime + B_prime) / denom2

      P_est_mat[j + 1L, ] <- A / (A + B)
      dP_dV[j + 1L, ] <- dPj_dV
      d2P_dV2[j + 1L, ] <- d2Pj_dV2
    }

    P_est_mat[1, ] <- 0
    P_est_mat[J + 2L, ] <- 1
    dP_dV[1, ] <- 0
    dP_dV[J + 2L, ] <- 0
    d2P_dV2[1, ] <- 0
    d2P_dV2[J + 2L, ] <- 0

    pr <- diag(P_est_mat[Y + 1L, ]) - diag(P_est_mat[Y, ])
    dpr_dV <- diag(dP_dV[Y + 1L, ]) - diag(dP_dV[Y, ])
    d2pr_dV2 <- diag(d2P_dV2[Y + 1L, ]) - diag(d2P_dV2[Y, ])

    hess <- matrix(0, ncol = ncol(X), nrow = ncol(X))
    if (all(pr > 0 & !is.na(pr))) {
      valid <- (apply(abs(X) <= quantile(abs(X), 0.98), function(x) all(x), MARGIN=1))
      X_sub <- X[valid, , drop = FALSE]
      term1 <- (gweights[valid] / pr[valid]^2) * (dpr_dV[valid]^2)
      term2 <- (gweights[valid] / pr[valid]) * d2pr_dV2[valid]
      hess <- ((term1 - term2) * t(X_sub)) %*% X_sub
    }
    return(hess)
  }
  # }}}

  # Optimization
  beta_opt <- optim(
    fit_ks[[1]],
    # round(coef(lm(Y  ~ X))[-1], 1), 
    fn=quasi_likelihood, 
    gr=quasi_gradient,
    method='BFGS',
    hessian=TRUE
  )
  beta_hat <- beta_opt$par
  # beta_hess <- quasi_hessian(beta_hat)
  beta_hess <- beta_opt$hessian
  beta_vcov <- MASS::ginv(beta_hess)
  beta_se <- sqrt(diag(beta_vcov))

  return(list(beta_hat, beta_se))
}
# }}}

Kernel <- function(x) { 
  return(dnorm(x)) 
} # Gaussian Kernel

Kernel_prime <- function(u) {
  return(-u * dnorm(u))  # derivative of standard normal density
}

Kernel_double_prime <- function(u) { 
  return((u^2 - 1) * dnorm(u)) 
}

Y <- df$tasa2
X <- df[, c(
  'arm3' , 'arm5', 'arm6', 'arm7', 'arm8'
)]

Y <- as.numeric(Y)
n <- length(Y)
X <- as.matrix(X)

fit_ks <- ks_estimator(Y, X, gweights=rep(1, n))

ks_result <- sapply(fit_ks, rbind)
row.names(ks_result) <- names(fit_ks[[1]])
colnames(ks_result) <- c('coef', 'se')
print(ks_result)

write.csv(ks_result, file='./ks_result.csv')
