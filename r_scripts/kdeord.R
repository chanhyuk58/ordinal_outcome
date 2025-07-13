# Required Libraries
library('MASS')  # For multivariate normal data generation
library('np')    # For Non-parametric KDE
library('data.table')
library('ggplot2')


# -------------------------------
# Functions
# -------------------------------

# Function to do parallel replicate {{{
parReplicate <- function(cl, rep, expr, simplify=TRUE, USE.NAMES=TRUE){
  parallel::parSapply(cl, integer(rep), function(i, ex) eval(ex, envir=.GlobalEnv),
    substitute(expr), simplify=simplify, USE.NAMES=USE.NAMES)
}
# }}}

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
pilot_kde <- function(Y, V, l, h_p, sigma, j) {
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
final_kde <- function(Y, V, l, lambda, h, j) {
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

      g1_pilot <- pilot_kde(Y, V_hat, 1, h_p, sigma1, j)
      g0_pilot <- pilot_kde(Y, V_hat, 0, h_p, sigma0, j)

      # Local bandwidths
      lambda1 <- compute_local_bandwidth(g1_pilot, sigma1, n1)
      lambda0 <- compute_local_bandwidth(g0_pilot, sigma0, n0)

      # Final KDE
      g1_final <- final_kde(Y, V_hat, 1,  lambda1, h, j)
      g0_final <- final_kde(Y, V_hat, 0, lambda0, h, j)

      # Conditional Probabilities
      p1 <- n1 / length(Y)
      p0 <- n0 / length(Y)
      P_j_est <- (p1 * g1_final) / (p1 * g1_final + p0 * g0_final)
      P_est_list[[j]] <- P_j_est
    }
    P_est_list[[J]] <- rep(1, n)

    pmin(P_est_list)


    # Quasi-likelihood calculation
    likelihood <- 0
    for (i in 1:length(Y)) {
      y_i <- Y[i]
      if (y_i == 1) {
        pr <- P_est_list[[y_i]][1]
      } else {
        pr <- P_est_list[[y_i]][i] - P_est_list[[y_i - 1]][i]
      }
      if (!is.na(pr)) {
        if (pr > 0) {
          likelihood <- likelihood + 1/n * as.numeric(abs(X[i]) < quantile(X, 0.95)) * log(pr)
        } else {
          likelihood <- Inf
        }
      } else {
        likelihood <- Inf
      }
    }
    return(- likelihood)
  }
  # }}}

  # Quasi-gradient {{{
  # quasi_gradient <- function(beta) {
  #   V_hat <- X %*% beta
  #   gradient_beta <- numeric(length(beta))
  #
  #   P_est_list <- list()
  #   for (j in 1:J) {
  #     n1 <- sum(Y <= j)
  #     n0 <- sum(Y > j)
  #
  #     # Pilot KDE estimation
  #     sigma1 <- sd(V_hat[Y <= j])
  #     sigma0 <- sd(V_hat[Y > j])
  #
  #     g1_pilot <- pilot_kde(V_hat, 1, h_p, sigma1, j)
  #     g0_pilot <- pilot_kde(V_hat, 0, h_p, sigma0, j)
  #
  #     # Local bandwidths
  #     lambda1 <- compute_local_bandwidth(g1_pilot, sigma1, n1)
  #     lambda0 <- compute_local_bandwidth(g0_pilot, sigma0, n0)
  #
  #     # Final KDE
  #     g1_final <- final_kde(V_hat, 1,  lambda1, h, j)
  #     g0_final <- final_kde(V_hat, 0, lambda0, h, j)
  #
  #     # Conditional Probabilities
  #     p1 <- n1 / length(Y)
  #     p0 <- n0 / length(Y)
  #     P_j_est <- (p1 * g1_final) / (p1 * g1_final + p0 * g0_final)
  #     P_est_list[[j]] <- P_j_est
  #   }
  #
  #   # Gradient Calculation
  #   for (i in 1:n) {
  #     xi <- X[i, ]
  #     vi <- V_hat[i]
  #     indicator_1 <- ifelse(Y[i] <= j, 1, 0)
  #     indicator_0 <- ifelse(Y[i] > j, 1, 0)
  #
  #     if (j == 1) {
  #       gradient <- gradient + indicator_1 * (1 - P_j_est[i]) * xi
  #     } else if (j == J) {
  #       gradient <- gradient - indicator_0 * P_j_est[i] * xi
  #     } else {
  #       gradient <- gradient + indicator_1 * (1 - P_j_est[i]) * xi - indicator_0 * P_j_est[i] * xi
  #     }
  #   }
  #   return(-gradient)
  # }
  # # }}}

  # Optimization
  beta_opt <- optim(round(coef(lm(as.numeric(Y) ~ X))[-1], 1), fn=quasi_likelihood, 
    # gr=quasi_gradient, method = 'BFGS')
    method = 'BFGS')
  beta_hat <- beta_opt$par

  return(beta_hat)
}
# }}}

# KDE Based Estimator {{{
# Y <- pop$y_ord
# X <- as.matrix(pop[, c(1, 2)])
kde_estimator <- function(Y, X) {
  # J <- max(Y)
  # Quasi-likelihood 
  quasi_likelihood <- function(beta) {
        # beta <- coef(lm(as.numeric(Y) ~ X))[-1]
    V_hat <- X %*% beta

    g_bw <- np::npcdistbw(xdat=V_hat, ydat=ordered(Y))
    # plot(g_bw)
    p1 <- (np::npcdist(g_bw, exdat=V_hat, eydat=ordered(Y)))$condist
    p2 <- (np::npcdist(g_bw, exdat=V_hat, eydat=ordered(Y + 1L)))$condist
    # P_est_list <- list()
    # for (j in 1:J) {
    #   g_bw <- np::npcdensbw(ydat=V_hat, xdat=ordered(as.numeric(Y <= j)))
    #   g1 <- (np::npcdens(g_bw, eydat=V_hat, exdat=1))$condens
    #   g0 <- (np::npcdens(g_bw, eydat=V_hat, exdat=0))$condens
    #
    #
    #   # Conditional Probabilities
    #   p1 <- sum(Y <= j) / length(Y)
    #   p0 <- sum(Y > j) / length(Y)
    #   P_j_est <- (p1 * g1) / (p1 * g1 + p0 * g0)
    #   P_est_list[[j]] <- P_j_est
    # }
    # P_est_list[[J]] <- rep(1, n)

    # Quasi-likelihood calculation
    likelihood <- p2 - p1
    # likelihood <- 0
    # for (i in 1:length(Y)) {
    #   y_i <- Y[i]
    #   if (y_i == 1) {
    #     pr <- P_est_list[[y_i]][1]
    #   } else {
    #     pr <- P_est_list[[y_i]][i] - P_est_list[[y_i - 1]][i]
    #   }
    #   if (!is.na(pr)) {
    #     if (pr > 0) {
    #       likelihood <- likelihood + 1/n * as.numeric(abs(X[i]) < quantile(X, 0.95)) * log(pr)
    #     } else {
    #       likelihood <- Inf
    #     }
    #   } else {
    #     likelihood <- Inf
    #   }
    # }
    if (all(likelihood > 0))
      return(-sum(log(likelihood)))
    else
      Inf
  }


  # Optimization
  beta_opt <- optim(coef(lm(as.numeric(Y) ~ X))[-1], fn=quasi_likelihood, method = 'BFGS')
  beta_hat <- beta_opt$par

  return(beta_opt)
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

  beta_hat_logit / beta_hat_logit[2]
  beta_hat_kde / beta_hat_kde[2]
  out <- data.table(
    rbind(
      beta_hat_logit,
      beta_hat_probit,
      beta_hat_ols,
      beta_hat_ks,
      beta_hat_kde
    )
  )
  out <- out[, ':='(
    models = list('ologit', 'oprobit', 'OLS', 'KS', 'KDE'),
    n = n,
    loc = loc,
    dist = dist
  )
    ]
}
# }}}

# -------------------------------
# Generate Population
# -------------------------------

# Parameters
n_sim <- 20         # Number of simulations
Kernel <- function(x) dnorm(x) # Gaussian Kernel

# -------------------------------
# Simulation
# -------------------------------
ns <- c(500, 1000, 2000)
# n <- ns[2]
locs <- seq(-2, 2, 1)
# loc <- locs[1]
# loc <- 0

estimates <- data.table()
for (n in ns) {
  # for (loc in locs) {
    set.seed(42)
    distributions <- c('logistic', 'normal', 'tdis', 'chisq')
    dist <- distributions[4]
    df <- 3
    N <- 1e+5
    beta <- c(1, 1)
    neg <- TRUE
    pop <- generate_pop(dist=dist, df=df, beta=beta, negative=neg, location=loc, N=N)
    h_p <- 0.5           # Pilot bandwidth
    h <- n^(-1/6)             # Final bandwidth

    cl <- parallel::makePSOCKcluster(4)
    parallel::clusterExport(cl, varlist=c(ls(), list('pop', 'simul', 'polr', 'ks_estimator', 'kde_estimator', 'data.table')), envir=environment())
    out <- rbindlist(parReplicate(cl, n_sim, simul(n=n, pop=pop), simplify=FALSE))
    estimates <- rbindlist(list(estimates, out))
  # }
}

fwrite(estimates, file='../data/estimates.csv', bom=T)

# -------------------------------
# Simulation Results
# -------------------------------
estimates <- fread('../data/estimates.csv')
estimates[, lapply(.(x1/x2, x2/x2), mean), ,by=.(as.character(models), n, loc, dist)]

# -------------------------------
# Graph
# -------------------------------
# gd <- estimates[, lapply(.(x1/x2, x2/x2), mean), ,by=.(as.character(models), n, loc, dist)]
# ggplot(aes(x=n, y=V1, col=as.character), data=gd) +
#   facet_grid(~loc) +
#   geom_line() +
#   geom_point() + 
#   geom_hline(aes(yintercept=-0.1))
