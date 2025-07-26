# Required Libraries
library('MASS')  # For multivariate normal data generation
# library('np')    # For Non-parametric KDE
library('data.table')
# library('ggplot2')


# -------------------------------
# Functions
# -------------------------------

# Function to do parallel replicate {{{
parReplicate <- function(cl, rep, expr, simplify=TRUE, USE.NAMES=TRUE){
  parallel::parSapply(cl, integer(rep), function(i, ex) eval(ex, envir=.GlobalEnv), substitute(expr), simplify=simplify, USE.NAMES=USE.NAMES)
}
# }}}

# Generate the population {{{
generate_pop <- function(N=10e+5, xdim=2, beta=c(1, 1), dist='logistic', 
                         df=1, seed=63130, negative=FALSE, location=0, sd=1) {
  set.seed(seed) 

  X <- matrix(rnorm(N*(xdim)), ncol=xdim)
  X <- cbind(X, sample(c(0,1), N, replace=T))
  colnames(X) <- c(paste0('x', 1:xdim), 'T')
  e <- switch(dist,
    logistic = rlogis(N),
    normal   = rnorm(N, mean=location, sd=sd),
    tdis     = rt(N, df=df),
    chisq    = rchisq(N, df=df) + location,
    stop('Unknown distribution'))

  if (negative == TRUE) {
    e <- -e
  }
  y_star <- X %*% beta + e  # Only x1 has true effect
  thresholds <- c(-1, -0.6, 3, 4)
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
ks_estimator <- function(formula, data) {
  y_lab <- all.vars(formula)[1]
  x_lab <- all.vars(formula)[-1]
  J <- max(data[, y_lab])
  n <- dim(data)[1]
  h_p <- n^(-0.11)           # Pilot bandwidth
  h <- n^(-0.16)             # Final bandwidth

  Kernel <- function(x) {return(dnorm(x))} # Gaussian Kernel
  Kernel_prime <- function(u) {return(-u * dnorm(u))}  # derivative of standard normal density

  P_est_mat <- matrix(0, (J + 2L), n)
  dP_dV <- array(0, dim = c(J + 2L, n))  # dP_j/dV for all j, all i
  # Quasi-likelihood {{{
  quasi_likelihood <- function(beta, Y, X) {
    V_hat <- X %*% beta

    for (j in 1:(J)) {
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
      P_est_mat[(j + 1L), ] <- P_j_est
    }
    P_est_mat[1, ] <- rep(0, n)
    P_est_mat[(J + 2L), ] <- rep(1, n)

    # Quasi-likelihood calculation
    pr <- diag(P_est_mat[(Y + 1L), ]) - diag(P_est_mat[(Y), ])

    if (all(pr > 0 & !is.na(pr))) {
      print('Yes')
      likelihood <- -sum(
        log(pr[(apply(abs(X) <= quantile(abs(X), 0.98), function(x) all(x), MARGIN=1))])
        # log(pr)
      )
    } else {
      print('no')
      likelihood <- 1e+100
    }
    return(likelihood)
  }
  # }}}
  # Quasi_gradient {{{
  quasi_gradient <- function(beta, Y, X) {
    V_hat <- X %*% beta
    grad <- rep(0, length(beta))

    for (j in 1:(J)) {
      n1 <- sum(Y <= j)
      n0 <- sum(Y > j)
      p1 <- n1 / length(Y)
      p0 <- n0 / length(Y)

      # Pilot KDE
      sigma1 <- sd(V_hat[Y <= j])
      sigma0 <- sd(V_hat[Y > j])
      g1_pilot <- pilot_kde(Y, V_hat, 1, h_p, sigma1, j)
      g0_pilot <- pilot_kde(Y, V_hat, 0, h_p, sigma0, j)

      lambda1 <- compute_local_bandwidth(g1_pilot, sigma1, n1)
      lambda0 <- compute_local_bandwidth(g0_pilot, sigma0, n0)

      g1 <- final_kde(Y, V_hat, 1, lambda1, h, j)
      g0 <- final_kde(Y, V_hat, 0, lambda0, h, j)

      # Derivatives of g1 and g0 w.r.t. V
      dg1_dV <- numeric(n)
      dg0_dV <- numeric(n)
      for (i in 1:n) {
        weights1 <- (Y <= j) * Kernel_prime((V_hat[i] - V_hat) / (lambda1 * h)) / (lambda1 * h)^2
        weights0 <- (Y > j) * Kernel_prime((V_hat[i] - V_hat) / (lambda0 * h)) / (lambda0 * h)^2
        if (n1 > 0) dg1_dV[i] <- -mean(weights1)
        if (n0 > 0) dg0_dV[i] <- -mean(weights0)
      }

      A <- p1 * g1
      B <- p0 * g0
      A_prime <- p1 * dg1_dV
      B_prime <- p0 * dg0_dV

      # dPj_dV <- (A_prime * (A + B) - A * (A_prime + B_prime)) / (A + B)^2
      dPj_dV <- (A_prime * B) - (A * B_prime) / (A + B)^2
      dP_dV[j + 1L, ] <- dPj_dV
    }

    # Fill boundary probabilities
    dP_dV[1, ] <- 0
    dP_dV[J + 2L, ] <- 0

    pr <- diag(P_est_mat[(Y + 1L), ]) - diag(P_est_mat[(Y), ])
    dpr_dV <- diag(dP_dV[Y + 1L, ]) - diag(dP_dV[Y, ])
    summary(dpr_dV)

    # valid <- pr > 0 & !is.na(pr) & (apply(abs(X) <= quantile(abs(X), 0.98), function(x) all(x), MARGIN=1))
    if (all(pr > 0 & !is.na(pr))) {
      valid <- (apply(abs(X) <= quantile(abs(X), 0.98), function(x) all(x), MARGIN=1))
      X_sub <- X[valid, , drop = FALSE]
      w <- (-1 / pr[valid]) * dpr_dV[valid]
      grad <- w %*% X_sub
    }
    else {
      grad <- rep(NA_real_, dim(X)[2])
    }
    return(grad)
  }
  # }}}
  # Optimization {{{
  opt <- function(data, ind) {
    Y <- data[ind, y_lab]
    X <- as.matrix(data[ind, x_lab])

    beta_opt <- optim(
      coef(lm(Y  ~ X))[-1], 
      fn=quasi_likelihood, 
      gr=quasi_gradient,
      method='BFGS',
      Y = Y, X =X
    )
    beta_hat <- beta_opt$par
    return(beta_hat)
  }
  # }}}

  boot_obj <- boot::boot(sample_data, opt, 5)

    boot::boot(sample_data, statistic=function(data, ind){
    Y <- data[ind, y_lab]
    X <- as.matrix(data[ind, x_lab])

    beta_opt <- optim(
      round(coef(lm(Y  ~ X))[-1], 1), 
      fn=quasi_likelihood, 
      gr=quasi_gradient,
      method='BFGS',
      Y = Y, X =X
    )
    beta_hat <- beta_opt$par
    print('bootstrap!')
    return(beta_hat)
  }, 5)$t0

  return(t0)
}
# }}}

# KDE Based Estimator {{{
# Y <- pop$y_ord
# X <- as.matrix(pop[, c(1, 2)])
kde_estimator <- function(Y, X) {
  J <- max(Y)
   
  # Quasi-likelihood {{{
  quasi_likelihood <- function(beta) {
        # beta <- coef(lm(as.numeric(Y) ~ X))[-1]
    V_hat <- X %*% beta

    P_est_mat <- matrix(0, (J + 2L), n)
    for (j in 1:(J-1)) {
      g_bw <- np::npcdensbw(ydat=V_hat, xdat=as.numeric(Y <= j))
      g1 <- (np::npcdens(g_bw, eydat=V_hat, exdat=rep(1, n)))$condens
      g0 <- (np::npcdens(g_bw, eydat=V_hat, exdat=rep(0, n)))$condens


      # Conditional Probabilities
      p1 <- sum(Y <= j) / length(Y)
      p0 <- sum(Y > j) / length(Y)
      P_j_est <- (p1 * g1) / (p1 * g1 + p0 * g0)
      P_est_mat[(j + 1L), ] <- P_j_est
    }
    P_est_mat[1, ] <- rep(0, n)
    P_est_mat[(J + 1L), ] <- rep(1, n)
    P_est_mat[(J + 2L), ] <- rep(1, n)

    # Quasi-likelihood calculation
    pr <- diag(P_est_mat[(Y + 1L), ]) - diag(P_est_mat[Y, ])

    if (all(pr > 0 & !is.na(pr))) {
      print('yes')
      likelihood <- -sum(
        # log(pr[(apply(abs(X) <= quantile(abs(X), 0.95), function(x) all(x), MARGIN=1))])
        log(pr)
      )
    } else {
      print('no')
      likelihood <- Inf
    }
    return(likelihood)
  }
  # }}}

  # Optimization
  beta_opt <- optim(
    coef(lm(Y  ~ X))[-1], 
    fn=quasi_likelihood, 
    method = 'BFGS')
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
  X <- as.matrix(sample_data[, c(1:3)])

  # Ordered logit
  fit_logit <- MASS::polr(ordered(y_ord) ~ x1 + x2 + T, data=sample_data, method = 'logistic', Hess=T)
  beta_hat_logit <- coef(summary(fit_logit))[1:dim(X)[2], 1]
  beta_hat_se_logit <- coef(summary(fit_logit))[1:dim(X)[2], 2]
  print('probit done')

  # Ordered probit
  fit_probit <- MASS::polr(ordered(Y) ~ X, method = 'probit', Hess=T)
  beta_hat_probit <- coef(summary(fit_probit))[1:dim(X)[2], 1]
  beta_hat_se_probit <- coef(summary(fit_probit))[1:dim(X)[2], 2]
  print('probit done')

  # OLS
  fit_ols <- lm(as.numeric(Y) ~ X)
  beta_hat_ols <- coef(summary(fit_ols))[(1:dim(X)[2]+1L), 1]
  beta_hat_se_ols <- coef(summary(fit_ols))[(1:dim(X)[2]+1L), 2]
  print('ols done')

  # Klein and Sherman
  fit_ks <- ks_estimator(y_ord ~ x1 + x2 + T, data=sample_data)
  beta_hat_ks <- fit_ks[[1]]
  beta_hat_se_ks <- fit_ks[[2]]
  print('ks done')

  # KDE
  # beta_hat_kde <- kde_estimator(Y, X)$par

  out <- data.table(
      rbind(
        c(beta_hat_logit, beta_hat_se_logit),
        c(beta_hat_probit, beta_hat_se_probit),
        c(beta_hat_ols, beta_hat_se_ols),
        c(beta_hat_ks, beta_hat_se_ks)
        # c(beta_hat_kde, beta_hat_se_kde),
      )  
  )
  names(out) <- c('X1', 'X2', 'XT', 'X1_se', 'X2_se', 'XT_se')
  out <- out[, ':='(
    models = list('ologit', 'oprobit', 'OLS', 'KS'),
    # models = list('ologit', 'oprobit', 'OLS'),
    n = n,
    loc = loc,
    dist = dist
  )
    ]
  print('One simulation is done!')
  return(out)
}
# }}}

# -------------------------------
# Generate Population
# -------------------------------

# Parameters
n_sim <- 100         # Number of simulations

# -------------------------------
# Simulation
# -------------------------------
ns <- c(1000, 2000)
# n <- ns[1]
locs <- seq(1, 10, 3)
# loc <- locs[1]

estimates <- data.table()
for (n in ns) {
  for (loc in locs) {
    set.seed(42)
    distributions <- c('logistic', 'normal', 'tdis', 'chisq')
    dist <- distributions[4]
    df <- loc
    N <- 1e+5
    beta <- c(-1, 1, 0.5)
    neg <- F
    pop <- generate_pop(dist=dist, df=df, loc=0, beta=beta, negative=neg, N=N, xdim=2)

    cl <- parallel::makePSOCKcluster(4)
    parallel::clusterExport(cl, varlist=c(ls(), list('N', 'n', 'pop', 'simul', 'polr', 'ks_estimator', 'data.table')), envir=environment())
    out <- rbindlist(parReplicate(cl, n_sim, simul(n, pop), simplify=FALSE))
    print(paste('sample size:', n, '\nlocation:', loc))
    estimates <- rbindlist(list(estimates, out))
    fwrite(estimates, file='../data/estimates.csv', bom=T)
    print(estimates[, lapply(.(X1/X2, X1_se, X2/X2, X2_se, XT/X2, XT_se), mean), ,by=.(as.character(models), n, loc, dist)])
  }
}

# -------------------------------
# Simulation Results
# -------------------------------
estimates <- fread('../data/estimates.csv')
estimates[, lapply(.(X1/X2, X1_se, X2/X2, X2_se, XT/X2, XT_se), mean, na.rm=T), ,by=.(as.character(models), n, loc, dist)]

# -------------------------------
# Graph
# -------------------------------
# gd <- estimates[loc%in%c(-7, 7), lapply(.(Xx3/Xx2, Xx2/Xx2), mean), ,by=.(as.character(models), n, loc, dist)]
# ggplot(aes(x=n, y=V1, col=as.character), data=gd) +
#   facet_grid(~loc) +
#   geom_line() +
#   geom_point() + 
#   geom_hline(aes(yintercept=4))
