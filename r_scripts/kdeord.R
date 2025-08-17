# Required Libraries
library("MASS")  # For multivariate normal data generation
# library("sn")
# library("np")    # For Non-parametric KDE
library("data.table")
# library("ggplot2")
# library("sure")
# library("moments")
# library("texreg")
library("txtplot")
# library("KernSmooth")
# library("lpdensity")
# library("hdrcde")

# -------------------------------
# Functions
# -------------------------------

# Function to do parallel replicate {{{
parReplicate <- function(cl, rep, expr, simplify=TRUE, USE.NAMES=TRUE){
  parallel::parSapply(cl, integer(rep), function(i, ex) eval(ex, envir=.GlobalEnv), substitute(expr), simplify=simplify, USE.NAMES=USE.NAMES)
}
# }}}

# Generate the population {{{
generate_pop <- function(N=10e+5, xdim=2, beta=c(1, 1), dist="logistic", 
                         par=1, negative=FALSE, location=0, sd=1, ncp=0) {

  X <- matrix(rnorm(N*(xdim), 0, 5), ncol=xdim)
  X <- cbind(X, sample(c(0,1), N, replace=T))
  # X <- cbind(rep(1, N), sample(c(0,1), N, replace=T))
  colnames(X) <- c(paste0("x", 1:xdim), "T")
  # colnames(X) <- c("Intercept", "T")

  e <- switch(dist,
    logistic = rlogis(N),
    normal   = rnorm(N, mean=location, sd=par),
    snormal  = rsn(N, alpha=par),
    lnormal  = rlnorm(N, sdlog=par),
    st       = rst(N, omega=1, nu=3, alpha=par),
    exp      = rexp(N, rate=par),
    tdis     = rt(N, df=par),
    chisq    = rchisq(N, df=par, ncp=ncp) + location,
    stop("Unknown distribution"))

  if (negative == TRUE) {
    e <- -e
  }
  txtdensity(e)

  y_star <- X %*% beta + e
  txtdensity(y_star)

  thresholds <- quantile(y_star, c(0.3, 0.7))
  y_ord <- cut(y_star, breaks = c(-Inf, thresholds, Inf),
    labels = FALSE, ordered_result = TRUE)
  txtbarchart(factor(y_ord))

  data <- as.data.frame(cbind(X, e))
  data$y_ord <- as.numeric(y_ord)
  return(data)
}
# }}}

## Pilot KDE {{{
pilot_kde <- function(Y, V, l, h_p, sigma, j, n) {
  Kernel <- function(x) {return(dnorm(x))} # Gaussian Kernel
  kde_values <- numeric(n)
  if (l == 1) {
    n1 <- length(V[Y <= j])
    if (n1 > 0) {
      for (i in 1:n) {
        kde_values[i] <- 1/n1 * 
          sum(
            Kernel((V[i] - V[-i][Y[-i] <= j]) / (sigma * h_p)) / (sigma * h_p)
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

## Smooth Damping weights {{{
compute_local_bandwidth <- function(pilot_density, sigma, n, delta) {
  # pilot_density <- g1_pilot
  # summary(log(pilot_density))
  # summary(pilot_density)
  m <- exp(sum(log(pilot_density)) / length(pilot_density))
  l <- pilot_density / m
  a_n <- log(n)^(-1)
  d <- 1 / (1 + exp(-n^((1/40 - delta*1/20)/2) * (l - a_n)))
  lambda <- sigma / sqrt(l * d + a_n * (1 - d))
  return(lambda)
}
# }}}

## Final KDE {{{
final_kde <- function(Y, V, l, lambda, h, j, n) {
  Kernel <- function(x) {return(dnorm(x))} # Gaussian Kernel
  kde_values <- numeric(n)
  if (l == 1) {
    n1 <- length(V[Y <= j])
    if (n1 > 0) {
      for (i in 1:n) {
        kde_values[i] <- 1/n1 * 
          sum(
            Kernel((V[i] - V[Y <= j]) / (lambda[Y <= j] * h)) / (lambda[Y <= j] * h)
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
            Kernel((V[i] - V[Y > j]) / (lambda[Y > j] * h)) / (lambda[Y > j] * h)
          )
      }
    }
  }
  return(kde_values)
}
# }}}

# Klien and Sherman (2002) Estimator {{{
ks_estimator <- function(formula, data, B=100) {
  data <- data.frame(data)
  data <- data[complete.cases(data[, all.vars(formula)]), ]
  y_lab <- all.vars(formula)[1]
  x_lab <- all.vars(formula)[-1]
  # Y <- data[, y_lab]
  # X <- data[, x_lab]

  J <- max(data[, y_lab])
  n <- length(data[, y_lab])

  delta <- 1/6
  h_p <- n^( -(1/10 + (3 + delta)/3)/2 )           # Pilot bandwidth
  h <- n^( ( (3 + delta)/20 + 1/6 ) /2 )             # Final bandwidth

  Kernel <- function(x) {return(dnorm(x))} # Gaussian Kernel
  # Kernel_prime <- function(u) {return(-u * dnorm(u))}  # derivative of standard normal density

  # Quasi-likelihood {{{
  quasi_likelihood <- function(beta, Y, X) {
    P_est_mat <- array(0, dim=c((J + 2L), n))
    V_hat <- X %*% beta

    for (j in 1:(J)) {
      n1 <- sum(Y <= j)
      n0 <- sum(Y > j)

      # Pilot KDE estimation
      sigma1 <- sd(V_hat[Y <= j])
      sigma0 <- sd(V_hat[Y > j])

      g1_pilot <- pilot_kde(Y, V_hat, 1, h_p, sigma1, j, n)
      g0_pilot <- pilot_kde(Y, V_hat, 0, h_p, sigma0, j, n)

      # Local bandwidths
      lambda1 <- compute_local_bandwidth(g1_pilot, sigma1, n1, delta)
      lambda0 <- compute_local_bandwidth(g0_pilot, sigma0, n0, delta)

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
    # trim_lvl <- min(0.999, 0.895 + 0.0001 * n)
    trim_lvl <- 0.95
    valid <- (apply(abs(X) <= quantile(abs(X), trim_lvl), function(x) all(x), MARGIN=1))
    pr <- pr[valid]
    if (all(pr > 0 & !is.na(pr))) {
      print("Yes")
      likelihood <- -sum(
        log(pr)
      )
    } else {
      print("no")
      likelihood <- 1e+100
    }
    return(likelihood)
  }
  # }}}
  # Optimization {{{
  opt <- function(data, ind) {
    Y <- as.numeric(factor(data[ind, y_lab]))
    X <- as.matrix(data[ind, x_lab])

    s <- coef(lm(Y ~ X))[-1]
    beta_opt <- optim(
      s/s[1], 
      fn=quasi_likelihood, 
      # gr=quasi_gradient,
      method="BFGS",
      Y = Y, X = X
    )

    i <- i + 1
    beta_hat <- beta_opt$par
    print(paste("#####", i, "th KS boot! #####"))
    return(beta_hat)
  }
  # }}}

  i <- 0 # bootstrap replication counter
  boot_obj <- boot::boot(data, opt, B)
  out <- list()
  # out$boot_raw <- boot_obj$t
  out$coef <- boot_obj$t0
  out$bootstrap_se <- apply(boot_obj$t, sd, MARGIN=2) 
  out$bootstrap_mean <- apply(boot_obj$t, mean, MARGIN=2) 

  return(out)
}
# }}}

# # KDE Based Estimator {{{
# kde_estimator <- function(formula, data, B=200) {
#   y_lab <- all.vars(formula)[1]
#   x_lab <- all.vars(formula)[-1]
#   # Y <- sample_data[, "y_ord"]
#   # X <- as.matrix(sample_data[, c("x1", "T")])
#
#   J <- max(data[, y_lab])
#   n <- length(data[, y_lab])
#
#   # Quasi-likelihood {{{
#   quasi_likelihood <- function(beta, Y, X) {
#     P_est_mat <- matrix(0, (J + 2L), n)
#     # beta <- coef(lm(as.numeric(Y) ~ X))[-1]
#     V_hat <- X %*% beta
#
#     for (j in 1:(J-1)) {
#       # g_hdrcde <- (hdrcde::cde(y=V_hat, x=as.numeric(Y <= j), rescale=FALSE,
#       # x.margin=c(0, 1), y.margin=V_hat, x.nmae="Y <= j", y.name="V_hat"))
#       # g0 <- g_hdrcde$z[1, match(V_hat, g_hdrcde$y)]
#       # g1 <- g_hdrcde$z[2, match(V_hat, g_hdrcde$y)]
#       # txtplot(V_hat, g1)
#
#       # g1 <- lpdensity::lpdensity(data=V_hat[Y <= j], grid=V_hat)
#       # head(g1$Estimate)
#       # txtplot(g1$Estimate[, 1], g1$Estimate[, 5])
#       # g1$BW[, 2]
#       # $Estimate[, 5]
#       # g0 <- lpdensity::lpdensity(data=V_hat[Y > j], grid=V_hat, bwselect = "mse-dpi", kernel="epanechnikov")$Estimate[, 5]
#       # # plot(g1)
#       g1 <- np::npudens(tdat=V_hat[Y <= j], edat=V_hat)$dens
#       g0 <- np::npudens(tdat=V_hat[Y > j], edat=V_hat)$dens
#       # txtplot(as.vector(g1$eval$frame), g1$dens)
#
#       # Conditional Probabilities
#       p1 <- sum(Y <= j) / length(Y)
#       p0 <- sum(Y > j) / length(Y)
#       P_j_est <- (p1 * g1) / (p1 * g1 + p0 * g0)
#       P_est_mat[(j + 1L), ] <- P_j_est
#     }
#     P_est_mat[1, ] <- rep(0L, n)
#     P_est_mat[(J + 1L), ] <- rep(1, n) # - colSums(P_est_mat[1:J, ])
#     P_est_mat[(J + 2L), ] <- rep(1, n)
#
#     # Quasi-likelihood calculation
#     pr <- diag(P_est_mat[(Y + 1L), ]) - diag(P_est_mat[Y, ])
#     # summary(pr)
#
#     if (all(pr > 0 & !is.na(pr))) {
#       print("yes")
#       likelihood <- -sum(
#         # log(pr[(apply(abs(X) <= quantile(abs(X), 0.95), function(x) all(x), MARGIN=1))])
#         log(pr)
#       )
#     } else {
#       print("no")
#       likelihood <- 1e+100
#     }
#     return(likelihood)
#   }
#   # }}}
#   # Optimization {{{
#   opt <- function(data, ind) {
#     Y <- data[ind, y_lab]
#     X <- as.matrix(data[ind, x_lab])
#
#     beta_opt <- optim(
#       coef(lm(Y ~ X))[-1], 
#       fn=quasi_likelihood, 
#       method="BFGS",
#       Y = Y, X = X
#     )
#     beta_hat <- beta_opt$par
#     # beta_hat / beta_hat[1]
#
#     i <- i + 1
#     print(paste("#####", i, "th boot! #####"))
#     return(beta_hat)
#   }
#   # }}}
#
#   i <- 0 # bootstrap replication counter
#   boot_obj <- boot::boot(data, opt, B)
#   out <- list()
#   # out$boot_raw <- boot_obj$t
#   out$coef <- boot_obj$t0
#   out$bootstrap_se <- apply(boot_obj$t, sd, MARGIN=2) 
#   out$bootstrap_mean <- apply(boot_obj$t, mean, MARGIN=2) 
#
#   return(out)
# }
# # }}}

# One Simulation {{{

# {{{
# lines(density(rlogis(1000)), lty=2)
# plot(gof(fit_logit, nsim=100, test="ks"))
# plot(density(res))
# lines(density(res))

# skewness(rlnorm(1e+5, sdlog=10))
#
# res <- resids(fit_logit)
# skewness(res)
# res2 <- resids(fit_probit)
# skewness(res2)

# pdf(width=5, height=5, file="../figures/ks_logit_lnorm10.pdf")
# plot(gof(fit_logit, nsim=50, test="ks"))
# plot(gof(fit_probit, nsim=50, test="ks"))
# sure::autoplot.polr(fit_logit, nsim=100, what="qq", distribution=qlogis)
# sure::autoplot.polr(fit_probit, nsim=100, what="qq", distribution=qnorm)
# dev.off()
# }}}

simul <- function(n, pop) {
  # Sample
  idx = sample(N, n)
  sample_data <- pop[idx, ]
  # sample_data <- pop

  formula <- ordered(y_ord) ~ x1 + T
  formula2 <- (y_ord) ~ x1 + T

  # # Ordered logit
  # fit_logit <- MASS::polr(formula, data=sample_data, method = "logistic", Hess=T)
  # beta_hat_logit <- coef(summary(fit_logit))[1:(xdim + 1), 1]
  # # beta_hat_logit / beta_hat_logit[1]
  # beta_hat_se_logit <- coef(summary(fit_logit))[1:(xdim + 1), 2]
  # print("ologit done")
  #
  # # Ordered probit
  # fit_probit <- MASS::polr(formula, data=sample_data, method = "probit", Hess=T)
  # beta_hat_probit <- coef(summary(fit_probit))[1:(xdim + 1), 1]
  # # beta_hat_probit / beta_hat_probit[1]
  # beta_hat_se_probit <- coef(summary(fit_probit))[1:(xdim + 1), 2]
  # print("oprobit done")
  #
  # # OLS
  # fit_ols <- lm(formula2, data=sample_data)
  # beta_hat_ols <- coef(summary(fit_ols))[(2:(xdim + 2)), 1]
  # # beta_hat_ols / beta_hat_ols[2]
  # beta_hat_se_ols <- coef(summary(fit_ols))[(2:(xdim + 2)), 2]
  # print("ols done")

  # screenreg(list(fit_logit, fit_probit, fit_ols))

  # Klein and Sherman   
  # start_time <- Sys.time()
  fit_ks <- ks_estimator(formula2, data=sample_data, B=0)
  # end_time <- Sys.time()
  # execution_time <- end_time - start_time
  # print(execution_time)

  # beta_hat_ks <- fit_ks$bootstrap_mean
  beta_hat_ks <- fit_ks$coef
  # beta_hat_ks / beta_hat_ks[1]
  # beta_hat_se_ks <- fit_ks$bootstrap_se
  beta_hat_se_ks <- c(0, 0)
  print("ks done")

  # KDE
  # start_time <- Sys.time()
  # fit_kde <- kde_estimator(formula2, data=sample_data, B=0)
  # end_time <- Sys.time()
  # execution_time <- end_time - start_time
  # print(execution_time)

  # beta_hat_kde <- fit_kde$coef
  # beta_hat_kde <- fit_kde$bootstrap_mean
  # beta_hat_kde / beta_hat_kde[1]
  # beta_hat_se_kde <- apply(fit_kde$boot_raw / fit_kde$boot_raw[, 1], sd, MARGIN=2)
  # beta_hat_se_kde <- c(0, 0)
  # print("kde done")

  out <- data.table(
      rbind(
        # c(beta_hat_logit, beta_hat_se_logit),
        # c(beta_hat_probit, beta_hat_se_probit),
        # c(beta_hat_ols, beta_hat_se_ols)
        c(beta_hat_ks, beta_hat_se_ks)
        # c(beta_hat_kde, beta_hat_se_kde)
      )  
  )
  
  names(out) <- c(paste0("X", 1:xdim), "XT", paste0("X", 1:xdim, "_se"), "XT_se")
  out <- out[, ":="(
    # models = list("ologit", "oprobit", "OLS", "KS", "KDE"),
    # models = list("ologit", "oprobit", "OLS", "KS"),
    models = list("KS"),
    # models = list("ologit", "oprobit", "OLS"),
    n = n,
    par = par,
    dist = dist
  )
    ]
  print("One simulation is done!")
  return(out)
}
# }}}

# -------------------------------
# Generate Population
# -------------------------------

# Parameters
n_sim <- 1000        # Number of simulations

# -------------------------------
# Simulation
# -------------------------------
ns <- c(250, 500, 750, 1000)
# n <- ns[1]
# pars <- c(0.2, 2.5)
# pars <- c(T, F)
# par <- pars[1]

# distributions <- c("logistic", "normal", "snormal", "st", "tdis", "chisq", "exp", "lnormal")

pairs <- list(c("tdis", 1))

estimates <- data.table()
for (n in ns) {
    for (pair in pairs) {
        set.seed(42)
        N <- 1e+5
        beta <- c(1, 0.5)
        xdim <- 1
        neg <- F
        dist <- pair[1]
        par <- as.numeric(pair[2])

        pop <- generate_pop(dist=dist, par=par, beta=beta, negative=neg, N=N, xdim=xdim)

        cl <- parallel::makePSOCKcluster(4)
        # obj_lst <- list("N", "n", "pop", "simul", "polr", "ks_estimator", "data.table", "kde_estimator", "npudens", "npudensbw")
        obj_lst <- list("N", "n", "pop", "simul", "polr", "ks_estimator", "data.table")
        parallel::clusterExport(cl, varlist=c(ls(), obj_lst), envir=environment())
        out <- rbindlist(parReplicate(cl, n_sim, simul(n, pop), simplify=FALSE))
        print(paste("sample size:", n, "df:", par))
        estimates <- rbindlist(list(estimates, out))
        fwrite(estimates, file="../data/estimates_tdisdf1_ks.csv", bom=T)
        print(estimates[order(as.character(models), par), lapply(.(XT/X1, XT_se), mean), ,by=.(as.character(models), n, par, dist)])
        pop <- NA
    }
}

# -------------------------------
# Simulation Results
# -------------------------------
# estimates <- fread("../data/estimates_tdist_df1to10.csv")
# head(estimates)
# print(estimates[order(as.character(models), loc), lapply(.(XT/X2), mean), ,by=.(as.character(models), n, loc, dist)])

# -------------------------------
# Graph
# -------------------------------
# gd <- estimates[loc%in%c(-7, 7), lapply(.(Xx3/Xx2, Xx2/Xx2), mean), ,by=.(as.character(models), n, loc, dist)]
# ggplot(aes(x=n, y=V1, col=as.character), data=gd) +
#   facet_grid(~loc) +
#   geom_line() +
#   geom_point() + 
#   geom_hline(aes(yintercept=4))
