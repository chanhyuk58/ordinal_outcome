# library('sn')
# library('np')
library('parallel')
library('tidyverse')
library('MASS')
# library('philentropy')
library('transport')


# GMS function 
# gms(y, x, v, M) {{{
gms <- function(y, x, v, M) {
  n <- length(y)

  # Function S 
  # S(x, v, pars) {{{
  S <- function(x, v, pars) {
    sign2 <- function(x) {
      if (!is.numeric(x)) {
        stop("Input must be numeric.")
      }
      return(ifelse(x > 0, 1, -1))
    }
    # pars <- params
    # alpha <- c(0, tail(pars, M - 2))
    alpha <- tail(pars, M - 1)
    # gamma <- c(pars[1:M-1], 1)
    gamma <- pars[1:M]
    Sn <- vector()
    for (m in 1:(M - 1)){
      # model out
      f <- sign2(gamma[1] + gamma[2] * x + gamma[3] * v1 - alpha[m])

      Sn <- cbind(Sn, f)
    }
    Smn <- rowSums(Sn)
    out <- mean(as.numeric(y == (Smn + 1)))
    return(out)
  }
  # }}}

  # Find S_max
  print('Find S_max')
  S_max <- optim(par=c(0,0,0,0,0), fn=S, x=x, v=v,
    method='BFGS', control=list(maxit=500, fnscale=-1))
  gc()


  # test
  S_true <- S(x, v, params)
  print(paste0('S_true: ', S_true))

  ### Graph -- holding other coef fixed {{{
  beta <- seq(-2, 2, 0.2)
  St <- numeric(length(beta))
  # pdf(width=15, height=5, file='../figures/gmms_test.pdf')
  par(mfrow=c(1, 5))
  for (j in 1:(3 + M-1)) {
    gt <- params
    for (i in 1:length(beta)) {
      gt[j] <- beta[i]
      St[i] <- S(x, v, gt)
    }
    preffixes <- c('beta_0', 'beta_1', 'beta_2', 'alpha_0', 'alpha_1')
    plot(x=beta, y=St, main=paste0(preffixes[j], ' = ', params[j]))
    abline(v=params[j], lty=2, col='black')
    abline(v=(S_max$par[j]), lty=2, col='blue')
  }
# dev.off()
# }}}
  return(S_max)
}
# }}}

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
  
  data <- as.data.frame(cbind(X, e))
  data$y_ord <- factor(y_ord, ordered = TRUE)
  return(data)
}
# }}}

# Compute biases for one replication {{{
simulate_bias <- function(n) {
  # Sample
  idx = sample(N, n)
  # Ordered logit
  fit_logit <- polr(y_ord ~ ., data = data[idx, ], method = "logistic")
  beta_hat_logit <- coef(fit_logit)[1] # / coef(fit_logit)[xdim]
  
  # Ordered probit
  fit_probit <- polr(y_ord ~ ., data = data[idx,], method = "probit")
  beta_hat_probit <- coef(fit_probit)[1] # / coef(fit_probit)[5]
  
  # OLS
  fit_ols <- lm(as.numeric(y_ord) ~ ., data=data[idx, ])
  beta_hat_ols <- coef(fit_ols)[2]
  
  c(logit = beta_hat_logit - beta[1],
    probit = beta_hat_probit - beta[1],
    ols = beta_hat_ols - beta[1]
  )
}
# }}}

# KL Divergence {{{
# Discretize the support (only positive part due to Chi-squared)
x_vals <- seq(0, 10, length.out = 1000)

# Get density values
pd <- density(rnorm(N))
p <- pd$y
qd <- density(-(rchisq(N, df = 1) - 1)) 
q <- qd$y

# Normalize to get proper probability mass vectors (they must sum to 1)
p <- p / sum(p)
q <- q / sum(q)

# Combine into a matrix as required by philentropy (rows = distributions)
dist_matrix <- rbind(p, q)

# Compute KL divergence from p (normal) to q (chi-squared)
kl_result <- KL(dist_matrix, unit = "log")

# Display result
cat("KL divergence D_KL(N(0,1) || Chi^2_1) =", kl_result, "\n")
# }}}

# Wasserstein Distance {{{
# Generate samples
a <- sort(rnorm(10e+5))
b <- sort(data$e)

# Compute 1D Wasserstein distance (p=1)
wdist <- wasserstein1d(a, b)

cat("Wasserstein (p=1) distance =", wdist, "\n")
# }}}

# Settings
distributions <- c("logistic", "normal", "tdis", "chisq")
dist <- distributions[4]
df <- 5
locations <- seq(-10, -5, 5)
N <- 10e+5
all <- data.frame()
for (loc in locations) {
  beta <- c(-0.1, 1)
  loc <- 5
  neg = TRUE
  data <- generate_pop(dist=dist, df=df, beta=beta, negative=neg, location=loc)
  sample_sizes <- seq(500, 4000, 500)
  
  hist(as.numeric(data$y_ord), main=paste('n:', n, '| location:', loc))
  
  # Run Simulations {{{
  data <- subset(data, select=-e)
  simulation_results <- data.frame()
  for (n in sample_sizes){
    seed <- 42
    set.seed(seed)
    results <- data.frame()
    for (mc in seq(1, 100, 1)){
      results <- rbind(results, simulate_bias(n=n))
    }
    names(results) <- c('logit', 'probit', 'ols')
    results_df <- data.frame(
      n = n,
      distribution = dist,
      model = names(results),
      bias = colMeans(results),
      location = loc
    )
    simulation_results <- rbind(simulation_results, results_df)
  }
  # }}}
  all <- rbind(all, simulation_results)
}

# Plot {{{
file_name <- paste0('../figures/seed', seed, '_negative', neg, '_', dist, df, '_locations(2).pdf')
# file_name <- paste0('../figures/seed', seed, '_negative', neg, '_', dist, df, '.pdf')
cairo_pdf(file=file_name, width=10, height=5)
  ggplot(all, aes(x = as.numeric(n), y = bias, col = model)) +
    facet_wrap(~location, scales='free') +
    geom_line() +
    geom_point() +
    labs(title = paste0("Bias of Ordered Logit and Probit (β1 =", beta[1], ")"),
         x = "Sample Size (n)",
         y = "Bias (Estimated - True)",
         fill = "Model") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_size = 14) +
    geom_hline(yintercept=beta[1], lty=2) +
    geom_hline(yintercept=-beta[1], lty=2)
dev.off()
# }}}
