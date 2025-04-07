# Load required libraries
library('sn')
library('np')
library('parallel')
library('tidyverse')
library('MASS')
library('gridExtra')


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
      f <- as.numeric(gamma[1] + gamma[2] * x + gamma[3] * v - alpha[m] >= 0)
      # f <- sign2(gamma[1] + gamma[2] * x + gamma[3] * v - alpha[m])
      # f <- sign2(y - m) * sign2(gamma[1] + gamma[2] * x + gamma[3] * v - alpha[m])

      Sn <- cbind(Sn, f)
    }
    Smn <- rowSums(Sn)
    # out <- cbind(y, Smn, Sn)
    out <- mean(y == (Smn + 1))
    # out <- mean(abs(y - (Smn + 1)))
    # out <- mean(sign2(y - median(y))*(Smn))
    out <- mean(Smn)
    return(out)
  }
  # }}}

  # Find S_max
  print('Find S_max')
  S_max <- optim(par=c(0,0,0,0,0), fn=S, x=x, v=v,
    method='BFGS', 
    control=list(maxit=1000, fnscale=-1))
    # control=list(maxit=500, fnscale=-1, abstol=1e-10000, ndeps=rep(1, 5)))

  # test
  S_true <- S(x, v, params)
  # head(S_true)
  print(S_max$par)
  print(paste0('S_true: ', S_true))
  print(paste0('S_max: ', S_max$value))

  ### Graph -- holding other coef fixed {{{
  beta <- seq(-3, 5, 0.5)
  St <- numeric(length(beta))
  # pdf(width=15, height=5, file='../figures/gmms_test.pdf')
  layout(mat=matrix(1:5, nrow=1))
  # layout.show(4)
  for (j in 1:(3 + M-1)) {
    gt <- params
    # gt <- S_max$par
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

# Monte Carlo Simulation
# {{{
# MC Parameters
mc <- 100
set.seed(123)
N <- 10**6
mc_results <- data.frame()

# eps <- rnorm(N, 0, 1) # Normal
# epssn <- rsn(N, 0, 2, -20) # Skewed left
# epsln <- rlnorm(N, 6, 1) # lognormal
# eps <- rt(N, 6, 2) # t-distribution
# es <- list(epssn, epsln, epst)

# eps <- rweibull(N, 10, 10) # weibull
# eps <- rlnorm(N, 1, 0.2) # lognormal
# eps <- rsn(N, 0, 1, 20) # Skewed right
eps <- rexp(N, 20) # Skewed right
# eps <- eps - median(eps)
# plot(density(eps))
plot(density(y))
# for (epsN in es) {

  gamma <- c(2, -2, 2) # True coefficients
  # Population {{{
  x <- rnorm(N, mean=1, sd=4)  # Covariate 
  v <- rnorm(N, mean=0, sd=2) # Interval valued covariate

  y_star <- 1*gamma[1] + gamma[2]*x + gamma[3]*v + eps
  alpha <- c(2, 3)
  M <- (length(alpha) + 1)
  y <- cut(y_star, breaks=c(-Inf, alpha, Inf), labels=1:M, right=TRUE)
  y <- as.numeric(as.character(y))  # Convert factor to numeric
  simul_df <- data.frame(y, x, v, eps)
  params <- c(gamma, alpha)
  # }}}
  for (n in c(500, 1500, 2500)){
    # n <- 200
    for (i in 1:mc) {
      # sample data {{{
      ind <- sample(N, n)
      smpl <- simul_df[ind, ]
      yn <- smpl[, 'y']
      xn <- smpl[, 'x']
      vn <- smpl[, 'v']
      epsn <- smpl[, 'eps']
      # }}}

      # Ordered Probit {{{
      oprobit <- polr(factor(yn) ~ xn + vn, method='probit')
      # opro <- coef(summary(oprobit))[1:2, 1:2]
      opro <- coef(oprobit)

      # Ordered Logit
      ologit <- polr(factor(yn) ~ xn + vn, method='logistic')
      # olo <- coef(summary(ologit))[1:2, 1:2]
      olo <- coef(ologit)
      # }}}

      # GMS
      gms_out <- gms(yn, xn, vn, M)

      out <- cbind(t(gms_out$par[2:3]), i, n)
      colnames(out) <- c('beta_1', 'beta_2', 'mc', 'n')
      out <- cbind.data.frame(rbind(gms_out$par[2:3], olo, opro), 
      iter=i, sample_size=n, model=c('gms', 'ologit', 'oprobit'))
      # rownames(out) <- c('gms', 'coef-ologit', 'se-ologit', 'coef-oprobit', 'se-ologit', 'mc', 'n')
      mc_results <- rbind(mc_results, out)
    }
  }
# }
  # }}}

table <- mc_results %>%
  mutate(coef_x = (xn * 2)/vn, coef_v = 2) %>%
  group_by(sample_size, model) %>%
  summarize(across(everything(), mean), .groups='drop')
print(table)

cairo_pdf(width=10, height=5, file='../figures/simul_bias.pdf')
ggplot(aes(y=(coef_x - gamma[2]), x=sample_size, group=model, col=model, fill=model), data=table) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0), lty=2) +
  ylab('bias X') +
  theme_minimal()
dev.off()
