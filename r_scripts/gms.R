library('sn')
library('np')
library('parallel')
library('tidyverse')
library('MASS')


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

# KDE inspired method
kdeord <- function(y, x, v, M) {
  # estimate p(y | x, v)
  np.cdencbw(y ~ x + v)
}

n <- 200
set.seed(123) 
# eps <- rnorm(n, 0, 1) # Normal
# eps <- runif(n, -1, 1) # Normal
# eps <- rsn(n, 0, 5, 20) # Skewed right
eps <- rweibull(n, 1.5, 1) # weibull
# eps <- rsn(N, 0, 20, -20) # Skewed left
# eps <- rlnorm(n, 10, 1) # lognormal
# eps <- rt(N, 1000, 2) # t-distribution
# es <- list(epssn, epsln, epst)
eps <- eps - median(eps)
gamma <- c(2, -1, 1) # True coefficients
#{{{
x <- rnorm(n, mean=0, sd=5)  # Covariate 
v <- rnorm(n, mean=0, sd=3) # Interval valued covariate
v1 <- ceiling(v)
v0 <- v1 - 1

y_star <- gamma[1] + gamma[2]*x + gamma[3]*v + eps
# alpha <- sort(runif(2, 0, quantile(y_star, 0.98)))
# alpha <- alpha - min(alpha) # Normalize
alpha <- c(0, 2)
M <- (length(alpha) + 1)
y <- cut(y_star, breaks=c(-Inf, alpha, Inf), labels=1:M, right=TRUE)
y <- as.numeric(as.character(y))  # Convert factor to numeric
params <- c(gamma, alpha)
# }}}

# Ordered Probit {{{
oprobit <- polr(factor(y) ~ x + rowMeans(cbind(v0, v1)), method='probit')
# oprobit <- polr(factor(y) ~ x + v, method='probit')
opro <- coef(summary(oprobit))[1:4, 1:2]

# Ordered Logit
ologit <- polr(factor(y) ~ x + rowMeans(cbind(v0, v1)), method='logistic')
# ologit <- polr(factor(y) ~ x + v, method='logistic')
olo <- coef(summary(ologit))[1:4, 1:2]
opro
olo
# }}}

par(mfrow=c(1, 3))
plot(density(y_star))
plot(density(eps))
hist(y)

gms <- gms(y, x, v, M)
