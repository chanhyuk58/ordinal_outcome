library('sn')
library('np')
library('parallel')
library('tidyverse')
library('MASS')


# GMMS function 
# gmms(y, x, v0, v1, M) {{{
gmms <- function(y, x, v0, v1, M) {
  n <- length(y)

  # Nadraya-Watson 
  # mNW(x, X, Y, h, K = dnorm) {{{
  mNW <- function(x, X, Y, h, K = dnorm) {

  # Arguments
  # x: evaluation points
  # X: vector (size n) with the predictors
  # Y: vector (size n) with the response variable
  # h: bandwidth
  # K: kernel
    #
    # X <- X.eval
    # Y <- y
    # x <- ymi_t
    # h <- 0.2
    # K <- dnorm
  # Matrix of size length(x) x n
  Kx <- sapply(X, function(Xi) K((x - Xi) / h) / h)

  # Weights
  W <- Kx / rowSums(Kx) # Column recycling!

  # Means at x ("drop" to drop the matrix attributes)
  drop(W * Y)
  }
 # }}}
  # bw <- np::npcdistbw(ordered(y) ~ v0 + v1 + x) 
  bw <- np::npcdensbw(ordered(y) ~ v0 + v1 + x) 
  # estimate Pn and Lambda_mn First to avoid repetition {{{
  print('Pn, Lambda_mn')
  ymi <- list()
  pn <- list()
  for (m in 1:(M - 1)){
    # Ymi
    ymi_t <- as.numeric(y > m)
    ymi[[m]] <- ymi_t
    # X.eval <- cbind(v0, v1, x)
    # pn[[m]] <- colSums(ymi_t * ker) / colSums(ker)
    # pn[[m]] <- mNW(ymi_t, X.eval, y, 0.2)

    # Estimate Pn(Ymi = m | Xi, v0i, v1i)
    X.eval <- data.frame(y=m, v0, v1, x)
    pn[[m]] <- predict(np::npcdens(bws=bw), newdata=X.eval)
    # pn[[m]] <- 1 - predict(np::npcdist(bws=bw), newdata=X.eval)
  }
  # }}}

  # Function S 
  # S(x, v0, v1, pars, pn, ymi) {{{
  S <- function(x, v0, v1, pars, pn, ymi) {
    sign2 <- function(x) {
      if (!is.numeric(x)) {
        stop("Input must be numeric.")
      }
      return(ifelse(x > 0, 1, -1))
    }
    # alpha <- c(0, tail(pars, M - 2))
    alpha <- tail(pars, M - 1)
    # gamma <- c(pars[1:M-1], 1)
    gamma <- pars[1:M]
    Sn <- vector()
    for (m in 1:(M - 1)){
      # model out
      f1 <- gamma[1] + gamma[2] * x + gamma[3] * v1 - alpha[m]
      f0 <- gamma[1] + gamma[2] * x + gamma[3] * v0 - alpha[m]

      # Sn <- cbind(Sn, (ymi[[m]]) * ((pn[[m]] > 0.5) * sign2(f1) + (1 - (pn[[m]] > 0.5)) * sign2(f0)))
      Sn <- cbind(Sn, (ymi[[m]] - 0.5) * ((pn[[m]] > 0.5) * sign2(f1) + (1 - (pn[[m]] > 0.5)) * sign2(f0)))
      # Sn <- cbind(Sn, ((pn[[m]] - 0.5) * ((pn[[m]] > 0.5) * sign2(f1) + (1 - (pn[[m]] > 0.5)) * sign2(f0))))
    }
    Smn <- mean(rowSums(Sn))
    gc()
    return(Smn)
  }
  # }}}

  # Find S_max
  print('Find S_max')
  # S_max <- optim(par=c(0, 0, 0, 0, 0), fn=S, x=x, v0=v0, v1=v1, 
  #   ymi=ymi, pn=pn,
  #   lower=c(-Inf, -Inf, 0, 0, 0), method='L-BFGS-B',
  #   control=list(maxit=500, fnscale=-1))
  S_max <- optim(par=c(0,0,1,0,0), fn=S, x=x, v0=v0, v1=v1, pn=pn, ymi=ymi,
  # S_max <- optim(par=params, fn=S, x=x, v0=v0, v1=v1, pn=pn, ymi=ymi,
    method='BFGS',
    control=list(maxit=500, fnscale=-1))
  gc()

  # Relaxing
  epsilon_N <- log(n)/n

  # test
  S_true <- S(x, v0, v1, params, pn, ymi)
  print(S_true >= (S_max$value - epsilon_N))
  print(paste0('S_true: ', S_true, '\nS_max: ', S_max$value))
  # print(paste0('S_true: ', S_true))

  ### Graph -- holding other coef fixed {{{
  beta <- seq(-2, 2, 0.2)
  St <- numeric(length(beta))
  # pdf(width=15, height=5, file='../figures/gmms_test.pdf')
  par(mfrow=c(1, 5))
  for (j in 1:(3 + M-1)) {
    gt <- params
    for (i in 1:length(beta)) {
      gt[j] <- beta[i]
      St[i] <- S(x, v0, v1, gt, pn, ymi)
    }
    preffixes <- c('beta_0', 'beta_1', 'alpha')
    plot(x=beta, y=St, main=paste0(preffixes[j], ' = ', params[j]))
    abline(v=params[j], lty=2, col='black')
    abline(h=(S_max$value - epsilon_N), lty=3, col='red')
    abline(v=(S_max$par[j]), lty=2, col='blue')
  }
# dev.off()
# }}}

  candidates <- expand.grid(
    lapply(S_max$par, FUN=function(x){seq(x - 1, x + 1, 0.2)})
  )
  print('C_star')
  C_star <- candidates[
    parallel::parApply(cl=cl, candidates, 
      # FUN=function(c){(S(x, v0, v1, c, pn, lam) >= (S_max$value - epsilon_N))}, 
      FUN=function(c){(S(x, v0, v1, c, ymi, lam) >= (S_max$value - epsilon_N))}, 
      MARGIN=1)
    , ]
  return(C_star)
  gc()
}
# }}}

n <- 200
set.seed(123) 
# eps <- rnorm(n, 0, 1) # Normal
# eps <- runif(n, -1, 1) # Normal
# eps <- rsn(n, 0, 5, 20) # Skewed right
eps <- rweibull(n, 1.5, 1) # weibull
ps <- 0
# eps <- rsn(N, 0, 20, -20) # Skewed left
# eps <- rlnorm(n, 10, 1) # lognormal
# eps <- rt(N, 1000, 2) # t-distribution
# es <- list(epssn, epsln, epst)
eps <- eps - median(eps)
gamma <- c(1, -1, 1) # True coefficients
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
# GMMS
cl <- makeCluster(detectCores())
clusterExport(cl, varlist = c(ls(), 'npcdens'), envir=environment())

c_star <- NA
c_star <- gmms(y=y, x=x, v0=v0, v1=v1, M=M)
stopCluster(cl)
bounds <- cbind(apply(c_star, min, MARGIN=2), apply(c_star, max, MARGIN=2))
bounds
