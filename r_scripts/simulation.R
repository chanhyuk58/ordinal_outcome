# Load required libraries
library('sn')
# library('moments')
library('np')
library('parallel')
# library('optimParallel')
library('tidyverse')
library('MASS')
# library('modelsummary')


# GMMS function 
# gmms(y, x, v0, v1, M) {{{
gmms <- function(y, x, v0, v1, M) {
  n <- length(y)

  bw <- np::npcdistbw(ordered(y) ~ v0 + v1 + x) 
  # bw <- np::npcdens(ordered(y) ~ v0 + v1 + x) 
  # estimate Pn and Lambda_mn First to avoid repetition {{{
  print('Pn, Lambda_mn')
  ymi <- list()
  pn <- list()
  for (m in 1:(M - 1)){
    # Ymi
    ymi_t <- as.numeric(y > m)
    ymi[[m]] <- ymi_t

    # Estimate Pn(Ymi = m | Xi, v0i, v1i)
    X.eval <- data.frame(y=m, v0, v1, x)
    # pn[[m]] <- predict(np::npcdens(bws=bw), newdata=X.eval)
    pn[[m]] <- 1 - predict(np::npcdist(bws=bw), newdata=X.eval)
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
      # Sn <- cbind(Sn, (ymi[[m]] - 0.5) * ((pn[[m]] > 0.5) * sign2(f1) + (1 - (pn[[m]] > 0.5)) * sign2(f0)))
      Sn <- cbind(Sn, ((pn[[m]] - 0.5) * ((pn[[m]] > 0.5) * sign2(f1) + (1 - (pn[[m]] > 0.5)) * sign2(f0))))
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

# GMS function 
# gms(y, x, v, gamma) {{{
gms <- function(y, x, v, gamma) {
  M <- 1:max(y)
  Sn <- vector() 
    for (m in M[1:length(M) - 1]){
      X.eval <- data.frame(y=m, v0, v1, x)

      # model out
      f <- gamma[1] + gamma[2] * x + gamma[3] * v

      # Y_mi
      y_mi <- (y > m)

      Sn <- cbind(Sn, (y_mi - 1/2) * sign(f))
      }
      Smn <- mean(rowSums(Sn))
      gc()
      return(Smn)
    }
# }}}

# Monte Carlo Simulation
# {{{
# MC Parameters
mc <- 100
set.seed(123)
N <- 10**6
mc_results <- numeric()

# eps <- rnorm(N, 0, 1) # Normal
# epssn <- rsn(N, 0, 2, -20) # Skewed left
# epsln <- rlnorm(N, 6, 1) # lognormal
# epst <- rt(N, 6, 2) # t-distribution
# es <- list(epssn, epsln, epst)

eps <- rweibull(N, 10, 10) # weibull
# eps <- rlnorm(N, 1, 1) # lognormal
# eps <- rsn(N, 0, 1, 20) # Skewed right
# eps <- rexp(N, 2) # Skewed right
eps <- eps - median(eps)
plot(density(eps), xlim=c(-2, 2))
# for (epsN in es) {

  gamma <- c(1, -1, 1) # True coefficients
  # Population {{{
  x <- rnorm(N, mean=1, sd=4)  # Covariate 
  v <- rnorm(N, mean=0, sd=2) # Interval valued covariate

  y_star <- 1*gamma[1] + gamma[2]*x + gamma[3]*v + eps
  alpha <- c(0, 2)
  M <- (length(alpha) + 1)
  y <- cut(y_star, breaks=c(-Inf, alpha, Inf), labels=1:M, right=TRUE)
  y <- as.numeric(as.character(y))  # Convert factor to numeric
  simul_df <- data.frame(y, x, v, eps)
  # }}}
  for (n in c(200, 500, 800, 1100, 1400)){
    # n <- 200
    for (i in 1:mc) {
      # sample data {{{
      smpl <- sample_n(simul_df, n, replace=T)
      yn <- smpl[, 'y']
      xn <- smpl[, 'x']
      vn <- smpl[, 'v']
      v1n <- ceiling(vn)
      v0n <- v1n - 1
      epsn <- smpl[, 'eps']
      # }}}

      # Ordered Probit {{{
      oprobit <- polr(factor(yn) ~ xn + rowMeans(cbind(v0n, v1n)), method='probit')
      # oprobit <- polr(factor(y) ~ x + v, method='probit')
      opro <- coef(summary(oprobit))[1:2, 1:2]

      # Ordered Logit
      ologit <- polr(factor(yn) ~ xn + rowMeans(cbind(v0n, v1n)), method='logistic')
      # ologit <- polr(factor(y) ~ x + v, method='logistic')
      olo <- coef(summary(ologit))[1:2, 1:2]
      opro
      olo
      # }}}

      # GMMS
      # c_star <- NA
      # c_star <- 
        gmms(y=yn, x=xn, v0=v0n, v1=v1n, M=M)
      # bounds <- cbind(apply(c_star, min, MARGIN=2), apply(c_star, max, MARGIN=2))

      # GMS
      # gmse <- optim(par=c(0,0,0), fn=gms, y=y, x=x, v=v, method="BFGS",
      # control=list(fnscale=-1))

      # out <- cbind(bounds[2:3, ], olo, opro, i, n, k)
      out <- cbind(olo, opro, i, n)
      colnames(out) <- c('coef-ologit', 'se-ologit', 'coef-oprobit', 'se-ologit', 'mc', 'n')
      print(out)

      mc_results <- rbind(mc_results, out)
    }
  }
# }
  # }}}

df <- data.frame(cbind(rownames(mc_results), mc_results))
names(df)

head(df)
df <- 
  df %>%
  # filter(V1=='xn') %>%
  mutate(n = as.numeric(n)) %>%
  group_by(V1, n) %>%
  summarize(mean_ologit = mean(as.numeric(coef.ologit)), mean_oprobit = mean(as.numeric(coef.oprobit))) %>%
  gather(key=model, value=coef, mean_ologit:mean_oprobit)

df$true <- NA
df[df$V1 == 'xn', 'bias'] <- df[df$V1 == 'xn', 'coef'] +1
df[df$V1 != 'xn', 'bias'] <- df[df$V1 == 'xn', 'coef'] -1

df[df$V1 == 'xn', 'var'] <- 'X'
df[df$V1 != 'xn', 'var'] <- 'v_midpoint'

df[df$model == 'mean_ologit', 'model2'] <- 'Odered Logit'
df[df$model != 'mean_ologit', 'model2'] <- 'Ordered Probit'

df %>%
  ggplot() +
  facet_wrap(~var) +
  geom_line(aes(x=n, y=bias, group=model2, col=model2)) +
  geom_point(aes(x=n, y=bias, group=model2, col=model2)) +
  theme_minimal()
  # geom_hline(aes(yintercept = true))


names(df)

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
