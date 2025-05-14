# polr.fit {{{
polr.fit <- function(x, y, wt, start, offset, method, ...)
{
    fmin <- function(beta) {
        theta <- beta[pc + ind_q]
        gamm <- c(-Inf , cumsum(c(theta[1L], exp(theta[-1L]))), Inf)
        eta <- offset
        if (pc) eta <- eta + drop(x %*% beta[ind_pc])
        pr <- pfun(pmin(100, gamm[y + 1] - eta)) -
            pfun(pmax(-100, gamm[y] - eta))
        if (all(pr > 0)) -sum(wt * log(pr)) else Inf
    }

    gmin <- function(beta)
    {
        jacobian <- function(theta) { ## dgamma by dtheta matrix
            k <- length(theta)
            etheta <- exp(theta)
            mat <- matrix(0 , k, k)
            mat[, 1L] <- rep(1, k)
            for (i in 2L:k) mat[i:k, i] <- etheta[i]
            mat
        }
        theta <- beta[pc + ind_q]
        gamm <- c(-Inf, cumsum(c(theta[1L], exp(theta[-1L]))), Inf)
        eta <- offset
        if(pc) eta <- eta + drop(x %*% beta[ind_pc])
        z1 <- pmin(100, gamm[y+1L] - eta)
        z2 <- pmax(-100, gamm[y] - eta)
        pr <- pfun(z1) - pfun(z2)
        p1 <- dfun(z1); p2 <- dfun(z2)
        g1 <- if(pc) t(x) %*% (wt*(p1 - p2)/pr) else numeric()
        xx <- .polrY1*p1 - .polrY2*p2
        g2 <- - t(xx) %*% (wt/pr)
        g2 <- t(g2) %*% jacobian(theta)
        if(all(pr > 0)) c(g1, g2) else rep(NA_real_, pc+q)
    }

    pfun <- switch(method, logistic = plogis, probit = pnorm,
                   loglog = pgumbel, cloglog = pGumbel, cauchit = pcauchy)
    dfun <- switch(method, logistic = dlogis, probit = dnorm,
                   loglog = dgumbel, cloglog = dGumbel, cauchit = dcauchy)
    n <- nrow(x)
    pc <- ncol(x)
    ind_pc <- seq_len(pc)
    lev <- levels(y)
    if(length(lev) <= 2L) stop("response must have 3 or more levels")
    y <- unclass(y)
    q <- length(lev) - 1L
    ind_q <- seq_len(q)
    Y <- matrix(0, n, q)
    .polrY1 <- col(Y) == y; .polrY2 <- col(Y) == (y - 1L)
    # pc could be 0.
    s0 <- if(pc) c(start[seq_len(pc+1L)], log(diff(start[-seq_len(pc)])))
    else c(start[1L], log(diff(start)))
    res <- optim(s0, fmin, gmin, method="BFGS", ...)
    beta <- res$par[seq_len(pc)]
    theta <- res$par[pc + ind_q]
    zeta <- cumsum(c(theta[1L], exp(theta[-1L])))
    deviance <- 2 * res$value
    names(zeta) <- paste(lev[-length(lev)], lev[-1L], sep="|")
    if(pc) names(beta) <- colnames(x)
    list(coefficients = beta, zeta = zeta, deviance = deviance, res = res)
}
# }}}

set.seed(42)

# Simulate Data
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
x <- cbind(x1, x2)

# Define coefficients and thresholds
beta_true <- c(0.8, -0.5)
thresholds <- c(-1, 0, 1)

# Compute linear predictor (eta)
eta <- x %*% beta_true

# Generate probabilities for each category
p1 <- plogis(thresholds[1] - eta)
p2 <- plogis(thresholds[2] - eta) - p1
p3 <- plogis(thresholds[3] - eta) - p2 - p1
p4 <- 1 - (p1 + p2 + p3)

# Sample response variable (ordinal levels)
y <- apply(cbind(p1, p2, p3, p4), 1, function(p) sample(1:4, 1, prob = p))

y <- factor(cut(eta, breaks = c(-Inf, thresholds, Inf),
    labels = FALSE, ordered_result = TRUE))


# Set initial values
start_values <- c(0, 0, 0, 0, 0)

# Fit the model using polr.fit
model_custom <- polr.fit(x = x, y = y, wt = rep(1, n), start = start_values, 
                         offset = rep(0, n), method = "logistic")

# Output the results
coef <- model_custom$coefficients
coef[1] / coef[2]
