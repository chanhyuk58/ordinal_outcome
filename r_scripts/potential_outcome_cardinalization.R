library("MASS")
library("xtable")

set.seed(42)
n <- 5
beta <- 0.2
Yl0 <- rnorm(n)
Yl1 <- beta + rnorm(n)


sum(Yl0 > Yl1)

lte <- mean(Yl1) - mean(Yl0)
cbind(Yl0, Yl1)

g_t <- function(x) {
  cut(x, 
    # breaks = c(-Inf, -5, 0.2, 1, 5, Inf),
    breaks = c(-Inf, -3, -1, 0, 2, Inf),
    labels = c("Strongly Disagree", "Disagree Somewhat", "Neither", "Agree Somewhat", "Strongly Agree")
  )
}

g <- function(x) {
  cut(x, 
    breaks = c(-Inf, -3, -1, 0, 2, Inf),
    labels = c(1, 2, 3, 4, 5)
  )
}

Y0_t <- g_t(Yl0)
Y1_t <- g_t(Yl1)

Y0 <- g(Yl0)
Y1 <- g(Yl1)

t1 <- cbind(
  round(Yl0, 2), 
  round(Yl1, 2), 
  as.character(Y0_t), 
  as.character(Y1_t))
xtable(t1)

t2 <- cbind(
  round(Yl0, 2), 
  round(Yl1, 2), 
  as.character(Y0), 
  as.character(Y1))
xtable(t2)

ols <- mean(as.numeric(as.character(Y1))) - mean(as.numeric(as.character(Y0)))

pY0 <- ecdf(Y0)
pY1 <- ecdf(Y1)

alte <- - mean(c(
  (qnorm(pY1(1)) - qnorm(pY0(1))),
  (qnorm(pY1(2)) - qnorm(pY0(2))),
  (qnorm(pY1(3)) - qnorm(pY0(3))),
  (qnorm(pY1(4)) - qnorm(pY0(4))),
  (qnorm(pY1(5)) - qnorm(pY0(5)))
), na.rm=T)


cbind(Yl0, Yl1, Y0, Y1)
rbind(lte, ols, alte)
