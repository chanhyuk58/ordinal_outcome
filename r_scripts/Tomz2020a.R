library("haven")
library(tidyverse)
library(modelsummary)
library("MASS")
library("txtplot")
library("texreg")
library("sure")

source("./ks_estimator.R")

# -----------------
# LOAD DATA
# -----------------
dat <- haven::read_dta("~/Dropbox/projects/ordinal_outcome/replications/Tomz2020a/TomzWeeks-HumanRights-JOP-Files/2012-10-01-Main-YouGov/output/2012-10-01-Main-prepped.dta")
names(dat)

# Variable groups
ts <- c("hrts", "democ")  # treatments
ms <- c("threat", "moral", "success", "cost")  # mediators
controls <- c("h1", "i1", "p1", "e1", "r1", "male", "white", "age", "ed4")



# -------------------------------
# TABLE S3: LINEAR REGRESSION (5-POINT DV)
# -------------------------------
lm1 <- lm(strike5 ~ hrts, data = dat)
lm2 <- lm(strike ~ hrts + democ + ., data = dat[, c("strike", "hrts", "democ", controls)])
screenreg(list(lm1, lm2))

logit1 <- polr(factor(strike5) ~ hrts, data = dat, method="logistic")
logit2 <- polr(factor(strike5) ~ hrts + democ + ., data = dat[, c("strike5", "hrts", "democ", controls)], method="logistic")
coef(logit2) / (pi / sqrt(3))
screenreg(list(logit1, logit2))

probit1 <- polr(factor(strike5) ~ hrts, data = dat, method="probit")
probit2 <- polr(factor(strike5) ~ hrts + democ + ., data = dat[, c("strike5", "hrts", "democ", controls)], method="probit")
coef(probit2) / coef(probit2)[2]
screenreg(list(probit1, probit2))

# -------------------------------
# SURE Check
# -------------------------------
resids(probit2)
sure::autoplot.polr(probit2, nsim=50, what="qq")

# -------------------------------
# KS Estimator
# -------------------------------
dat$f_strike5 <- as.numeric(factor(dat$strike5))
dat$hrtsdemoc <- dat$hrts*dat$democ
ks1 <- ks_estimator(f_strike5 ~ hrts + democ + h1 + i1 + p1 + e1 + r1 + male + white + age + ed4, data = dat[, c("f_strike5", "hrts", "democ", "hrtsdemoc", controls)], B=0)

ks1_rescaled <- ks_rescale(ks1)

summary(ks1_rescaled)
coefficients(ks1_rescaled)

write.csv(coefficients(ks1_rescaled), file = "~/Dropbox/projects/ordinal_outcome/r_scripts/Tomz(2).csv", row.names=T)

