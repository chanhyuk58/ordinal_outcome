library("data.table")
library("ggplot2")
library("latex2exp")


# -------------------------------
# Simulation Results
# -------------------------------
# others <- fread("../data/estimates_tdisdf1_others.csv")
others <- fread("../data/estimates_normal1_others.csv")
# ks <- fread("../data/estimates_tdisdf1_ks(2).csv")
ks <- fread("../data/estimates_normal1_ks.csv")

others_bias <- others[models != "OLS", (lapply(.(XT/X1 - 0.5), mean)), ,by=.(models, n, par, dist)]
ols_bias <- others[models == "OLS", (lapply(.(XT - 0.5), mean)), ,by=.(models, n, par, dist)]
ks_bias <- ks[order(models, par), lapply(.(XT/X1 - 0.5), mean), ,by=.(models, n, par, dist)]

bias <- rbindlist(list(others_bias, ols_bias, ks_bias))

others_rmse <- 
  others[models != "OLS", mean := (lapply(.(XT/X1), mean)), ,by=.(models, n, par, dist)][
  models != "OLS", lapply(.(sqrt((XT/X1 - mean)**2)), mean), by=.(models, n, par, dist)
  ]
ols_rmse <- 
  others[models == "OLS", mean := (lapply(.(XT/X1), mean)), ,by=.(models, n, par, dist)][
  models == "OLS", lapply(.(sqrt((XT/X1 - mean)**2)), mean), by=.(models, n, par, dist)
  ]
ks_rmse <- 
  ks[, mean := (lapply(.(XT/X1), mean)), ,by=.(models, n, par, dist)][
  , lapply(.(sqrt((XT/X1 - mean)**2)), mean), by=.(models, n, par, dist)
  ]

rmse_df <- rbindlist(list(others_rmse, ols_rmse, ks_rmse))

# -------------------------------
# Graph
# -------------------------------
pdf(width=6, height=6, file="../figures/bias_tdis.pdf")
ggplot(aes(x=n, y=V1, col=models), data=bias) +
  geom_line() +
  geom_point() + 
  geom_hline(aes(yintercept=0)) +
  ylim(-0.5, 0.1) +
  theme_minimal() +
  ylab(TeX("Bias ($\\tau_{ALTE} - \\hat{\\tau}_{ALTE})")) +
  xlab("Sample Size") +
  scale_color_hue(breaks = c("OLS", "KS", "ologit", "oprobit"), labels = c("OLS", "KDE", "Ordered Logit", "Ordered Probit")) +
  scale_x_continuous(breaks=seq(0, 1000, 250))
dev.off()

pdf(width=6, height=6, file="../figures/rmse_tdis.pdf")
ggplot(aes(x=n, y=abs(V1), col=models), data=rmse_df) +
  geom_line() +
  geom_point() + 
  geom_hline(aes(yintercept=0)) +
  ylim(0, 0.5) +
  theme_minimal() +
  ylab(TeX("RMSE ($\\sqrt{(\\hat{\\tau}_{ALTE} - \\mathbb{E}[\\hat{\\tau}_{ALTE}])})")) +
  xlab("Sample Size") +
  scale_color_hue(breaks = c("OLS", "KS", "ologit", "oprobit"), labels = c("OLS", "KDE", "Ordered Logit", "Ordered Probit")) + 
  scale_x_continuous(breaks=seq(0, 1000, 250))
dev.off()
