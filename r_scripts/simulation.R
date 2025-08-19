library("data.table")
library("ggplot2")
library("latex2exp")


# -------------------------------
# Simulation Results
# -------------------------------
others <- fread("../data/estimates_tdisdf1_others.csv")
# others <- fread("../data/estimates_norm1_others(1).csv")
ks <- fread("../data/estimates_tdis1_ks(1).csv")
# ks <- fread("../data/estimates_norm1_ks(1).csv")

others_bias <- others[!(models %in% c("OLS", "OLS1", "OLS2")), (lapply(.(XT/X1 - 0.5), mean)), ,by=.(models, n, par, dist)]
ols_bias <- others[(models %in% c("OLS", "OLS1", "OLS2")), (lapply(.(XT - 0.5), mean)), ,by=.(models, n, par, dist)]
others[(models %in% c("OLS", "OLS1", "OLS2")), (lapply(.(XT), mean)), ,by=.(models, n, par, dist)]
ks_bias <- ks[order(models, par), lapply(.(XT/X1 - 0.5), mean), ,by=.(models, n, par, dist)]

bias <- rbindlist(list(others_bias, ols_bias, ks_bias))
bias <- bias[models != "OLS2"]

others_rmse <- 
  others[!(models %in% c("OLS", "OLS1", "OLS2")), lapply(.(sqrt((XT/X1 - 0.5)**2)), mean), by=.(models, n, par, dist)]
ols_rmse <- 
  others[(models %in% c("OLS", "OLS1", "OLS2")), lapply(.(sqrt((XT/X1 - 0.5)**2)), mean), by=.(models, n, par, dist)]
ks_rmse <- 
  ks[, lapply(.(sqrt((XT/X1 - 0.5)**2)), mean), by=.(models, n, par, dist)]

rmse_df <- rbindlist(list(others_rmse, ols_rmse, ks_rmse))
rmse_df <- rmse_df[(models != "OLS2")]

# -------------------------------
# Graph
# -------------------------------
pdf(width=6, height=6, file="../figures/bias_norm.pdf")
ggplot(aes(x=n, y=V1, col=models), data=bias) +
  geom_line() +
  geom_point() + 
  geom_hline(aes(yintercept=0)) +
  ylim(-0.5, 0.1) +
  theme_minimal() +
  ylab(TeX("Bias ($\\tau_{ALTE} - \\hat{\\tau}_{ALTE})")) +
  xlab("Sample Size") +
  # scale_color_hue(breaks = c("OLS1", "OLS2", "KS", "ologit", "oprobit"), labels = c("OLS: 1, 2, 3", "OLS: 1, 5, 10", "KDE", "Ordered Logit", "Ordered Probit")) +
  scale_color_hue(breaks = c("OLS1", "KS", "ologit", "oprobit"), labels = c("OLS", "KDE", "Ordered Logit", "Ordered Probit")) +
  scale_x_continuous(breaks=seq(0, 1000, 250))
dev.off()

pdf(width=6, height=6, file="../figures/rmse_norm.pdf")
ggplot(aes(x=n, y=abs(V1), col=models), data=rmse_df) +
  geom_line() +
  geom_point() + 
  geom_hline(aes(yintercept=0)) +
  ylim(0, 0.5) +
  theme_minimal() +
  ylab(TeX("RMSE")) +
  xlab("Sample Size") +
  # scale_color_hue(breaks = c("OLS1", "OLS2", "KS", "ologit", "oprobit"), labels = c("OLS: 1, 2, 3", "OLS: 1, 5, 10", "KDE", "Ordered Logit", "Ordered Probit")) +
  scale_color_hue(breaks = c("OLS", "KS", "ologit", "oprobit"), labels = c("OLS", "KDE", "Ordered Logit", "Ordered Probit")) +
  scale_x_continuous(breaks=seq(0, 1000, 250))
dev.off()
