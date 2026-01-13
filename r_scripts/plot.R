library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(ggbreak)   # for broken y-axis

# Load data
df <- read.csv("~/Dropbox/projects/ordinal_outcome/mc_results/mc_expect_all.csv")
# error <- "normal"  # or "lognormal", "mixture_gaussian"
error <- "lognormal"  # or "lognormal", "mixture_gaussian"
# error <- "mixture_gaussian"  # or "lognormal", "mixture_gaussian"

df_error <- df[df$error_name == error, ]

# Choose y-range and break to cover all models, including OLS
# e.g. low segment 0–0.4 for ordinal models, high segment ~1.0–2.5 for OLS
y_min <- -0.2
y_break_low  <- 1.0
y_break_high <- 2.0
y_max <- 2.3   # adjust if your maximum OLS bias/RMSE is larger

plt_rmse <- ggplot(data = df_error, aes(x = n, y = rmse, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(x = "Sample Size (n)",
       y = "RMSE",
       color = "Model") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(limits = c(y_min, y_max)) +
  scale_y_break(c(y_break_low, y_break_high))
file_rmse <- paste0("~/Dropbox/projects/ordinal_outcome/figures/expect_rmse_oprobit_ologit_flow_", error, "_OLS.pdf")
pdf(file = file_rmse, width = 7, height = 5)
print(plt_rmse)
dev.off()

file_bias <- paste0("~/Dropbox/projects/ordinal_outcome/figures/expect_bias_oprobit_ologit_flow_", error, "_OLS.pdf")
pdf(file = file_bias, width = 7, height = 5)
ggplot(data = df_error, aes(x = n, y = bias, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(x = "Sample Size (n)",
       y = "Bias (Estimated - True)",
       color = "Model") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(limits = c(y_min, y_max)) +
  scale_y_break(c(y_break_low, y_break_high))
dev.off()
