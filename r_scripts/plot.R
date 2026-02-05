library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbreak)   # for broken y-axis
library(patchwork)

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

file_bias_beamer <- paste0("~/Dropbox/projects/ordinal_outcome/figures/bias_", error)

model_order <- c("ols", "ologit", "oprobit", "ks", "flow")
df_error$model <- factor(df_error$model, levels = model_order)

beamer_df <- df_error[df_error$n == 1000, ]
beamer_df <- beamer_df %>%
  mutate(
    variance = rmse^2 - bias^2,
    variance = ifelse(variance < 0, 0, variance),
    sd = sqrt(variance)
  )

#{{{
# Define the global order and mapping once
model_order <- c("ols", "ologit", "oprobit", "ks", "flow")

generate_plot <- function(data_subset, filename) {
  
  # 1. Define clean display names
  model_labels <- c(
    "ols"     = "OLS", 
    "ologit"  = "Ordered Logit", 
    "oprobit" = "Ordered Probit", 
    "ks"      = "KDE", 
    "flow"    = "NF"
  )
  
  # 2. Define consistent colors using Color Brewer Set2
  # We map specific hex codes to specific model IDs
  model_colors <- c(
    "ols"     = "#66C2A5", # Greenish
    "ologit"  = "#FC8D62", # Orange
    "oprobit" = "#8DA0CB", # Blue/Purple
    "ks"      = "#E78AC3", # Pink
    "flow"    = "#A6D854"  # Light Green
  )

  p <- ggplot(data_subset, aes(x = model, y = bias, color = model)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.7) +
    
    # Use coord_cartesian to keep OLS error bars visible if they exceed 2.5
    geom_errorbar(aes(ymin = bias - sd, ymax = bias + sd), width = 0.1) +
    geom_point(size = 3) +
    
    # Labels and Theme
    labs(y = "Bias (Estimated ATE - True ATE)", x = "") +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_line(linetype = "dashed", color = "gray"),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_text(face = "bold"),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "none" # Color is redundant because of X-axis labels
    ) +
    
    # Apply the fixed color mapping
    scale_color_manual(values = model_colors) +
    
    # Apply the display names and keep the X-axis positions fixed
    scale_x_discrete(limits = model_order, labels = model_labels) +
    
    scale_y_continuous(breaks = seq(-0.5, 2.5, by = 0.5)) +
    coord_cartesian(ylim = c(-0.5, 2.5))
  
  ggsave(filename, plot = p, width = 7, height = 4.5, units = "in")
}
#}}}

reveal_sequence <- list(
  c("ols"),
  c("ols", "ologit", "oprobit"),
  c("ols", "ologit", "oprobit", "ks", "flow")
)
for (i in 1:length(reveal_sequence)) {
  subset_data <- beamer_df[beamer_df$model %in% reveal_sequence[[i]],]
  generate_plot(subset_data, paste0(file_bias_beamer, i, ".pdf"))
}
