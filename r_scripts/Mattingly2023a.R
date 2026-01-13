
# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ------------------------------------------------------------------
# ______________________________________________________________________________

# Author: Trevor Incerti and Changwook Ju

# This code runs the analyses of primary outcome variables:
# Preference for political model, preference for economic model,
# and preference for world leader.

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES AND DATA IMPORT ----------------------------------------------------
# ______________________________________________________________________________

rm(list = ls())

library(DeclareDesign)
library(kableExtra)
library(modelsummary)
library(tidyverse)
library("estimatr")

options(modelsummary_format_numeric_latex = 'plain')
options(modelsummary_factory_latex = 'kableExtra')

setwd("~/Dropbox/projects/ordinal_outcome/r_scripts/")
source("ks_estimator.R")

setwd("~/Dropbox/projects/ordinal_outcome/replications/Mattingly2023a/")

source("0. functions.R")

# Import survey data
load("Data/clean.RData")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# ESTIMATE MODELS --------------------------------------------------------------
# ______________________________________________________________________________

# List of covariates
covs = ~ country + gender + age + education + national_pride + leftright

clean <- clean[, c("political_model", "econ_model", "treatment", "world_leader", "country", "gender", "age", "education", "national_pride", "leftright")]
clean <- na.omit(clean)
write.csv(clean, "~/Dropbox/projects/ordinal_outcome/data/Mattingly2023a.csv", row.names=FALSE)

# Primary outcomes -------------------------------------------------------------
# With covariate adjustment
primary <- list(
  "Political model" = lm_robust(
    political_model ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean
  ),
  "Economic model" = lm_robust(
    econ_model ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean
  ),
  "World leader" = lm_robust(
    world_leader ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean
  )
)

primary_probit <- list(
  "Political model" = MASS::polr(
    factor(political_model) ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean, method = "probit"
  ),
  "Economic model" = MASS::polr(
    factor(econ_model) ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean, method = "probit"
  ),
  "World leader" = MASS::polr(
    factor(world_leader) ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean, method = "probit"
  )
)

primary_logistic <- list(
  "Political model" = MASS::polr(
    factor(political_model) ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean, method = "logistic"
  ),
  "Economic model" = MASS::polr(
    factor(econ_model) ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean, method = "logistic"
  ),
  "World leader" = MASS::polr(
    factor(world_leader) ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean, method = "logistic"
  )
)

ks1 <- ks_estimator(
    factor(political_model) ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean, B = 0
    )
ks1_rs <- ks_rescale(ks1)

modelsummary(primary)
modelsummary(primary_probit)
summary(primary_probit[[1]])
sure::autoplot.polr(primary_probit[[2]], nsim=50, what="qq")
modelsummary(primary_logistic)
coef(primary_logistic[[1]]) / (pi/sqrt(3))
summary(primary_logistic[[2]])$coefficients / (pi/sqrt(3))
sure::autoplot.polr(primary_logistic[[3]], nsim=50, what="qq")

primary <- primary[c("World leader",
                     "Economic model",
                     "Political model")]
primary_tab <- primary[c("Political model",
                         "Economic model",
                         "World leader")]

primary_wwl <- list(
  "Political model" = lm_robust(
    political_model ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean
  ),
  "Economic model" = lm_robust(
    econ_model ~ treatment + country +
      gender + age + education + national_pride + leftright,
    data = clean
  )
)

primary_wwl <- primary_wwl[c("Economic model",
                             "Political model")]

# Without covariate adjustment
primary_nocovs <- list(
  "Political model" = lm_robust(political_model ~ treatment, data = clean),
  "Economic model" = lm_robust(econ_model ~ treatment, data = clean),
  "World leader" = lm_robust(world_leader ~ treatment, data = clean)
)

primary_nocovs <- primary_nocovs[c("World leader",
                                   "Economic model",
                                   "Political model")]

primary_nocovs_tab <- primary_nocovs[c("Political model",
                                       "Economic model",
                                       "World leader")]

primary_nocovs$`Political model`$coefficients[2] / sd(clean$political_model[clean$treatment == "Control"], na.rm = TRUE)

primary_nocovs$`Economic model`$coefficients[2] / sd(clean$econ_model[clean$treatment == "Control"], na.rm = TRUE)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CREATE FIGURES ---------------------------------------------------------------
# ______________________________________________________________________________

# Primary outcomes -------------------------------------------------------------
# With covariates
modelplot(primary,
          coef_omit = 'Interc',
          coef_map = treatments,
          draw = F) %>%
  ggplot(aes(
    x = estimate,
    y = term,
    group = model,
    color = model
  )) +
  gglayers_primary

ggsave(file = "Figures/primary.pdf",
       height = 4,
       width = 7)

modelplot(primary_wwl,
          coef_omit = 'Interc',
          coef_map = treatments,
          draw = F) %>%
  ggplot(aes(
    x = estimate,
    y = term,
    group = model,
    color = model
  )) +
  gglayers_primary_wwl

ggsave(file = "Figures/primary_wwl.pdf",
       height = 3,
       width = 6)

# Without covariates
modelplot(
  primary_nocovs,
  coef_omit = 'Interc',
  coef_map = treatments,
  draw = F
) %>%
  ggplot(aes(
    x = estimate,
    y = term,
    group = model,
    color = model
  )) +
  gglayers_primary

ggsave(file = "Figures/primary_nocovs.pdf",
       height = 4,
       width = 7)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# CREATE TABLES ----------------------------------------------------------------
# ______________________________________________________________________________

# Combine models into one table
table <- c(primary_tab, primary_nocovs_tab)

# Create table -----------------------------------------------------------------

# Rename variables
vars <- c(
  '(Intercept)' = 'Constant',
  'treatmentChina' = 'China',
  'treatmentUSA' = 'USA',
  'treatmentCompetition' = 'Competition'
)

modelsummary(
  table,
  coef_map = vars,
  stars = TRUE,
  gof_omit = omit,
  add_rows = rows,
  fmt = 2,
  notes = c("Note: HC2 robust standard errors in parentheses."),
  output = "latex"
) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  row_spec(c(1, 3, 5, 7), background = '#D3D3D3') %>%
  save_kable("Tables/primary_table.tex")
