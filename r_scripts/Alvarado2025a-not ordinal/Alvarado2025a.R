# Load necessary libraries
library(tidyverse)
library(haven)
library(fastDummies)
library(broom)
library(modelsummary) # For coefplots
library(estimatr)    # For robust SEs (optional, but standard in Stata-to-R)


# Set file paths (replace these with your actual paths)
dir0 <- "~/Dropbox/projects/ordinal_outcome/replications/Alvarado2025a/"
output_path <- "./"
gpt_path <- "./"

# ==================================================
#                Import and Fix data
# ==================================================

# 1. Import Data
# Assuming rawdata.csv is in the directory
df <- read_csv(paste0(dir0, "rawdata.csv"))
names(df) <- tolower(names(df))

# 2. Drop those who did not consent
df <- df %>% filter(consent != "0")

# 3. Drop unnecessary variables
df <- df %>%
  dplyr::select(-any_of(c("status", "distributionchannel", "userlanguage",
                   "startdate", "enddate", "consent",
                   "finished", "progress", "q_terminateflag")))

# 4. Drop first two rows (Common in Qualtrics exports)
df <- df %>% slice(-(1:2))

# 5. Destring (Convert columns to numeric where possible)
df <- df %>% mutate(across(everything(), ~type.convert(as.character(.), as.is = TRUE)))

# 6. Renaming and Labelling
# Note: R doesn't show labels in the console like Stata, but 'haven' keeps them for export
df <- df %>%
  rename(
    ideology = q2_1,
    gvt_role = q3,
    pre_trust_gvt = q4,
    equalt = q5_1
  )

# Add variable labels
attr(df$ideology, "label") <- "Ideological self-placement 0(left)-10(right)"
attr(df$gvt_role, "label") <- "Need a strong government to handle economic problems"
attr(df$pre_trust_gvt, "label") <- "How much do you trust British gvts to place needs of nation above (pre-T)?"

# 7. Attention Checks
df <- df %>%
  mutate(
    att1 = if_else(q9 == 3, 1, 0, missing = 0),
    att2 = if_else(q10 == 2, 1, 0, missing = 0),
    att3 = if_else(q11 == 1, 1, 0, missing = 0),
    attention = att1 + att2 + att3
  ) %>%
  dplyr::select(-q9, -q10, -q11, -att1, -att2, -att3)
attr(df$attention, "label") <- "Number of correct answers to mock vignette questions (/3)"

# 8. Treatment Description Logic
df <- df %>%
  mutate(description = case_when(
    treatment == "T1" ~ as.character(q14),
    treatment == "T2" ~ as.character(q17),
    treatment == "T3" ~ as.character(q20),
    TRUE ~ NA_character_
  )) %>%
  dplyr::select(-q14, -q17, -q20)

# 9. Renaming Policy Variables
df <- df %>%
  rename(
    inc_rate = q23_1,
    corp_rate = q24_6,
    # ubi = q106_1,
    # min_wage = q106_2,
    # unemp = q106_3,
    # soc_spend = q106_4,
    tax_profits = q26,
    temp_tax = q27_2,
    wealth_tax = q27_3,
    inher_tax = q27_4,
    rem_loops = q27_6,
    other_tax = q27_7,
    other_tax_open = q27_7_text,
    no_tax = q27_8,
    rich_comp_burden = q50_1,
    rich_comp_profit = q50_2,
    poor_comp_vat = q50_3,
    poor_comp_welf = q50_4,
    corp_comp_profit = q50_5,
    fair_profit = q30,
    manip = q32,
    reason1 = q33_1,
    reason2 = q33_2,
    male = q36,
    age = q37,
    race = q38,
    region = q39,
    hhinc = q40,
    educ = q41,
    marital = q42,
    work = q43,
    partyid = q44,
    post_trust_gvt = q45,
    received = q46_1,
    hh_received = q46_2,
    no_received = q46_0
  )

# 10. Recoding and Cleaning
df <- df %>%
  mutate(
    # Recode male: Stata's replace male=0 if male==9
    male = if_else(male == 9, 0, male),
    # Recode fair_profit: (1=2) (2=1)
    fair_profit = case_when(
      fair_profit == 1 ~ 2,
      fair_profit == 2 ~ 1,
      TRUE ~ fair_profit
    )
  )

# 11. Handle missing values (-99 to NA)
# vars_to_clean <- c("received", "no_received", "hh_received", "hhinc", "ideology", 
#                    "gvt_role", "equalt", "temp_tax", "wealth_tax", "inher_tax", 
#                    "rem_loops", "other_tax", "no_tax", "inc_rate", "corp_rate", 
#                    "soc_spend", "tax_profits", "corp_comp_profit", "fair_profit")
vars_to_clean <- c("received", "no_received", "hh_received", "hhinc", "ideology", 
                   "gvt_role", "equalt", "temp_tax", "wealth_tax", "inher_tax", 
                   "rem_loops", "other_tax", "no_tax", "inc_rate", "corp_rate", 
                   "tax_profits", "corp_comp_profit", "fair_profit")

df <- df %>%
  mutate(across(all_of(vars_to_clean), ~na_if(., -99)))

# 12. Row totals and Tax Dummies
df <- df %>%
  mutate(
    n_tax = rowSums(dplyr::select(., temp_tax, wealth_tax, inher_tax, rem_loops, other_tax), na.rm = TRUE),
    tax_rich = 1 - no_tax
  )

# 13. Treatment Numeric Variable
df <- df %>%
  mutate(treat = case_when(
    treatment == "T1" ~ 1,
    treatment == "T2" ~ 2,
    treatment == "T3" ~ 3,
    treatment == "Control" ~ 0,
    TRUE ~ NA_real_
  ))

# 14. Time spent in treatment
# The Stata code logic: timer_t`num'_pagesubmit if treatment=="T`num'"
df <- df %>%
  mutate(time_treat = coalesce(!!!dplyr::select(., starts_with("timer_t")))) %>%
  # Drop all the individual timer columns now that they are merged
  dplyr::select(-starts_with("timer_t"))

attr(df$time_treat, "label") <- "Time spent in treatment page"

# 15. Create Dummies
df <- df %>%
  mutate(
    dum_white = if_else(race == 1, 1, 0),
    dum_fulltime = if_else(work == 1, 1, 0),
    dum_parttime = if_else(work == 2, 1, 0),
    dum_notlab = if_else(work == 6, 1, 0),
    dum_married = if_else(marital == 1, 1, 0),
    dum_conservative = if_else(partyid == 1, 1, 0),
    dum_labour = if_else(partyid == 2, 1, 0),
    dummy_educ = if_else(educ == 5, 1, 0)
  )

# Region dummies (equivalent to tab region, gen(dumreg_))

df <- df %>%
  # Create a copy of the column with the name you want as a prefix
  mutate(dumreg = region) %>% 
  dummy_cols(
    select_columns = "dumreg", 
    ignore_na = TRUE
  ) %>%
  # Remove the temporary 'dumreg' column (but the 'dumreg_1' etc. will stay)
  dplyr::select(-dumreg)

# Save main file
write.csv(df, paste0(output_path, "uk_jul2023_rep.csv"), row.names=FALSE)

# ==================================================
#    Append response classification (GPT Part)
# ==================================================

# The Stata code uses preserve/restore to stack reason1 and reason2
responses <- df %>%
  dplyr::select(responseid, manip, reason1, reason2) %>%
  pivot_longer(
    cols = c(reason1, reason2),
    names_to = "num_reason",
    values_to = "reason"
  ) %>%
  mutate(num_reason = if_else(num_reason == "reason1", 1, 2))

# Save responses file
write.csv(responses, paste0(gpt_path, "responses.csv"), row.names=FALSE)


# ==================================================
#    Main Analysis
# ==================================================


# --- 1. Load Data ---
df <- read.csv("./uk_jul2023_rep.csv")

# Create stdgroup indicator for normalization
df <- df %>% mutate(stdgroup = (treat == 0))

# --- 2. Standardized Weighted Index (Anderson Index Function) ---
# This function replicates Stata's swindex logic (inverse-covariance weighting)
calculate_anderson_index <- function(data, vars, group_col) {
  # Subset the data to the variables of interest
  sub_data <- as.matrix(data[, vars])
  
  # Calculate mean and SD based on the control group (stdgroup)
  control_means <- colMeans(sub_data[data[[group_col]] == TRUE, ], na.rm = TRUE)
  control_sds <- apply(sub_data[data[[group_col]] == TRUE, ], 2, sd, na.rm = TRUE)
  
  # Standardize the variables
  std_data <- scale(sub_data, center = control_means, scale = control_sds)
  
  # Calculate the inverse of the covariance matrix of the standardized variables
  # (using only complete cases for the matrix inverse calculation)
  sigma_inv <- solve(cov(std_data, use = "pairwise.complete.obs"))
  
  # Sum the rows of the inverse covariance matrix to get weights
  weights <- rowSums(sigma_inv)
  
  # Calculate the weighted average for each row
  index <- as.numeric(std_data %*% weights)
  
  # Full rescale: Mean 0, SD 1 (based on control group)
  control_idx_mean <- mean(index[data[[group_col]] == TRUE], na.rm = TRUE)
  control_idx_sd <- sd(index[data[[group_col]] == TRUE], na.rm = TRUE)
  
  final_index <- (index - control_idx_mean) / control_idx_sd
  return(final_index)
}

# Apply the function to create index_all and index_fair
vars_all <- c("inc_rate", "temp_tax", "wealth_tax", "inher_tax", "rem_loops", "other_tax", "corp_rate")
df$index_all <- calculate_anderson_index(df, vars_all, "stdgroup")

vars_fair <- c("rich_comp_burden", "rich_comp_profit", "poor_comp_vat", "poor_comp_welf", "corp_comp_profit")
df$index_fair <- calculate_anderson_index(df, vars_fair, "stdgroup")

# --- 3. Regression Analysis ---

# Define the controls (including the dummies created earlier)
controls <- c("male", "age", "ideology", "hhinc", names(df)[grep("dumreg_", names(df))])

# Ensure 'treat' is a factor with 0 as the reference (equivalent to ib0.treat)
df$treat <- factor(df$treat, levels = c(0, 1, 2, 3))

write.csv(df, "./Alvarado2025a_cleaned.csv")

# Create formula string
f_all <- as.formula(paste("index_all ~ treat +", paste(controls, collapse = " + ")))
f_all2 <- as.formula(paste("factor(index_all) ~ treat +", paste(controls, collapse = " + ")))
f_fair <- as.formula(paste("index_fair ~ treat +", paste(controls, collapse = " + ")))
f_fair2 <- as.formula(paste("factor(index_fair) ~ treat +", paste(controls, collapse = " + ")))
f_manip <- as.formula(paste("manip ~ treat +", paste(controls, collapse = " + ")))

# Run regressions
i_all2 <- lm(f_all, data = df)
i_fairv2 <- lm(f_fair, data = df)
m_manip <- lm(f_manip, data = df)

i_all2 <- polr(f_all2, data = df, method = "probit")
i_fairv2 <- polr(f_fair2, data = df, method = "probit")
summary(i_all2)
class(i_all2)

sure::autoplot.polr(i_all2, nsim=50, what="qq")
sure::autoplot.polr(i_fairv2, nsim=50, what="qq")

screenreg(list(i_all2, i_fairv2, m_manip))

# --- 4. Plots (Replicating Coefplot) ---

# Define coefficients to keep (the treatment dummies)
cm <- c("treat1" = "T1", "treat2" = "T2", "treat3" = "T3")

# Plot 1: Combined index_all and index_fair
plot_combined <- modelplot(list("Panel A: Support for Taxes" = i_all2, 
                                "Panel B: Perceived Fairness" = i_fairv2),
                           coef_map = cm) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  facet_wrap(~model, ncol = 1, scales = "free_y") +
  labs(title = "Treatment Effects")

ggsave("combined2.jpg", plot = plot_combined, width = 10, height = 6)

# Plot 2: Manipulation check
plot_manip <- modelplot(m_manip, coef_map = cm) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Manipulation Check")

ggsave("manip2.jpg", plot = plot_manip, width = 10, height = 6)
