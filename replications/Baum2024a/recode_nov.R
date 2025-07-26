library(haven)      # to read Stata files
library(dplyr)      # for data manipulation
library(tidyr)      # for handling NAs

# Load the data
df <- read_dta("November Data File (for replication).dta")

df <- df %>%
  mutate(
    violence051 = case_when(
      el_fair_4 %in% 1:2 ~ 0,
      el_fair_4 == 3 ~ 0.5,
      el_fair_4 %in% 4:5 ~ 1,
      TRUE ~ NA_real_
    )
  )

df <- df %>%
  mutate(
    olderpeople = fn_1,
    chinabioweapon = fn_3,
    bats = fn_6,
    natlemergency = case_when(fn_8 == 2 ~ 1, TRUE ~ 0),
    novaccine = case_when(fn_10 == 2 ~ 1, TRUE ~ 0),
    curewithheld = if_else(fn_11 %in% 2:3, 0, fn_11),
    fluvacriskscovid = if_else(fn_13 %in% 2:3, 0, fn_13),
    hydroxy = if_else(fn_14 %in% 2:3, 0, fn_14),
    facemaskcausecovid = if_else(fn_15 %in% 2:3, 0, fn_15),
    binladenalive = if_else(fn_17 %in% 2:3, 0, fn_17),
    ballotsindumpsters = if_else(fn_18 %in% 2:3, 0, fn_18),
    bidenprofitedfamily = if_else(fn_19 %in% 2:3, 0, fn_19),
    
    misperceptionssum12 = (
      hydroxy + facemaskcausecovid + binladenalive + ballotsindumpsters +
      bidenprofitedfamily + olderpeople + chinabioweapon + bats +
      curewithheld + fluvacriskscovid + natlemergency + novaccine
    ) / 11
  )

df <- df %>%
  mutate(
    trumpsupporter = case_when(
      voted20 %in% c(1, 3, 4, 5) | is.na(voted20) ~ 0,
      voted20 == 2 ~ 1,
      TRUE ~ 0
    ),
    trumpsupporter = if_else(support20 == 2, 1, trumpsupporter)
  )

df <- df %>%
  mutate(
    phq9 = phq9_1 + phq9_2 + phq9_3 + phq9_4 + phq9_5 + phq9_6 + phq9_7 + phq9_8 + phq9_9 - 9
  )

df <- df %>%
  mutate(
    dem = if_else(party == 2, 1, 0),
    rep = if_else(party == 1, 1, 0),
    ind = if_else(party == 3, 1, 0)
  )

df <- df %>%
  mutate(male = if_else(gender_r == 2, 0, gender_r))

df <- df %>%
  mutate(libcon = ideology)

df <- df %>%
  mutate(
    hispanic = if_else(race_r == 2, 1, 0),
    black = if_else(race_r == 3, 1, 0),
    asian = if_else(race_r == 4, 1, 0),
    white = if_else(race_r == 1, 1, 0)
  )

df <- df %>%
  mutate(across(pol_par_1:pol_par_6, ~replace_na(., 0))) %>%
  mutate(
    polactivesum6 = pol_par_1 + pol_par_2 + pol_par_3 + pol_par_4 + pol_par_5 + pol_par_6,
    anypolactive = if_else(polactivesum6 >= 2, 1, polactivesum6)
  )

df <- df %>%
  mutate(
    phq9_fn = phq9 * misperceptionssum12,
    phq9_anypolactive = phq9 * anypolactive,
    phq9_fn_anypolactive = phq9 * anypolactive * misperceptionssum12,
    
    phq9low = if_else(phq9 <= 4, 1, 0),
    phq9med = if_else(phq9 >= 5 & phq9 <= 14, 1, 0),
    phq9hi = if_else(phq9 >= 15, 1, 0),
    
    phq9low_anypolactive = phq9low * anypolactive,
    phq9med_anypolactive = phq9med * anypolactive,
    phq9hi_anypolactive = phq9hi * anypolactive,
    
    phq9low_fn = phq9low * misperceptionssum12,
    phq9med_fn = phq9med * misperceptionssum12,
    phq9hi_fn = phq9hi * misperceptionssum12,
    
    phq9low_anypolactive_fn = phq9low * anypolactive * misperceptionssum12,
    phq9med_anypolactive_fn = phq9med * anypolactive * misperceptionssum12,
    phq9hi_anypolactive_fn = phq9hi * anypolactive * misperceptionssum12
  )

# Save as RDS or CSV; if you want to keep Stata format, use haven
write_dta(df, "November_Data_File_recoded.dta")  # Stata format
# OR
saveRDS(df, "November_Data_File_recoded.rds")     # R format
