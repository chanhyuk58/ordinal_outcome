# This script creates all figures for the AJPS article:
  # The Political Consequences of Depression: How Conspiracy Beliefs, Self-Efficacy, 
  # and Depression Affect Support for Political Violence
# Code by Matthew Simonson
# Last Updated: 2023-June-6
# Status: Good to go!
# Note: the following packages must be installed: 
  # tidyverse 1.3.1 or higher f(or general data manipulation)
  # readxl 1.4.0 or higher (for reading in the data from an excel file)
  # patchwork 1.1.1 or higher (for combining plots)
  # ggh4x 0.2.3 or higher
  # ggstance 0.3.5 or higher

### PLEASE SET WORKING DIRECTORY TO SOURCE FILE LOCATION ### 
# Setup -------------------------------------------------------------------

cat("\14")
rm(list = ls())

## Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl) # to load excel files
library(patchwork) # to combine plots into one figure (appendix only)

## Strings -----------------------------------------------------------------

# name of input data file
main_results_filepath <- "Stata Output for R - Main Figures.xlsx"
si_results_filepath <- "Stata Output for R - SI Figures.xlsx"
# height and width of figures
w <- 6.5
h <- 6

# labels for each outcome variable
nov_lab <- "November Hypothetical\nElection Violence"
jan_lab <- "January Hypothetical\nElection Violence"
storm_lab <- "Support Capitol Riot"

# labels for each set of values used to generate outcomes
levels_h1 <- 
  c("Low depression\n         Low conspiracy beliefs",      
    "Low depression\n        High conspiracy beliefs",  
    "High depression\n        Low conspiracy beliefs", 
    "High depression\n       High conspiracy beliefs")

levels_h2 <- 
  c("Low depression\nLow participatory inclination",      
    "Low depression\nHigh participatory inclination",  
    "High depression\nLow participatory inclination", 
    "High depression\nHigh participatory inclination")

levels_h3 <- 
  c("Low depression\nLow participatory inclination\nLow conspiracy beliefs",      
    "Low depression\nLow participatory inclination\nHigh conspiracy beliefs",  
    "Low depression\nHigh participatory inclination\nLow conspiracy beliefs",  
    "Low depression\nHigh participatory inclination\nHigh conspiracy beliefs",
    "High depression\nLow participatory inclination\nLow conspiracy beliefs", 
    "High depression\nLow participatory inclination\nHigh conspiracy beliefs", 
    "High depression\nHigh participatory inclination\nLow conspiracy beliefs",
    "High depression\nHigh participatory inclination\nHigh conspiracy beliefs")

# labels for each set of values in the trichotomous depression models
levels_h1_moderate <- 
  c("Low depression\nLow conspiracy beliefs",      
    "Low depression\nHigh conspiracy beliefs",  
    "Moderate depression\nLow conspiracy beliefs", 
    "Moderate depression\nHigh conspiracy beliefs")


levels_h2_moderate <- 
  c("Low depression\nLow participatory inclination",      
    "Low depression\nHigh participatory inclination",  
    "Moderate depression\nLow participatory inclination", 
    "Moderate depression\nHigh participatory inclination")

levels_h3_moderate <- 
  c("Low depression\nLow participatory inclination\nLow conspiracy beliefs",      
    "Low depression\nLow participatory inclination\nHigh conspiracy beliefs",  
    "Low depression\nHigh participatory inclination\nLow conspiracy beliefs",  
    "Low depression\nHigh participatory inclination\nHigh conspiracy beliefs",
    "Moderate depression\nLow participatory inclination\nLow conspiracy beliefs", 
    "Moderate depression\nLow participatory inclination\nHigh conspiracy beliefs", 
    "Moderate depression\nHigh participatory inclination\nLow conspiracy beliefs",
    "Moderate depression\nHigh participatory inclination\nHigh conspiracy beliefs")

## Functions -------------------------------------------------------------

# this function cleans up the variable labels output by Stata to make them 
# publication-ready
clean_input_table <- function(tab){
  tab %>%
    rename(Scenario = 1) %>%
    mutate(
      value_label = sprintf("%.2f", Mean),
      Scenario = Scenario %>% 
        
        str_replace_all("Low conspiracy beliefs/Low depression", 
                        "Low depression/Low conspiracy beliefs")  %>%
        str_replace_all("Low conspiracy beliefs/Moderate depression", 
                        "Moderate depression/Low conspiracy beliefs")  %>%
        str_replace_all("Low conspiracy beliefs/High depression", 
                        "High depression/Low conspiracy beliefs")  %>%

        str_replace_all("High conspiracy beliefs/Low depression", 
                        "Low depression/High conspiracy beliefs")  %>%
        str_replace_all("High conspiracy beliefs/Moderate depression", 
                        "Moderate depression/High conspiracy beliefs")  %>%
        str_replace_all("High conspiracy beliefs/High depression", 
                        "High depression/        High conspiracy beliefs")  %>%
        
        str_replace_all("Low participatory inclination/Low depression", 
                        "Low depression/Low participatory inclination")  %>%
        str_replace_all("Low participatory inclination/Moderate depression", 
                        "Moderate depression/Low participatory inclination")  %>%
        str_replace_all("Low participatory inclination/High depression", 
                        "High depression/Low participatory inclination")  %>%
        
        str_replace_all("High participatory inclination/Low depression", 
                        "Low depression/High participatory inclination")  %>%
        str_replace_all("High participatory inclination/Moderate depression", 
                        "Moderate depression/High participatory inclination")  %>%
        str_replace_all("High participatory inclination/High depression", 
                        "High depression/High participatory inclination")  %>%
        
        str_replace_all("/", "\n")

      )
  }

# this function plots the results
plot_results <- function(tab, facet_var="Outcome", appendix = F){
  the_plot <- tab %>%
    ggplot(aes(y = Scenario, 
                       x = Mean, 
                       xmin = `95% CI Low`, 
                       xmax = `95% CI High`, 
                       label = value_label)) + 
    geom_text(size = 2.5, nudge_y = 0.22) +
    ggh4x::facet_wrap2(facet_var, axes = "all", remove_labels = "y") +
    ggstance::geom_pointrangeh(shape = 1) 
  
  if(appendix == F){
    the_plot <- the_plot  + 
      theme_classic() +
      theme(axis.title.y = element_blank(),
            axis.text = element_text(color="Black"),
            text = element_text(size = 9)) +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank())
  } else{
    the_plot <- the_plot  + 
      theme_bw() +
      theme(text = element_text(size = 8))
  }
  
  the_plot
          
}

# Main Text ---------------------------------------------------------------

## Figure 1 ----------------------------------------------------------------

### Read and clean data -----------------------------------------------------

h1_nov_main <- read_excel(main_results_filepath, sheet = "Figure 1a", range = "A5:D9") %>%
  mutate(Outcome = nov_lab, 
         Hypothesis = 1) %>%
  clean_input_table()

h1_jan_main <- read_excel(main_results_filepath, sheet = "Figure 1a", range = "A15:D19") %>%
  mutate(Outcome = jan_lab, 
         Hypothesis = 1) %>%
  clean_input_table()

h1_storm_main <- read_excel(main_results_filepath, sheet = "Figure 1a", range = "A25:D29") %>%
  mutate(Outcome = storm_lab, 
         Hypothesis = 1) %>%
  clean_input_table()



h2_nov_main <- read_excel(main_results_filepath, sheet = "Figure 1b", range = "A5:D9") %>%
  mutate(Outcome = nov_lab, 
         Hypothesis = 2) %>%
  clean_input_table()

h2_jan_main <- read_excel(main_results_filepath, sheet = "Figure 1b", range = "A15:D19") %>%
  mutate(Outcome = jan_lab, 
         Hypothesis = 2) %>%
  clean_input_table()

h2_storm_main <- read_excel(main_results_filepath, sheet = "Figure 1b", range = "A25:D29") %>%
  mutate(Outcome = storm_lab, 
         Hypothesis = 2) %>%
  clean_input_table()

### Plot -----------------------------------------------------

h1_main <- bind_rows(h1_nov_main, h1_jan_main, h1_storm_main) %>% 
  mutate(Outcome = fct_relevel(Outcome, nov_lab)) %>% 
  plot_results()  + 
  #ggtitle('Hypothesis 1') +
  expand_limits(x = c(0, .35))

h2_main <- bind_rows(h2_nov_main, h2_storm_main, h2_jan_main) %>% 
  mutate(Outcome = fct_relevel(Outcome, nov_lab)) %>% 
  plot_results() +
  #ggtitle('Hypothesis 2') + 
  expand_limits(x = c(0, .35))

h1_main
ggsave("figures/Figure_1A.png", width = w, height = 2.5)
h2_main
ggsave("figures/Figure_1B.png", width = w, height = 2.5)

## Figure 2 ----------------------------------------------------------------

### Read and clean data -----------------------------------------------------
h3_nov_main <- read_excel(main_results_filepath, sheet = "Figure 2", range = "A5:D13") %>%
  mutate(Outcome = nov_lab, 
         Hypothesis = 3) %>%
  clean_input_table()

h3_jan_main <- read_excel(main_results_filepath, sheet = "Figure 2", range = "A19:D27") %>%
  mutate(Outcome = jan_lab, 
         Hypothesis = 3) %>%
  clean_input_table()


h3_storm_main <- read_excel(main_results_filepath, sheet = "Figure 2", range = "A33:D41") %>%
  mutate(Outcome = storm_lab, 
         Hypothesis = 3) %>%
  clean_input_table()

### Plot --------------------------------------------------------------------

bind_rows(h3_nov_main, h3_jan_main, h3_storm_main) %>% 
  mutate(Outcome = fct_relevel(Outcome, nov_lab)) %>% 
  plot_results() +
  #ggtitle('Hypothesis 3') + 
  expand_limits(x = c(0, .35))

ggsave("figures/Figure_2.png", width = w, height = 5)

## Figure 3 ----------------------------------------------------------------

### Read and clean data -----------------------------------------------------
h1_nov_party_dem <- read_excel(main_results_filepath, sheet = "Figure 3a", range = "A5:D9") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Democrat") %>%
  clean_input_table()

h1_nov_party_rep <- read_excel(main_results_filepath, sheet = "Figure 3a", range = "A15:D19") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Republican") %>%
  clean_input_table()

h2_nov_party_dem <- read_excel(main_results_filepath, sheet = "Figure 3b", range = "A5:D9") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Democrat") %>%
  clean_input_table()

h2_nov_party_rep <- read_excel(main_results_filepath, sheet = "Figure 3b", range = "A15:D19") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Republican") %>%
  clean_input_table()

### Plot --------------------------------------------------------------------
h1_party <- bind_rows(h1_nov_party_dem, h1_nov_party_rep) %>% 
  plot_results(facet_var = "Party") + 
  #ggtitle('Hypothesis 1') + 
  expand_limits(x = c(0, .35))

h2_party <- bind_rows(h2_nov_party_dem, h2_nov_party_rep) %>% 
  plot_results(facet_var = "Party") +
  #ggtitle('Hypothesis 2') + 
  expand_limits(x = c(0, .35))


h1_party
ggsave("figures/Figure_3A.png", width = w, height = 2.5)
h2_party
ggsave("figures/Figure_3B.png", width = w, height = 2.5)


## Figure 4 ----------------------------------------------------------------

### Read and clean data -----------------------------------------------------
h3_nov_party_dem <- read_excel(main_results_filepath, sheet = "Figure 4", range = "A5:D13") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Democrat") %>%
  clean_input_table()

h3_nov_party_rep <- read_excel(main_results_filepath, sheet = "Figure 4", range = "A19:D27") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Republican") %>%
  clean_input_table()

### Plot --------------------------------------------------------------------

h3_party <- bind_rows(h3_nov_party_dem, h3_nov_party_rep) %>% 
  plot_results(facet_var = "Party") + 
  #ggtitle('Hypothesis 3') + 
  expand_limits(x = c(0, .35))

h3_party
ggsave("figures/Figure_4.png", width = w, height = 5)

## Figure 5 ----------------------------------------------------------------

### Read and clean data -----------------------------------------------------
h1_nov_trump_no <- read_excel(main_results_filepath, sheet = "Figure 5a", range = "A5:D9") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Non-Supporter") %>%
  clean_input_table()

h1_nov_trump_yes <- read_excel(main_results_filepath, sheet = "Figure 5a", range = "A15:D19") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Trump Supporter") %>%
  clean_input_table()

h2_nov_trump_no <- read_excel(main_results_filepath, sheet = "Figure 5b", range = "A5:D9") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Non-Supporter") %>%
  clean_input_table()

h2_nov_trump_yes <- read_excel(main_results_filepath, sheet = "Figure 5b", range = "A15:D19") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Trump Supporter") %>%
  clean_input_table()

### Plot --------------------------------------------------------------------

h1_trump <- bind_rows( h1_nov_trump_no, h1_nov_trump_yes) %>% 
  plot_results(facet_var = "Trump") + 
  #ggtitle('Hypothesis 1') + 
  expand_limits(x = c(0, .35))

h2_trump <- bind_rows(h2_nov_trump_no, h2_nov_trump_yes) %>% 
  plot_results(facet_var = "Trump") + 
  #ggtitle('Hypothesis 2') + 
  expand_limits(x = c(0, .35))

h1_trump
ggsave("figures/Figure_5A.png", width = w, height = 2.5)
h2_trump
ggsave("figures/Figure_5B.png", width = w, height = 2.5)


## Figure 6 ----------------------------------------------------------------

### Read and clean data -----------------------------------------------------
h3_nov_trump_no <- read_excel(main_results_filepath, sheet = "Figure 6", range = "A5:D13") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Non-Supporter") %>%
  clean_input_table()

h3_nov_trump_yes <- read_excel(main_results_filepath, sheet = "Figure 6", range = "A19:D27") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Trump Supporter") %>%
  clean_input_table()

h3_trump <- bind_rows(h3_nov_trump_no, h3_nov_trump_yes) %>% 
  plot_results(facet_var = "Trump") + 
  #ggtitle('Hypothesis 3') + 
  expand_limits(x = c(0, .35))

h3_trump
ggsave("figures/Figure_6.png", width = w, height = 5)

## Figure 7 -------------------------------------------------------------

### Read and clean data -----------------------------------------------------
h1_jan_gender_men <- read_excel(main_results_filepath, sheet = "Figure 7a", range = "A5:D9") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Men") %>%
  clean_input_table()

h1_jan_gender_women <- read_excel(main_results_filepath, sheet = "Figure 7a", range = "A15:D19") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Women") %>%
  clean_input_table()

h2_jan_gender_men <- read_excel(main_results_filepath, sheet = "Figure 7b", range = "A5:D9") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Men") %>%
  clean_input_table()

h2_jan_gender_women <- read_excel(main_results_filepath, sheet = "Figure 7b", range = "A15:D19") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Women") %>%
  clean_input_table()

### Plot --------------------------------------------------------------------
h1_gender <- bind_rows(h1_jan_gender_women, h1_jan_gender_men) %>% 
  plot_results(facet_var = "gender") + 
  #ggtitle('Hypothesis 1') + 
  expand_limits(x = c(0, .35))

h2_gender <- bind_rows(h2_jan_gender_women, h2_jan_gender_men) %>% 
  plot_results(facet_var = "gender") + 
  #ggtitle('Hypothesis 2') + 
  expand_limits(x = c(0, .35))

h1_gender
ggsave("figures/Figure_7A.png", width = w, height = 2.5)
h2_gender
ggsave("figures/Figure_7B.png", width = w, height = 2.5)

## Figure 8 ----------------------------------------------------------------

### Read and clean data -----------------------------------------------------
h3_jan_gender_men <- read_excel(main_results_filepath, sheet = "Figure 8", range = "A5:D13") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Men") %>%
  clean_input_table()

h3_jan_gender_women <- read_excel(main_results_filepath, sheet = "Figure 8", range = "A19:D27") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Women") %>%
  clean_input_table()

### Plot --------------------------------------------------------------------

h3_gender <- bind_rows(h3_jan_gender_women, h3_jan_gender_men) %>% 
  plot_results(facet_var = "gender") + 
  #ggtitle('Hypothesis 3') + 
  expand_limits(x = c(0, .35))

h3_gender
ggsave("figures/Figure_8.png", width = w, height = 5)


# Online Appendix ---------------------------------------------------------

## Test for Moderate Depression ------------------------------------------------------

### Figure A.2-------------------------------------------------------------------

h1_nov_moderate <- read_excel(si_results_filepath, sheet = "Figure A.2", range = "A7:D11") %>%
  mutate(Outcome = nov_lab, 
         Hypothesis = 1) %>%
  clean_input_table()

h1_jan_moderate <- read_excel(si_results_filepath, sheet = "Figure A.2", range = "A17:D21") %>%
  mutate(Outcome = jan_lab, 
         Hypothesis = 1) %>%
  clean_input_table()

h1_storm_moderate <- read_excel(si_results_filepath, sheet = "Figure A.2", range = "A27:D31") %>%
  mutate(Outcome = storm_lab, 
         Hypothesis = 1) %>%
  clean_input_table()



h2_nov_moderate <- read_excel(si_results_filepath, sheet = "Figure A.2", range = "F7:I11") %>%
  mutate(Outcome = nov_lab, 
         Hypothesis = 2) %>%
  clean_input_table()

h2_jan_moderate <- read_excel(si_results_filepath, sheet = "Figure A.2", range = "F17:I21") %>%
  mutate(Outcome = jan_lab, 
         Hypothesis = 2) %>%
  clean_input_table()

h2_storm_moderate <- read_excel(si_results_filepath, sheet = "Figure A.2", range = "F27:I31") %>%
  mutate(Outcome = storm_lab, 
         Hypothesis = 2) %>%
  clean_input_table()


h1_moderate <- bind_rows(h1_nov_moderate, h1_jan_moderate, h1_storm_moderate) %>% 
  mutate(Outcome = fct_relevel(Outcome, nov_lab),
         Scenario = fct_rev(fct_relevel(Scenario, levels_h1_moderate))) %>% 
  plot_results(appendix = T)  + 
  ggtitle('Hypothesis 1') + expand_limits(x = c(0, .35))

h2_moderate <- bind_rows(h2_nov_moderate, h2_jan_moderate, h2_storm_moderate) %>% 
  mutate(Outcome = fct_relevel(Outcome, nov_lab),
         Scenario = fct_rev(fct_relevel(Scenario, levels_h2_moderate))) %>% 
  plot_results(appendix = T) +
  ggtitle('Hypothesis 2') + expand_limits(x = c(0, .35))

h1_moderate / h2_moderate 

ggsave("figures/SI/Figure_A2.png", width = w, height = h)


### Figure A.3 ----------------------------------------------------------------------

h3_nov_moderate <- read_excel(si_results_filepath, sheet = "Figure A.3", range = "A7:D15") %>%
  mutate(Outcome = nov_lab, 
         Hypothesis = 3) %>%
  clean_input_table() 

h3_jan_moderate <- read_excel(si_results_filepath, sheet = "Figure A.3", range = "A21:D29") %>%
  mutate(Outcome = jan_lab, Hypothesis = 3) %>%
  clean_input_table()

h3_storm_moderate <- read_excel(si_results_filepath, sheet = "Figure A.3", range = "A35:D43") %>%
  mutate(Outcome = storm_lab, Hypothesis = 3) %>%
  clean_input_table()


bind_rows(h3_nov_moderate, h3_jan_moderate, h3_storm_moderate) %>% 
  mutate(Outcome = fct_relevel(Outcome, nov_lab),
         Scenario = fct_rev(fct_relevel(Scenario, levels_h3_moderate))) %>% 
plot_results(appendix = T)

ggsave("figures/SI/Figure_A3.png", width = w, height = h)


## Split Sample Replication for Partisans on Support for Capitol Riots  ----------------------------

### Figure A.4 --------------------------------------------------------------------


h1_storm_party_dem <- read_excel(si_results_filepath, sheet = "Figure A.4", range = "A7:D11") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Democrat") %>%
  clean_input_table()

h1_storm_party_rep <- read_excel(si_results_filepath, sheet = "Figure A.4", range = "A17:D21") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Republican") %>%
  clean_input_table()

h2_storm_party_dem <- read_excel(si_results_filepath, sheet = "Figure A.4", range = "F7:I11") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Democrat") %>%
  clean_input_table()

h2_storm_party_rep <- read_excel(si_results_filepath, sheet = "Figure A.4", range = "F17:I21") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Republican") %>%
  clean_input_table()

h1_party <- bind_rows(h1_storm_party_dem, h1_storm_party_rep) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Party`)) + 
  ggtitle('Hypothesis 1') + expand_limits(x = c(0, .35))

h2_party <- bind_rows(h2_storm_party_dem, h2_storm_party_rep) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Party`)) + 
  ggtitle('Hypothesis 2') + expand_limits(x = c(0, .35))

h1_party / h2_party

ggsave("figures/SI/Figure_A4.png", width = w, height = h)


### Figure A.5 ----------------------------------------------------------------------

h3_storm_party_dem <- read_excel(si_results_filepath, sheet = "Figure A.5", range = "A7:D15") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Democrat") %>%
  clean_input_table()

h3_storm_party_rep <- read_excel(si_results_filepath, sheet = "Figure A.5", range = "A21:D29") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Republican") %>%
  clean_input_table()

h3_party <- bind_rows(h3_storm_party_dem, h3_storm_party_rep) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Party`)) + 
  ggtitle('Hypothesis 3') + expand_limits(x = c(0, .35))

h3_party

ggsave("figures/SI/Figure_A5.png", width = w, height = h)

### Figure A.6 --------------------------------------------------------------------

h1_jan_party_dem <- read_excel(si_results_filepath, sheet = "Figure A.6", range = "A7:D11") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Democrat") %>%
  clean_input_table()

h1_jan_party_rep <- read_excel(si_results_filepath, sheet = "Figure A.6", range = "A17:D21") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Republican") %>%
  clean_input_table()

h2_jan_party_dem <- read_excel(si_results_filepath, sheet = "Figure A.6", range = "F7:I11") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Democrat") %>%
  clean_input_table()

h2_jan_party_rep <- read_excel(si_results_filepath, sheet = "Figure A.6", range = "F17:I21") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Republican") %>%
  clean_input_table()

h1_party <- bind_rows(h1_jan_party_dem, h1_jan_party_rep) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Party`)) + 
  ggtitle('Hypothesis 1') + expand_limits(x = c(0, .35))

h2_party <- bind_rows(h2_jan_party_dem, h2_jan_party_rep) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Party`)) + 
  ggtitle('Hypothesis 2') + expand_limits(x = c(0, .35))


h1_party / h2_party

ggsave("figures/SI/Figure_A6.png", width = w, height = h)


### Figure A.7 ----------------------------------------------------------------------


h3_jan_party_dem <- read_excel(si_results_filepath, sheet = "Figure A.7", range = "A7:D15") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Democrat") %>%
  clean_input_table()

h3_jan_party_rep <- read_excel(si_results_filepath, sheet = "Figure A.7", range = "A21:D29") %>%
  mutate(Outcome = nov_lab, 
         `Party` = "Republican") %>%
  clean_input_table()

h3_party <- bind_rows(h3_jan_party_dem, h3_jan_party_rep) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Party`)) + 
  ggtitle('Hypothesis 3') + expand_limits(x = c(0, .35))

h3_party

ggsave("figures/SI/Figure_A7.png", width = w, height = h)

## Split Sample Replication by Trump Support ----------------------------------------------------------------

### Figure A.8 --------------------------------------------------------------------

h1_jan_trump_no <- read_excel(si_results_filepath, sheet = "Figure A.8", range = "A7:D11") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Non-Supporter") %>%
  clean_input_table()

h1_jan_trump_yes <- read_excel(si_results_filepath, sheet = "Figure A.8", range = "A17:D21") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Trump Supporter") %>%
  clean_input_table()


h2_jan_trump_no <- read_excel(si_results_filepath, sheet = "Figure A.8", range = "F7:I11") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Non-Supporter") %>%
  clean_input_table()

h2_jan_trump_yes <- read_excel(si_results_filepath, sheet = "Figure A.8", range = "F17:I21") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Trump Supporter") %>%
  clean_input_table()


h1_trump <- bind_rows(h1_jan_trump_no, h1_jan_trump_yes) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Trump`)) + 
  ggtitle('Hypothesis 1') + expand_limits(x = c(0, .35))

h2_trump <- bind_rows(h2_jan_trump_no, h2_jan_trump_yes) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Trump`)) + 
  ggtitle('Hypothesis 2') + expand_limits(x = c(0, .35))


h1_trump / h2_trump

ggsave("figures/SI/Figure_A8.png", width = w, height = h)


### Figure A.9 ----------------------------------------------------------------------

h3_jan_trump_no <- read_excel(si_results_filepath, sheet = "Figure A.9", range = "A7:D15") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Non-Supporter") %>%
  clean_input_table()

h3_jan_trump_yes <- read_excel(si_results_filepath, sheet = "Figure A.9", range = "A21:D29") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Trump Supporter") %>%
  clean_input_table()


h3_trump <- bind_rows(h3_jan_trump_no, h3_jan_trump_yes) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Trump`)) + 
  ggtitle('Hypothesis 3') + expand_limits(x = c(0, .35))

h3_trump

ggsave("figures/SI/Figure_A9.png", width = w, height = h)


### Figure A.10 --------------------------------------------------------------------

h1_jan_trump_no <- read_excel(si_results_filepath, sheet = "Figure A.10", range = "A7:D11") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Non-Supporter") %>%
  clean_input_table()

h1_jan_trump_yes <- read_excel(si_results_filepath, sheet = "Figure A.10", range = "A17:D21") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Trump Supporter") %>%
  clean_input_table()


h2_jan_trump_no <- read_excel(si_results_filepath, sheet = "Figure A.10", range = "F7:I11") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Non-Supporter") %>%
  clean_input_table()

h2_jan_trump_yes <- read_excel(si_results_filepath, sheet = "Figure A.10", range = "F17:I21") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Trump Supporter") %>%
  clean_input_table()



h1_trump <- bind_rows(h1_jan_trump_no, h1_jan_trump_yes) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Trump`)) + 
  ggtitle('Hypothesis 1') + expand_limits(x = c(0, .35))

h2_trump <- bind_rows(h2_jan_trump_no, h2_jan_trump_yes) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Trump`)) + 
  ggtitle('Hypothesis 2') + expand_limits(x = c(0, .35))


h1_trump / h2_trump

ggsave("figures/SI/Figure_A10.png", width = w, height = h)


### Figure A.11 ----------------------------------------------------------------------

h3_jan_trump_no <- read_excel(si_results_filepath, sheet = "Figure A.11", range = "A7:D15") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Non-Supporter") %>%
  clean_input_table()

h3_jan_trump_yes <- read_excel(si_results_filepath, sheet = "Figure A.11", range = "A21:D29") %>%
  mutate(Outcome = nov_lab, 
         `Trump` = "Trump Supporter") %>%
  clean_input_table()


h3_trump <- bind_rows(h3_jan_trump_no, h3_jan_trump_yes) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`Trump`)) + 
  ggtitle('Hypothesis 3') + expand_limits(x = c(0, .35))

h3_trump

ggsave("figures/SI/Figure_A11.png", width = w, height = h)

## Split Sample Replication by Gender -------------------------------------------------------------

### Figure A.12 --------------------------------------------------------------------


h1_storm_gender_men <- read_excel(si_results_filepath, sheet = "Figure A.12", range = "A7:D11") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Men") %>%
  clean_input_table()

h1_storm_gender_women <- read_excel(si_results_filepath, sheet = "Figure A.12", range = "A17:D21") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Women") %>%
  clean_input_table()

h2_storm_gender_men <- read_excel(si_results_filepath, sheet = "Figure A.12", range = "F7:I11") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Wen") %>%
  clean_input_table()

h2_storm_gender_women <- read_excel(si_results_filepath, sheet = "Figure A.12", range = "F17:I21") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Women") %>%
  clean_input_table()


h1_gender <- bind_rows(h1_storm_gender_men, h1_storm_gender_women) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`gender`)) + 
  ggtitle('Hypothesis 1') + expand_limits(x = c(0, .35))

h2_gender <- bind_rows(h2_storm_gender_men, h2_storm_gender_women) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`gender`)) + 
  ggtitle('Hypothesis 2') + expand_limits(x = c(0, .35))


h1_gender / h2_gender

ggsave("figures/SI/Figure_A12.png", width = w, height = h)

### Figure A.13 ----------------------------------------------------------------------


h3_storm_gender_men <- read_excel(si_results_filepath, sheet = "Figure A.13", range = "A7:D15") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Men") %>%
  clean_input_table()

h3_storm_gender_women <- read_excel(si_results_filepath, sheet = "Figure A.13", range = "A21:D29") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Women") %>%
  clean_input_table()


h3_gender <- bind_rows(h3_storm_gender_men, h3_storm_gender_women) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`gender`)) + 
  ggtitle('Hypothesis 3') + expand_limits(x = c(0, .35))


h3_gender

ggsave("figures/SI/Figure_A13.png", width = w, height = h)

### Figure A.14 --------------------------------------------------------------------

h1_nov_gender_men <- read_excel(si_results_filepath, sheet = "Figure A.14", range = "A7:D11") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Men") %>%
  clean_input_table()

h1_nov_gender_women <- read_excel(si_results_filepath, sheet = "Figure A.14", range = "A17:D21") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Women") %>%
  clean_input_table()



h2_nov_gender_men <- read_excel(si_results_filepath, sheet = "Figure A.14", range = "F7:I11") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Men") %>%
  clean_input_table()

h2_nov_gender_women <- read_excel(si_results_filepath, sheet = "Figure A.14", range = "F17:I21") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Women") %>%
  clean_input_table()


h1_gender <- bind_rows(h1_nov_gender_men, h1_nov_gender_women) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`gender`)) + 
  ggtitle('Hypothesis 1') + expand_limits(x = c(0, .35))

h2_gender <- bind_rows(h2_nov_gender_men, h2_nov_gender_women) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`gender`)) + 
  ggtitle('Hypothesis 2') + expand_limits(x = c(0, .35))



h1_gender / h2_gender

ggsave("figures/SI/Figure_A14.png", width = w, height = h)

### Figure A.15 ----------------------------------------------------------------------

h3_nov_gender_men <- read_excel(si_results_filepath, sheet = "Figure A.15", range = "A7:D15") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Men") %>%
  clean_input_table()


h3_nov_gender_women <- read_excel(si_results_filepath, sheet = "Figure A.15", range = "A21:D29") %>%
  mutate(Outcome = jan_lab, 
         `gender` = "Women") %>%
  clean_input_table()


h3_gender <- bind_rows(h3_nov_gender_men, h3_nov_gender_women) %>% 
plot_results(appendix = T) + facet_grid(cols = vars(`gender`)) + 
  ggtitle('Hypothesis 3') + expand_limits(x = c(0, .35))

h3_gender

ggsave("figures/SI/Figure_A15.png", width = w, height = h)



