# global.R
# Benedito Chou
# Dec 2 2020


# --- Load packages ---------------------------------------

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(DT)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(readxl)
library(corrplot)
library(knitr)
library(kableExtra)
library(arsenal)
library(GGally)
library(broom)
library(jtools) # plot and visualize regression
library(relaimpo)
library(MASS, exclude = "select")
library(sjPlot)
library(olsrr)
library(janitor)
library(QuantPsyc)
library(EnvStats) # For Outlier detection
library(bestglm)


# --- Import Processed data ----------------------------------

load("www/temp_shiny_data.RData")
load("www/temp_ana_data.RData")

# --- Calculate Index Score ----------------------------------

# Pivot to long format for easy standardization
ana_data_1_wgeo_long <- ana_data_1_wgeo %>%
  pivot_longer(cols = percent_fair_or_poor_health:percent_rural, 
               names_to = c("var_name"))

ana_data_1_criterion <- ana_data_1_wgeo %>%
  dplyr::select(fips, state, county, 
         years_of_potential_life_lost_rate,
         # average_number_of_physically_unhealthy_days,
         average_number_of_mentally_unhealthy_days,
         preventable_hospitalization_rate)

# Join with step-wise full table to get the weight
ana_data_1_wgeo_long <- ana_data_1_wgeo_long %>%
  left_join(m_step_df, by = "var_name")

measure_lst <- filter(m_step_df, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

measure_top5_lst <- measure_lst[c(1:5)]
measure_lst <- measure_lst[c(-1:-5)]