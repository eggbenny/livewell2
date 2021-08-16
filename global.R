# global.R
# Benedito Chou
# Aug 15 2021


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
library(urbnmapr)
library(ggalluvial)


# --- Import Processed data ----------------------------------

# OlD not use
# load("www/temp_shiny_data.RData")
# load("www/temp_ana_data_extra_measure.RData")
# load("www/temp_ana_data_v2.RData")
# load("www/temp_extra_data.RData")

# Load Main Data used in Regression Modeling
load("www/main_ana_data_df.RData")

# Temp fix name
names(ana_data_full)[80] <- "per_of_housing_with_no_vehicle_available"
names(ana_data_full_wgeo)[83] <- "per_of_housing_with_no_vehicle_available"

# Load Extra Data
load("www/temp_extra_data_labour.RData")
load("www/temp_extra_data_medicare.RData")

# social Connectedness index
load("www/social_cx_index.RData")

# Income change
load("www/income_chg_at_26.RData")

# load Domain Map
load("www/domain_map.RData")

# Temp fix name
domain_map <- domain_map %>%
  mutate(var_name = ifelse(var_name == "per_of_housing_with_vehicles_available", 
                           "per_of_housing_with_no_vehicle_available", var_name))

# load Region Map
load("www/region_lkup.RData")

# For each additional Index, load the regression model table
# Play Index & 2nd Layers
load("www/m_step_df_play.RData")
load("www/m_step_df_fp_health.RData")
load("www/m_step_df_grad.RData")
load("www/m_step_df_diabetes.RData")

# Rest Index & 2nd Layers
load("www/m_step_df_rest.RData")
# load("www/m_step_df_doc_checkup.RData")
# load("www/m_step_df_ypll.RData")
load("www/m_step_df_avg_mdays.RData")
# per_physically_inactive aka play
m_step_df_phy_inactive <- m_step_df_play
load("www/m_step_df_obesity.RData")

# Work Index & 2nd Layers
load("www/m_step_df_work.RData")

# Temp name fix
m_step_df_work <- m_step_df_work  %>%
  mutate(var_name = ifelse(var_name == "per_of_housing_with_vehicles_available", 
                           "per_of_housing_with_no_vehicle_available", var_name))

load("www/m_step_df_teen_brate.RData")

# Temp name fix
m_step_df_teen_brate <- m_step_df_teen_brate  %>%
  mutate(var_name = ifelse(var_name == "per_of_housing_with_vehicles_available", 
                           "per_of_housing_with_no_vehicle_available", var_name))


# per_w_grad_or_prof_degree
# load("www/m_step_df_grad.RData")
load("www/m_step_df_avg_mdays.RData")



# --- Some Format Setting ------------------------------------

# Old colour
# quintile_colour_pal <- c("#d53e4f", "#ffff99", "#ff7f00", "#377eb8", "#4daf4a")
quintile_colour_pal <- c("#f26957", "#9ea1cf", "#f7d9f0", "#e5eb45", "#33a654")

# --- Calculate Index Score ----------------------------------

# Add region
ana_data_wgeo <- left_join(ana_data_full_wgeo, region_lkup, by = c("fips" = "FIPS")) %>%
  dplyr::mutate(
    RegionOrg = Region,
    Region = ifelse(is.na(Region), county, Region))

# Pivot to long format for easy standardization
ana_data_wgeo_long <- ana_data_wgeo %>%
  pivot_longer(cols = years_of_potential_life_lost_rate:public_health_fund, 
               names_to = c("var_name"))

# Join Domain map
ana_data_wgeo_long <- ana_data_wgeo_long %>%
  left_join(domain_map, by = "var_name")

# Setup criterion df for the index correlation page

criterion_ana <- ana_data_full_wgeo %>%
  dplyr::select(fips, state, county, 
                years_of_potential_life_lost_rate,
                avg_no_of_physically_unhealthy_days,
                avg_no_of_mentally_unhealthy_days,
                preventable_hospitalization_rate,
                per_uninsured,
                primary_care_physicians_ratio,
                per_unemployed,
                x20th_perile_income,
                per_single_parent_households,
                severe_housing_cost_burden,
                violent_crime_rate,
                social_association_rate,
                per_adults_with_diabetes)

criterion_extra_1 <- data_medicare_1 %>%
  dplyr::select(fips, 
                medicare_spending_age_sex_race_adjusted_4,
                medicare_spending_price_age_sex_race_adjusted_5)

criterion_extra_2 <- income_chg_1 %>%
  dplyr::select(fips, 
                income_chg_at_26)

criterion_extra_3 <- social_cx_index_1 %>%
  dplyr::select(fips,
                social_cx_index)

criterion <- full_join(criterion_ana, criterion_extra_1, by = "fips") %>%
  full_join(criterion_extra_2, by = "fips") %>%
  full_join(criterion_extra_3, by = "fips")


ana_data_criterion <- ana_data_full_wgeo %>%
  dplyr::select(fips, state, county, 
                # years_of_potential_life_lost_rate,
                # average_number_of_physically_unhealthy_days,
                avg_no_of_physically_unhealthy_days,
                avg_no_of_mentally_unhealthy_days,
                # preventable_hospitalization_rate,
                # per_adults_with_diabetes,
                primary_care_physicians_ratio,
                per_unemployed,
                per_single_parent_households,
                age_adjusted_death_rate,
                social_association_rate,
                severe_housing_cost_burden,
                violent_crime_rate,
                x20th_perile_income,
                age_adjusted_death_rate,
                social_association_rate
  )


rest_ana_data_criterion <- ana_data_full_wgeo %>%
  dplyr::select(fips, state, county, 
                years_of_potential_life_lost_rate,
                # average_number_of_physically_unhealthy_days,
                avg_no_of_physically_unhealthy_days,
                avg_no_of_mentally_unhealthy_days,
                preventable_hospitalization_rate,
                per_adults_with_diabetes,
                primary_care_physicians_ratio,
                per_unemployed,
                per_single_parent_households,
                age_adjusted_death_rate,
                social_association_rate,
                severe_housing_cost_burden,
                violent_crime_rate,
                x20th_perile_income,
                age_adjusted_death_rate,
                social_association_rate
  )

work_ana_data_criterion <- ana_data_full_wgeo %>%
  dplyr::select(fips, state, county, 
                years_of_potential_life_lost_rate,
                # average_number_of_physically_unhealthy_days,
                avg_no_of_physically_unhealthy_days,
                # avg_no_of_mentally_unhealthy_days,
                # preventable_hospitalization_rate,
                per_adults_with_diabetes,
                primary_care_physicians_ratio,
                # per_unemployed,
                # per_single_parent_households,
                age_adjusted_death_rate,
                social_association_rate,
                # severe_housing_cost_burden,
                violent_crime_rate,
                x20th_perile_income,
                age_adjusted_death_rate,
                social_association_rate
  )


# Join with step-wise full table to get the weight
play_ana_data_wgeo_long <- ana_data_wgeo_long %>%
  left_join(m_step_df_play, by = "var_name")

# Join with step-wise full table to get the weight
rest_ana_data_wgeo_long <- ana_data_wgeo_long %>%
  left_join(m_step_df_rest, by = "var_name")

# Join with step-wise full table to get the weight
work_ana_data_wgeo_long <- ana_data_wgeo_long %>%
  left_join(m_step_df_work, by = "var_name")


# Domain lst
domain_lst <- filter(domain_map, !is.na(Domain)) %>%
  filter(Domain != "Demographics") %>%
  distinct(Domain) %>% unlist() %>% as.character()


# Measure lst for Play Index
measure_lst_play <- filter(m_step_df_play, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

measure_all_lst_play <- measure_lst_play[c(-14, -16, -18, -20, -22, -24, -25, -26)]
measure_top3_lst_play <- measure_lst_play[c(1:3)]
measure_lst_play <- measure_lst_play[c(-1, -2, -3, -14, -16, -18, -20, -22, -24, -25, -26)]

# Measure lst for Rest Index
measure_lst_rest <- filter(m_step_df_rest, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

measure_all_lst_rest <- measure_lst_rest[c(-1, -2, -5, -13, -15, -16, -18, -19, -21, -22, -23, -25, -26, -27, -28, -29)]
measure_top3_lst_rest <- measure_lst_rest[c(3, 4, 6)]
measure_lst_rest <- measure_lst_rest[c(-1, -2, -3, -4, -5, -6, -13, -15, -16, -18, -19, -21, -22, -23, -25, -26, -27, -28, -29)]

# Measure lst for Work Index
measure_lst_work <- filter(m_step_df_work, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

measure_all_lst_work <- measure_lst_work[c(-1, -12, -15, -16, -17, -18, -19, -20, -22, -24, -25, -27, -28, -29, -31, -32, -33, -34, -35, -36)]
measure_top3_lst_work <- measure_lst_work[c(2:4)]
measure_lst_work <- measure_lst_work[c(-1, -2, -3, -4, -12, -15, -16, -17, -18, -19, -20, -22, -24, -25, -27, -28, -29, -31, -32, -33, -34, -35, -36)]

# Measure lst for Fair and Poor Health as Outcome
measure_lst_fp_health <- filter(m_step_df_fp_health, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_fp_health <- filter(m_step_df_fp_health, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_fp_health <- measure_lst_fp_health[c(-18, -25, -26, -27, -28, -34, -36)]
measure_top3_lst_fp_health <- measure_lst_fp_health[c(1:3)]
measure_lst_fp_health <- measure_lst_fp_health[c(-1, -2, -3, -18, -25, -26, -27, -28, -34, -36)]

# Measure lst for Grad as Outcome
measure_lst_grad <- filter(m_step_df_grad, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_grad <- filter(m_step_df_grad, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_grad <- measure_lst_grad[c(-2, -23, -26, -27, -28, -30, -31)]
measure_top3_lst_grad <- measure_lst_grad[c(1,3,4)]
measure_lst_grad <- measure_lst_grad[c(-1, -2, -3, -4, -23, -26, -27, -28, -30, -31)]

# Measure lst for Diabetes as Outcome
measure_lst_diabetes <- filter(m_step_df_diabetes, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_diabetes <- filter(m_step_df_diabetes, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_diabetes <- measure_lst_diabetes[c(-1, -8, -16, -17, -19)]
measure_top3_lst_diabetes <- measure_lst_diabetes[c(2:4)]
measure_lst_diabetes <- measure_lst_diabetes[c(-1, -2, -3, -4, -8, -16, -17, -19)]

# Measure lst for Routine Doc Checkup as Outcome
# measure_lst_doc_checkup <- filter(m_step_df_doc_checkup, var_name != "(Intercept)") %>%
#   arrange(desc(pratt)) %>%
#   dplyr::select(var_name) %>%
#   unlist() %>%
#   as.character()
# 
# domain_lst_doc_checkup <- filter(m_step_df_doc_checkup, var_name != "(Intercept)") %>%
#   arrange(desc(pratt)) %>%
#   left_join(domain_map, by = "var_name") %>%
#   filter(!is.na(Domain)) %>%
#   dplyr::select(Domain) %>%
#   distinct(Domain) %>%
#   unlist() %>%
#   as.character()
# 
# measure_all_lst_doc_checkup <- measure_lst_doc_checkup
# measure_top3_lst_doc_checkup <- measure_lst_doc_checkup[c(5,7,8)]
# measure_lst_doc_checkup <- measure_lst_doc_checkup[c(5,7,8)]


# Measure lst for YPLL as Outcome
# measure_lst_ypll <- filter(m_step_df_ypll, var_name != "(Intercept)") %>%
#   arrange(desc(pratt)) %>%
#   dplyr::select(var_name) %>%
#   unlist() %>%
#   as.character()
# 
# domain_lst_ypll <- filter(m_step_df_ypll, var_name != "(Intercept)") %>%
#   arrange(desc(pratt)) %>%
#   left_join(domain_map, by = "var_name") %>%
#   filter(!is.na(Domain)) %>%
#   dplyr::select(Domain) %>%
#   distinct(Domain) %>%
#   unlist() %>%
#   as.character()
# 
# measure_all_lst_ypll <- measure_lst_ypll
# measure_top3_lst_ypll <- measure_lst_ypll[c(4, 7, 8)]
# measure_lst_ypll <- measure_lst_ypll[c(-4, -7, -8)]

# Measure lst for Avg # of Mentally Unhealty Days as Outcome
measure_lst_avg_m_days <- filter(m_step_df_avg_m_days, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_avg_m_days <- filter(m_step_df_avg_m_days, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_avg_m_days <- measure_lst_avg_m_days[c(-16, -17, -18, -19, -20, -21, -22)]
measure_top3_lst_avg_m_days <- measure_lst_avg_m_days[c(1:3)]
measure_lst_avg_m_days <- measure_lst_avg_m_days[c(-1, -2, -3, -16, -17, -18, -19, -20, -21, -22)]


measure_lst_obesity <- filter(m_step_df_obesity, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_obesity <- filter(m_step_df_obesity, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_obesity <- measure_lst_obesity[c(-23, -24, -25, -26)]
measure_top3_lst_obesity <- measure_lst_obesity[c(1:3)]
measure_lst_obesity <- measure_lst_obesity[c(-1, -2, -3, -23, -24, -25, -26)]


measure_lst_phy_inactive <- filter(m_step_df_phy_inactive, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_phy_inactive <- filter(m_step_df_phy_inactive, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_phy_inactive <- measure_lst_phy_inactive[c(-14, -16, -18, -20, -22, -24, -25, -26)]
measure_top3_lst_phy_inactive <- measure_lst_phy_inactive[c(1:3)]
measure_lst_phy_inactive <- measure_lst_phy_inactive[c(-1, -2, -3, -14, -16, -18, -20, -22, -24, -25, -26)]


measure_lst_teen_brate <- filter(m_step_df_teen_brate, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  dplyr::select(var_name) %>%
  unlist() %>%
  as.character()

domain_lst_teen_brate <- filter(m_step_df_teen_brate, var_name != "(Intercept)") %>%
  arrange(desc(pratt)) %>%
  left_join(domain_map, by = "var_name") %>%
  filter(!is.na(Domain)) %>%
  dplyr::select(Domain) %>%
  distinct(Domain) %>%
  unlist() %>%
  as.character()

measure_all_lst_teen_brate <- measure_lst_teen_brate[c(-2, -4, -13, -22, -26, -29, -33, -35)]
measure_top3_lst_teen_brate <- measure_lst_teen_brate[c(1, 3, 5)]
measure_lst_teen_brate <- measure_lst_teen_brate[c(-1, -2, -3, -4, -5, -13, -22, -26, -29, -33, -35)]


# Calculate Index with Fixed Slider values
play_fixed_z_data_wgeo_long <- play_ana_data_wgeo_long %>%
  ungroup() %>%
  group_by(var_name) %>%
  mutate(
    z_value = as.numeric(scale(value)),
    score = scales::rescale(z_value, c(0, 100)),
    score = ifelse(Direction == "N", 100 - score, score),
    rank_value = rank(-score),
    per_rank_value = percent_rank(score) * 100,
    quintile = ntile(score, 5),
    play = ifelse(!is.na(b), 1, 0)) %>%
  filter(play == 1) %>%
  group_by(fips, state, county) %>%
  mutate(
    play_uw = mean(score, na.rm = T),
    play_w = weighted.mean(score, pratt, na.rm = T)
  ) %>%
  dplyr::select(-play)

# Convert back to wide format
play_fixed_z_data_wgeo <- play_fixed_z_data_wgeo_long %>%
  dplyr::select(fips, state, county, var_name, value, z_value, score, play_uw, play_w) %>%
  pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
  ungroup() %>%
  mutate(
    score = play_w_per_fair_or_poor_health,
    quintile = ntile(score, 5)) # Use fair and poor health as a proxy

names(play_fixed_z_data_wgeo) <- str_replace_all(names(play_fixed_z_data_wgeo), "^value_", "")

# Data All out
play_data_out <- dplyr::select(play_fixed_z_data_wgeo, fips,	state,	county, years_of_potential_life_lost_rate:copd_18plus, score, quintile)

# Add physical_inactivity_back
phy_inactive_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, population, per_physically_inactive)

play_data_out <- left_join(play_data_out, phy_inactive_wgeo, by = c("fips", "state", "county"))


# Save fixed play index score into csv
# write_csv(data_out, "../Beta/data/play_index_score_all_counties_all_stepwise_measure.csv", na = "")


# Get quintile out for Gregg
play_q_out <- play_fixed_z_data_wgeo_long %>%
  dplyr::select(fips, state, county, var_name, quintile) %>%
  pivot_wider(names_from = "var_name", values_from = c(quintile), names_prefix = "quintile_") %>%
  ungroup()

# Rest (aka Home Index dump data outs)
rest_fixed_z_data_wgeo_long <- rest_ana_data_wgeo_long %>%
  ungroup() %>%
  group_by(var_name) %>%
  mutate(
    z_value = as.numeric(scale(value)),
    score = scales::rescale(z_value, c(0, 100)),
    score = ifelse(Direction == "N", 100 - score, score),
    rank_value = rank(-score),
    per_rank_value = percent_rank(score) * 100,
    quintile = ntile(score, 5),
    rest = ifelse(!is.na(b), 1, 0)) %>%
  filter(rest == 1) %>%
  group_by(fips, state, county) %>%
  mutate(
    rest_uw = mean(score, na.rm = T),
    rest_w = weighted.mean(score, pratt, na.rm = T)
  )

# Convert back to wide format
rest_fixed_z_data_wgeo <- rest_fixed_z_data_wgeo_long %>%
  dplyr::select(fips, state, county, var_name, value, z_value, score, rest_uw, rest_w) %>%
  pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, rest_uw, rest_w)) %>%
  ungroup() %>%
  mutate(
    score = rest_w_avg_no_of_mentally_unhealthy_days ,
    quintile = ntile(score, 5)) # avg_no_of_mentally_unhealthy_days as a proxy

names(rest_fixed_z_data_wgeo) <- str_replace_all(names(rest_fixed_z_data_wgeo), "^value_", "")

# Data All out
rest_data_out <- dplyr::select(rest_fixed_z_data_wgeo, fips,	state,	county, years_of_potential_life_lost_rate:routine_doctor_checkup_past_years_18plus, score, quintile)

# Add percent insufficient sleep back
per_insufficient_sleep_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_insufficient_sleep)

rest_data_out <- left_join(rest_data_out, per_insufficient_sleep_wgeo, by = c("fips", "state", "county"))

# # Save fixed Rest index score into csv
# write_csv(data_out, "../Beta/data/rest_index_score_all_counties_all_stepwise_measure.csv", na = "")

# Get quintile out for Gregg
rest_q_out <- rest_fixed_z_data_wgeo_long %>%
  dplyr::select(fips, state, county, var_name, quintile) %>%
  pivot_wider(names_from = "var_name", values_from = c(quintile), names_prefix = "quintile_") %>%
  ungroup()

# Work Index dump data outs
work_fixed_z_data_wgeo_long <- work_ana_data_wgeo_long %>%
  ungroup() %>%
  group_by(var_name) %>%
  mutate(
    z_value = as.numeric(scale(value)),
    score = scales::rescale(z_value, c(0, 100)),
    score = ifelse(Direction == "N", 100 - score, score),
    rank_value = rank(-score),
    per_rank_value = percent_rank(score) * 100,
    quintile = ntile(score, 5),
    work = ifelse(!is.na(b), 1, 0)) %>%
  filter(work == 1) %>%
  group_by(fips, state, county) %>%
  mutate(
    work_uw = mean(score, na.rm = T),
    work_w = weighted.mean(score, pratt, na.rm = T)
  )

# Convert back to wide format
work_fixed_z_data_wgeo <- work_fixed_z_data_wgeo_long %>%
  dplyr::select(fips, state, county, var_name, value, z_value, score, work_uw, work_w) %>%
  pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, work_uw, work_w)) %>%
  ungroup() %>%
  mutate(
    score = work_w_teen_birth_rate,
    quintile = ntile(score, 5)) # teen_birth_rate as a proxy

names(work_fixed_z_data_wgeo) <- str_replace_all(names(work_fixed_z_data_wgeo), "^value_", "")

# Data All out
work_data_out <- dplyr::select(work_fixed_z_data_wgeo, fips,	state,	county, avg_no_of_mentally_unhealthy_days:current_asthma_18plus, score, quintile)

# Add percent insufficient sleep back
per_w_a_disability <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_w_a_disability)

work_data_out <- left_join(work_data_out, per_w_a_disability, by = c("fips", "state", "county"))

# # Save fixed Work index score into csv
# write_csv(data_out, "../Beta/data/work_index_score_all_counties_all_stepwise_measure.csv", na = "")

# Get quintile out for Gregg
work_q_out <- work_fixed_z_data_wgeo_long %>%
  dplyr::select(fips, state, county, var_name, quintile) %>%
  pivot_wider(names_from = "var_name", values_from = c(quintile), names_prefix = "quintile_") %>%
  ungroup()

# save Quintile out
q_data_out <- left_join(play_q_out, rest_q_out, by = c("fips", "state", "county")) %>%
  left_join(work_q_out, by = c("fips", "state", "county"))

names(q_data_out) <- str_replace(names(q_data_out), "\\.x|\\.y", "")

q_data_out2 <- q_data_out[!duplicated(as.list(q_data_out))]

# saveRDS(q_data_out2, file = "www/all_indices_all_counties_all_stepwise_measures_quintiles.Rda")
# write_csv(q_data_out2, "../Beta/data/all_indices_all_counties_all_stepwise_measure_quintiles.csv", na = "")


# Save indices data out
data_out <- left_join(play_data_out, rest_data_out, by = c("fips", "state", "county")) %>%
  left_join(work_data_out, by = c("fips", "state", "county")) %>%
  rename(play_index = score.x,
         play_quintile = quintile.x,
         rest_index = score.y,
         rest_quintile = quintile.y,
         work_index = score,
         work_quintile = quintile)

names(data_out) <- str_replace(names(data_out), "\\.x|\\.y", "")

# remove duplicate column
# see https://www.marsja.se/how-to-remove-duplicates-in-r-rows-columns-dplyr/
data_out2 <- data_out[!duplicated(as.list(data_out))]

# saveRDS(data_out2, file = "www/all_indices_all_counties_all_stepwise_measures.Rda")

# Create cross indices map data
play_map_data <- play_fixed_z_data_wgeo %>%
  rename(play_quintile = quintile) %>%
  dplyr::select(fips, play_quintile)

rest_map_data <- rest_fixed_z_data_wgeo %>%
  rename(rest_quintile = quintile) %>%
  dplyr::select(fips, rest_quintile)

work_map_data <- work_fixed_z_data_wgeo %>%
  rename(work_quintile = quintile) %>%
  dplyr::select(fips, work_quintile)

cross_map_data <- full_join(play_map_data, rest_map_data, by = "fips") %>%
  full_join(work_map_data, by = "fips")

cross_map_data <- cross_map_data %>%
  rowwise() %>%
  mutate(
    sum_check = play_quintile + rest_quintile + work_quintile,
    all_quintile = NA,
    all_quintile = ifelse(play_quintile == 1 & rest_quintile == 1 & work_quintile == 1, 1, all_quintile),
    all_quintile = ifelse(play_quintile == 2 & rest_quintile == 2 & work_quintile == 2, 2, all_quintile),
    all_quintile = ifelse(play_quintile == 1 & rest_quintile == 1 & work_quintile == 2, 3, all_quintile),
    all_quintile = ifelse(play_quintile == 1 & rest_quintile == 2 & work_quintile == 2, 3, all_quintile),
    all_quintile = ifelse(play_quintile == 1 & rest_quintile == 2 & work_quintile == 1, 3, all_quintile),
    all_quintile = ifelse(play_quintile == 2 & rest_quintile == 1 & work_quintile == 1, 3, all_quintile),
    all_quintile = ifelse(play_quintile == 2 & rest_quintile == 2 & work_quintile == 1, 3, all_quintile),
    all_quintile = ifelse(play_quintile == 2 & rest_quintile == 1 & work_quintile == 2, 3, all_quintile),
    all_quintile = ifelse(play_quintile == 3 | rest_quintile == 3 | work_quintile == 3, 4, all_quintile))

cross_map_data <- left_join(dplyr::select(play_fixed_z_data_wgeo, fips, state, county), 
                            cross_map_data, by = "fips")

quintile_colour_pal_df <- tibble(
    quintile = c(1, 2, 3, 4, 5),
    qcolor = c("#f26957", "#9ea1cf", "#f7d9f0", "#e5eb45", "#33a654"))
quintile_colour_pal_lst <- c("#f26957", "#9ea1cf", "#f7d9f0", "#e5eb45", "#33a654")

# new_cross_map <- read_excel(file.choose())
# new_cross_map <- read_excel("../Beta/data/About Us cross_indices_map v2.xlsx")
load("www/About Us cross_indices_map v2.Rdata")
#
new_cross_map <- new_cross_map %>%
  dplyr::mutate(
    length = nchar(fips),
    fips = as.character(fips),
    fips = ifelse(length == 4, paste0("0", fips), fips)) %>%
  dplyr::select(-length)

new_cross_map <- new_cross_map %>% left_join(quintile_colour_pal_df,
                                             by = c("US All Quintile Map" = "quintile"))

# # make cross map to remove after
# map_data <- cross_map_data  %>%
#   left_join(countydata, by = c("fips" = "county_fips")) %>%
#   left_join(urbnmapr::counties, by = c("fips" = "county_fips"))
# 
# 
# ggplot(map_data, aes(long, lat, group = fips, fill = factor(all_quintile))) +
#   geom_polygon(color = "black") +
#   coord_map() +
#   labs(fill = "Cross Indices") +
#   theme_minimal() +
#   scale_fill_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
#   scale_fill_manual(values = quintile_colour_pal) +
#   # guides(fill = guide_colourbar(nbbin = 100)) +
#   theme(legend.position = "bottom",
#         legend.key.width = unit(3, "cm"))
# 
# # make ak map temp to remove after
# 
# ak_map <- read_excel(file.choose())
# 
# ak_map <- ak_map %>%
#   dplyr::mutate(
#     length = nchar(fips),
#     fips = as.character(fips),
#     fips = ifelse(length == 4, paste0("0", fips), fips)) %>%
#   dplyr::select(-length)
# 
# ak_map2 <- left_join(ak_map, cross_map_data, by = "fips")
# 
# ak_map2 <- ak_map2 %>%
#   mutate(
#     `All Quintile` = ifelse(is.na(`Second Map Indicator`), NA, `All Quintile`),
#     all_quintile = ifelse(is.na(`Second Map Indicator`), NA, all_quintile))
# 
# ak_map_data <- ak_map2  %>%
#   left_join(countydata, by = c("fips" = "county_fips")) %>%
#   left_join(urbnmapr::counties, by = c("fips" = "county_fips"))
# 
# 
# ggplot(ak_map_data, aes(long, lat, group = fips, fill = factor(all_quintile))) +
#   geom_polygon(color = "black") +
#   coord_map() +
#   labs(fill = "Cross Indices") +
#   theme_minimal() +
#   scale_fill_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
#   scale_fill_manual(values = quintile_colour_pal) +
#   # guides(fill = guide_colourbar(nbbin = 100)) +
#   theme(legend.position = "bottom",
#         legend.key.width = unit(3, "cm"))
# 
# ak_quintile_colour_pal <- c("#377eb8", "#e41a1c", "#ffff99", "#ff7f00")
# 
# ggplot(ak_map_data, aes(long, lat, group = fips, fill = factor(`All Quintile`))) +
#   geom_polygon(color = "black") +
#   coord_map() +
#   labs(fill = "Cross Indices") +
#   theme_minimal() +
#   scale_fill_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
#   scale_fill_manual(values = ak_quintile_colour_pal) +
#   # guides(fill = guide_colourbar(nbbin = 100)) +
#   theme(legend.position = "bottom",
#         legend.key.width = unit(3, "cm"))

# write_csv(cross_map_data, "../Beta/data/cross_indices_map.csv", na = "")


# Sankey domain colour central
# Estimate gglot colour
# See https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

domain_color_df <- tibble(
  Domain = c("Demographics", "Disease & Disability", "Health and Social Care", "Home",                
             "Length of Life", "Personal Behaviors", "Physical Environment", "Quality of Life",      
             "Socio-Economic", "Transportation"),
  # domain_color = c("E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999",
  # "black"),
  domain_color = gg_color_hue(10))
