##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 4/14/2020
#
# Last modified: 
#
# Purpose: calculate cc biomass things
#
# Outputs: 
#
# Notes:
####################################

rm(list = ls())
library(tidyverse)
#devtools::install_github("vanichols/PFIweeds2020", force = TRUE) ## <-- run if pkg changes
library(PFIweeds2020)


# cover crop biomass stuff ------------------------------------------------

#--package data
raw <- 
  pfi_ccbio %>% 
  unite(site_name, sys_trt, field, col = "site_sys", remove = TRUE) 


raw %>%
  ggplot(aes(year, ccbio_Mgha, color = site_sys)) + 
  geom_point(size = 4) + 
  geom_line()

#--average by site_sys (Boyd had 2 measurements per site_sys each year, one in corn phase, one in soybean
ccbio <- 
  pfi_ccbio %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = TRUE) %>% 
  group_by(site_sys, year) %>% 
  summarise(ccbio_Mgha = mean(ccbio_Mgha))

ccbio %>%
  ggplot(aes(year, ccbio_Mgha, color = site_sys)) + 
  geom_point(size = 4) + 
  geom_line()



# calculate different things ----------------------------------------------


cc_10yrs <-
  ccbio %>%
  #--number of years w/>1 Mg produced
  group_by(site_sys) %>%
  mutate(above1 = case_when(ccbio_Mgha > 1 ~ 1,
                            TRUE ~ 0)) %>%
  summarise(
    nabove1 = sum(above1),
    #--mean, med, var, cv, max
    ccbio_mean = mean(ccbio_Mgha,   na.rm = TRUE),
    ccbio_med  = median(ccbio_Mgha, na.rm = TRUE),
    ccbio_var  = var(ccbio_Mgha,     na.rm = TRUE),
    ccbio_cv   = ccbio_var / ccbio_mean,
    ccbio_max  = max(ccbio_Mgha, na.rm = TRUE),
    ccbio_stab = sd(ccbio_Mgha, na.rm = TRUE) / ccbio_mean
  ) %>% 
  mutate(yr_span = "10yr")


cc_5yrs <-
  ccbio %>%
  filter(year > 2014) %>% 
  #--number of years w/>1 Mg produced
  group_by(site_sys) %>%
  mutate(above1 = case_when(ccbio_Mgha > 1 ~ 1,
                            TRUE ~ 0)) %>%
  summarise(
    nabove1 = sum(above1),
    #--mean, med, var, cv, max
    ccbio_mean = mean(ccbio_Mgha,   na.rm = TRUE),
    ccbio_med  = median(ccbio_Mgha, na.rm = TRUE),
    ccbio_var  = var(ccbio_Mgha,     na.rm = TRUE),
    ccbio_cv   = ccbio_var / ccbio_mean,
    ccbio_max  = max(ccbio_Mgha, na.rm = TRUE),
    ccbio_stab = sd(ccbio_Mgha, na.rm = TRUE) / ccbio_mean
  ) %>% 
  mutate(yr_span = "5yr")


newdata <- 
  cc_5yrs %>% 
  bind_rows(cc_10yrs) %>% 
  select(site_sys, yr_span, everything())



# write it ----------------------------------------------------------------

newdata %>% 
  write_csv("data/smy/sd_ccbio-metrics.csv")
