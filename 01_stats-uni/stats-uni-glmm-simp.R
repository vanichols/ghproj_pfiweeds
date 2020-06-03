##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 5/1/2020
#
# Purpose: Use glmm to analyze nu weeds seeds as response
#
# Notes:
#
# Last modified: 5/21/2020 (moved to new folder, cleaned up)
#                6/1/2020 (I want estimates of contrasts also!
####################################

rm(list = ls())
library(tidyverse)
library(PFIweeds2020)

# data --------------------------------------------------------------------

#--use data and fucntion from package

pfi_ghobsraw

dat <- pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = T) %>% 
  select(-field, -rep)


dstat <- 
  dat %>% 
  mutate(obs_id = 1:n(),
         obs_id = paste("obs", obs_id, sep = "_"))


dstat_outrm <- 
  dstat %>% 
  filter(totseeds_m2 < 15000) #--outlier



# poisson -----------------------------------------------------------------

library(lme4) #--can do generalized linear models also
library(lmerTest)
library(emmeans)
library(broom)



# poisson -----------------------------------------------------------------

#--use random factor for obs and block?

pois_blobs <- glmer(totseeds ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = dstat_outrm, 
               family = poisson(link = "log"))  

pois_blobs_em <- emmeans(pois_blobs, pairwise ~ cc_trt|site_sys)

pois_blobs_em # ok these results are on the log scale

pois_cont <- tidy(pois_blobs_em$contrasts) %>% 
  mutate(model = "pois")

pois_est <- tidy(pois_blobs_em$emmeans) %>% 
  mutate(model = "pois")

# write results -----------------------------------------------------------

pois_est %>%
  write_csv("01_stats-uni/st_weedseed-estimates-simp.csv")

pois_cont %>% 
  write_csv("01_stats-uni/st_weedseed-contrasts-simp.csv")
