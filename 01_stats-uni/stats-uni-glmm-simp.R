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
  filter(totseeds_m2 < 15000) %>%  #--outlier
  mutate(cc_trt = recode(cc_trt, 
                         "rye" = "ccrye"))

#--F4 is the booger
dstat %>% 
  filter(totseeds_m2 > 15000)


# poisson -----------------------------------------------------------------

library(lme4) #--can do generalized linear models also
library(lmerTest)
library(emmeans)
library(broom)



# poisson -----------------------------------------------------------------

#--use random factor for obs and block

m1 <- glmer(totseeds ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = dstat_outrm, 
               family = poisson(link = "log"))  


# get estimates -----------------------------------------------------------


m1_emlog <- emmeans(m1, pairwise ~ cc_trt|site_sys)
m1_em <- emmeans(m1, pairwise ~ cc_trt|site_sys, type = "response")


#--log scale
m1_estlog <- tidy(m1_emlog$emmeans) %>% 
  mutate(model = "pois")

m1_contlog <- tidy(m1_emlog$contrasts) %>% 
  mutate(model = "pois")

#--original scale
m1_est <- tidy(m1_em$emmeans) %>% 
  mutate(model = "pois")

m1_cont <- 
  tidy(m1_em$contrasts) %>% 
  mutate(model = "pois") %>% 
  left_join(m1_em$contrasts %>% 
  confint( level = 0.9) %>% 
  as_tibble() %>% 
  mutate(model = "pois"))


#--overall
oa <- 
  emmeans(m1, pairwise ~ cc_trt:site_sys, type = "response", )$contrasts %>% 
  summary() %>% 
  as_tibble() %>% 
  separate(contrast, into = c("level1", "level2"), sep = "/") %>% 
  mutate(p.value = round(p.value, 3)) 

oa %>% 
  filter(grepl("Funcke", level1))
  
# write results -----------------------------------------------------------

#--log scale
m1_estlog %>%
  write_csv("01_stats-uni/st_weedseed-est-log.csv")

m1_contlog %>% 
  write_csv("01_stats-uni/st_weedseed-contr-log.csv")


#--response scale
m1_est %>%
  write_csv("01_stats-uni/st_weedseed-est.csv")

m1_cont %>% 
  write_csv("01_stats-uni/st_weedseed-contr.csv")
