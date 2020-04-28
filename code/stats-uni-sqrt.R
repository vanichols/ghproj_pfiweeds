##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 4/14/2020
#
# Last modified: 4/28/2020 (made it's own file)
#
# Purpose: do 'official' frequentist stats for manuscript, cleanly
#
# Outputs: sd_estimates, sd_contrasts
#
# Notes:
####################################

rm(list = ls())
library(tidyverse)
#devtools::install_github("vanichols/PFIweeds2020", force = TRUE) ## <-- run if pkg changes
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
  mutate(cc_trt2 = recode(cc_trt,          ##--I want rye to appear first alphabetically
                          no = "none",
                          rye = "aryecc")) %>% 
  filter(totseeds_m2 < 15000) #--outlier


# mixed model on data ------------------------------------------------

library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(broom)
library(emmeans)
library(e1071) #--skewness


skewness(dat$totseeds)

#--fit model
m1sqrt <- lmer(sqrt(totseeds_m2) ~ site_sys * cc_trt2 + (1|blockID), dstat)

#--use emmeans
m1sqrt_em <- (emmeans(m1sqrt, pairwise ~ cc_trt2|site_sys, type = "response"))

#--get p-values for contrasts
m1sqrt_cont <- tidy(m1sqrt_em$contrasts) %>% 
  mutate(mod = "lmersqrt")

#--get estimates, change to tot
m1sqrt_est <- tidy(m1sqrt_em$emmeans) %>% 
  mutate(mod = "lmersqrt") %>% 
  rename(totseeds_m2 = response)


# write results -----------------------------------------------------------

m1sqrt_est %>%
  select(mod, site_sys, cc_trt2, totseeds_m2, everything()) %>% 
  write_csv("data/smy/sd_estimates.csv")

m1sqrt_cont %>%
  select(mod, site_sys, p.value) %>% 
  write_csv("data/smy/sd_contrasts.csv")

