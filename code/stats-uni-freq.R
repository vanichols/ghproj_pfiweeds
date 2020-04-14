##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 4/14/2020
#
# Last modified: 
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
  select(-field, -rep, -totseeds)


#--skewness

library(e1071)

skewness(dat$totseeds_m2)

dat %>% 
  ggplot(aes(totseeds_m2)) +
  geom_histogram()

#--yup, log transform it

# mixed model on data ------------------------------------------------

library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(broom)
library(emmeans)

dstat <- 
  dat %>% 
  mutate(cc_trt2 = recode(cc_trt,          ##--I want rye to appear first alphabetically
                          no = "none",
                          rye = "aryecc")) %>% 
  filter(totseeds_m2 < 15000) #--outlier

#--fit model
m1 <- lmer(log(totseeds_m2) ~ site_sys * cc_trt2 + (1|blockID), data = dstat)
anova(m1) #--interaction, should I keep it?

m1_noint <- lmer(log(totseeds_m2) ~ site_sys + cc_trt2 + (1|blockID), data = dstat)

#--interaction sig improves fit (p = 0.04)
anova(m1, m1_noint)



# Get estimates from model ------------------------------------------------

m1em <- (emmeans(m1, pairwise ~ cc_trt2|site_sys, type = "response"))


m1_est <- tidy(m1em$emmeans) 

m1_cont <- tidy(m1em$contrasts) 



# write results -----------------------------------------------------------

m1_est %>% 
  write_csv("data/smy/sd_estimates.csv")

m1_cont %>% 
  write_csv("data/smy/sd_contrasts.csv")



# compare ccbio metrics and effects ---------------------------------------

ccbio <- read_csv("data/smy/sd_ccbio-metrics.csv")

ccbio

abs_eff <-
  m1_est %>% 
  select(site_sys, cc_trt2, response) %>% 
  pivot_wider(names_from = cc_trt2,
              values_from = response) %>% 
  mutate(diff_redseedsm2 = none - aryecc) %>% 
  select(site_sys, diff_redseedsm2)

rel_eff <- 
  m1_cont %>% 
  select(site_sys, ratio) %>% 
  mutate(diff_pctred = 100 - ratio*100) %>% 
  select(site_sys, diff_pctred)

ccbio_mod <- 
  ccbio %>% 
  left_join(abs_eff) %>% 
  left_join(rel_eff)
    
    
ccbio_mod %>% 
  pivot_longer(cols = c(nabove1:ccbio_stab), 
               names_to = 'metric') %>%
  arrange(metric, yr_span) %>% 
  group_by(metric, yr_span) %>% 
  nest() %>% 
  mutate(spcor_abs = data %>% 
           map(~cor(.$diff_redseedsm2, .$value, use = "complete.obs", method = "spearman")),
         spcor_rel = data %>% 
           map(~cor(.$diff_pctred, .$value, use = "complete.obs", method = "spearman"))) %>% 
  select(yr_span, metric, spcor_abs, spcor_rel) %>% 
  unnest(cols = c(spcor_abs, spcor_rel)) %>% 
  arrange(spcor_abs) 


ccbio_mod %>% 
  filter(yr_span == "5yr") %>% 
  ggplot(aes(ccbio_cv, diff_pctred)) + 
  geom_point()
  
  
  