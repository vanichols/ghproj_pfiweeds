##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 5/1/2020
#
# Last modified: 
#
# Purpose: explore glmm more in depth
#
# Outputs: 
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
  mutate(obs_id = 1:n(),
         obs_id = paste("obs", obs_id, sep = "_"))


dstat_outrm <- 
  dstat %>% 
  filter(totseeds_m2 < 15000) #--outlier



# poisson -----------------------------------------------------------------

library(lme4) #--can do generalized linear models also
library(lmerTest)
library(performance)
library(emmeans)
library(broom)



# poisson -----------------------------------------------------------------

#--fit a fixed effect glm to pooled data (?)
pois1 <- glm(totseeds ~ site_sys*cc_trt, data = dstat_outrm, 
               family = poisson(link = "log"))

# umm what am I supposed to look at?
anova(pois1)
summary(pois1)

#--use a random factor for block
pois_bl <- glmer(totseeds ~ site_sys*cc_trt + (1|blockID), data = dstat_outrm, 
            family = poisson(link = "log"))

performance::check_overdispersion(pois_bl)   # there is overdispersion... don't use 

#--use random factor for obs

pois_obs <- glmer(totseeds ~ site_sys*cc_trt + (1|obs_id), data = dstat_outrm, 
             family = poisson(link = "log"))  
performance::check_overdispersion(pois_obs)

#--use random factor for obs and block?

pois_blobs <- glmer(totseeds ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = dstat_outrm, 
               family = poisson(link = "log"))  
performance::check_overdispersion(pois_blobs)

#--which is better?
performance::compare_performance(pois_obs, pois_blobs)

#--ummmm block isn't doing much, it seems
emmeans(pois_obs, pairwise ~ cc_trt|site_sys)

#--buuuut it makes things significant
pois_blobs_em <- emmeans(pois_blobs, pairwise ~ cc_trt|site_sys)
pois_blobs_em # ok these results are on the log scale
pois_cont <- tidy(pois_blobs_em$contrasts) %>% 
  mutate(model = "pois")
pois_est <- tidy(pois_blobs_em$emmeans) %>% 
  mutate(model = "pois")

#--fit pois_obs w/full dataset (includes outlier)
pois_blobs_full <- glmer(totseeds ~ site_sys*cc_trt + (1|obs_id) + (1|blockID),
                       data = dstat,
                       family = poisson(link = "log"))
pois_blobs_em_full <- emmeans(pois_blobs_full, pairwise ~ cc_trt|site_sys)
pois_cont_full <- tidy(pois_blobs_em_full$contrasts) %>% 
  mutate(model= "pois_full")
pois_est_full <- tidy(pois_blobs_em_full$emmeans) %>% 
  mutate(model= "pois_full")



# neg binomial ------------------------------------------------------------

# try negative binomial instead...
binom <- glmer.nb(totseeds ~ site_sys*cc_trt + (1|blockID), data = dstat_outrm)
binom_em <- emmeans(binom, pairwise ~ cc_trt|site_sys)
binom_cont <- tidy(binom_em$contrasts) %>% 
  mutate(model = "binom")
binom_est <- tidy(binom_em$emmeans) %>% 
  mutate(model = "binom")


binom_full <- glmer.nb(totseeds ~ site_sys*cc_trt + (1|blockID), data = dstat)
binom_em_full <- emmeans(binom_full, pairwise ~ cc_trt|site_sys)
binom_cont_full <- tidy(binom_em_full$contrasts) %>% 
  mutate(model= "binom_full")
binom_est_full <- tidy(binom_em_full$emmeans) %>% 
  mutate(model= "binom_full")


# compare poisson and binomial
performance::compare_performance(binom, pois_blobs)    # looks like model m1log fits better, although g1a explains more variation

#--katherine says don't use amt of variation explained as a metric. Or AIC. Blech. Then what do you use?
# seems like an ok resource https://www.theanalysisfactor.com/about/about-jeff-meyer/

performance::check_model(binom)
performance::check_model(pois_blobs)



# summarise model results -------------------------------------------------

tom2conv <- 1 / (((pi * 2.8575^2) * 20 ) / 10000 )

est_sum <- 
  pois_est %>% 
  bind_rows(pois_est_full) %>% 
  bind_rows(binom_est) %>% 
  bind_rows(binom_est_full) %>% 
  mutate(totseeds = exp(estimate),
         totseeds_lo = exp(asymp.LCL),
         totseeds_hi = exp(asymp.UCL),
         totseeds_m2 = totseeds * tom2conv,
         totseeds_m2_lo = totseeds_lo * tom2conv,
         totseeds_m2_hi = totseeds_hi * tom2conv)  %>% 
  select(site_sys, cc_trt, totseeds_m2, totseeds_m2_lo, totseeds_m2_hi)

pval_sum <- 
  pois_cont %>% 
  bind_rows(pois_cont_full) %>% 
  bind_rows(binom_cont) %>% 
  bind_rows(binom_cont_full) %>% 
  select(level1, level2, site_sys, p.value, model) %>% 
  pivot_wider(names_from = model, values_from = p.value)


# write results -----------------------------------------------------------

est_sum %>%
  write_csv("data/smy/sd_estimates.csv")

pval_sum %>% 
  write_csv("data/smy/sd_contrasts.csv")

