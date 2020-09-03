##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 5/1/2020
#
# Purpose: Try to use ulam/bayes with this data
#
# Notes:
#
# Last modified: 
#
####################################

rm(list = ls())
library(rethinking)
library(tidyverse)
library(PFIweeds2020)

# data --------------------------------------------------------------------

#--use data and fucntion from package

#--keep field in site_id to see if soy/corn phase differed in central site
d <- 
  pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, field, sys_trt, col = "site_sys", remove = T) %>% 
  select(-rep) %>% 
  mutate(obs_id = 1:n(),
         obs_id = paste("obs", obs_id, sep = "_")) %>% 
  mutate(cc_trt = recode(cc_trt, 
                         "rye" = "ccrye"))

dout <- 
  dstat2 %>% 
  filter(totseeds_m2 < 15000)  #--outlier


# old way, poisson -----------------------------------------------------------------

library(lme4) #--can do generalized linear models also
library(lmerTest)
library(emmeans)
library(broom)

#--use random factor for obs and block

m1 <- glmer(totseeds ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = dout, 
            family = poisson(link = "log"))   


m1em <- emmeans(m1, pairwise ~cc_trt|site_sys, type = "response")

m1em$contrasts

emmeans(m1, pairwise ~cc_trt, type = "response")

#--I actually think site id should be random...

m2 <- glmer(totseeds ~ cc_trt + (1|site_sys) + (1|blockID) + (1|obs_id), data = dout, 
            family = poisson(link = "log"))   

summary(m2)
emmeans(m2, pairwise ~cc_trt, type = "response")$contrasts

m1em$contrasts
m1em$estimates

ranef(m2)

#--get rid of obs_id as random?

m3 <- glmer(totseeds ~ cc_trt + (1|site_sys) + (1|blockID), data = dout, 
            family = poisson(link = "log"))   

summary(m3)
emmeans(m3, pairwise ~cc_trt, type = "response")$contrasts

library(performance)


check_overdispersion(m3)
# try ulam ----------------------------------------------------------------

dbayes <- 
  list(
    S = dout$site_sys,
    C = dout$cc_trt,
    W = dout$totseeds,
    B = dout$blockID,
    OBS = dout$obs_id
  )

bm1 <- ulam(
  alist(
    W ~ dpois(lambda),
    log(lambda) <- a[]
  )
)