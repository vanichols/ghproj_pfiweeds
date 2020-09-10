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
#                8/31/2020 (run w/central-grain phases)
####################################

rm(list = ls())
library(tidyverse)
library(PFIweeds2020)

# data --------------------------------------------------------------------

#--use data and fucntion from package

pfi_ghobsraw

dat <- 
  pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = T) %>% 
  select(-field, -rep)


dstat <- 
  dat %>% 
  mutate(obs_id = 1:n(),
         obs_id = paste("obs", obs_id, sep = "_")) %>% 
  mutate(cc_trt = recode(cc_trt, 
                         "rye" = "ccrye"))
dstat_outrm <- 
  dstat %>% 
  filter(totseeds_m2 < 15000)  #--outlier

#--keep field in site_id to see if soy/corn phase differed in central site
dat2 <- 
  pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, field, sys_trt, col = "site_sys", remove = T) %>% 
  select(-rep)

dstat2 <- 
  dat2 %>% 
  mutate(obs_id = 1:n(),
         obs_id = paste("obs", obs_id, sep = "_")) %>% 
  mutate(cc_trt = recode(cc_trt, 
                         "rye" = "ccrye"))
dstat_outrm2 <- 
  dstat2 %>% 
  filter(totseeds_m2 < 15000)  #--outlier

#--can I pool w/in central-grain? test sig of field effect. no, can't pool
datcg <- 
  dat2 %>% 
  separate(site_sys, into = c("site", "field", "sys"), remove = F) %>% 
  filter(site == "Boyd",
         sys == "grain") %>% 
  mutate(obs_id = 1:n(),
         obs_id = paste("obs", obs_id, sep = "_"))

m1 <- glmer(totseeds ~ field*cc_trt + (1|blockID) + (1|obs_id), data = datcg, 
            family = poisson(link = "log"))  

summary(m1)
  
# poisson -----------------------------------------------------------------

library(lme4) #--can do generalized linear models also
library(lmerTest)
library(emmeans)
library(broom)



############### keep soy/corn of central-grain separate #################

# poisson -----------------------------------------------------------------

#--use random factor for obs and block

m1 <- glmer(totseeds_m2 ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = dstat_outrm2, 
            family = poisson(link = "log"))  
m1nox <- glmer(totseeds_m2 ~ site_sys+cc_trt + (1|blockID) + (1|obs_id), data = dstat_outrm2, 
            family = poisson(link = "log"))  

#--do it on converted seeds, it barks at you but it's fine
m1_m2 <- glmer(totseeds_m2 ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = dstat_outrm2, 
            family = poisson(link = "log"))  

emmeans(m1_m2,pairwise ~cc_trt, type = "response")
tidy(emmeans(m1_m2, ~cc_trt|site_sys, type = "response"))
anova(emmeans(m1_m2, ~site_sys, type = "response"))


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



############### outlier stays #################

# poisson -----------------------------------------------------------------

#--use random factor for obs and block

mf1 <- glmer(totseeds ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = dstat2, 
            family = poisson(link = "log"))  


# get estimates -----------------------------------------------------------


mf1_emlog <- emmeans(mf1, pairwise ~ cc_trt|site_sys)
mf1_em <- emmeans(mf1, pairwise ~ cc_trt|site_sys, type = "response")


#--log scale
mf1_estlog <- tidy(mf1_emlog$emmeans) %>% 
  mutate(model = "pois_full")

mf1_contlog <- tidy(mf1_emlog$contrasts) %>% 
  mutate(model = "pois_full")

#--original scale
mf1_est <- tidy(mf1_em$emmeans) %>% 
  mutate(model = "pois_full")

mf1_cont <- 
  tidy(mf1_em$contrasts) %>% 
  mutate(model = "pois_full") %>% 
  left_join(mf1_em$contrasts %>% 
              confint( level = 0.9) %>% 
              as_tibble() %>% 
              mutate(model = "pois_full"))


#--overall
oaf <- 
  emmeans(mf1, pairwise ~ cc_trt:site_sys, type = "response", )$contrasts %>% 
  summary() %>% 
  as_tibble() %>% 
  separate(contrast, into = c("level1", "level2"), sep = "/") %>% 
  mutate(p.value = round(p.value, 3)) 

oaf %>% 
  filter(grepl("Funcke", level1))

# write results -----------------------------------------------------------

#--log scale
mf1_estlog %>%
  write_csv("01_stats-uni/st_weedseed-est-log-full.csv")

mf1_contlog %>% 
  write_csv("01_stats-uni/st_weedseed-contr-log-full.csv")


#--response scale
mf1_est %>%
  write_csv("01_stats-uni/st_weedseed-est-full.csv")

mf1_cont %>% 
  write_csv("01_stats-uni/st_weedseed-contr-full.csv")



