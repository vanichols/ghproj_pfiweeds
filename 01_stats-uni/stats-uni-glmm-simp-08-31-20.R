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

library(lme4) #--can do generalized linear models also
library(lmerTest)
library(emmeans)
library(broom)




# data --------------------------------------------------------------------

#--use data and fucntion from package

pfi_ghobsraw

dat <- 
  pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = T) %>% 
  select(-field, -rep)



# data for stats w/obs_id -------------------------------------------------

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
  filter(site == "Central",
         sys == "grain") %>% 
  mutate(obs_id = 1:n(),
         obs_id = paste("obs", obs_id, sep = "_"))

m1 <- glmer(totseeds ~ field*cc_trt + (1|blockID) + (1|obs_id), data = datcg, 
            family = poisson(link = "log"))  

summary(m1)

############### keep soy/corn of central-grain separate #################

# poisson -----------------------------------------------------------------

#--use random factor for obs and block

m1 <- glmer(totseeds ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = dstat_outrm2, 
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




# do a leave-one-out sens analysis ----------------------------------------

#--full model

m.full <- glmer(totseeds ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = dstat2, 
               family = poisson(link = "log"))  

outs <- 
  dstat2 %>% 
  mutate(cooksd = cooks.distance(m.full)) %>% 
  unite(site_sys, cc_trt, col = "id")

ggplot(outs, aes(obs_id, cooksd)) + 
  geom_point() + 
  #geom_label(data = outs %>% filter(cooksd > 0.002), aes(label = id)) + 
  geom_label_repel(data = outs %>% filter(grepl("West", id)), aes(label = id)) + 
  coord_flip()




m.loo <- emmeans(m.full, pairwise ~ cc_trt|site_sys, type = "response")

m.loo.cont <- 
  tidy(m.loo$contrasts) %>% 
  mutate(loo = 0) %>% 
  left_join(m.loo$contrasts %>% 
              confint( level = 0.9) %>% 
              as_tibble() %>% 
              mutate(loo = 0))



for (i in 1:length(dstat$obs_id)){
  
  d.tmp <- dstat2[-i,]
  
  m.tmp <- glmer(totseeds ~ site_sys*cc_trt + (1|blockID) + (1|obs_id), data = d.tmp, 
              family = poisson(link = "log"))  

  
  m.tmp.em <- emmeans(m.tmp, pairwise ~ cc_trt|site_sys, type = "response")
  
  m.tmp.cont <- 
    tidy(m.tmp.em$contrasts) %>% 
    mutate(loo = i) %>% 
    left_join(m.tmp.em$contrasts %>% 
                confint( level = 0.9) %>% 
                as_tibble() %>% 
                mutate(loo = i))
  
  m.loo.cont <- rbind(m.loo.cont, m.tmp.cont)
  
  
}

m.loo.cont %>% 
  ggplot(aes(site_sys, p.value)) + 
  geom_point() + 
  geom_text_repel(aes(label = loo)) +
  geom_hline(yintercept = 0.10, linetype = "dashed")

m.loo.cont %>% 
  ggplot(aes(site_sys, p.value)) + 
  geom_jitter(width = 0.1, size = 2) + 
  #geom_text_repel(aes(label = loo)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.10, linetype = "dashed")
