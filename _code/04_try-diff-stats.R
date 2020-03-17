##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 30 2019
# Last modified: 
#
# Purpose: do 'official' stats for manuscript
#
# Inputs: td_GHspecies, td_GHsum, td-all-ryebm2008-2019
#
# Outputs: 
#
# Notes:
#
#
####################################

rm(list = ls())
library(tidyverse)
library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(broom)
library(emmeans)



# what is spread in locs? -------------------------------------------------

dat <- read_csv("_data/tidy/td-GHsum.csv") 

dat %>% 
  group_by(loc) %>% 
  summarise(min = min(totseeds_m2),
            max = max(totseeds_m2),
            mean = mean(totseeds_m2))

# matt doesn't use a ratio ------------------------------------------------

dstat <- 
  dat %>% 
  unite(loc, cropsys, col = "loc_sys") %>% 
  mutate(rep2 = paste0(loc_sys, rep),
         cc_trt2 = recode(cc_trt,
                          no = "none",
                          rye = "aryecc"))

#--full data set
m1 <- lmer(log(totseeds_m2) ~ loc_sys * cc_trt2 + (1|rep2), data = dstat)
anova(m1)
emmeans(m1, pairwise ~ cc_trt2|loc_sys, type = "response")

#--outlier removed
m2 <- lmer(log(totseeds_m2) ~ loc_sys * cc_trt2 + (1|rep2), data = filter(dstat, totseeds_m2 < 15000))
anova(m2)
emmeans(m2, pairwise ~ cc_trt2|loc_sys, type = "response")


#--examples to help
#pigs.emm.s <- emmeans(pigs.lm, "source")
#pairs(pigs.emm.s)
#emm_s.t <- emmeans(noise.lm, pairwise ~ size | type, )


# make a ratio ------------------------------------------------------------

datr <- 
  dat %>%
  spread(cc_trt, value = totseeds_m2) %>%
  mutate(rat = (rye/no))


#--fit models w/ratio 
# NOTE: only one obs for each rep, so can't include 'rep' in the model

# use a transformation in the actual model
mr1 <- lmer(log(rat) ~ cropsys + (1|loc), data = datr)
mr2 <- lm(log(rat) ~ cropsys, data = datr)
summary(mr2)
mr1em <- emmeans(mr1, "cropsys", weights = "proportional") #--this results in silage not being sig...
mr1em <- emmeans(mr2, "cropsys")

# get CIs/pvals, from https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html
test(mr1em)

res92 <- as_tibble(confint(mr1em, level = .925, type = "response")) %>% 
  mutate(CL = "92.5%")
res95 <- as_tibble(confint(mr1em, level = .95, type = "response")) %>% 
  mutate(CL = "95%")

res <- bind_rows(res92, res95) %>% 
  mutate(cropsys = str_to_title(cropsys))

res %>% write_csv("_data/smy/sd_stats-lrr.csv")
  


# cc bio vs ratio? --------------------------------------------------------

ccbio <- read_csv("_data/smy/sd_ccbio-metrics.csv") %>% 
  rename(loc = location,
         cropsys = crop_sys)

bior <- datr %>% left_join(ccbio)

# nabove1, almost sig
cc1 <- lmer(log(rat) ~ nabove1 + (1|loc), data = bior)
summary(cc1)
confint(cc1)
# mean is sig, but come on....
# nabove2, shan, ccbio_cv, shan_hill, not
#cc4 <- lmer(log(rat) ~ shan_hill + (1|loc), data = bior)
#summary(cc4)

