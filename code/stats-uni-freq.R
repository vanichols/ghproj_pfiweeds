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

# Alternatively, try poisson model ----------------------------------------

library(performance)

p1 <- glmer(totseeds_m2 ~ site_sys*cc_trt2 + (1|blockID), data = dstat, 
            family = poisson(link = "log"))

# hmmm doesn't like this bc totseeds_m2 are not integers...
dstat <- 
  dstat %>%
  mutate(seeds_m2_int = as.integer(totseeds_m2))

p2 <- glmer(seeds_m2_int ~ site_sys*cc_trt2 + (1|blockID), data = dstat, 
            family = poisson(link = "log"))
summary(p2) # is this right?

# checking for overdispersion
performance::check_overdispersion(p2)   # there is overdispersion... don't use poisson

# try negative binomial instead...

g1 <- glmer.nb(seeds_m2_int ~ site_sys*cc_trt2 + (1|blockID), data = dstat)

#ooo it fit!
summary(g1) 
performance::check_model(g1)
performance::compare_performance(g1, m1) # idk if this is correct, but AIC is much lower with lmer model...
performance::r2(g1) # looking at marginal and conditional r2

# Get estimates from model ------------------------------------------------

m1em <- (emmeans(m1, pairwise ~ cc_trt2|site_sys, type = "response"))
m1em

m1_est <- tidy(m1em$emmeans) 

m1_cont <- tidy(m1em$contrasts) 


# estimates from negative binomial
g1em <- emmeans(g1, pairwise ~ cc_trt2|site_sys)
g1em # ok these results are on the log scale

# is this ok?
g1em_resp <- emmeans(g1, pairwise ~ cc_trt2|site_sys, type = "response")
g1em_resp  # looks very similar to our lmer model emmeans, which is good! Perhaps these can be interpreted as means

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


dat_av <- 
  dstat %>% 
  group_by(site_sys) %>% 
  summarise(avgseeds_m2 = mean(totseeds_m2)) %>% 
  left_join(abs_eff) %>% 
  left_join(rel_eff)


dat_av %>% 
  pivot_longer(cols = c(diff_redseedsm2, diff_pctred)) %>% 
  ggplot(aes(avgseeds_m2, value)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~name, scales = "free")

# compare ccbio metrics and effects ---------------------------------------

#--via spearman ranking correlations?

ccbio <- read_csv("data/smy/sd_ccbio-metrics.csv")

ccbio



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


# try using regression models? ---------------------------------------------


ccbio_mod %>% 
  select(-diff_pctred) %>% 
  pivot_longer(nabove1:ccbio_stab) %>% 
  group_by(yr_span, name) %>% 
  nest()


ccstat <- 
  dstat %>% 
  left_join(ccbio)

ccstat %>% 
  pivot_longer(nabove1:ccbio_stab) %>% 
  group_by(yr_span, name) %>% 
  nest() %>% 
  mutate(mod = data %>% map(~lmer(log(totseeds_m2) ~ cc_trt2 + value + (1|blockID), data = .)),
         res = mod %>% map(anova),
         res2 = res %>% map(tidy)) %>% 
  unnest(cols = c(res2)) %>% 
  select(yr_span, name, term, p.value) %>%
  filter(term == "value") %>% 
  arrange(p.value)


#--try 5 yr mean
ccm1 <- lmer(log(totseeds_m2) ~ cc_trt2 + ccbio_mean + (1 | blockID),
             data = ccstat %>% filter(yr_span == "5yr"))
             
anova(ccm1)

ccbio_mod %>% 
  filter(yr_span == "5yr") %>% 
  ggplot(aes(ccbio_mean, diff_redseedsm2)) + 
  geom_point()