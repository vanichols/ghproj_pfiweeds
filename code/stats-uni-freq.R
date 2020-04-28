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
  select(-field, -rep)


#--skewness

library(e1071)

skewness(dat$totseeds)

dat %>% 
  ggplot(aes(totseeds)) +
  geom_histogram()

dat %>% 
  ggplot(aes(sqrt(totseeds))) +
  geom_histogram()

dat %>% 
  ggplot(aes(log(totseeds))) +
  geom_histogram()

#--yup, log or sqrttransform...

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

#--fit model
m1log <- lmer(log(totseeds) ~ site_sys * cc_trt2 + (1|blockID), data = dstat)
anova(m1log) #--interaction, should I keep it?
m1_noint <- lmer(log(totseeds) ~ site_sys + cc_trt2 + (1|blockID), data = dstat)

#--interaction sig improves fit (p = 0.04)
anova(m1log, m1_noint)


#--try sqrt intstead of log transformation
m1sqrt <- lmer(sqrt(totseeds) ~ site_sys * cc_trt2 + (1|blockID), dstat)

#--compare log vs sqrt trans
ggResidpanel::resid_compare(list(m1log, m1sqrt))
# well, the QQ plot looks better. 

# Alternatively, try poisson model ----------------------------------------

library(performance)

p1a <- glmer(totseeds ~ site_sys*cc_trt2 + (1|blockID), data = dstat, 
            family = poisson(link = "log"))

performance::check_overdispersion(p1a)   # there is overdispersion... don't use 
performance::check_model(p1a) # look at it for fun
performance::check_collinearity(p1a) #-->2.5 is a problem apparently

# # is multicollinarity inherent to interactions...? Gina you can ignore this..
# ex_df <- tibble(resp = runif(40),
#                 site = rep(c("site1", "site2", "site3", "site4"), each = 10),
#                 trt  = rep(c("treat", "control"), each = 5, times = 4),
#                 reps = rep(1:5, times = 8),
#                 block = paste0(site,"_",reps, sep = ""))
# ex_mod <- lmer(resp ~ site*trt + (1|block), ex_df) # oh well singular fit
# ex_mod_1 <- lm(resp ~ site*trt, ex_df)
# performance::check_model(ex_mod_1) # here the interaction has a high VIF... 

# try negative binomial instead...
g1a <- glmer.nb(totseeds ~ site_sys*cc_trt2 + (1|blockID), data = dstat)
#ooo it fit!
summary(g1a) 
performance::check_model(g1a) 
ggResidpanel::resid_panel(g1a) #--hmmm

# compare diagnostic plots of m1log and g1a which both use totseeds
ggResidpanel::resid_compare(list(m1log, g1a))

# compare fit of m1log and g1a
performance::compare_performance(m1log, g1a)    # looks like model m1log fits better, although g1a explains more variation
#--katherine says don't use amt of variation explained as a metric. Or AIC. Blech. 

# Get estimates from models ------------------------------------------------

#--lmer, assumes normally dist errors

#--log transformed data
m1log_em <- (emmeans(m1log, pairwise ~ cc_trt2|site_sys, type = "response"))
m1log_cont <- tidy(m1log_em$contrasts) %>% 
  mutate(mod = "lmerlog")

#--sqrt tranformed data
m1sqrt_em <- (emmeans(m1sqrt, pairwise ~ cc_trt2|site_sys, type = "response"))
m1sqrt_cont <- tidy(m1sqrt_em$contrasts) %>% 
  mutate(mod = "lmersqrt")
m1sqrt_est <- tidy(m1sqrt_em$emmeans) %>% 
  mutate(mod = "lmersqrt")

ggResidpanel::resid_compare(list(m1log, m1sqrt))

#--estimates from negative binomial (gina needs to read about this)
g1em <- emmeans(g1a, pairwise ~ cc_trt2|site_sys)
g1em # ok these results are on the log scale

# is this ok?
g1em_resp <- emmeans(g1a, pairwise ~ cc_trt2|site_sys, type = "response")
g1em_resp  # looks very similar to our lmer model emmeans, which is good! 

#Perhaps these can be interpreted as means

#--Gina compare them:
tidy(g1em_resp$contrasts) %>% 
  mutate(mod = "neg_bin") %>% 
  bind_rows(m1log_cont) %>% 
  bind_rows(m1sqrt_cont) %>% 
  select(site_sys, mod, p.value) %>% 
  arrange(site_sys, mod) %>% 
  pivot_wider(names_from = site_sys, values_from = p.value)

#--Funcke is way more sig w/sqrt. Hmm. 
# I think we can go with the sqrt for p-values, but just do straightup means for reporting? I don't love it. But...we do love significant things...


# ok here I'm comparing our two models that were fit to exactly the same responses (tot seeds)
tidy(g1a_means$contrasts) %>% 
  mutate(mod = "neg_bin") %>% 
  bind_rows(m1log_cont)

# it's interesting because the ratios are very similar, but the standard erros are lower in the 
# negative binomial model, so the p-values are more significant. Also what's a z-ratio? Does it matter? 



# Use sqrt transformed linear model ---------------------------------------


# write results -----------------------------------------------------------

m1sqrt_est %>%
  rename(totseeds = response) %>% 
  mutate(totseeds_m2 = totseeds / ( ( (pi * 2.8575^2) * 20 ) / 10000 )) %>% 
  select(mod, site_sys, cc_trt2, totseeds, totseeds_m2, everything()) %>% 
  write_csv("data/smy/sd_estimates.csv")

m1sqrt_cont %>%
  select(mod, site_sys, p.value) %>% 
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
  summarise(avgseeds_m2 = mean(totseeds)) %>% 
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
  mutate(mod = data %>% map(~lmer(log(totseeds) ~ cc_trt2 + value + (1|blockID), data = .)),
         res = mod %>% map(anova),
         res2 = res %>% map(tidy)) %>% 
  unnest(cols = c(res2)) %>% 
  select(yr_span, name, term, p.value) %>%
  filter(term == "value") %>% 
  arrange(p.value)


#--try 5 yr mean
ccm1 <- lmer(log(totseeds) ~ cc_trt2 + ccbio_mean + (1 | blockID),
             data = ccstat %>% filter(yr_span == "5yr"))
             
anova(ccm1)

ccbio_mod %>% 
  filter(yr_span == "5yr") %>% 
  ggplot(aes(ccbio_mean, diff_redseedsm2)) + 
  geom_point()
