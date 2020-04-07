##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 30 2019
#
# Last modified: march 23 2020 (now use package for data)
#
# Purpose: do 'official' stats for manuscript
#
# Outputs: 
#
# Notes: m2 model (site_sys*cc_trt2 + (1|blockID) is the best fitting
#     
#        need to tidy emmeans output and write it
#
#
####################################

rm(list = ls())
library(tidyverse)
#devtools::install_github("vanichols/PFIweeds2020", force = TRUE) ## <-- run if pkg changes
library(PFIweeds2020)



# what is spread in locs? -------------------------------------------------

#--use data and fucntion from package
pfi_ghobsraw

dat <- pfifun_sum_byeu(pfi_ghobsraw) %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = F) 

dat %>% write_csv('data/misc_for-uncle-steve.csv')


dat %>% 
  group_by(site_name, field) %>% 
  summarise(min = min(totseeds_m2),
            max = max(totseeds_m2),
            mean = mean(totseeds_m2))

# mixed model on data ------------------------------------------------

library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(broom)
library(emmeans)

dstat <- 
  dat %>% 
  mutate(cc_trt2 = recode(cc_trt,          ##--I want rye to appear first alphabetically?
                          no = "none",
                          rye = "aryecc")) %>% 
  
  group_by(site_sys, sys_trt, blockID) %>% 
  mutate(ctl_mean_totseeds = mean(totseeds_m2),
         ctl_mean_totseeds_sc = scale(ctl_mean_totseeds)) %>% 
  ungroup()


#--full data set
m1 <- lmer(log(totseeds_m2) ~ site_sys * cc_trt2 + (1|blockID), data = dstat)
anova(m1)
emmeans(m1, pairwise ~ cc_trt2|site_sys, type = "response")

#--outlier removed
m2 <- lmer(log(totseeds_m2) ~ site_sys * cc_trt2 + (1|blockID), data = filter(dstat, totseeds_m2 < 15000))
anova(m2)
summary(m2)

# how the crap do I tidy emmeans output? I've done it before.....#####################################
m2em <- (emmeans(m2, pairwise ~ cc_trt2|site_sys, type = "response"))
marginal <- emmeans(oranges_rg1, "day")

tidy(m2em$contrasts) 
  



#emmeans(m2, "cc_trt2")
#emmeans(m2, pairwise ~ cc_trt2, type = "response")  #--back transformed to ratio
#emmeans(m2, pairwise ~ cc_trt2, type = "lp") #--log scale? confused

#--outlier removed, only fixed effects
m2f <- lm(log(totseeds_m2) ~ site_sys * cc_trt2, data = filter(dstat, totseeds_m2 < 15000))
anova(m2f)

#--outlier removed, not log-transformed
m2r <- lmer((totseeds_m2) ~ site_sys * cc_trt2 + (1|blockID), data = filter(dstat, totseeds_m2 < 15000))
anova(m2r)


#--including block as a random effect improves model fit. 
anova(m2, m2f)

#--different model
m3 <- lmer(log(totseeds_m2) ~ 1 + site_sys * cc_trt2 + (1 + cc_trt2|blockID), data = filter(dstat, totseeds_m2 < 15000))
anova(m3)
emmeans(m3, pairwise ~ cc_trt2|site_sys, type = "response")  #--back transformed to ratio

#--these two models are the same. No need to fit a separate slope for each block. I guess.
anova(m3, m2)

#--try one more form
# Reaction ~ 1 + Days + (1 | Subject) + (0 + Days | Subject)
# Reaction ~ 1 + Days + (1 + Days | Subject)
m4 <- lmer(log(totseeds_m2) ~ 1 + site_sys * cc_trt2 + (1 | blockID) + (0 + cc_trt2|blockID), 
           data = filter(dstat, totseeds_m2 < 15000))
anova(m4)

anova(m2, m4)

# No, m2 is the best. 

#--include average of none trt seeds as covariate? No. I don't want to CONTROL for it.
# I'm not sure how to do this. 
m5 <- lmer(log(totseeds_m2) ~ site_sys * cc_trt2 + ctl_mean_totseeds + (1|blockID), 
           data = filter(dstat, totseeds_m2 < 15000))
summary(m5)


# the winner --------------------------------------------------------------

#--outlier removed
m2 <- lmer(log(totseeds_m2) ~ site_sys * cc_trt2 + (1|blockID), data = filter(dstat, totseeds_m2 < 15000))
anova(m2)
summary(m2)

m2em <- (emmeans(m2, pairwise ~ cc_trt2|site_sys, type = "response"))
tidy(m2em$contrasts)  %>% 
  arrange(ratio)



#--examples to help
#pigs.emm.s <- emmeans(pigs.lm, "source")
#pairs(pigs.emm.s)
#emm_s.t <- emmeans(noise.lm, pairwise ~ size | type, )

library(ggResidpanel)

ggResidpanel::resid_interact(m2)


# try bayesian w/brms and m2 --------------------------------------------
# https://juliasilge.com/blog/salary-gender/
library(brms)
options(mc.cores = parallel::detectCores())

# example from blog:
# fit_bayes1 <- brm(
#   ConvertedComp ~ (1 | DevType) + EdLevel + OpenSourcer +
#     YearsCodePro + Dependents + Gender,
#   data = modeling_df
# )

# note: it takes a few minutes
fit_bayes1 <- brm(
  log(totseeds_m2) ~ 1 +(1 | blockID) + site_sys*cc_trt2,
  data = filter(dstat, totseeds_m2 < 15000))


summary(fit_bayes1)

prior_summary(fit_bayes1)
prior_samples(fit_bayes1)
launch_shinystan(fit_bayes1)

library(tidybayes)

res_bayes1 <- fit_bayes1 %>%
  tidybayes::gather_draws(`b_.*`, regex = TRUE) %>%
  ungroup()

res_bayes1 %>% write_rds("data/tidy/td_bayes1.rds")


# try rstanarm ------------------------------------------------------------
# https://mc-stan.org/users/documentation/case-studies/tutorial_rstanarm.html#using-the-rstanarm-package

library(rstanarm)
options(mc.cores = parallel::detectCores())

# M1_stanlmer <- stan_lmer(formula = course ~ 1 + (1 | school), 
#                          data = GCSE,
#                          seed = 349)

M1_stanlmer <- stan_lmer(formula = log(totseeds_m2) ~ (1 | blockID) + site_sys*cc_trt2, 
                         data = filter(dstat, totseeds_m2 < 15000),
                         seed = 349)

summary(M1_stanlmer, 
        pars = c("(Intercept)", "sigma"),
        probs = c(0.025, 0.975),
        digits = 2)


# argh I don't care. Its fine. Use brms. 
M1_stanlmer %>%
  tidy_draws() %>% 
  select(.chain, .iteration, .draw, contains("(Intercept)") | contains("site")) %>% 
  gather_variables()  %>% 
  ungroup() %>%
  mutate(.variable = str_remove_all(.variable, "cc_trt2|site_sys")) %>% 
  pivot_wider(names_from = .variable, values_from = .value) %>% 
  clean_names() %>% 
  select(chain, iteration, draw, intercept, #--intercept is actually boyd_grain_rye 
         ryecc, boyd_silage_ryecc, funcke_grain_ryecc, stout_grain_ryecc) %>% 
  mutate(boyd_grain_ryecc = intercept - intercept + ryecc, #--I don't know what to do here....
         funcke_ryecc = funcke_grain_ryecc,
         stout_ryecc = stout_grain_ryecc) %>% 
  select(chain, iteration, draw, boyd_grain_ryecc, boyd_silage_ryecc, funcke_ryecc, stout_ryecc) %>% 
  pivot_longer(boyd_grain_ryecc:stout_ryecc) %>% 
  ggplot(aes(x = value, y = reorder(name, -value, mean))) +
  geom_vline(xintercept = 0, color = "gray50", size = 1.2, lty = 2, alpha = 0.5) +
  geom_halfeyeh(fill = "gray80", .width = c(0.9)) +
  #stat_pointintervalh(.width = c(.66, .95)) +
  #stat_pointintervalh(.width = 0.9, color = "red", alpha = 0.5) +
  theme(legend.position = "none") +
  labs(
    y = NULL,
    x = "Increase / decrease in weed seeds (log of total seeds)")

