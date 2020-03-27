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
# Notes:
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

dat <- pfifun_sum_byeu(pfi_ghobsraw)


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
  unite(site_name, sys_trt, col = "site_sys", remove = F) %>% 
  mutate(cc_trt2 = recode(cc_trt,          ##--I want rye to appear first alphabetically?
                          no = "none",
                          rye = "ryecc")) %>% 
  mutate(repmatt = paste(site_name, sys_trt, rep))

#--full data set
m1 <- lmer(log(totseeds_m2) ~ site_sys * cc_trt2 + (1|blockID), data = dstat)
anova(m1)
emmeans(m1, pairwise ~ cc_trt2|site_sys, type = "response")

#--outlier removed
m2 <- lmer(log(totseeds_m2) ~ site_sys * cc_trt2 + (1|blockID), data = filter(dstat, totseeds_m2 < 15000))
anova(m2)
emmeans(m2, pairwise ~ cc_trt2|site_sys, type = "response")
emmeans(m2, "cc_trt2")
emmeans(m2, pairwise ~ cc_trt2, type = "response")  #--back transformed to ratio
emmeans(m2, pairwise ~ cc_trt2, type = "lp") #--log scale? confused

#--examples to help
#pigs.emm.s <- emmeans(pigs.lm, "source")
#pairs(pigs.emm.s)
#emm_s.t <- emmeans(noise.lm, pairwise ~ size | type, )

# maybe I should try bayesian? --------------------------------------------
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
  log(totseeds_m2) ~ (1 | blockID) + site_sys*cc_trt2,
  data = filter(dstat, totseeds_m2 < 15000))


summary(fit_bayes1)

library(tidybayes)

dummy <- fit_bayes1 %>%
  gather_draws(`b_.*`, regex = TRUE) %>%
  ungroup() %>% 
  group_by(.variable) %>%
  slice(1:5) %>% 
  ungroup()

library(janitor)
dummy  %>%  
  mutate(.variable = str_remove_all(.variable, "b_|cc_trt2|site_sys")) %>% 
  pivot_wider(names_from = .variable, values_from = .value) %>% 
  clean_names() %>% 
  select(chain, iteration, draw, intercept, #--intercept is actually boyd_grain_rye 
         ryecc, boyd_silage_ryecc, funcke_grain_ryecc, stout_grain_ryecc) %>% 
  mutate(boyd_grain_ryecc = intercept - intercept + ryecc,
         boyd_silage_ryecc = intercept + boyd_silage_ryecc,
         funcke_ryecc = intercept + funcke_grain_ryecc,
         stout_ryecc = intercept + stout_grain_ryecc) %>% 
  select(chain, iteration, draw, boyd_grain_ryecc, boyd_silage_ryecc, funcke_ryecc, stout_ryecc) %>% 
  pivot_longer(boyd_grain_ryecc:stout_ryecc)


fit_bayes1 %>%
  gather_draws(`b_.*`, regex = TRUE) %>%
  ungroup() %>%
  mutate(.variable = str_remove_all(.variable, "b_|cc_trt2|site_sys")) %>% 
  pivot_wider(names_from = .variable, values_from = .value) %>% 
  clean_names() %>% 
  select(chain, iteration, draw, intercept, #--intercept is actually boyd_grain_rye 
         ryecc, boyd_silage_ryecc, funcke_grain_ryecc, stout_grain_ryecc) %>% 
  mutate(boyd_grain_ryecc = intercept - intercept + ryecc,
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
