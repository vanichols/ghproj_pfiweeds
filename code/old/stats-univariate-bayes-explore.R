##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: march 27 2020
#
# Last modified: march 27 2020 
#                april 6 2020 (trying to get estimates)
#
# Purpose: explore brms bayes results
#
# Outputs: 
#
# Notes: used model site_sys*cc_trt2 + (1|blockID) 
#
#
####################################

rm(list = ls())
library(tidyverse)
#devtools::install_github("vanichols/PFIweeds2020", force = TRUE) ## <-- run if pkg changes
library(PFIweeds2020)
library(tidybayes)
library(janitor)


# package data, for reference ---------------------------------------------

dat <- pfifun_sum_byeu(pfi_ghobsraw) %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = F) 

res_bayes <- read_rds("data/tidy/td_bayes1.rds")



# try to visualize? -------------------------------------------------------

res_bayes2 <- res_bayes  %>%  
  mutate(.variable = str_remove_all(.variable, "b_|cc_trt2|site_sys")) %>% 
  pivot_wider(names_from = .variable, 
              values_from = .value) %>% 
  clean_names() 

res_bayes_fig <- 
  res_bayes2 %>% 
  select(
    chain, iteration, draw, 
         intercept, #--intercept is actually boyd_grain_ryecc 
         #ryecc, #--need to add to intercept to get boyd_grain_ryecc
    ryecc,
         boyd_silage_ryecc, 
         funcke_grain_ryecc, 
         stout_grain_ryecc
    ) %>% 
  mutate(
    boyd_grain_ryecc = intercept - intercept + ryecc,
         boyd_silage_ryecc = boyd_silage_ryecc,
         funcke_ryecc = funcke_grain_ryecc,
         stout_ryecc = stout_grain_ryecc
    ) %>% 
  select(chain, iteration, draw, boyd_grain_ryecc, boyd_silage_ryecc, funcke_ryecc, stout_ryecc) %>% 
  pivot_longer(boyd_grain_ryecc:stout_ryecc)


res_means <- 
  res_bayes_fig %>% 
  group_by(name) %>% 
  summarise(mean = mean(value)) %>% 
  arrange(mean)



res_bayes_fig %>% 
  ggplot(aes(x = value, y = reorder(name, -value, mean))) +
  geom_vline(xintercept = 0, color = "gray50", size = 1.2, lty = 2, alpha = 0.5) +
  geom_halfeyeh(fill = "gray80", .width = c(0.9)) +
  #stat_pointintervalh(.width = c(.66, .95)) +
  theme(legend.position = "none") +
  labs(
    y = NULL,
    x = "Change in weed seeds, look at direction and relative magnitude")

# no idea how to convert values into something meaningful. -0.8. 0.4. What the hell does that mean. 
1/-.8
-.8/1 # means it decreased 20%?


