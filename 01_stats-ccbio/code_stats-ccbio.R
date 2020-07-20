##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 4/14/2020
#
# Last modified: 6/1/2020 (cleaning up?)
#                7/16/2020 (redoing calc of changes, used code from bar fig)
#
# Purpose: calculate cc biomass things
#
# Outputs: 
#
# Notes: The results are absolute nonsense
#
####################################

rm(list = ls())
library(tidyverse)
library(PFIweeds2020)


pfi_ccbio

ccbio <- read_csv("01_stats-ccbio/sc_ccbio-metrics.csv") #--this is created in code-summarise-ccbio

sb_chng <- 
  read_csv("01_stats-uni/st_weedseed-est.csv") %>% 
  mutate(totseeds_m2 = pfifun_seedstom2conv(rate)) %>% 
  select(cc_trt, site_sys, totseeds_m2) %>% 
  pivot_wider(names_from = cc_trt, values_from = totseeds_m2) %>% 
  mutate(trt_eff_abs = ccrye - no,
         trt_eff_rel = trt_eff_abs/no) %>% 
    select(site_sys, trt_eff_abs, trt_eff_rel)


ccbio_mod <- 
  ccbio %>% 
  left_join(sb_chng)


ccbio_mod %>% 
  write_csv("01_stats-ccbio/sc_ccbio-metrics-effects.csv")


ccbio_mod %>%
  pivot_longer(cols = c(nabove1:ccbio_2019),
               names_to = 'metric') %>%
  arrange(metric, yr_span) %>%
  group_by(metric, yr_span) %>%
  nest() %>%
  mutate(spcor_abs = data %>%
           purrr::map(
             ~ cor(
               .$trt_eff_abs,
               .$value,
               use = "complete.obs",
               method = "spearman"
             )
           ),
         spcor_rel = data %>%
           purrr::map(
             ~ cor(
               .$trt_eff_rel,
               .$value,
               use = "complete.obs",
               method = "spearman"
             )
           )) %>%
  select(yr_span, metric, spcor_abs, spcor_rel) %>%
  unnest(cols = c(spcor_abs, spcor_rel)) %>%
  arrange(yr_span, spcor_abs) %>% 
  write_csv("01_stats-ccbio/sc_ccbio-spear-cors.csv")
  



