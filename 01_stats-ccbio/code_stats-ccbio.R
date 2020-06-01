##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 4/14/2020
#
# Last modified: 6/1/2020 (cleaning up?)
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


ccbio <- read_csv("01_stats-ccbio/sc_ccbio-metrics.csv")

wseed <- read_csv("01_stats-uni/st_weedseed-contrasts.csv") %>%
  filter(model == "pois") %>%
  mutate(estimate = -estimate) %>% #--I want rye vs none
  select(site_sys, estimate)

rye_eff <- 
  read_csv("01_stats-uni/st_weedseed-estimates.csv") %>% 
  filter(model == "pois") %>% 
  select(site_sys, cc_trt, totseeds_m2) %>% 
  pivot_wider(names_from = cc_trt,
              values_from = totseeds_m2) %>% 
  mutate(absdiff_rye_to_no_seedsm2 = rye - no,
         reldiff_rye_to_no_seedsm2 = (absdiff_rye_to_no_seedsm2 / no) * 100) %>% 
  select(site_sys, absdiff_rye_to_no_seedsm2, reldiff_rye_to_no_seedsm2)



ccbio_mod <- 
  ccbio %>% 
  left_join(rye_eff)


ccbio_mod %>%
  pivot_longer(cols = c(nabove1:ccbio_stab),
               names_to = 'metric') %>%
  arrange(metric, yr_span) %>%
  group_by(metric, yr_span) %>%
  nest() %>%
  mutate(spcor_abs = data %>%
           map(
             ~ cor(
               .$absdiff_rye_to_no_seedsm2,
               .$value,
               use = "complete.obs",
               method = "spearman"
             )
           ),
         spcor_rel = data %>%
           map(
             ~ cor(
               .$reldiff_rye_to_no_seedsm2,
               .$value,
               use = "complete.obs",
               method = "spearman"
             )
           )) %>%
  select(yr_span, metric, spcor_abs, spcor_rel) %>%
  unnest(cols = c(spcor_abs, spcor_rel)) %>%
  arrange(yr_span, spcor_abs)

ccbio_mod %>%
  filter(yr_span == "10yr") %>% 
  ggplot(aes(ccbio_cv, absdiff_rye_to_no_seedsm2)) + 
  geom_point()
