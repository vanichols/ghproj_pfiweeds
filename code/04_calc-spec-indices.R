##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Oct 30 2019
# Last modified: Oct 30 2019
#
# Purpose: calculate things like species evenness
#
# Inputs: td_GHspecies
#
# Outputs:
#
# Notes:
#
#
####################################


library(tidyverse)


# data --------------------------------------------------------------------

dat <- read_csv("_data/tidy/td-GHspecies.csv")

dat %>% 
  unite(loc_sys, cc_trt, col = "id") %>% pull(id) %>%  unique()


# calc things -------------------------------------------------------------

shandiv <- dat %>%
  filter(nmbr != 0) %>% 
  group_by(loc_sys, cc_trt, rep) %>%
  mutate(tot.m2 = sum(nmbr.m2),
         p = nmbr.m2/tot.m2,
         lnp = log(p),
         plnp = p*lnp) %>% 
  group_by(loc_sys, cc_trt, rep) %>% 
  summarise(h = -(sum(plnp)))

srich <- 
  dat %>%
  filter(nmbr != 0) %>% 
  select(loc_sys, cc_trt, rep, weed) %>% 
  distinct() %>% 
  group_by(loc_sys, cc_trt, rep) %>% 
  summarise(tots = n()) %>% 
  mutate(lns = log(tots)) 

div <- shandiv %>% 
  left_join(srich) %>% 
  mutate(equ = h / lns) %>% 
  # Funcke_grain_no rep 4 has only 1 species, so it's 0/0
  replace(is.na(.), 0)


div %>% write_csv("_data/smy/sd_spec-div.csv")

  