##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Oct 30 2019
# Last modified: 
#
# Purpose: make abundance curve upon Matt's suggestion
#
# Inputs: rd_site-locs
#
# Outputs: 
#
# Notes:
#
#
####################################


library(tidyverse)


# weed results ------------------------------------------------------------

dat <- read_csv("_data/tidy/td-GHsum.csv")
dats <- read_csv("_data/tidy/td-GHspecies.csv")


# abundance curve (Matt suggestion) ---------------------------------------

rnks <- dats %>% 
  left_join(dat) %>% 
  mutate(p = nmbr.m2 / tot.m2) %>%
  select(loc_sys, cc_trt, rep, weed, p) %>% 
  #filter(loc_sys == "boyd_grain",
         #cc_trt == "no", 
  #       rep == 1) %>%
  group_by(loc_sys, cc_trt, rep) %>% 
  mutate(rank = rank(-p, ties.method = "random")) %>% 
  arrange(rank) %>% 
  mutate(psum = cumsum(p)) 

rnks %>% 
  select(loc_sys, cc_trt, rep) %>% 
  unique() %>% 
  mutate(weed = "dummy", p = 0, rank = 0, psum = 0) %>% 
  bind_rows(rnks) %>%
  group_by(loc_sys, cc_trt, rank) %>% 
  mutate(mpsum = mean(psum)) %>% 
  ggplot(aes(rank, mpsum)) + 
  geom_line(aes(color = cc_trt), size = 4, alpha = 0.5) +
  geom_line(aes(color = cc_trt), size = 1) +
  facet_grid(.~loc_sys, scales = "free") + 
  coord_cartesian(xlim = c(0, 6)) + 
  theme_bw()

ggsave("_figs/fig_abund-curve.png")



# pie chart? --------------------------------------------------------------

dats
