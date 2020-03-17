##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: May 16 2019
# Last modified: Aug 22 2019
#                Aug 27 2019
#                Oct 22 2019 (updated SOLPT and CONCA weed codes)
#                Oct 30 2019 (made funke into funcke, it was spelled wrong)
#                Dec 25 2019 (looking at the outlier in funcke)
#
# Purpose: process GH data, get totals and totals by weed species
#
# Inputs: td_GHobs
#
# Outputs: td_GHsum, td_GHspecies
#
# Notes:
#
#
####################################


library(tidyverse)
library(janitor)

source("_code/01_read-GHobs.R")
raw <- read_csv("_data/tidy/td-GHobs-raw.csv")


# process data ------------------------------------------------------------

# look at data, funcke rep 4 is very high
raw  %>% 
  filter(loc == "funcke") %>% 
  mutate_if(is.numeric, replace_na, 0) %>%
  group_by(cc_trt, obs_date, rep) %>%
  summarise_if(is.numeric, sum) %>%
  gather(-(cc_trt:rep), key = "weed", value = "nmbr") %>%
  # 2.8575 cm radius cores, 20 cores per 'rep'
  mutate(nmbr.m2 = nmbr / ( ( (pi * 2.8575^2) * 20 ) / 10000 )) %>%
  # fix mislabeled weed abbs
  mutate(weed = recode(weed,
                       "SOPT7" = "SOLPT",
                       "HPPVU" = "CONCA")) %>% 
  ungroup() %>% 
  ggplot(aes(obs_date, nmbr)) +
  geom_point(aes(color = weed)) + 
  facet_grid(cc_trt~rep)

# look at data, funcke rep 4 is very high
raw  %>% 
  filter(loc == "funcke") %>% 
  mutate_if(is.numeric, replace_na, 0) %>%
  group_by(cc_trt, obs_date, rep) %>%
  summarise_if(is.numeric, sum) %>%
  gather(-(cc_trt:rep), key = "weed", value = "nmbr") %>%
  # 2.8575 cm radius cores, 20 cores per 'rep'
  mutate(nmbr.m2 = nmbr / ( ( (pi * 2.8575^2) * 20 ) / 10000 )) %>%
  # fix mislabeled weed abbs
  mutate(weed = recode(weed,
                       "SOPT7" = "SOLPT",
                       "HPPVU" = "CONCA")) %>% 
  ungroup() %>% 
  filter(rep == 4, cc_trt == "rye") %>% 
  ggplot(aes(obs_date, nmbr)) +
  geom_point(aes(color = weed)) + 
  facet_grid(cc_trt~rep)

# I looked through the scanned sheets. I think it's real. 

#--sum ind species
datsp <- 
  raw %>%
  # sum individual weeds
  mutate_if(is.numeric, replace_na, 0) %>%
  group_by(loc, crop_sys, cc_trt, rep) %>%
  summarise_if(is.numeric, sum) %>%
  gather(-(loc:rep), key = "weed", value = "seeds") %>%
  # 2.8575 cm radius cores, 20 cores per 'rep'
  mutate(seeds_m2 = seeds / ( ( (pi * 2.8575^2) * 20 ) / 10000 )) %>%
  # fix mislabeled weed abbs (they were mislabeled consistently at least)
  mutate(weed = recode(weed,
                       "SOPT7" = "SOLPT",
                       "HPPVU" = "CONCA")) %>% 
  ungroup() %>%
  select(-seeds) %>% 
  rename("cropsys" = "crop_sys")

#--sum across species
datsum <- 
  datsp %>% 
  group_by(loc, cropsys, cc_trt, rep) %>% 
  summarise(totseeds_m2 = sum(seeds_m2, na.rm = T))

# look at it --------------------------------------------------------------

# that one stinker in funcke, grr
datsum %>%    
  ggplot(aes(cc_trt, totseeds_m2)) + 
  geom_point(size = 3, fill = "red", pch = 19) +
  facet_grid(cropsys~loc) + 
  labs(x = "Cover Crop Treatment", y = "Total Weed Seeds per m2") + 
  theme_bw()

# unhelpful bar chart
datsp %>%    
  filter(weed != "AMATU") %>%
  ggplot(aes(rep, seeds_m2)) + 
  geom_col(aes(fill = weed)) +
  facet_grid(cc_trt~loc) + 
  theme_bw()


# write it ----------------------------------------------------------------

write_csv(datsum, "_data/tidy/td-GHsum.csv")
write_csv(datsp, "_data/tidy/td-GHspecies.csv")
