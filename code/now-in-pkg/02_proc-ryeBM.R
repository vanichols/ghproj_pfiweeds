##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: May 16 2019
# Last modified: Aug 22 2019
#                Oct 2 2019 (updated w/cleaned pfi data, made fig)
#                Oct 22 2019 (made dataset w/0s changed to small number for Shannon's Div calcs)
#                Oct 29 2019 (wrote a Shannon's div file in smy)
#                Dec 30 2019 (write summary of all metrics)
#
# Purpose: Read in rye biomass measurements
#
# Inputs: td-boyd-ryebm2008-2019, td-pfi-ryebm2009-2019
#
# Outputs: td-all-ryebm2008-2019, 
#          td-all-ryebm2008-2019-no0s
#          sd_shannons-ccbio
#          sd_ccbio-metrics
# Notes:
#
#
####################################


library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)

# Boyd --------------------------------------------------------------------

boyd <- read_csv("_data/tidy/td-boyd-ryebm2008-2019.csv")

# STOUT AND FUNCKE --------------------------------------------------------

pfi <- read_csv("_data/tidy/td-pfi-ryebm2009-2019.csv")

# Combine-----------------------------------------

# make boyd match fs
boyd2 <- 
  boyd %>%
  mutate(cash_crop = "both")

# make fs match boyd cols
pfi2 <- 
  pfi %>%
  rename(year = crop_year) %>%
  mutate(crop_sys = "grain") %>%
  group_by(year, location, cc_trt, crop_sys) %>%
  summarise(ccbio_Mgha = mean(ccbio_Mgha, na.rm = T))

# combine them

cc <- 
  boyd2 %>% 
  bind_rows(pfi2) %>%
  group_by(location, cc_trt, crop_sys, year) %>%
  summarise(ccbio_Mgha = mean(ccbio_Mgha, na.rm = T)) %>%
  ungroup() %>%
  mutate(location = tolower(location)) %>%
  select(year, location, crop_sys, cc_trt, ccbio_Mgha) %>%
  arrange(year, location, crop_sys, cc_trt)


cc %>% write_csv("_data/tidy/td-all-ryebm2008-2019.csv")



# make a dataset w/no 0s --------------------------------------------------

fakemin <- 0.1*(cc %>% 
  filter(ccbio_Mgha != 0) %>%
  pull(ccbio_Mgha) %>% 
  min())

ccno0 <- cc %>% 
  mutate(ccbio_Mgha = ccbio_Mgha + fakemin)
#ifelse(ccbio_Mgha == 0, fakemin, ccbio_Mgha))

ccno0 %>% write_csv("_data/tidy/td-all-ryebm2008-2019-no0s.csv")

# visualize to check --------------------------------------------------------------


cc %>%
  unite(location, crop_sys, col = "locsys", remove = F) %>%
  ggplot(aes(year, ccbio_Mgha, group = locsys)) + 
  geom_point(aes(color = location), size = 3) + 
  geom_line(aes(color = location, linetype = crop_sys), size = 3) 

ggsave("_figs/QC-figs/fig_ccbio.png")


# calculate an 'effective' cc biomass -------------------------------------
# aka Shannon's Diversity index
# http://www.tiem.utk.edu/~gross/bioed/bealsmodules/shannonDI.html

shan <- 
  ccno0 %>% 
  filter(year > 2009) %>% 
  group_by(location, crop_sys) %>% 
  mutate(tot = sum(ccbio_Mgha)) %>% 
  ungroup() %>% 
  mutate(p = ccbio_Mgha/tot,
         lnp = log(p),
         plnp = p * lnp) %>% 
  group_by(location, crop_sys) %>% 
  summarise(shan = sum(plnp)*-1) %>% 
  mutate(shan_hill = exp(shan)) #%>% # this creates "effective" biomass
  

shan %>% write_csv("_data/smy/sd_shannons-ccbio.csv")


# combine all summaries ---------------------------------------------------

cc_sum_stats <- 
  cc %>%
  group_by(location, crop_sys, cc_trt) %>%
  mutate(above1 = case_when(
    ccbio_Mgha > 1 ~ 1,
    TRUE ~ 0),
    above2 = case_when(
      ccbio_Mgha > 2 ~ 1,
      TRUE ~ 0)
  ) %>%
  summarise(ccbio_mean = mean(ccbio_Mgha,   na.rm = TRUE),
            ccbio_med  = median(ccbio_Mgha, na.rm = TRUE),
            ccbio_var  = sd(ccbio_Mgha,     na.rm = TRUE),
            ccbio_cv   = ccbio_var/ccbio_mean,
            ccbio_max  = max(ccbio_Mgha, na.rm = TRUE),
            nabove1 = sum(above1),
            nabove2 = sum(above2)) %>%
  ungroup() %>% 
  select(-cc_trt) %>% 
  left_join(shans)

cc_sum_stats %>% 
  write_csv("_data/smy/sd_ccbio-metrics.csv")
