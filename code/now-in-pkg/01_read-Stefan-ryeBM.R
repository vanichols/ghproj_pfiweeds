##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: May 16 2019
# Last modified: Aug 22 2019
#                Aug 28 (separated boyd and stefan data wrangling)
#                Oct 2 manually fixed pfi data, adjusted code, calculated term-plant interval
#                Oct 25 mannually fixed things have disappeared for some reason....re-added them
#                  no term-plant interval
#
# Purpose: Read in rye biomass measurements
#
# Inputs: Stefan_onlyStoutFuncke-covercropbiomass-GNmodified
#
#
#
# Outputs: td-pfi-ryebm2009-2019
#
# Notes:
#
#
####################################


library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)

# STOUT AND FUNCKE --------------------------------------------------------

fsraw <- read_excel("_data/raw/Stefan_StoutFuncke-covercropbiomass-GNmod.xlsx", na = "NA") %>%
  janitor::clean_names()  %>%
  # get only what I want
  select(location, cooperator, trt, rep, crop_year, cash_crop, 
         spring_cc_biomass_lbs_a,
         #spring_cc_sample_date, 
         #cc_planting_date, 
         #cc_kill_date, 
         #cash_crop_planting_date,
         ) %>%
  # rename things to match my conventions
  rename(site = location,
         location = cooperator,
         cc_trt = trt) %>%
  # call it ccrye so it comes first alphabeticaclly (for arranging)
  mutate(cc_trt = recode(cc_trt,
                         Cover = "ccrye",
                         `No cover` = "no")) %>%
  filter(!is.na(rep)) %>%
  arrange(site, location, cc_trt, rep, crop_year) %>%
  # fill things (?)
 # group_by(site, location, crop_year, cash_crop) %>%
  #do(fill(., spring_cc_sample_date)) %>%
  #fill(spring_cc_sample_date:cash_crop_planting_date) %>%
  #ungroup() %>%
  # rename rye now that things are sorted
  mutate(cc_trt = recode(cc_trt,
                         ccrye = "rye")) %>%
  mutate_if(is.character, tolower)
  

# should deal with things by trt
#
fsrawrye <- fsraw %>% filter(cc_trt == "rye")  
fsrawno <- fsraw %>% filter(cc_trt == "no")  


# rye data ----------------------------------------------------------------

# why are there some with duplicates? ex Stout rep 1 crop_year 2010
# why do some 'no' have cc biomass?

# I manually fixed everything. The data was just a mess, period. Reps were off, 0s vs NAs, etc

fstidy <- 
  fsrawrye %>%
  
  # make it in Mg/ha
  mutate(ccbio_Mgha = spring_cc_biomass_lbs_a * (1/0.892) * (1/1000)) %>% 
  
  select(site, location, crop_year, cc_trt, everything(), -spring_cc_biomass_lbs_a) %>%
  
  # I know Stout 2012 cash_crop planting date should read 5/21/2012
  #mutate(cash_crop_planting_date = ifelse(
  #  (location == "stout" & crop_year == 2012),
  #  "5/21/2012",
  #  cash_crop_planting_date
  #)) %>%
  
  # mutate_at(.vars = vars(spring_cc_sample_date, cc_planting_date, cc_kill_date, cash_crop_planting_date),
  #           .funs = mdy) %>%
  # 
  # mutate(kpint_days = time_length(interval(ymd(cc_kill_date), ymd(cash_crop_planting_date)), "day"),
  #        kpint_days = ifelse(kpint_days > 60, NA, kpint_days)) %>%

arrange(site, location, crop_year, cc_trt, rep)
  
fstidy

write_csv(fstidy, "_data/tidy/td-pfi-ryebm2009-2019.csv")
