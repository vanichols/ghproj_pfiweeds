##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: May 16 2019
# Last modified: Dec 25 2019 (got rid of 'here', I use projects now)
#                            deleted 20190524 file, it was blank, and the 20190530 file had 5/24 data
#                            added 'no check' to check columns in all data sheets
#                            changed output to GHobs-raw, bc there is no trouble shooting here
#
# Purpose: Read in greenhouse observation sheets
#
# Inputs: 
#
# Outputs: 
#
# Notes:
#
#
####################################

rm(list = ls())

library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)

library(tidyr)
nest <- nest_legacy
unnest <- unnest_legacy


dat <- 
  tibble(files = list.files("_data/raw/data-entered/")) %>%
  mutate(path = paste0("_data/raw/data-entered/", files)) %>%
  filter(str_detect(files, "rd_GHobs")) %>%
  mutate(data = path %>% map(read_excel, skip = 1)) %>%
  select(data) %>%
  unnest() %>%
  remove_empty("rows") %>% 
  # remove unneeded cols
  select(-check, -tray, -obs_initials, -electrec_initials) %>%
  # make sure date is consistent
  mutate(obs_date = ymd(obs_date)) %>% 
  # drop down obs_date
  fill(obs_date) %>%
  # fix records
  mutate_if(is.character, tolower) %>% #--they aren't consistent w/capitalization
  mutate_if(is.character, str_replace_all, " ", "") %>%  #--sometimes they put spaces
  # get loc, crop_sys, cc_trt, rep
  mutate(rep = str_sub(cc_trt, -1),
         cc_trt = str_sub(cc_trt, 1, -2),
         crop_sys = case_when(str_detect(crop_trt2019, "sil") ~ "silage"),
         crop_sys = ifelse(is.na(crop_sys), "grain", crop_sys)) %>% 
  # funcke is spelled wrong
  mutate(location = recode(location, 
                           "funke" = "funcke")) %>% 
  rename("loc" = location)


write_csv(dat, "_data/tidy/td-GHobs-raw.csv")
