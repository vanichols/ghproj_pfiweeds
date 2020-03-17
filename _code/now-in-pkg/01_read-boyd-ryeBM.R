##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: May 16 2019
# Last modified: Aug 22 2019
#
# Purpose: Read in rye biomass measurements
#
# Inputs: rd_boyd-trt-key
#         rd_boyd-plot-key
#         rd_kaspar-boyd-ccbio
#         Micki_Boyd42_rye-biomass
#         Micki_Boyd44_rye-biomass
#
#         rd_funcke-stout-ccbio
#
#
#
# Outputs: td-boyd-ryebm2019
#
# Notes:
#
#
####################################


library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(janitor)
setwd(here())


# BOYD --------------------------------------------------------------------

# Keys --------------------------------------------------------------------

trtkey <- read_excel("_data/raw/data-entered/rd_boyd-trt-key.xlsx", na = "NA") %>%
  mutate(treatment = as.character(treatment))
plotkey <- read_excel("_data/raw/data-entered/rd_boyd-plot-key.xlsx", na = "NA") 

key <- plotkey %>% left_join(trtkey)



# Kaspar data -------------------------------------------------------------

boydkaspraw <- read_excel("_data/raw/data-entered/rd_kaspar-boyd-ccbio.xlsx")

boydkasp <- 
  boydkaspraw %>%
  fill(length_cm:date) %>%
  mutate(year = year(date),
         area_cm2 = length_cm * width_cm) %>%
  group_by(trt_nu, year) %>%
    summarise(ccbio_g = mean(biomass_g, na.rm = T),
              area_cm2 = mean(area_cm2)) %>%
    ungroup() %>%
    mutate(ccbio_gcm2 = ccbio_g / area_cm2,
           ccbio_Mgha = ccbio_gcm2*100,
           trt_nu = as.character(trt_nu)) %>%
    select(year, trt_nu, ccbio_Mgha) %>%
    rename(treatment = trt_nu) %>%
  left_join(trtkey) %>%
    filter(!is.na(cc_trt))
    

# Our 2019 data --------------------------------------------------------------------

boyd42 <- read_excel("_data/raw/data-students/Micki_Boyd42_rye-biomass.xlsx")
boyd44 <- read_excel("_data/raw/data-students/Micki_Boyd44_rye-biomass.xlsx") 

boyd19raw <- bind_rows(boyd42, boyd44) %>%
  janitor::clean_names() %>%
  mutate(site = paste0("Boyd", site),
         treatment = as.character(treatment)) %>%
  left_join(key) %>%
  # 32"x12" quadrat, change to ha, change to Mg
  mutate(ccbio_Mgha = weight_g * (1/(32*12)) * 1.55*10^7 * (1/10^6),
         crop_year = 2019) %>%
  select(crop_year, site, location, treatment, crop_sys, cc_trt, plot, rep, sample, weight_g, ccbio_Mgha) %>%
  arrange(crop_year, site, location, treatment, crop_sys, cc_trt, plot, rep, sample)

#write_csv(boyd19raw, "_data/tidy/td-boyd-ryebm2019.csv")

#--average by treatment
boyd19 <- 
  boyd19raw %>%
  rename(year = crop_year) %>%
  group_by(year, treatment, location, cc_trt, crop_sys) %>%
  summarise(ccbio_Mgha = mean(ccbio_Mgha, na.rm = T))


# Combine MY msmts w/Keiths -----------------------------------------------


allboyd <- 
  boyd19 %>%
  bind_rows(boydkasp)

write_csv(allboyd, "_data/tidy/td-boyd-ryebm2008-2019.csv")

