##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: March 4 2019
# Last modified: 
#
# Purpose: Create list of EUs to make tray labels
#
# Inputs: _docs/PFIweeds_trtIDs.xlsx, euID-forR sheet
#
# Outputs: _data/tmpd_euIDs.csv
#
# Notes:
#
#
####################################


library(tidyverse)
library(readxl)
library(here)
setwd(here())



# Data --------------------------------------------------------------------

raw <- read_excel("_docs/PFIweeds_trtIDs.xlsx", sheet = "euID-forR", skip = 3)


# Wrangle -----------------------------------------------------------------

raw %>%
  fill(site_id:Keith_trt) %>%
  mutate(euID = paste0(site_id, sys_trt, cc_trt, "-", rep)) %>%
  select(-Keith_trt) %>%
  write_csv("_data/PFIweeds_templates/tmpd_euIDs.csv") 
