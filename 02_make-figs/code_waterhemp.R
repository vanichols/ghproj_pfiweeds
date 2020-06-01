##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 6/1/2020
#
# Purpose: look at waterhemp biomass versus seed prod
#
# Last modified:
#
# Notes:
####################################

rm(list = ls())
library(tidyverse)
library(readxl)

# data --------------------------------------------------------------------

wh <- read_excel("02_make-figs/huong_waterhemp-seeds.xlsx")


wh %>% 
  ggplot(aes(Biomass, Seed)) + 
  geom_point() + 
  scale_y_log10() + 
  scale_x_log10()
