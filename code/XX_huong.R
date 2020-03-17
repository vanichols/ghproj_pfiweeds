##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 2 2019
# Purpose: look at huong's data
# Notes:
#
#
####################################

rm(list = ls())
library(tidyverse)
library(readxl)
library(earth)

# read in data ------------------------------------------------------------


hu <- read_excel("_data/raw/huong_waterhemp-seeds.xlsx")

hu %>% 
  filter(Biomass < 500) %>% 
  ggplot(aes(Biomass, Seed)) + 
  geom_point()


mars1 <- earth(
  Sale_Price ~ .,  
  data = ames_train   
)

mars1 <- earth(Seed ~ Biomass, data = hu)
summary(mars1)
 # knots at 282.5 and 465.2

humrs <- hu %>% 
  mutate(grp = ifelse(Biomass < 282.5, "A", 
                      ifelse(Biomass > 465.2, "C", "B")))

humrs %>% 
  ggplot(aes(Biomass, Seed)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_grid(.~grp, scales = "free")

humrs %>% 
  filter(grp == "A") %>% 
  filter(Biomass <0.5) %>% 
  ggplot(aes(Biomass, Seed)) + 
  geom_point()
  

# restric to grp A
humrs %>% 
  filter(grp == "A") %>% 
  lm(Seed ~ 0 + Biomass, data = .)

# You gain about 1000 seeds per gram of biomass
