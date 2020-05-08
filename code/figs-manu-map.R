##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 30 2019
# Last modified: Jan 8 2020 - Lydia trying to run things (see note)...
#                April 28 2020 - blowing things up and starting over....
#
# Purpose: make manuscript figs
#
# Inputs: 
#
# Outputs: 
#
#
####################################

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(ggpubr)
library(wesanderson)
library(patchwork)
library(maps)
library(PFIweeds2020)

pfi_siteinfo

# fig things --------------------------------------------------------------

mytheme <- theme(legend.position = c(0.1, 0.9),
                 legend.justification = c(0,1),
                 legend.background = element_rect(color = "black"),
                 axis.text = element_text(size = rel(1.2)),
                 legend.text = element_text(size = rel(1.3)),
                 axis.title = element_text(size = rel(1.3)))


# map of locs ----------------------------------------------------------

map_iowa <- as_tibble(map_data('state')) %>%
  filter(region == "iowa")

locs <- pfi_siteinfo %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = F) %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"))


set.seed(94)

map_iowa <- as_tibble(map_data('state')) %>% 
  filter(region == "iowa")

map_county <- as_tibble(map_data('county')) %>% 
  filter(region == "iowa") 

map_county3 <- map_county %>% filter(subregion %in% c("boone", "greene", "washington"))

#--make a map

ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = map_county, aes(x = long, y = lat, group = group), 
               color = "gray70", fill = NA) +
  geom_polygon(data = map_county3, aes(x = long, y = lat, group = group), 
               color = "gray70", fill = "gold", size = 3) +
  
  geom_text(aes(x = -94.65, y = 42.05), size = 7, label = "West") +
  geom_text(aes(x = -93.7, y = 42.05), size = 7, label = "Central") +
  geom_text(aes(x = -91.7, y = 41.35), size = 7, label = "East") +
  theme_minimal()  +
  mytheme + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  coord_quickmap()

ggsave("figs/manu/fig_locs-map.png")

