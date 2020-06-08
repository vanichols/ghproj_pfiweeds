##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 30 2019
# Last modified: Jan 8 2020 - Lydia trying to run things (see note)...
#                April 28 2020 - blowing things up and starting over....
#                june 3 2020 - change groupings based on Matt's feedback
#
# Purpose: make manuscript figs
#
# Inputs: 
#
# Outputs: 
#
# Notes: 
#
####################################

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

library(patchwork)
library(maps)
library(PFIweeds2020)



# constant themes ---------------------------------------------------------

mylegendtheme <- theme(legend.position = c(0.1, 0.9),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)),
                         strip.text = element_text(size = rel(1.3)))


p_green <- "#619B44"
p_blue <- "#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FFC000"
p_gray <- "#E7E6E6"

scales::show_col(pptgreen)


# map ---------------------------------------------------------------------


map_iowa <- as_tibble(map_data('state')) %>%
  filter(region == "iowa")

locs <- pfi_siteinfo %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = F) %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2\n(Silage)",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"))


map_iowa <- as_tibble(map_data('state')) %>% 
  filter(region == "iowa")

map_county <- as_tibble(map_data('county')) %>% 
  filter(region == "iowa") 

map_county3 <- map_county %>% filter(subregion %in% c("boone", "greene", "washington"))

fig_map <- 
  ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = map_county, aes(x = long, y = lat, group = group), 
               color = "gray80", fill = NA) +
  geom_polygon(data = map_county3, aes(x = long, y = lat, group = group), 
               color = "gray80", fill = p_pink, size = 2) +
  
  geom_text(aes(x = -95.5, y = 42.05), size = 5, label = "West") +
  geom_text(aes(x = -93., y = 42.05), size = 5, label = "Central") +
  geom_text(aes(x = -91.7, y = 41.70), size = 5, label = "East") +
  theme_minimal()  +
  mylegendtheme +
  myaxistexttheme +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  coord_quickmap() + 
  theme(panel.grid = element_blank())


fig_map

ggsave("02_make-figs/figs/fig_map.png", height = 3, width = 3.5)

