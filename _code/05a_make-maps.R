##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Oct 23 2019
# Last modified: 
#
# Purpose: make figs
#
# Inputs: rd_site-locs
#
# Outputs: 
#
# Notes:
#
#
####################################


library(tidyverse)
library(ggpubr)
library(ggthemes)
library(wesanderson)
library(patchwork)
library(maps)

# Map of locs ----------------------------------------------------------

#--get that data gurrrrrl

locs <- read_excel("_data/raw/rd_site-locs.xlsx") %>% 
  mutate(system = str_to_title(system)) %>% 
  unite(coop_name, system, col = "loc_sys", remove = F)

map_all <- as_tibble(map_data('state'))

map_iowa <- map_all %>%
  filter(region == "iowa")

mypal <- c("royalblue", (wes_palette("Zissou1", n = 5)[c(2,3,5)]))[c(4, 1, 2, 3)]


#--make that plot gurrrrrl

ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = "darkolivegreen1", color = "white") +
   geom_jitter(data = locs, width = 0.1, color = "black", size = 7,
              aes(x = lon, y = lat,
                  fill = loc_sys,
                  pch = system)) +
  labs(fill = NULL, pch = NULL) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = mypal) + 
                      #c("forestgreen", "darkolivegreen1")) + 
  guides(fill = F, pch = F) +
  theme_map() + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.2, 0.85),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = rel(1.2)),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave("_figs/fig_site-locs.png")


