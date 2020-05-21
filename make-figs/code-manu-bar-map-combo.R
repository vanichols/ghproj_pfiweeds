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
# Notes: 
#
####################################

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

library(patchwork)
library(maps)
library(PFIweeds2020)


# map ---------------------------------------------------------------------

mylegendtheme <- theme(legend.position = c(0.1, 0.9),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)))


map_iowa <- as_tibble(map_data('state')) %>%
  filter(region == "iowa")

locs <- pfi_siteinfo %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = F) %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2",
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
               color = "gray80", fill = "gold", size = 2) +
  
  geom_text(aes(x = -95, y = 42.05), size = 5, label = "West") +
  geom_text(aes(x = -93.3, y = 42.05), size = 5, label = "Central") +
  geom_text(aes(x = -91.7, y = 41.60), size = 5, label = "East") +
  theme_minimal()  +
  mylegendtheme +
  myaxistexttheme +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  coord_quickmap() + 
  theme(panel.grid = element_blank())


fig_map
ggsave("make-figs/fig1_map.png")


# bar graph ---------------------------------------------------------------


cctrtpal <- c("darkolivegreen3", "lightsalmon4")

labseedsm2 = expression('Weed Seeds\n (1000s m'^"-2)")
labseedsm2 <- bquote("Weed Seeds (1000s"~m^-2~")")


sb_est <- read_csv("01_stats-uni/st_estimates.csv") %>% 
  filter(model == "pois")

fig_sb <- 
  sb_est %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"),
         cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Rye Cover Crop")) %>%  
  ggplot(aes(reorder(site_id, -totseeds_m2, mean), totseeds_m2/1000, fill = cc_trt)) +
  geom_col(position = position_dodge(width = 0.9), 
           color = "black", size = 1.2) +
  geom_linerange(position = position_dodge(width = 0.9),
                 aes(ymin = se_lo/1000, ymax = se_hi/1000)) +
    labs(y = labseedsm2,
         x = NULL,
         fill = NULL, 
         color = NULL) +
    scale_fill_manual(values = c("None" = cctrtpal[2],
                                 "Rye Cover Crop" = cctrtpal[1])) +
    scale_color_manual(values = cctrtpal) +
    theme_bw() +
  myaxistexttheme +
    theme(#legend.direction = "horizontal",
          #legend.position = "bottom",
          legend.justification = c(1, 1),
          legend.position = c(0.9, 0.9),
          legend.background = element_blank(),
          legend.key.width = unit(1.4, "cm"),
          legend.key.height = unit(0.5, "cm"),
          legend.key.size = unit(1, "cm"),
          #axis.title.y = element_text(angle = 0, 
          #                            vjust = 0.5, 
          #                            hjust = -1)
          )
  
fig_sb
ggsave("make-figs/figs/fig1_bar-totseeds.png")

# put together ------------------------------------------------------------

#--total hack
library(gridExtra)
fig_sb + (fig_map /gridExtra::tableGrob(mtcars[1:10, c('mpg', 'disp')]))

ggsave("make-figs/figs/fig1_bar-map.png")
