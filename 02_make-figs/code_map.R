##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 30 2019
# Last modified: Jan 8 2020 - Lydia trying to run things (see note)...
#                April 28 2020 - blowing things up and starting over....
#                june 3 2020 - change groupings based on Matt's feedback
#                june 9 2020 - add ccbio
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

mylegendtheme <- theme(legend.position = c(0.01, 0.99),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.2)),
                         axis.title = element_text(size = rel(1.3)),
                         strip.text = element_text(size = rel(1.3)),
                         legend.title = element_text(size = rel(1.3)))


p_green <- "#619B44"
p_blue <- "#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FFE100"
p_gray <- "#E7E6E6"

scales::show_col(p_yellow)



# prep work ---------------------------------------------------------------

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


# map ---------------------------------------------------------------------

fig_map <- 
  ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = map_county, aes(x = long, y = lat, group = group), 
               color = "gray80", fill = NA) +
  geom_point(data = locs, aes(lon, lat, fill = county), pch = 21, size = 7, stroke = 2) +
  geom_text(aes(x = -94.9, y = 41.65), size = 5, label = "West (grain)") +
  geom_text(aes(x = -92.5, y = 42.3), size = 5, label = "Central (grain, silage)") +
  geom_text(aes(x = -91.7, y = 41.70), size = 5, label = "East (grain)") +
  theme_minimal()  +
  guides(fill = F) +
  scale_fill_manual(values = c(p_pink, p_yellow, p_blue)) +
  mylegendtheme +
  myaxistexttheme +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  coord_quickmap() + 
  theme(panel.grid = element_blank())


fig_map


fig_ccbio <- 
  pfi_ccbio %>% 
  filter(year > 2009) %>% 
  group_by(site_name, sys_trt, year) %>% 
  summarise(mbio = mean(ccbio_Mgha)) %>% 
  ungroup() %>% 
  mutate(site_name = recode(site_name,
                            "Boyd" = "Central",
                            "Funcke" = "West",
                            "Stout" = "East"),
         site_name = factor(site_name, levels = c("West", "Central", "East")),
         sys_trt = str_to_title(sys_trt)) %>% 
  ggplot(aes(year, mbio, color = site_name, 
             group = interaction(site_name, sys_trt))) + 
  geom_line(aes(linetype = sys_trt), size = 2) + 
  scale_x_continuous(breaks = seq(2010, 2018, 2)) +
  scale_color_manual(values = c("Central" = p_pink, 
                                "West" = p_yellow, 
                                "East" = p_blue)) +
  guides(color = F, linetype = F) + 
  labs(linetype = "Cropping System",
       #color = "Site",
       #y = labccbio,
       y = expression(paste("Cover Crop Biomass (Mg  ", ha^-1, ")")),
       x = NULL) + 
  theme_bw() + 
  myaxistexttheme + 
  theme(axis.title.y = element_text(hjust = 0.5),
        legend.position = "bottom")

fig_ccbio

ggdraw(fig_ccbio + theme_half_open(12)) +
  draw_plot(fig_map, 0.05, 0.5, .5, .5) 

ggsave("02_make-figs/figs/fig_map-plus-ccbio-inset.png")





# old figs ----------------------------------------------------------------

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



# map combined with ccbio -------------------------------------------------


#labccbio = expression('Winter Rye Cover Crop Biomass (Mg ha'^"-1",")")
labccbio <- bquote("Winter Rye\nCover Crop Biomass (Mg "~ha^-1~")")

















fig_map2 <- 
  ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = map_county, aes(x = long, y = lat, group = group), 
               color = "gray80", fill = NA) +
  geom_polygon(data = map_county3, 
               aes(x = long, y = lat, group = group, fill = subregion), 
               color = "gray80", #fill = p_pink, 
               size = 2) +
  geom_text(aes(x = -95.5, y = 42.05), size = 5, label = "West") +
  geom_text(aes(x = -93., y = 42.05), size = 5, label = "Central") +
  geom_text(aes(x = -91.7, y = 41.70), size = 5, label = "East") +
  theme_minimal()  +
  guides(fill = F) +
  scale_fill_manual(values = c(p_pink, p_yellow, p_blue)) +
  mylegendtheme +
  myaxistexttheme +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  coord_quickmap() + 
  theme(panel.grid = element_blank())


fig_map2


fig_ccbio <- 
  pfi_ccbio %>% 
  filter(year > 2009) %>% 
  group_by(site_name, sys_trt, year) %>% 
  summarise(mbio = mean(ccbio_Mgha)) %>% 
  ungroup() %>% 
  mutate(site_name = recode(site_name,
                          "Boyd" = "Central",
                          "Funcke" = "West",
                          "Stout" = "East"),
         site_name = factor(site_name, levels = c("West", "Central", "East")),
         sys_trt = str_to_title(sys_trt)) %>% 
  ggplot(aes(year, mbio, color = site_name, 
             group = interaction(site_name, sys_trt))) + 
  geom_line(aes(linetype = sys_trt), size = 2) + 
  scale_x_continuous(breaks = seq(2010, 2018, 2)) +
  scale_color_manual(values = c("Central" = p_pink, 
                               "West" = p_yellow, 
                               "East" = p_blue)) +
  guides(color = F) + 
  labs(linetype = "Cropping System",
       #color = "Site",
       #y = labccbio,
       y = expression(paste("Cover Crop Biomass (Mg  ", ha^-1, ")")),
       x = NULL) + 
  theme_bw() + 
  mylegendtheme + 
  myaxistexttheme + 
  theme(axis.title.y = element_text(hjust = 0.5))

fig_ccbio

ggsave("02_make-figs/figs/fig_ccbio.png", height = 4, width = 4.5)


library(patchwork)

# horizontal
fig_map2 + fig_ccbio + plot_layout(ncol = 2, widths = c(1.5, 1))
ggsave("02_make-figs/figs/fig_map-plus-ccbio.png")

# vertical
fig_map2 / fig_ccbio + plot_layout(nrow = 2, heights = c(1, 1.5), widths = 1)

ggsave("02_make-figs/figs/fig_map-plus-ccbio.png")

library(cowplot)

