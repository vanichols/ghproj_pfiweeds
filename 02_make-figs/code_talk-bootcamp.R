##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Jan 21 2019
# Last modified: 12/1/2020
#
# Purpose: make figs for PFI CC bootcamp
#
# Inputs: 
#
# Outputs: 
#
# Notes:
#
#
####################################

library(devtools)
library(tidyverse)
library(readxl)
library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(broom)
library(ggpubr)
library(wesanderson)
library(patchwork)
library(maps)
#devtools::install_github("vanichols/PFIweeds2020")
library(PFIweeds2020)



# data --------------------------------------------------------------------

#--counts by spec 
wdcnts <- 
  pfi_ghobsraw %>% pfifun_sum_weedbyeu() %>% 
  filter(seeds < 800) %>% #--funcke stinker
  group_by(cc_trt, weed) %>% 
  summarise(seeds = sum(seeds, na.rm = T)) %>% 
  mutate(weedcat = "other",
         weedcat = case_when(
           str_detect(weed, "AMATU") ~ "Waterhemp",
           str_detect(weed, "CHEAL") ~ "Lambsquarters",
           str_detect(weed, "POROL") ~ "Purslane",
           str_detect(weed, "DIGSA") ~ "Hairy crabgrass",
           TRUE ~ "other"
         ),
         weedcat = factor(weedcat, 
                          levels = c("Waterhemp", "Lambsquarters", 
                                     "Purslane", "Hairy crabgrass", "other")),
         cc_trt = recode(cc_trt,
                         no = "None" ,
                         rye = "Rye Cover Crop"))

#--counts by spec 
wdcnts_loc <- 
  pfi_ghobsraw %>% 
  pfifun_sum_weedbyeu() %>% 
  filter(seeds < 800) %>% #--funcke stinker
  left_join(pfi_siteinfo) %>%
  mutate(sys_trt = str_to_title(sys_trt)) %>% 
  unite(county, sys_trt, col = "county_sys", sep = " ") %>% 
  group_by(county_sys, cc_trt, rep) %>% 
  summarise(totseeds_m2 = sum(seeds_m2, na.rm = T)) %>% 
  mutate(totseeds_ft2 = totseeds_m2/10.764) %>% 
  mutate(
    cc_trt = recode(cc_trt, 
                       "no" = "None",
                       "rye" = "Rye Cover Crop"),
    county_sys = factor(county_sys, 
                             levels = c("Greene Grain", 
                                        "Boone Silage", 
                                        "Washington Grain", 
                                        "Boone Grain")))
  



# fig things --------------------------------------------------------------
cctrtpal <- c("#69431D", "#45AD45")

mytheme <- theme(legend.position = c(0.1, 0.9),
                 legend.justification = c(0,1),
                 legend.background = element_rect(color = "black"),
                 axis.text = element_text(size = rel(1.2)),
                 legend.text = element_text(size = rel(1.3)),
                 axis.title = element_text(size = rel(1.3)))
                 
Mgha_to_tonac <- (1000) * (2.2) * (1/2000) * (1/2.47) #--essentially half it

labseedsm2 = expression('Weed Seeds m'^"-2")
labseedsft2 = expression('Weed Seeds ft'^"-2")




# weeds found -------------------------------------------------------------

wdcnts %>% 
  ggplot(aes(weedcat, seeds, fill = cc_trt)) + 
  geom_col(position = position_dodge(width = 0.75), color = "black", size = 2) +
  labs(y = "Number of\nweed seeds\nfound",
       x = NULL,
       fill = NULL, 
       color = NULL) +
  scale_fill_manual(values = cctrtpal) +
  theme_bw() +
  mytheme + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        panel.background = element_rect(fill = "gray90"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

ggsave("02_make-figs/figs/talk_weedseeds-found.png")


wdcnts_loc %>% 
  ggplot(aes(county_sys, totseeds_ft2, fill = cc_trt)) + 
  geom_hline(yintercept = 5000/10.764, color = "red", linetype = "dashed") +
  stat_summary(fun = mean, geom = "bar", 
               position = position_dodge(width = 0.75), color = "black", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.75), width = 0.2) + 
  labs(y = labseedsft2,
       x = NULL,
       fill = NULL, 
       color = NULL) +
  scale_fill_manual(values = cctrtpal) +
  theme_bw() +
  mytheme + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        panel.background = element_rect(fill = "gray90"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

ggsave("02_make-figs/figs/talk_by-loc.png")

# map ---------------------------------------------------------------------
wes <- wes_palette("Zissou1")
wes2 <- wes_palette("Rushmore1")
wes3a <- wes_palette("Moonrise3")
wes3 <- wes_palette("Moonrise2")


map_iowa <- as_tibble(map_data('state')) %>% 
  filter(region == "iowa")

map_county <- as_tibble(map_data('county')) %>% 
  filter(region == "iowa") 

map_county3 <- map_county %>% filter(subregion %in% c("boone", "greene", "washington"))

pfi_siteinfo

library(ggthemes)
set.seed(3)
ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = wes[2]) +
  geom_polygon(data = map_county, aes(x = long, y = lat, group = group), 
               color = "gray80", fill = NA) +
  geom_polygon(data = map_county3, aes(x = long, y = lat, group = group), 
               color = wes[4], fill = wes[3], size = 3) +
  geom_jitter(data = pfi_siteinfo %>% rename("long" = "lon"),
             aes(x = long, y = lat), size = 4,
             fill = wes[5], width = 0.07, pch = 21) +
  geom_text(aes(x = -94.85, y = 42.105), size = 7, label = "Greene") +
  geom_text(aes(x = -93.5, y = 42.205), size = 7, label = "Boone") +
  geom_text(aes(x = -91.7, y = 41.15), size = 7, label = "Washington") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  coord_quickmap() + 
  theme_map()

ggsave("02_make-figs/figs/talk_map.png")
