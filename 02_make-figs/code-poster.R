##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Jan 21 2019
# Last modified: 
#
# Purpose: make figs for PFI farminar
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
library(here)

setwd(here::here())
setwd("../Box/1_Gina_Projects/proj_PFIweeds/")
getwd()
#devtools::install_github("vanichols/weedseedspfi2020")


# fig things --------------------------------------------------------------

pptgreen <- "#619B44"
pptblue <- "#46B2B5"
pptpink <- "#DC1A64"
pptorange <- "#FFA726"
pptyellow <- "#FFC000"
pptgray <- "#E7E6E6"

mypietheme <- theme(strip.background = element_rect(fill = "white", color = "black"),
                    strip.text = element_text(size = rel(1.3)),
                    legend.text = element_text(size = rel(1.3)))

mytheme <- theme(legend.position = c(0.1, 0.9),
                 legend.justification = c(0,1),
                 legend.background = element_rect(color = "black"),
                 axis.text = element_text(size = rel(1.2)),
                 legend.text = element_text(size = rel(1.3)),
                 axis.title = element_text(size = rel(1.3)))

Mgha_to_tonac <- (1000) * (2.2) * (1/2000) * (1/2.47) #--essentially half it

labseedsm2 = expression('Weed Seeds m'^"-2")
labseedsft2 = expression('Weed Seeds ft'^"-2")


# data --------------------------------------------------------------------

#--site lat/lons
locs <- read_excel("_data/raw/rd_site-locs.xlsx") %>% 
  mutate(system = str_to_title(system))

#--raw data
rawdat <- read_csv("_data/tidy/td-GHsum.csv") %>% 
  mutate(county = case_when(
    loc == "boyd" ~ "Boone",
    loc == "funcke" ~ "Greene",
    loc == "stout" ~ "Washington"),
    cropsys = str_to_title(cropsys)) %>% 
  unite(county, cropsys, sep = " ", col = "county_sys", remove = F)

#--raw data 2
rawdat2 <- 
  read_csv("_data/tidy/package_data_sum_eu.csv") %>% 
  mutate(county = case_when(
    grepl("Boyd", site_name) ~ "Boone",
    site_name == "Funcke" ~ "Greene",
    site_name == "Stout" ~ "Washington"),
    sys_trt = str_to_title(sys_trt)) %>% 
  unite(county, sys_trt, sep = " ", col = "county_sys", remove = F)

#--cover crop biomass
cc <- read_csv("_data/tidy/td-all-ryebm2008-2019.csv") %>% 
  filter(year > 2009) %>% 
  unite(location, crop_sys, col = "locsys", remove = F) %>% 
  mutate(county = case_when(
    location == "boyd" ~ "Boone",
    location == "funcke" ~ "Greene",
    location == "stout" ~ "Washington"),
    crop_sys = str_to_title(crop_sys)) %>% 
  unite(county, crop_sys, sep = " ", col = "county_sys", remove = F)

#--means (etc.) of biomass
thresh <- cc %>%
  mutate(above2  = ifelse(ccbio_Mgha > 2, "Y", "N")) %>%
  # num of yrs w/>2 Mg
  group_by(location, crop_sys, locsys, above2) %>% 
  summarise(nyo2 = n()) %>% 
  filter(above2 == "Y") %>% 
  select(-above2)
    
    
mcc <- cc %>%
  group_by(location, crop_sys, locsys) %>% 
  summarise(ccbio_mean = mean(ccbio_Mgha),
            ccbio_sd   = sd(ccbio_Mgha),
            ccbio_stab = ccbio_mean/ccbio_sd) %>% 
  left_join(thresh)


#--counts by spec 
rawdats <- 
  read_csv("_data/tidy/td-GHspecies.csv") %>%   
  unite(loc, cropsys, col = "loc_sys", remove = F) %>% 
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
                                     "Purslane", "Hairy crabgrass", "other")))

dats <- 
  rawdats %>%
  # make things nice
  mutate(cc_trt = recode(cc_trt,
                         no = "None" ,
                         rye = "Rye Cover Crop")) %>% 
  # find total, then percentage of each weed
  group_by(loc_sys, cc_trt, rep) %>% 
  mutate(tot.m2 = sum(seeds_m2),
         p = seeds_m2/tot.m2) %>% 
  # find mean p across reps
  group_by(loc_sys, cc_trt, weed) %>% 
  summarise(p = mean(p)) %>%
  
  select(loc_sys, cc_trt, weed, p) %>% 
  #filter(p != 0) %>% 
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
                                     "Purslane", "Hairy crabgrass", "other"))) %>% 
  group_by(loc_sys, cc_trt, weedcat) %>% 
  summarise(p = sum(p)) %>% 
    mutate(nice = recode(loc_sys,
                               boyd_grain = "Boyd",
                               boyd_silage = "Boyd Silage",
                               funcke_grain = "Funcke",
                               stout_grain = "Stout"),
           nice = factor(nice, levels = c("Boyd Silage", "Stout", "Funcke", "Boyd")))
  

dats1val <- 
  rawdats %>%
  # make things nice
  mutate(cc_trt = recode(cc_trt,
                         no = "None" ,
                         rye = "Rye Cover Crop")) %>% 
  mutate(weedcat = "other",
         weedcat = case_when(
           str_detect(weed, "AMATU") ~ "Waterhemp",
           str_detect(weed, "CHEAL") ~ "Lambsquarters",
           str_detect(weed, "POROL") ~ "Purslane",
           #str_detect(weed, "DIGSA") ~ "Hairy Crabgrass",
           TRUE ~ "Other"
         ),
         weedcat = factor(weedcat, 
                          levels = c("Waterhemp", "Lambsquarters", "Other",
                                     "Purslane" #"Hairy Crabgrass"
                                     )))  %>% 
  # find total, then percentage of each weed
  group_by(weedcat) %>% 
  summarise(tot.m2 = sum(seeds_m2)) %>%
  mutate(tot = sum(tot.m2),
         p = tot.m2/tot)  
  


# Map of locs ----------------------------------------------------------


map_iowa <- as_tibble(map_data('state')) %>% 
  filter(region == "iowa")

map_county <- as_tibble(map_data('county')) %>% 
  filter(region == "iowa") 

map_county3 <- map_county %>% filter(subregion %in% c("boone", "greene", "washington"))

#--make a map

ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = pptblue) +
  geom_polygon(data = map_county, aes(x = long, y = lat, group = group), 
               color = "gray80", fill = NA) +
  geom_polygon(data = map_county3, aes(x = long, y = lat, group = group), 
               color = pptpink, fill = pptyellow, size = 3) +
  
  geom_text(aes(x = -94.65, y = 42.05), size = 7, label = "Greene") +
  geom_text(aes(x = -93.7, y = 42.05), size = 7, label = "Boone") +
  geom_text(aes(x = -91.7, y = 41.35), size = 7, label = "Washington") +
  theme_minimal()  +
  mytheme + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  coord_quickmap()

ggsave("_figs/poster-map.png", bg = "transparent")



# Boxplots --------------------------------------------------

figdat <- rawdat %>% 
  filter(totseeds_m2 < 15000) %>% 
  mutate(totseeds_ft2 = totseeds_m2/10.764,
         cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Rye Cover Crop")) %>% 
  mutate(county_sys = factor(county_sys, 
                             levels = c("Greene Grain", 
                                        "Boone Silage", 
                                        "Washington Grain", 
                                        "Boone Grain")))

figdat %>% 
  ggplot(aes(county_sys, totseeds_ft2, fill = cc_trt)) + 
  geom_hline(yintercept = 5000/10.764, color = "red", linetype = "dashed") +
  geom_boxplot() + 
    labs(y = labseedsft2,
       x = NULL,
       fill = NULL, 
       color = NULL) +
  scale_fill_manual(values = c(pptyellow, pptblue)) +
  scale_color_manual(values = c(pptyellow, pptblue)) +
  theme_bw() +
  mytheme + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = pptgray),
        axis.title.y = element_text(angle = 90, vjust = 0.5))

ggsave("_figs/poster-boxplot.png", bg = "transparent", width = 8)


# Pie water hemp -----------------------------------------------------

dats1val %>%
  mutate(id = "one") %>% 
  #filter(cc_trt == "Rye Cover Crop") %>% 
  ggplot(aes(id, p)) + 
  geom_bar(aes(fill = weedcat), stat = "identity", color = "black") + 
  #facet_grid(. ~ cc_trt) + 
  labs(fill = NULL) +
  theme_pubclean() + 
  coord_polar(theta = "y") +
  guides(fill = guide_legend(nrow = 2,byrow = TRUE)) +
  scale_fill_manual(values = c(pptpink, pptblue, pptgreen, pptorange, pptyellow)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) + 
  mypietheme + 
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent"),
        panel.grid = element_blank())

ggsave("_figs/poster-pie-weeds.png", bg = "transparent")

dats1val %>%  
  filter(cc_trt == "None") %>% 
  ggplot(aes(cc_trt, p)) + 
  geom_bar(aes(fill = weedcat), stat = "identity", color = "black") + 
  facet_grid(. ~ cc_trt) + 
  labs(fill = NULL) +
  theme_pubclean() + 
  coord_polar(theta = "y") +
  scale_fill_viridis_d(option = "plasma") +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) + 
  mypietheme


pie2 <- 
  dats %>% 
  filter(cc_trt == "Rye Cover Crop") %>% 
  ggplot(aes(cc_trt, p)) + 
  geom_bar(aes(fill = weedcat), stat = "identity", color = "black") + 
  facet_grid(cc_trt ~ nice) + 
  guides(fill = F) +
  theme_pubclean() + 
  coord_polar(theta = "y") +
  scale_fill_viridis_d(option = "plasma") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_blank()) + 
  mypietheme

pie2

pie1 / pie2

ggsave("_figs/fig_pie-div.png")

equdiffdat <- 
  div %>% 
  group_by(loc_sys, nice, cc_trt) %>% 
  summarise(equ = mean(equ)) %>% 
  spread(key = cc_trt, value = equ) %>% 
  mutate(equdif = `Rye Cover Crop` - None) %>%
  rename(locsys = loc_sys) %>% 
  select(locsys, nice, equdif)

div %>% 
  group_by(nice, loc_sys, cc_trt) %>% 
  summarise(equ = mean(equ)) %>% 
  rename(locsys = loc_sys) %>% 
  left_join(mcc) %>% 
  left_join(equdiffdat) %>% 

  ggplot(aes(reorder(nice, equ, max), equ)) + 
  geom_point(aes(color = cc_trt, pch = cc_trt), size = 9) + 
  labs(color = NULL, pch = NULL) +
  coord_flip() + 
  scale_color_manual(values = c("brown", "green3")) + 
  labs(y = "Species Evenness", x = NULL) + 
  theme_bw() + 
  mytheme 

ggsave("_figs/fig_spec-even.png")



div %>% 
  group_by(nice, loc_sys, cc_trt) %>% 
  summarise(equ = mean(equ)) %>% 
  rename(locsys = loc_sys) %>% 
  left_join(mcc) %>% 
  left_join(equdiffdat) %>%
  mutate(cc_trt = recode(cc_trt,
                         `Rye Cover Crop` = "Rye")) %>% 
  select(nice, cc_trt, equ) %>% 
  spread(cc_trt, value = equ) %>% 
  ungroup() %>% 
  mutate(ceq = log((Rye - None)/None * 100),
         rat = 0,
         nice = factor(nice, levels = c("Stout", "Boyd", "Boyd Silage", "Funcke"))) %>% 

  ggplot(aes(nice, rat)) +
  #geom_linerange(aes(ymin = rat, ymax = ceq, x = nice), size = 2) +
  geom_segment(aes(x = nice, y = rat, xend = nice, yend = ceq, color = nice),
               arrow =  arrow(length = unit(0.3,"cm")), size = 2) +
#  coord_flip(ylim = c(-300, 300)) + 
  coord_flip() +
  scale_color_manual(values = mypal) +
  guides(color = F) +
  labs(y = "Change in Weed Species Evenness\nWith Addition of Cover Crop\n(Log of Ratio)",
       x = NULL) + 
  theme_bw() +
  mytheme

ggsave("_figs/fig_change-in-evenness.png", width = 6, height= 8)
