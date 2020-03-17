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

#devtools::install_github("vanichols/weedseedspfi2020")



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


#--more creative biomass things
shan <- read_csv("_data/smy/sd_shannons-ccbio.csv") %>% 
  unite(location, crop_sys, col = "locsys", remove = F)

#--community evenness
div <- 
  read_csv("_data/smy/sd_spec-div.csv") %>% 
  # make things nice
  mutate(cc_trt = recode(cc_trt,
                       no = "None" ,
                       rye = "Rye Cover Crop"),
         nice = recode(loc_sys,
                       boyd_grain = "Boyd",
                       boyd_silage = "Boyd Silage",
                       funcke_grain = "Funcke",
                       stout_grain = "Stout"),
         nice = factor(nice, levels = c("Boyd Silage", "Stout", "Funcke", "Boyd")))



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
  


# fig things --------------------------------------------------------------
wes <- wes_palette("Zissou1")
wes2 <- wes_palette("Rushmore1")
wes3a <- wes_palette("Moonrise3")
wes3 <- wes_palette("Moonrise2")

cctrtpal <- c(wes3[4], wes3a[3])

mypal <- c("royalblue", (wes_palette("Zissou1", n = 5)[c(2,3,5)]))

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


# Map of locs ----------------------------------------------------------


map_iowa <- as_tibble(map_data('state')) %>% 
  filter(region == "iowa")

map_county <- as_tibble(map_data('county')) %>% 
  filter(region == "iowa") 

map_county3 <- map_county %>% filter(subregion %in% c("boone", "greene", "washington"))

#--make a map

ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = wes[1]) +
  geom_polygon(data = map_county, aes(x = long, y = lat, group = group), 
               color = "gray80", fill = NA) +
  geom_polygon(data = map_county3, aes(x = long, y = lat, group = group), 
               color = wes[4], fill = wes[3], size = 3) +
  
  geom_text(aes(x = -94.65, y = 42.05), size = 7, label = "Greene") +
  geom_text(aes(x = -93.7, y = 42.05), size = 7, label = "Boone") +
  geom_text(aes(x = -91.7, y = 41.35), size = 7, label = "Washington") +
  
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  coord_quickmap()

ggsave("_figs/fig_talk-site-locs-counties.png")



# raw values, anonymized bar-ified --------------------------------------------------

rawdat %>% 
  filter(totseeds_m2 < 15000) %>% 
  mutate(totseeds_ft2 = totseeds_m2/10.764,
         cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Rye Cover Crop")) %>% 
  mutate(county_sys = factor(county_sys, 
                             levels = c("Greene Grain", 
                                        "Boone Silage", 
                                        "Washington Grain", 
                                        "Boone Grain"))) %>% 
  ggplot(aes(county_sys, totseeds_ft2, fill = cc_trt)) + 
  geom_hline(yintercept = 5000/10.764, color = "red", linetype = "dashed") +
  stat_summary(fun.y = mean, geom = "bar", 
               position = position_dodge(width = 0.75), alpha = 0.8) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.75), width = 0.2)+ #-or pointrange
  labs(y = labseedsft2,
       x = NULL,
       fill = NULL, 
       color = NULL) +
  scale_fill_manual(values = cctrtpal) +
  scale_color_manual(values = cctrtpal) +
  theme_bw() +
  mytheme + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        panel.background = element_rect(fill = "gray90"),
        axis.title.y = element_text(angle = 90, vjust = 0.5))

ggsave("_figs/fig_talk-raw-data-nice-v2.png")


# raw values, bar-ified poster colors --------------------------------------------------

cctrtpal2 <- rev(c('darkgoldenrod4', "olivedrab3"))
rawdat %>% 
  filter(totseeds_m2 < 15000) %>% 
  mutate(totseeds_ft2 = totseeds_m2/10.764,
         cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Rye Cover Crop")) %>% 
  mutate(county_sys = factor(county_sys, 
                             levels = c("Greene Grain", 
                                        "Boone Silage", 
                                        "Washington Grain", 
                                        "Boone Grain"))) %>% 
  mutate(cc_trt = factor(cc_trt, levels = rev(c("None", "Rye Cover Crop")))) %>% 
  ggplot(aes(reorder(county_sys, totseeds_m2, max), totseeds_m2, fill = cc_trt)) + 
  geom_hline(yintercept = 5000, color = "red", linetype = "dashed") +
  stat_summary(fun.y = mean, geom = "bar", 
               position = position_dodge(width = 0.75), alpha = 0.8) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.75), width = 0.2)+ #-or pointrange
  labs(y = labseedsm2,
       x = NULL,
       fill = NULL, 
       color = NULL) +
  scale_fill_manual(values = cctrtpal2) +
  scale_color_manual(values = cctrtpal2) +
  theme_bw() +
  mytheme + 
  theme(#legend.direction = "horizontal",
        legend.position = c(0.9, 0.2),
        legend.justification = c(1, 1),
        plot.background = element_rect(fill = "transparent", colour = NA),
        #panel.background = element_rect(fill = "gray90"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) + 
  coord_flip() 

ggsave("_figs/fig_talk-raw-data-nice-poster.png", width = 8, bg = "transparent")



# raw values2, anonymized --------------------------------------------------

rawdat2 %>% 
  filter(totseeds_m2 < 15000) %>% 
  mutate(totseeds_ft2 = totseeds_m2/10.764,
         cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Rye Cover Crop")) %>% 
  mutate(county_sys = factor(county_sys, 
                             levels = c("Greene Grain", 
                                        "Boone Silage", 
                                        "Washington Grain", 
                                        "Boone Grain"))) %>% 
  ggplot(aes(county_sys, totseeds_ft2)) + 
  geom_hline(yintercept = 5000/10.764, color = "red", linetype = "dashed") +
  geom_boxplot(aes(color = cc_trt, fill = cc_trt), alpha = 0.3, lwd = 1) +
  geom_point(aes(color = cc_trt), 
             position = position_dodge(width = 0.75), size = 3, alpha = 0.4) + 
  labs(y = labseedsft2,
       x = NULL,
       fill = NULL, 
       color = NULL) +
  scale_fill_manual(values = cctrtpal) +
  scale_color_manual(values = cctrtpal) +
  theme_bw() +
  mytheme + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        panel.background = element_rect(fill = "gray90"),
        axis.title.y = element_text(angle = 90, vjust = 0.5))


ggsave("_figs/fig_talk-raw-data-nice-no-outlier.png")


# water hemp --------------------------------------------------------------

rawdats %>% 
 filter(seeds_m2 < 15000) %>% 
  mutate(seeds_ft2 = seeds_m2/10.764) %>%  
  mutate(cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Rye Cover Crop")) %>% 
  ggplot(aes(weedcat, seeds_ft2, fill = cc_trt))  +
  # geom_boxplot(aes(color = cc_trt, fill = cc_trt), alpha = 0.3, lwd = 1) +
  # geom_point(aes(color = cc_trt), 
  #            position = position_dodge(width = 0.75), size = 3, alpha = 0.4) + 
  stat_summary(fun.y = mean, geom = "bar", 
               position = position_dodge(width = 0.75), alpha = 0.8) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.75), width = 0.2)+ #-or pointrange
  
  labs(y = labseedsft2,
       x = NULL,
       fill = NULL, 
       color = NULL) +
  scale_fill_manual(values = cctrtpal) +
  scale_color_manual(values = cctrtpal) +
  theme_bw() +
  mytheme + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        panel.background = element_rect(fill = "gray90"),
        axis.title.y = element_text(angle = 90, vjust = 0.5))

ggsave("_figs/fig_talk-counts-by-weed-v2.png")


# raw values with cc biomass --------------------------------------------------

cclabs <- 
  cc %>% 
  mutate(ccbio_tonac = ccbio_Mgha * 1000*2.2/2.471) %>% 
  mutate(cc_trt = recode(cc_trt, 
                    "no" = "None",
                    "rye" = "Rye Cover Crop")) %>% 
  mutate(county_sys = factor(county_sys, 
                             levels = c("Greene Grain", 
                                        "Boone Silage", 
                                        "Washington Grain", 
                                        "Boone Grain"))) %>%
  group_by(county_sys) %>% 
  summarise(mccbio = round(mean(ccbio_tonac, na.rm = T), -2))
  

rawdat %>% 
  filter(totseeds_m2 < 15000) %>% 
  mutate(totseeds_ft2 = totseeds_m2 / 10.764,
         cc_trt = recode(cc_trt, 
                    "no" = "None",
                    "rye" = "Rye Cover Crop")) %>% 
  mutate(county_sys = factor(county_sys, 
                             levels = c("Greene Grain", 
                                        "Boone Silage", 
                                        "Washington Grain", 
                                        "Boone Grain"))) %>% 
  ggplot(aes(county_sys, totseeds_ft2)) + 
  geom_hline(yintercept = 5000/10.764, color = "red", linetype = "dashed") +
  stat_summary(fun.y = mean, geom = "bar", 
               position = position_dodge(width = 0.75), alpha = 0.8, aes(fill = cc_trt)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.75), width = 0.2, aes(fill = cc_trt)) + #-or pointrange
  geom_text(data = cclabs, 
            aes(x = county_sys, y = -100, label = paste(mccbio, "lbs/ac", sep = " ")), 
            size = 8, 
            fontface = "italic",
            color = "red") +
  labs(y = labseedsft2,
       x = NULL,
       fill = NULL, 
       color = NULL) +
  scale_fill_manual(values = cctrtpal) +
  scale_color_manual(values = cctrtpal) +
  theme_bw() +
  mytheme + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        panel.background = element_rect(fill = "gray90"),
        axis.title.y = element_text(angle = 90, vjust = 0.5))


ggsave("_figs/fig_talk-raw-data-ccbiolabs-v2.png")



rawdat %>% 
  filter(totseeds_m2 < 15000) %>% 
  mutate(totseeds_ft2 = totseeds_m2 / 10.764,
         cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Rye Cover Crop")) %>% 
  mutate(county_sys = factor(county_sys, 
                             levels = c("Greene Grain", 
                                        "Boone Silage", 
                                        "Washington Grain", 
                                        "Boone Grain"))) %>% 
  ggplot(aes(county_sys, totseeds_ft2)) + 
  geom_hline(yintercept = 5000/10.764, color = "red", linetype = "dashed") +
  stat_summary(fun.y = mean, geom = "bar", 
               position = position_dodge(width = 0.75), alpha = 0.8, aes(fill = cc_trt)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.75), width = 0.2, aes(fill = cc_trt)) + #-or pointrange
  geom_text(data = cclabs, 
            aes(x = county_sys, y = -100, label = paste(mccbio, "lbs/ac", sep = " ")), 
            size = 8, 
            fontface = "italic",
            color = "red") +
  labs(y = labseedsft2,
       x = NULL,
       fill = NULL, 
       color = NULL) +
  scale_fill_manual(values = cctrtpal) +
  scale_color_manual(values = cctrtpal) +
  theme_bw() +
  mytheme + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        panel.background = element_rect(fill = "gray90"),
        axis.title.y = element_text(angle = 90, vjust = 0.5))


ggsave("_figs/fig_talk-raw-data-ccbiolabs-v2.png")




# cover crop biomass ------------------------------------------------------

p1 <- 
  cc %>%
  mutate(locsys = str_to_title(locsys),
         mytit = "Biomass Over 10 Years\n(Mg ha-1)") %>% 
 
  ggplot(aes(year, ccbio_Mgha, group = locsys)) + 
  geom_line(aes(color = locsys, linetype = crop_sys), size = 2) + 
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019)) + 
  scale_color_manual(values = mypal, labels = c("Boyd", "Boyd Silage", "Funcke", "Stout")) + 
  guides(linetype = F) +
  labs(color = NULL, y = "Mg ha-1", x = NULL) +
  theme_bw()  +
  facet_grid(. ~ mytit) +
   theme(legend.position = c(0, 1),
         legend.justification = c(0, 1),
         legend.direction = "vertical",
         legend.text = element_text(size = rel(1.2)),
         legend.background = element_rect(color = "black"),
         axis.text = element_text(size = rel(1.1)),
         axis.title = element_text(size = rel(1.1)),
         strip.text = element_text(size = rel(1.2)))
p1
p2 <- 
  mcc %>% 
  mutate(pos = "A",
         mytit = "Mean\n(Mg ha-1)") %>% 
  ggplot(aes(pos, ccbio_mean)) +
  geom_point(aes(color = locsys, pch = crop_sys), size = 5) +
  scale_color_manual(values = mypal) +
  guides(color = F, pch = F) +
  coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  facet_grid(. ~ mytit) +
  theme(strip.text = element_text(size = rel(1.2)),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = rel(1.1)))

p2b <- 
  cc %>% 
  mutate(pos = "A",
         mytit = "Mean") %>% 
  ggplot(aes(pos, ccbio_Mgha)) +
  #geom_point(aes(color = locsys, pch = crop_sys), size = 5) +
  stat_summary(aes(color = locsys, pch = crop_sys), size = 2) +
  scale_color_manual(values = mypal) +
  guides(color = F, pch = F) +
  coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  facet_grid(. ~ mytit) +
  theme(strip.text = element_text(size = rel(1.2)),
        axis.title = element_blank(),
        axis.text.x = element_blank())


             
p3 <- 
  shan %>% 
  mutate(pos = "A",
         mytit = "Effective Mean") %>% 
  ggplot(aes(pos, shan_hill)) +
  geom_point(aes(color = locsys, pch = crop_sys), size = 5) +
  scale_color_manual(values = mypal) +
  guides(color = F, pch = F) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_bw() +
  facet_grid(. ~ mytit) +
  theme(strip.text = element_text(size = rel(1.2)),
        axis.title = element_blank(),
        axis.text.x = element_blank())

# stability
p4 <- mcc %>% 
  mutate(pos = "A",
         mytit = "Signal-to-Noise Ratio\n(unitless)") %>% 
  ggplot(aes(pos, ccbio_stab)) +
  geom_point(aes(color = locsys, pch = crop_sys), size = 5) +
  scale_color_manual(values = mypal) +
  guides(color = F, pch = F) +
  coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  facet_grid(. ~ mytit) +
  theme(strip.text = element_text(size = rel(1.2)),
        axis.title = element_blank(),
        axis.text.x = element_blank())

# threshold
p5 <- mcc %>% 
  mutate(pos = "A",
         mytit = ">2 Mg ha-1 Threshold\n(Years)") %>% 
  ggplot(aes(pos, nyo2)) +
  geom_jitter(aes(color = locsys, pch = crop_sys), size = 5, width = 0.01) +
  scale_color_manual(values = mypal) +
  guides(color = F, pch = F) +
  #coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  facet_grid(. ~ mytit) +
  theme(strip.text = element_text(size = rel(1.2)),
        axis.title = element_blank(),
        axis.text.x = element_blank())
  
# with stability and threshold

p1 + p2 + p4 + p5 + plot_layout(ncol = 4, widths = c(2, 1, 1, 1))

ggsave("_figs/fig_ccbio-fun.png", width = 12, height = 5)


# community diversity -----------------------------------------------------


dats %>% 
  ggplot(aes(cc_trt, p)) + 
  geom_bar(aes(weedcat), stat = "identity") +
  facet_grid(cc_trt~nice)
  

pie1 <- 
  dats %>% 
  filter(cc_trt == "None") %>% 
  ggplot(aes(cc_trt, p)) + 
  geom_bar(aes(fill = weedcat), stat = "identity", color = "black") + 
  facet_grid(cc_trt ~ nice) + 
  labs(fill = NULL) +
  theme_pubclean() + 
  coord_polar(theta = "y") +
  scale_fill_viridis_d(option = "plasma") +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) + 
  mypietheme

pie1

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
