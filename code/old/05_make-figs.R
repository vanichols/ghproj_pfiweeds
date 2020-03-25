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
library(readxl)
library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(broom)
library(ggpubr)
library(wesanderson)
library(patchwork)
library(maps)


# data --------------------------------------------------------------------

#--site lat/lons
locs <- read_excel("_data/raw/rd_site-locs.xlsx") %>% 
  mutate(system = str_to_title(system))

#--cover crop biomass
cc <- read_csv("_data/tidy/td-all-ryebm2008-2019.csv") %>% 
  filter(year > 2009) %>% 
  unite(location, crop_sys, col = "locsys", remove = F)

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
dats <- 
  read_csv("_data/tidy/td-GHspecies.csv") %>% 
  # make things nice
  mutate(cc_trt = recode(cc_trt,
                         no = "None" ,
                         rye = "Rye Cover Crop")) %>% 
  # find total, then percentage of each weed
  group_by(loc_sys, cc_trt, rep) %>% 
  mutate(tot.m2 = sum(nmbr.m2),
         p = nmbr.m2/tot.m2) %>% 
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
                 

# Map of locs ----------------------------------------------------------


map_all <- as_tibble(map_data('state'))

map_iowa <- map_all %>%
  filter(region == "iowa")


#--make that plot gurrrrrl

ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = "lightgoldenrod1", color = "gray80") +
   geom_jitter(data = locs, width = 0.1, color = "black", size = 7,
              aes(x = lon, y = lat,
                  fill = system,
                  pch = system)) +
  labs(fill = NULL, pch = NULL) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = c("forestgreen", "darkolivegreen1")) + 
  #theme_pubclean() + 
  theme(legend.direction = "horizontal",
        legend.position = c(0.2, 0.85),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = rel(1.2)),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave("_figs/fig_site-locs.png")


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
