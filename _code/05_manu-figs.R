##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 30 2019
# Last modified: Jan 8 2020 - Lydia trying to run things (see note)...
#
# Purpose: make manuscript figs
#
# Inputs: 
#
# Outputs: 
#
# Notes: Where are the dats and div datasets coming from??? I can't run those figures in this script... (from Lydia )
# ^ Perhaps it's nbd and you don't want those figures anyways but I'm just curious. 
#
####################################


library(tidyverse)
library(readxl)
library(ggpubr)
library(wesanderson)
library(patchwork)
library(maps)


# fig things --------------------------------------------------------------


mypal <- c("royalblue", (wes_palette("Zissou1", n = 5)[c(2,3,5)]))

mytheme1 <- theme(axis.text = element_text(size = rel(1.2)),
                 legend.text = element_text(size = rel(1.3)),
                 axis.title = element_text(size = rel(1.3)))


mytheme2 <- theme(legend.position = c(0.1, 0.9),
                 legend.justification = c(0,1),
                 legend.background = element_rect(color = "black"),
                 axis.text = element_text(size = rel(1.2)),
                 legend.text = element_text(size = rel(1.3)),
                 axis.title = element_text(size = rel(1.3)))


# map of locs ----------------------------------------------------------

map_iowa <- as_tibble(map_data('state')) %>%
  filter(region == "iowa")

locs <- read_excel("_data/raw/rd_site-locs.xlsx") %>% 
  mutate(system = str_to_title(system))

set.seed(94)
fig_map <- 
  ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = "lightgoldenrod1", color = "white") +
  geom_jitter(data = locs, width = 0.1, color = "black", size = 7,
              aes(x = lon, y = lat,
                  fill = system,
                  pch = system)) +
  labs(fill = NULL, pch = NULL) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = c("forestgreen", "darkolivegreen1")) + 
  coord_cartesian() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "A. Map of sampling locations and cropping system type\n \n ") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.4)),
    legend.direction = "horizontal",
        legend.position = c(0.1, 0.9),
        legend.justification = c(0,1),
        #legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black")) +
  mytheme1

fig_map


# change in seedbank size -------------------------------------------------

sr <- read_csv("_data/smy/sd_stats-lrr.csv")

fig_sb <- 
  ggplot(data = sr, aes(cropsys, response)) +
  # geom_linerange(aes(ymin = lower.CL, ymax = upper.CL, color = CL), 
  #                size = 2) +
  geom_linerange(data = filter(sr, CL == "95%"),
                 aes(ymin = lower.CL, ymax = upper.CL), 
                 size = 2, color = "red") +
  geom_point(size = 3) +
  scale_color_manual(values = c("95%" = "gray80", 
                                "92.5%" = "red")) +
  scale_y_log10(breaks = c(0.3, 0.5, 1),
                labels = c("-70%", "-40%", "0%")) +
  #geom_text(x = 2, y = 0.03, label = "No Change", fontface = "italic") +
  geom_hline(yintercept = 1, linetype = "dotted") + 
  labs(#color = "Confidence Level",
       #y = "Cover-Cropped Seedbank Size",
       y = NULL,
       x = NULL,
       title = "B. Change in weed\nseedbank size with\ncover-crop treatment") + 
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.3))) +
  #theme(#legend.position = c(0.1, 0.1),
        #legend.justification = c(0,0),
        #legend.position = "top",
        #legend.background = element_rect(color = "black")) +
  mytheme1

fig_sb


fig_map + fig_sb + plot_layout(ncol = 2, widths = c(3, 1))
ggsave("_figs/manu/fig_locs-and-sb.png")


# cover crop biomass ------------------------------------------------------

ccbio <- read_csv("_data/tidy/td-all-ryebm2008-2019.csv")
mcc <- read_csv("_data/smy/sd_ccbio-metrics.csv")

p1 <- 
  ccbio %>%
  mutate(locsys = paste(location, crop_sys),
         mytit = "Biomass Over 10 Years\n(Mg ha-1)") %>% 
 
  ggplot(aes(year, ccbio_Mgha, group = locsys)) + 
  geom_line(aes(color = locsys, linetype = crop_sys), size = 2) + 
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019)) + 
  scale_color_manual(values = mypal, labels = c("Central", "Cental Silage", "West", "East")) + 
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
         mytit = "Mean\n(Mg ha-1)",
         locsys = paste(location, crop_sys)) %>% 
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


# threshold
p3 <- 
  mcc %>% 
  mutate(pos = "A",
         mytit = ">1 Mg ha-1 Threshold\n(Years)",
         locsys = paste(location, crop_sys)) %>% 
  ggplot(aes(pos, nabove1)) +
  geom_jitter(aes(color = locsys, pch = crop_sys), size = 5, width = 0.01) +
  scale_color_manual(values = mypal) +
  guides(color = F, pch = F) +
  #coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  facet_grid(. ~ mytit) +
  theme(strip.text = element_text(size = rel(1.2)),
        axis.title = element_blank(),
        axis.text.x = element_blank())
  
p1 + p3 + p2 + plot_layout(ncol = 3, widths = c(2, 1, 1))

ggsave("_figs/manu/fig_ccbio-metrics.png", width = 10, height = 5)


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
