# Gina, derived from lydia
# make manuscript figs
# created 5/21/2020
# updated 5/27/2020 (colors)

library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)

# fig settings ------------------------------------------------------------

mycols <- c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02")
scales::show_col(mycols)
theme_set(theme_bw())


cctrtpal <- c("lightsalmon4", "darkolivegreen3")
scales::show_col(cctrtpal)

# data --------------------------------------------------------------------

site_scores <- read_csv("01_stats-nmds/st_nmds-site.csv") %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2 (Silage)",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"),
         site_id = factor(site_id, levels = c("West", "Central1", "Central2 (Silage)", "East")),
         cc_trt = recode(cc_trt, 
                  "no" = "None",
                  "rye" = "Rye Cover Crop"))

spp_scores  <- read_csv("01_stats-nmds/st_nmds-spp.csv")

site_hull <- read_csv("01_stats-nmds/st_nmds-site-hulls.csv") %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2 (Silage)",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"),
         site_id = factor(site_id, levels = c("West", "Central1", "Central2 (Silage)", "East")),
         cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Rye Cover Crop"))


# figure ------------------------------------------------------------------

ggplot() +
  geom_point(data = site_scores, 
             aes(x = NMDS1, 
                 y = NMDS2, 
                 #color = cc_trt, 
                 shape = cc_trt
                 ), 
             size = 2, 
             alpha = 1) +
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   color = cc_trt,
                   fill = site_sys_trt),
               alpha = 0.7) + 
  
  geom_text_repel(data = spp_scores, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID), 
                  alpha = 0.5,
                  size = 3) + # Species as text - better!
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = cctrtpal) +
  scale_fill_manual(values = c(rep((cctrtpal), 4))) +
  labs(color = NULL, shape = NULL) +
  guides(fill = FALSE) +
  theme(legend.direction  = "horizontal",
        legend.position = "bottom",
        legend.background = element_rect(color = "black"),
        strip.text        = element_text(size = rel(1.2)),
        legend.text       = element_text(size = rel(1.2)),
        legend.title      = element_text(size = rel(1.2)),
        axis.title        = element_text(size = rel(1.2)),
        axis.text         = element_text(size = rel(1.2))) + 
  facet_wrap(~site_id)

ggsave("02_make-figs/figs/fig2_nmds.png")
