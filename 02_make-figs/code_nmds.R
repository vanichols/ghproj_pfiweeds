# Gina, derived from lydia
# make manuscript figs
# created 5/21/2020
# updated 


library(readr)
library(ggplot2)
library(ggrepel)

# fig settings ------------------------------------------------------------

mycols <- c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02")
scales::show_col(mycols)
theme_set(theme_bw())


# data --------------------------------------------------------------------

site_scores <- read_csv("01_stats-nmds/st_nmds-site.csv")
spp_scores  <- read_csv("01_stats-nmds/st_nmds-spp.csv")
site_hull <- read_csv("01_stats-nmds/st_nmds-site-hulls.csv")



# figure ------------------------------------------------------------------

library(ggrepel)

ggplot() +
  geom_point(data = site_scores, 
             aes(x = NMDS1, 
                 y = NMDS2, 
                 color = site_sys, shape = cc_trt), 
             size = 3, 
             alpha = 0.6) +
  geom_text_repel(data = spp_scores, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID), 
                  alpha = 0.5) + # Species as text - better!
  geom_polygon(data = hull_1, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = site_sys_trt),
               alpha = 0.3) + 
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = mycols) +
  scale_fill_manual(values = c("#1B9E77", "#1B9E77",
                               "#D95F02", "#D95F02",
                               "#7570B3", "#7570B3",
                               "#E6AB02", "#E6AB02")) +
  labs(color = "Site",
       shape = "Cover Crop Treatment")+
  guides(fill = FALSE)+
  theme_bw() +
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12))
