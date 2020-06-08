# Gina, derived from lydia
# make manuscript figs
# created 5/21/2020
# updated 5/27/2020 (colors)

library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)

# fig settings ------------------------------------------------------------



mylegendtheme <- theme(legend.position = c(0.1, 0.9),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)),
                         strip.text = element_text(size = rel(1.3)))


p_green <- "#619B44"
p_blue <- "#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FFC000"
p_gray <- "#E7E6E6"


mycols <- c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02")
scales::show_col(mycols)
theme_set(theme_bw())


cctrtpal <- c("lightsalmon4", "darkolivegreen3")

# data --------------------------------------------------------------------

site_scores <- 
  read_csv("01_stats-nmds/st_nmds-site.csv") %>% 
  mutate(
    site = recode(
      site,
      "Boyd" = "Central",
      "Funcke" = "West",
      "Stout" = "East"
    ),
    site = factor(site, levels = c("West", "Central", "East")),
    crop_sys = str_to_title(sys_trt),
    cc_trt = recode(cc_trt, 
                    "no" = "None",
                    "rye" = "Winter Rye"))

spp_scores  <- read_csv("01_stats-nmds/st_nmds-spp.csv")

site_hull <- 
  read_csv("01_stats-nmds/st_nmds-site-hulls.csv") %>%
  mutate(
    site = recode(
      site,
      "Boyd" = "Central",
      "Funcke" = "West",
      "Stout" = "East"
    ),
    site = factor(site, levels = c("West", "Central", "East")),
    crop_sys = str_to_title(sys_trt),
    cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Winter Rye"))


# figure ------------------------------------------------------------------

ggplot() +
  # geom_point(data = site_scores, 
  #            aes(x = NMDS1, 
  #                y = NMDS2, 
  #                #color = cc_trt, 
  #                shape = crop_sys
  #                ), 
  #            size = 2, 
  #            alpha = 1) +
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2,
                   color = crop_sys,
                   fill = cc_trt, 
                   linetype = crop_sys),
               size = 1,
               alpha = 0.8) + 
  geom_text_repel(data = spp_scores, 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID), 
                  alpha = 0.5,
                  size = 3) + # Species as text - better!
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  facet_grid(.~site) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = c("Silage" = "black",
                                "black")) +
  scale_fill_manual(values = c("None" = p_yellow, 
                               p_blue)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(color = NULL, shape = NULL, fill = NULL) +
  guides(color = F, linetype = F) +
  theme(legend.direction  = "horizontal",
        legend.position = "bottom",
        legend.background = element_rect(color = "black"),
        strip.text        = element_text(size = rel(1.2)),
        legend.text       = element_text(size = rel(1.2)),
        legend.title      = element_text(size = rel(1.2)),
        axis.title        = element_text(size = rel(1.2)),
        axis.text         = element_text(size = rel(1.2))) + 
  facet_wrap(~site)

ggsave("02_make-figs/figs/fig_nmds.png")
