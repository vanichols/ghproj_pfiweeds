# Gina, derived from lydia
# make manuscript figs
# created 5/21/2020
# updated 5/27/2020 (colors)
#         7/14/2020 (add fucntional group?)

library(PFIweeds2020)
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
p_blue <- "dodgerblue4"#"#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FAE549FD" #"#FFE100"
p_gray <- "#E7E6E6"
p_purp <- "#8B1C62"

# lydia's colors
mycols <- c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02")
scales::show_col(mycols)
theme_set(theme_bw())


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
    crop_sys = stringr::str_to_title(sys_trt),
    cc_trt = recode(cc_trt, 
                    "no" = "No Cover",
                    "rye" = "Winter Rye"),
    site_sys = paste(site, crop_sys, sep = " "))

spp_scores  <- read_csv("01_stats-nmds/st_nmds-spp.csv") %>% 
  left_join(pfi_weedsplist, by = c('speciesID' = 'code'))

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
    crop_sys = stringr::str_to_title(sys_trt),
    cc_trt = recode(cc_trt, 
                         "no" = "No Cover",
                         "rye" = "Winter Rye"))


# figure ------------------------------------------------------------------

site_hull2 <- 
  site_hull %>% 
  group_by(cc_trt, crop_sys, site) %>% 
  slice(c(1, n()))

nmds3 <- 
  ggplot() +
   #--grasses
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "grass"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "italic",
                  alpha = 0.5) + # Species as text - better!
  #--forbs
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "forb"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "bold",
                  alpha = 0.6) + # Species as text - better!
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = site_sys_trt),
               alpha = 0.3) + 
  geom_path(data = site_hull,
            aes(x = NMDS1, 
                y = NMDS2,
                group = interaction(cc_trt, crop_sys, site),
                linetype = cc_trt
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 1) +
  geom_path(data = site_hull2, 
            aes(x = NMDS1, 
                y = NMDS2,
                group = interaction(cc_trt, crop_sys, site),
                linetype = cc_trt
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 0.9) +
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = c(p_pink, p_green, p_blue, p_orange)) +
  scale_fill_manual(values = c(p_yellow, p_yellow,
                               p_purp, p_purp,
                               p_green, p_green,
                               p_blue, p_blue)) +
  labs(color = "Site",
       linetype = "Cover Crop Treatment")+
  guides(fill = FALSE,
         shape = F)+
  theme_bw() +
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.justification = c(0, 1),
        legend.position = c(0.01, 0.99),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12)) 

nmds3


#--this is just for the legend
nmds2 <- 
  ggplot() +
  geom_point(data = site_scores, 
             aes(x = NMDS1, 
                 y = NMDS2, 
                 color = site_sys, shape = cc_trt), 
             size = 3, 
             alpha = 0.5) +
  #--grasses
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "grass"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "italic",
                  alpha = 0.5) + # Species as text - better!
  #--forbs
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "forb"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "bold",
                  alpha = 0.6) + # Species as text - better!
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = site_sys_trt),
               alpha = 0.3) + 
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = c("Central Grain" = p_yellow, 
                                "Central Silage" = p_purp,
                                "East Grain" = p_blue,
                                "West Grain" = p_green)) +
  scale_fill_manual(values = c(p_yellow, p_yellow,
                               p_purp, p_purp,
                               p_green, p_green,
                               p_blue, p_blue)) +
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

nmds2

# get legend from different plot ------------------------------------------

library(cowplot)

#--remove point legend
dum_p1 <- 
  nmds2 + 
  guides(shape = F)
  
# extract legend w/just site
leg_nmds2 <- get_legend(dum_p1)

# #--plot legend next to plot
# ggdraw(plot_grid(plot_grid(nmds3, ncol = 2),
#                  plot_grid(NULL, leg_nmds2, ncol = 1)))

library(patchwork)

set.seed(61)
nmds3 + leg_nmds2 + 
  plot_layout(widths = c(2, 0.5))

ggsave("02_make-figs/figs/fig_nmds3.png")

# Now plots are aligned vertically with the legend to the right
# ggdraw(plot_grid(plot_grid(p1, plot.mpg, ncol=1, align='v'),
#                  plot_grid(NULL, legend, ncol=1),
#                  rel_widths=c(1, 0.2)))



# separate panels by site -------------------------------------------------


set.seed(2091)
ggplot() +
  geom_path(data = site_hull,
            aes(x = NMDS1, 
                y = NMDS2,
                group = interaction(cc_trt, crop_sys, site),
                linetype = crop_sys
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 1) +
  geom_path(data = site_hull2, 
            aes(x = NMDS1, 
                y = NMDS2,
                group = interaction(cc_trt, crop_sys, site),
                linetype = crop_sys
            ),
            na.rm = TRUE,
            color = "gray40",
            size = 0.9) +
  geom_polygon(data = site_hull,
               aes(x = NMDS1,
                   y = NMDS2,
                   group = interaction(cc_trt, crop_sys, site),
                   #color = crop_sys,
                   fill = cc_trt
               ),
               #color = "black",
               size = 1,
               alpha = 0.7) +
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "forb"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "italic",
                  alpha = 0.5,
                  size = 3) + 
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "grass"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "bold",
                  alpha = 0.5,
                  size = 3) +
  facet_grid(.~site) +
  scale_fill_manual(values = c("No Cover" = p_yellow, 
                               p_blue)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(fill = NULL, linetype = NULL) +
  guides(linetype = guide_legend(order = 1)) + #--make crop_sys leg appear first
  theme(legend.direction  = "horizontal",
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        legend.background = element_blank(),
        strip.text        = element_text(size = rel(1.2)),
        legend.text       = element_text(size = rel(1.2)),
        legend.title      = element_text(size = rel(1.2)),
        axis.title        = element_text(size = rel(1.2)),
        axis.text         = element_text(size = rel(1.2))) 

ggsave("02_make-figs/figs/fig_nmds.png")

# figure alt --------------------------------------------------------------

set.seed(59)

nmds2 <- 
  ggplot() +
  geom_point(data = site_scores, 
             aes(x = NMDS1, 
                 y = NMDS2, 
                 color = site_sys, shape = cc_trt), 
             size = 3, 
             alpha = 0.5) +
  #--grasses
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "grass"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "italic",
                  alpha = 0.5) + # Species as text - better!
  #--forbs
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "forb"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "bold",
                  alpha = 0.6) + # Species as text - better!
  geom_polygon(data = site_hull, 
               aes(x = NMDS1, 
                   y = NMDS2, 
                   fill = site_sys_trt),
               alpha = 0.3) + 
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = c(p_pink, p_green, p_blue, p_orange)) +
  scale_fill_manual(values = c(p_yellow, p_yellow,
                               p_purp, p_purp,
                               p_green, p_green,
                               p_blue, p_blue)) +
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

nmds2

ggsave("02_make-figs/figs/fig_nmds2.png")

