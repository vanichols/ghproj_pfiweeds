# Gina, derived from lydia
# make manuscript figs
# created 5/21/2020
# updated 5/27/2020 (colors)
#         7/14/2020 (add fucntional group?)
#         9/1/2020 (make it 5 trials)

library(PFIweeds2020)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(ggpubr)

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
    crop_sys = str_to_title(sys_trt),
    site_sys = paste(site, crop_sys, sep = " "),
    #--get crop_2019 to crop_2018
    residue = case_when(
      (blockID == "B42" & site_sys == "Central Grain") ~ "Maize",
      (blockID == "B44") ~ "Soybean",
      (site == "West") ~ "Soybean",
      (site == "East") ~ "Maize")
  ) %>% 
  #--get the order I want
  mutate(
    site_sys = factor(site_sys, levels = c("West Grain", "Central Silage", "Central Grain", "East Grain")),
    cc_trt = recode(cc_trt,
                    "no" = "No Cover",
                    "rye" = "Winter Rye")
  ) 

spp_scores  <- read_csv("01_stats-nmds/st_nmds-spp.csv") %>% 
  left_join(pfi_weedsplist, by = c('speciesID' = 'code'))

site_hull <- 
  read_csv("01_stats-nmds/st_nmds-site-hulls.csv") %>%
  mutate(
    crop_sys = str_to_title(sys_trt),
    site_sys = paste(site, crop_sys, sep = " "),
    #--get crop_2019 to crop_2018
    residue = case_when(
      (blockID == "B42" & site_sys == "Central Grain") ~ "Maize*",
      (blockID == "B44" & site_sys == "Central Silage") ~ "Soybean*",
      (blockID == "B44") ~ "Soybean",
      (site == "West") ~ "Soybean*",
      (site == "East") ~ "Maize")
  ) %>% 
  #--get the order I want
  mutate(
    site_sys = factor(site_sys, levels = c("West Grain", "Central Silage", "Central Grain", "East Grain")),
    cc_trt = recode(cc_trt,
                    "no" = "No Cover",
                    "rye" = "Winter Rye")
  ) 


# figure ------------------------------------------------------------------

site_hull2 <- 
  site_hull %>% 
  group_by(cc_trt, site_sys, residue) %>% 
  slice(c(1, n()))

#nmds3 <- 
ggplot() +
  #--grasses
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "grass"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "italic",
                  alpha = 0.5, 
                  color = "gray70") + # Species as text - better!
  #--forbs
  geom_text_repel(data = spp_scores %>% filter(functional_grp == "forb"), 
                  aes(x = NMDS1, 
                      y = NMDS2, 
                      label = speciesID),
                  fontface = "bold",
                  alpha = 0.6,
                  color = "gray70") + # Species as text - better!
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
  facet_wrap(.~site_sys+residue) +
  #geom_hline(yintercept = 0, lty = 2) +
  #geom_vline(xintercept = 0, lty = 2) +
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = c(p_pink, p_green, p_blue, p_orange, p_purp)) +
  scale_fill_manual(values = c(p_yellow, p_yellow,
                               p_purp, p_purp,
                               p_green, p_green,
                               p_blue, p_blue,
                               p_orange, p_orange)) +
  labs(color = "Site",
       linetype = "Cover Crop Treatment")+
  guides(fill = FALSE,
         shape = F)+
  theme_minimal() + 
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.justification = c(1, 0),
        legend.position = c(0.95, 0.15),
        #legend.text       = element_text(size = 12),
        #legend.title      = element_text(size = 14),
        #axis.title        = element_text(size = 14),
        #axis.text         = element_text(size = 12),
        strip.text = element_text(face = "bold", size = rel(1.2)),
        #legend.key.size = unit(0.8, "lines"),
        legend.title = element_text(size = rel(1), face = "bold"),
        legend.text = element_text(size = rel(1)))


ggsave("02_make-figs/manu-new/fig3.jpg", width = 8.3, height = 5.7)
