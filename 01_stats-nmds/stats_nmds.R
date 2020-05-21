### 2/25/2020 analyzing again

# ---- Getting data and loading packages ---- 
library(tidyverse)
library(vegan) #--does nmds
library(PFIweeds2020)



# data --------------------------------------------------------------------
# all from package

data("pfi_ghobsraw")
data("pfi_weedsplist")


# analysis ----------------------------------------------------------------

mat_dat <- 
  pfi_ghobsraw %>%
  group_by(site_name, sys_trt, cc_trt, blockID) %>%
  summarize_at(vars(AMATU:UB), ~sum(., na.rm = TRUE)) %>% 
  unite("eu", site_name, sys_trt, cc_trt, blockID, remove = TRUE) %>% 
  column_to_rownames(var = "eu")

nmds_res <- metaMDS(mat_dat, distance = 'bray', autotransform = F, expand = F)
#stress = 0.102
plot(nmds_res)

#--need help knowing what to report about this fit


site_scores <- 
  as.data.frame(scores(nmds_res)) %>%
  rownames_to_column() %>% 
  separate(rowname, into = c("site", "sys_trt", "cc_trt", "blockID", "rep"), remove = F) %>% 
  rename(site_sys = rowname) %>% 
  unite("site_sys", site, sys_trt, remove = F)

site_scores %>% 
  write_csv("01_stats-nmds/st_nmds-site.csv")

spp_scores  <- 
  as.data.frame(scores(nmds_res, "species")) %>%
  rownames_to_column(., var = "speciesID")

spp_scores %>% 
  write_csv("01_stats-nmds/st_nmds-spp.csv")

# Makes polygons for site by treatment
site_hull <- 
  site_scores %>% # dataframe of site scores
  unite("site_sys_trt", site, sys_trt, cc_trt, remove = FALSE) %>%
  group_by(site_sys_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull %>% 
  write_csv("01_stats-nmds/st_nmds-site-hulls.csv")


# exploratory figure ------------------------------------------------------

# note: manuscript figure is created in make-figs folder

library(ggrepel)

mycols <- c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02")
scales::show_col(mycols)
theme_set(theme_bw())


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


ggsave("01_stats-nmds/fig_nmds.png")

