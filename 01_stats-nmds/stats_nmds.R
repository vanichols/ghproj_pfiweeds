# Gina
# 5/29/2020, based on Lydia's code

# ---- Getting data and loading packages ---- 
library(tidyverse)
library(vegan) #--does nmds
library(PFIweeds2020)



# data --------------------------------------------------------------------
# all from package

data("pfi_ghobsraw")
data("pfi_weedsplist")

################# remove outlier ################################

# analysis ----------------------------------------------------------------

df_dat <- 
  pfi_ghobsraw %>%
  filter(!(site_name == "Funcke" & rep == 4)) %>% #--remove outlier
  group_by(site_name, sys_trt, cc_trt, blockID) %>%
  summarize_at(vars(AMATU:UD), ~sum(., na.rm = TRUE)) %>% 
  unite("eu", site_name, sys_trt, cc_trt, blockID, remove = TRUE) 

mat_dat <- 
  df_dat %>% 
  column_to_rownames(var = "eu")

nmds_res <- metaMDS(mat_dat, distance = 'bray', autotransform = F, expand = F)
#stress = 0.102
plot(nmds_res)
cc_pdist <- dist(scores(nmds_res, display = 'sites'))

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



# do anova ----------------------------------------------------------------


#--on all sites, duh site is sig, cc is not at all
adonis2(df_dat %>% select(-1) %>% as.matrix() ~ 
          site + cc_trt, data = (df_dat %>% 
                                   separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
                                   mutate_if(is.character, as.factor)),
        by = "margin"
)


# individual sites, where cc-ing was sig? ---------------------------------

#--funcke
adonis2(df_dat %>%
          separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
          filter(site == "Funcke") %>%  
          select(-(site:rep)) %>% 
          as.matrix() ~ 
          cc_trt, data = (df_dat %>% 
                            separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
                            filter(site == "Funcke") %>% 
                            mutate_if(is.character, as.factor)),
        by = "margin"
)


#--silage
adonis2(df_dat %>%
          separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
          filter(crop_sys == "silage") %>%  
          select(-(site:rep)) %>% 
          as.matrix() ~ 
          cc_trt, data = (df_dat %>% 
                            separate(eu, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
                            filter(crop_sys == "silage") %>% 
                            mutate_if(is.character, as.factor))
)


################# keep outlier ################################

# analysis ----------------------------------------------------------------

df_dat_full <- 
  pfi_ghobsraw %>%
  #filter(!(site_name == "Funcke" & rep == 4)) %>% #--remove outlier
  group_by(site_name, sys_trt, cc_trt, blockID) %>%
  summarize_at(vars(AMATU:UD), ~sum(., na.rm = TRUE)) %>% 
  unite("eu", site_name, sys_trt, cc_trt, blockID, remove = TRUE) 

mat_dat_full <- 
  df_dat_full %>% 
  column_to_rownames(var = "eu")

nmds_res <- metaMDS(mat_dat_full, distance = 'bray', autotransform = F, expand = F)
#stress = 0.102
plot(nmds_res)
cc_pdist <- dist(scores(nmds_res, display = 'sites'))

#--need help knowing what to report about this fit

site_scores <- 
  as.data.frame(scores(nmds_res)) %>%
  rownames_to_column() %>% 
  separate(rowname, into = c("site", "sys_trt", "cc_trt", "blockID", "rep"), remove = F) %>% 
  rename(site_sys = rowname) %>% 
  unite("site_sys", site, sys_trt, remove = F)

site_scores %>% 
  write_csv("01_stats-nmds/st_nmds-site-full.csv")

spp_scores  <- 
  as.data.frame(scores(nmds_res, "species")) %>%
  rownames_to_column(., var = "speciesID")

spp_scores %>% 
  write_csv("01_stats-nmds/st_nmds-spp-full.csv")

# Makes polygons for site by treatment
site_hull <- 
  site_scores %>% # dataframe of site scores
  unite("site_sys_trt", site, sys_trt, cc_trt, remove = FALSE) %>%
  group_by(site_sys_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

site_hull %>% 
  write_csv("01_stats-nmds/st_nmds-site-hulls-full.csv")




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
  geom_polygon(data = site_hull, 
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













# tutor me ----------------------------------------------------------------
# 
# https://rstudio-pubs-static.s3.amazonaws.com/246172_1930ddfb5f064b2bab54b11016ab407e.html

birds <- read.csv('https://raw.githubusercontent.com/collnell/lab-demo/master/bird_by_fg.csv')
trees <- read.csv('https://raw.githubusercontent.com/collnell/lab-demo/master/tree_comp.csv')

#--question, is tree B associated with differences in bird comp?

bird.matrix <- as.matrix(birds[,3:9])##response variables in a sample x species matrix
trees$B <- as.factor(trees$B)

bird.manova <- manova(bird.matrix~as.factor(B), data=trees) ##manova test
summary(bird.manova) #--sort of

#--make data a matrix
weed_matrix <- as.matrix(mat_dat)


#--get groupings
trts <- 
  mat_dat %>% 
  rownames_to_column() %>% 
  separate(rowname, into = c("site", "crop_sys", "cc_trt", "field", "rep")) %>% 
  mutate_if(is.character, as.factor) 

#--doesn't work. just use adonis like lydia
cc.manova <- manova(weed_matrix ~ as.factor(cc_trt), data = trts)
summary(cc.manova)



# lydias ------------------------------------------------------------------

#dbRDA - don't like this....
cc.dbrda <- capscale(weed_matrix ~ site + cc_trt, distance='bray', data = trts)
plot(cc.dbrda)
anova(cc.dbrda)

# marginal SS
# adonis2(matrix_dat ~ loc_sys+cc_trt, data = wide_datw, by = 'margin')
# adonis2(weed_matrix ~ site + cc_trt, data = trts, by = 'margin')



#-----more lydia, haven't gone through


# ^^ need to use betadispr to test for equal variance among groups
groups <- wide_datw %>%
  unite(loc_sys_cc, loc_sys, cc_trt, sep = "_") %>%
  select(loc_sys_cc) %>%
  unlist() %>%
  unname()
# testing homogeneity - CC is ok, CC*LOC is ok, LOC is not homoegenous...
b <- betadisper(vegdist(matrix_dat), wide_datw$cc_trt)
b
anova(b)
plot(b)
