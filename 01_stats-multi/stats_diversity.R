# derived from Lydia's 04-expl-data_LE code
# updated 5/21/2020 by gina


rm(list = ls())
# packages ----------------------------------------------------------------

library(tidyverse)
library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(vegan)
library(PFIweeds2020)
library(emmeans)


data("pfi_ghobsraw")
data("pfi_weedsplist")

# data manipulation -------------------------------------------------------

#--lydia
# Keeping each rep (i.e. block) separate
all_dat <- 
  pfi_ghobsraw %>%
  group_by(site_name, field, sys_trt, cc_trt, crop_2019, rep) %>%
  summarize_at(vars(AMATU:UB), ~sum(., na.rm = TRUE)) %>%
  filter(!(is.na(crop_2019))) %>%         
  # filtering out STT NA field treatment bc it has less than 4 reps ...
  ungroup() %>%
  unite("model_id1", site_name, sys_trt, crop_2019, remove = FALSE) %>%
  unite("model_id2", site_name, sys_trt, remove = FALSE)
# combining columns to make model ID columns..

#--gina parroting

all_dat <- 
  pfi_ghobsraw %>% 
  pfifun_sum_weedbyeu() %>% 
  select(-seeds_m2) %>% 
  pivot_wider(names_from = weed, values_from = seeds) %>% 
  unite("site_sys", site_name, sys_trt, remove = F)


# ---- calculating richness and diversity

rich <- function(x){rowSums(ifelse(x > 0, 1, 0))}

div_rich <- 
  all_dat %>%
  mutate(shan_hill = exp(vegan::diversity(.[, 8:25])),
         richness  = rich(.[, 8:25])) %>%
  select(-c(AMATU:UB))

# ---- making long dataset

long_spp_list <- 
  all_dat %>%
  pivot_longer(cols = c(AMATU:UB), 
               names_to = "code",
               values_to = "numSeedlings") %>%
  left_join(pfi_weedsplist, by = "code") %>%
  filter(numSeedlings > 0)

# ---- running univariate models

# i. diversity
# fixed effects model 
l1 <- lm(shan_hill ~ site_sys*cc_trt, data = div_rich)
summary(l1)
anova(l1)

div_rich %>%
  ggplot(aes(site_sys, shan_hill))+
  geom_boxplot(aes(color = cc_trt))


# ii. richness
# graphing richness

div_rich %>%
  ggplot(aes(site_sys, richness))+
  geom_boxplot(aes(color = cc_trt))

div_rich %>%
  ggplot(aes(model_id2, richness))+
  geom_boxplot(aes(color = cc_trt))

r1 <- lm(richness ~ model_id2*cc_trt, data = div_rich)
summary(r1)
anova(r1)   # significant interaction term since richness increased for Funcke and Stout
emmeans(r1, pairwise ~ cc_trt|model_id2, type = "response")
# interestingly boyd richness is significanly lower in silage treatment...

# --- does AMATU # decrease...no...

amatus <- long_spp_list %>%
  filter(code == "AMATU")

amatus %>%
  ggplot(aes(model_id2, log(numSeedlings)))+
  geom_boxplot(aes(color = cc_trt))

a1 <- lm(log(numSeedlings) ~ model_id2*cc_trt, amatus)
summary(a1)
anova(a1)

# ---- NMDS ----

# option 1 - have 5 treatments (i.e. separate boyd grain into corn and soybean treatments)

env_dat1 <- pfi_ghobsraw %>%
  distinct(site_name, sys_trt, cc_trt, crop_2019, rep) %>%
  filter(!(is.na(crop_2019))) %>%
  unite("loc_sys", site_name, sys_trt, cc_trt, crop_2019, rep, remove = FALSE) 

mat1 <- pfi_ghobsraw %>%
  group_by(site_name, sys_trt, cc_trt, crop_2019, rep) %>%
  summarize_at(vars(AMATU:UB), ~sum(., na.rm = TRUE)) %>%
  filter(!(is.na(crop_2019))) %>%
  unite("loc_sys", site_name, sys_trt, cc_trt, crop_2019, rep) %>%
  column_to_rownames(var = "loc_sys")

nmds1 <- metaMDS(mat1, distance='bray',autotransform=F, expand=F)
# stress = 0.10, ok!
plot(nmds1)

# ggplot

# option 2 - have 4 treatmetns (combine boyd grain corn and bean treatments)

env_dat2 <- pfi_ghobsraw %>%
  filter(!(is.na(crop_2019))) %>%
  distinct(site_name, sys_trt, cc_trt,  rep) %>%
  unite("loc_sys", site_name, sys_trt, cc_trt, rep, remove = FALSE) 

mat2 <- pfi_ghobsraw %>%
  filter(!(is.na(crop_2019))) %>%
  group_by(site_name, sys_trt, cc_trt, rep) %>%
  summarize_at(vars(AMATU:UB), ~sum(., na.rm = TRUE)) %>%
  unite("loc_sys", site_name, sys_trt, cc_trt, rep) %>%
  column_to_rownames(var = "loc_sys")

nmds2 <- metaMDS(mat2, distance='bray',autotransform=F, expand=F)
# stress = 0.066, better! 
plot(nmds2)

# ggplot2

site_scores_2 <- as.data.frame(scores(nmds2)) %>%
  # add in columns with useful info...
  bind_cols(env_dat2) 

spp_scores_2  <- as.data.frame(scores(nmds2, "species")) %>%
  rownames_to_column(., var = "speciesID")

# Makes polygons for site by treatment
hull_2 <- site_scores_2 %>% # dataframe of site scores
  unite("loc_sys2", site_name, sys_trt, cc_trt, remove = FALSE) %>%
  group_by(loc_sys2) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

# nice plot
ggplot()+
  geom_point(data = site_scores_2, 
             aes(x = NMDS1, y = NMDS2, color = site_name, shape = cc_trt), size = 3) +
  #geom_point(data = gg_sp_scores,   aes(x = NMDS1, y = NMDS2),color = "black", shape = 21)+ # Species as points - don't like
  geom_text(data = spp_scores_2, 
            aes(x = NMDS1, y = NMDS2, label = speciesID), alpha = 0.5) + # Species as text - better!
  geom_polygon(data = hull_2, aes(x = NMDS1, y = NMDS2, fill = loc_sys2), alpha = 0.5)+ # adding polygons from hull df
  geom_hline(yintercept = 0, lty = 2)+
  geom_vline(xintercept = 0, lty = 2)+
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = mycols)+
  scale_fill_manual(values = c("#1B9E77", "#1B9E77",
                               "#D95F02", "#D95F02",
                               "#7570B3", "#7570B3",
                               "#E6AB02", "#E6AB02"))+
  labs(color = "Farm Site",
       shape = "Cover Crop")+
  guides(fill = FALSE)+
  theme_bw()+
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12))













# ---- ALL OF THE FOLLOWING SECTIONS ARE OLD  ----

# analyzed as a fixed effects model - combined loc and cro_sys in first model; kept separate in second model
l1 <- lm(shan_hill ~ loc_sys*cc_trt, data = dat_div)
summary(l1)
car::Anova(l1, type = "II")
l2 <- lm(shan_hill ~ loc + crop_sys + cc_trt, data = dat_div)
summary(l2)
car::Anova(l2, type = "II")

# analyzed as a mixed model
m1 <- lmerTest::lmer(shan_hill ~ cc_trt + (1|loc_sys), data = dat_div)
summary(m1)
anova(m1)

dat_div %>%
  ggplot(aes(loc_sys, shan_hill))+
  geom_boxplot(aes(color = cc_trt)) # but does look like it's trending in that direction

# # getting total numbers of seedlings and percent that are unknown....
datr <- read_csv("_data/tidy/td-GHobs-raw.csv") %>%
  replace(is.na(.), 0) %>%
  group_by(loc, cc_trt, crop_sys) %>%
  summarize_if(is.numeric, sum) %>%
  select(-rep)
datr %>%
  ungroup() %>%
  summarize_if(is.numeric, sum) %>%
  rowSums(.)
#5240 total seeds found, 15 of them are unknowns (0.0029)

# total numbers of individual weeds species and their densities
datw <- read_csv("_data/tidy/td-GHspecies.csv") %>%
  unite("loc_sys", loc, cropsys, remove = FALSE)

# I thought this was Matt's model, but it's not.....
m1 <- lm(log(tot.m2) ~ loc_sys*cc_trt, data = datw)
summary(m1)
car::Anova(m1, type = "III") # lol everything is significant except cc? Why do I feel like this is wrong....

# cover crop biomass info
ccbio <- read_csv("_data/tidy/td-all-ryebm2008-2019-no0s.csv") %>%
  # stout starts 2010
  filter(year > 2009)

# species list
spp_list <- read_csv("_data/weed-spp-list.csv")

# making datw into a wide dataframe
wide_datw <- spread(datw, key = weed, value = seeds_m2) %>%
  select(loc:rep, ABUTH:UG) %>%
  replace(., is.na(.), 0) %>%
  group_by(loc, cropsys, cc_trt, rep) %>%
  summarise_at(vars(ABUTH:UG), sum) 

# environmental data matrix

site_info <- dat_div %>%
  distinct(loc_sys, loc, crop_sys, cc_trt) 

cc_env <- ccbio %>%
  unite(loc_sys, location, crop_sys, sep = "_") %>%
  group_by(loc_sys, cc_trt) %>% 
  summarise(cc_mean = mean(ccbio_Mgha, na.rm = TRUE), 
            cc_sd   = sd(ccbio_Mgha, na.rm = TRUE),
            cc_cv   = cc_sd/cc_mean*100, 
            cc_stab = cc_mean/cc_sd) %>%
  left_join(site_info, ., by = c("loc_sys", "cc_trt")) %>%
  mutate(cc_trt = as_factor(cc_trt))

# new matrix
matrix_dat <- wide_datw %>%
  ungroup() %>%
  #unite(loc_trt_rep_id, loc_sys, cc_trt, rep, sep = "_", remove = TRUE)%>%
  # making into matrix
  #column_to_rownames("loc_trt_rep_id")
  select(-c(loc:rep))

env_all <- wide_datw %>%
  left_join(., cc_env, by = c("loc", "cc_trt")) %>%
  dplyr::select(loc_sys, cc_trt, rep, cc_mean:cc_stab) %>%
  left_join(., dat, by = c("loc_sys", "cc_trt", "rep")) %>%
  separate(loc_sys, into = c("location", "crop_sys")) %>%
  unite(loc_sys, location, crop_sys, sep = " ", remove = FALSE) %>%
  mutate(loc_sys = str_to_sentence(loc_sys))

cc_div_tests <- matrix_dat %>%
  mutate(shan = diversity(.)) %>%
  mutate(shan_hill= exp(shan))%>%
  bind_cols(env_all, .) %>%
  select(loc_sys:LTccbio_Mgha, shan, shan_hill) %>%
  mutate(log_tot.m2 = log(tot.m2))
#%>%
#replace_na(list(cc_stab = 0))

# ------ 1. Do CC decrease number of weeds? Does is change the diveristy of weeds  ------

# number of weeds

l1 <- lmer(log(tot.m2) ~ cc_trt + (1|loc_sys), data = cc_div_tests) # if rep is added as a random effect then fit is singular
cc_div_tests$fit <- predict(l1)
summary(l1)
l1
anova(l1)
ggResidpanel::resid_panel(l1)
l2 <- lmer(log_tot.m2 ~ cc_trt+ (1|loc_sys), data = cc_div_tests)

visreg(l2, "cc_trt", gg = TRUE,
       trans=exp,
       partial = TRUE, 
       overlay = TRUE) +
  theme_bw() +
  geom_point(aes(color = loc_sys), size = 3)


line = list(col = mycols[1], lwd = 5),
points = list(size = 3, pch = 21, fill = "black")) + theme_bw() +
  labs(x = "Cover Crop Treatmnet",
       y = "Weed Seeds/m2")+
  
  theme(axis.title        = element_text(size = 16),
        axis.text         = element_text(size = 14))


lg <- glmer(tot ~ cc_trt + (1|loc_sys), family = negative.binomial(2), data = cc_div_tests)
summary(lg)
ggResidpanel::resid_panel(lg)

# if rep is added as a random effect then fit is singular
summary(l2) # almost meaningful...
ggResidpanel::resid_panel(l1)

# make a better plot of this... and include stability # or plot the results of the mixed model
cc_div_tests %>%
  ggplot(aes(loc_sys, tot.m2))+
  geom_boxplot(aes(color = cc_trt), lwd = 1)+
  geom_point(aes(color = cc_trt), alpha = 0.3, position=position_dodge(width=0.75))+
  #geom_point(aes(loc_sys, 10^(fit), color = cc_trt), size = 5, position=position_dodge(width=0.75))+
  scale_y_continuous(trans = "log", breaks = c(0,200,500,5000,20000))+
  labs(x = NULL,
       y = "Weed Seeds per m2",
       color = "cover crop")+
  theme_bw() +
  scale_color_manual(values = c(mycols[4], mycols[1]))+
  theme(legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12),
        strip.text        = element_text(size = 16))

# look at ratio like Gina?

# diveristy of weeds

l3 <- lmer(shan_hill ~ cc_trt + (1|loc_sys), data = cc_div_tests)
lg2 <- glmer(shan_hill ~ cc_trt + (1|loc_sys), family = poisson, data = cc_div_tests)
summary(l3)
ggResidpanel::resid_panel(l2)
cc_div_tests$fit_div <- predict(l2)

hist(cc_div_tests$shan)
visreg(l3, gg = TRUE, 
       line = list(col = mycols[1], lwd = 3),
       points = list(size = 3, pch = 21, fill = "black")) + theme_bw() +
  labs(x = "Cover Crop Treatmnet",
       y = "Shannon Hill Diversity")+
  theme_bw() +
  theme(axis.title        = element_text(size = 16),
        axis.text         = element_text(size = 14))
cc_div_tests %>%
  ggplot(aes(loc_sys, shan))+
  geom_boxplot(aes(color = cc_trt), lwd = 1)+
  geom_point(aes(color = cc_trt), alpha = 0.3, position=position_dodge(width=0.75))+
  #geom_point(aes(loc_sys, fit_div, color = cc_trt), size = 5, position=position_dodge(width=0.75))+
  labs(x = NULL,
       y = "Shannon Hill Diversity",
       color = "Cover crop")+
  theme_bw() +
  scale_color_manual(values = c(mycols[4], mycols[1]))+
  theme(legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12),
        strip.text        = element_text(size = 16))

cc_div_tests %>%
  ggplot(aes(loc_sys, shan_hill))+
  geom_boxplot(aes(color = cc_trt))+
  theme_bw()

# try with stability

l3 <- lmer(shan_hill ~ cc_stab + (1|loc_sys), data = cc_div_tests)
summary(l3) # doesn't make a difference
ggResidpanel::resid_panel(l3)



# ------- Do CC change the composition of the weedy community ----
# nMDS

matdat_bc <- vegdist(matrix_dat, method = "bray")
cc_nmds2 <- metaMDS(matrix_dat ,distance='bray',autotransform=F, expand=F)
# stress = 0.071, ok!
plot(cc_nmds2)
cc_pdist <- dist(scores(cc_nmds2, display='sites'))
# Euclidean distance between scores in 2D ordination
plot(cc_pdist, matdat_bc, pch=19, col=4,
     xlab='2D plot distance', ylab='Bray-Curtis dissimilarity')

# attempting to plot in ggplot2

# extracting site scores
gg_site_scores <- as.data.frame(scores(cc_nmds2)) %>%
  # add in columns with useful info...
  bind_cols(cc_div_tests %>% select(loc_sys, cc_trt, rep)) %>%
  # separate and unite things back together, ugh ugh. 
  separate(loc_sys, into = c("location", "crop_sys"), remove = FALSE) %>%
  unite(loc_sys, location, crop_sys, sep = " ", remove = FALSE) %>%
  mutate(loc_sys = str_to_title(loc_sys)) 
# extracting species scores
gg_sp_scores   <- as.data.frame(scores(cc_nmds2, "species")) %>%
  rownames_to_column(., var = "speciesID")

# Makes polygons for site by treatment
hull_cc <- gg_site_scores %>% # dataframe of site scores
  group_by(loc_sys, cc_trt) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) %>% # points that polygons will connect
  unite(loc_cc, loc_sys, cc_trt, sep = "_", remove = FALSE) # Ignore this

# nice plot
ggplot()+
  geom_point(data = gg_site_scores, aes(x = NMDS1, y = NMDS2, color = loc_sys, shape = cc_trt), size = 3)+
  #geom_point(data = gg_sp_scores,   aes(x = NMDS1, y = NMDS2),color = "black", shape = 21)+ # Species as points - don't like
  geom_text(data = gg_sp_scores,   aes(x = NMDS1, y = NMDS2, label = speciesID), alpha = 0.5)+ # Species as text - better!
  geom_polygon(data = hull_cc, aes(x = NMDS1, y = NMDS2, fill = loc_cc), alpha = 0.5)+ # adding polygons from hull df
  geom_hline(yintercept = 0, lty = 2)+
  geom_vline(xintercept = 0, lty = 2)+
  # -- the following stuff is for aesthetic purposes --
  scale_color_manual(values = mycols)+
  scale_fill_manual(values = c("#1B9E77", "#1B9E77",
                               "#D95F02", "#D95F02",
                               "#7570B3", "#7570B3",
                               "#E6AB02", "#E6AB02"))+
  labs(color = "Farm Site",
       shape = "Cover Crop")+
  guides(fill = FALSE)+
  theme_bw()+
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12))



#dbRDA - don't like this....
cc.dbrda <- capscale(matrix_dat ~ loc_sys + cc_trt, distance='bray', data = wide_datw)
plot(cc.dbrda)
anova(cc.dbrda2)

# marginal SS
adonis2(matrix_dat ~ loc_sys+cc_trt, data = wide_datw, by = 'margin')
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


# -------- distance-based redundancy analysis and PCoA ----

#PCoA
# first need to calculate dissimilartiy matrix
wmat_bc <- vegan::vegdist(wavg_mat, method = "bray")

pcoa_wmat <- MASS::isoMDS(wmat_bc)
plot(pcoa_wmat$points, type = "n")
text(pcoa_wmat$points, labels = as.character(1:8))
cc_shep <- MASS::Shepard(wmat_bc, pcoa_wmat$points)
plot(cc_shep)

# again without using average matrix
matdat_bc <- vegdist(matrix_dat, method = "bray")
pcoa_matdat <- MASS::isoMDS(matdat_bc)
plot(pcoa_matdat$points, type = "n")
text(pcoa_matdat$points, labels = as.character(1:36))
# replot with color and size 

#dbRDA
cc.dbrda <- capscale(wavg_mat ~ cc_trt, distance = 'bray', data = cc_env)
plot(cc.dbrda)
anova(cc.dbrda) # no significant distance between cc treatments

# trying again but with a matrix that isn't averaged - I like this more....

cc.dbrda2 <- capscale(matrix_dat ~ cc_trt + loc_sys, distance='bray', data = wide_datw)
plot(cc.dbrda2)
anova(cc.dbrda2) # model is sig, aka site is sig - figure out how to plot this....
# without loc_sys
cc.dbrda3 <- capscale(matrix_dat ~ cc_trt, distance = 'bray', data = wide_datw)
plot(cc.dbrda3)
anova(cc.dbrda3) # not significant

# -------- PCA and RDA -------

#pca
cc_pca <- rda(matrix_dat ~ 1)
biplot(cc_pca) # VERY clearly shows treatments are grouped by sites
biplot(cc_pca,
       display = c("sites", "species"),
       type    = c("text" , "points"))
ordihull(cc_pca, group = cc_env$loc_sys)

#rda
cc_rda <- rda(wavg_mat ~ cc_trt, data = cc_env)
plot(cc_rda)

# ---- do two groups (CC and no CC) have similar or different spp composition ---- 

anosim(matrix_dat, wide_datw$cc_trt) # Clarke's analysis of similarity, not significant
mrpp(matrix_dat, wide_datw$cc_trt, distance = "bray") # MRPP, same answer
adonis(matrix_dat ~ loc_sys + cc_trt, data = wide_datw) # PERMANOVA, site not cc matters
# marginal SS
adonis2(matrix_dat ~ loc_sys+cc_trt, data = wide_datw, by = 'margin')
# ^^ need to use betadispr to test for equal variance among groups
groups <- wide_datw %>%
  unite(loc_sys_cc, loc_sys, cc_trt, sep = "_") %>%
  select(loc_sys_cc) %>%
  unlist() %>%
  unname()
# testing homogeneity - CC is ok, CC*LOC is ok, LOC is not homoegenous...
b <- betadisper(vegdist(matrix_dat), wide_datw$loc_sys)
b
anova(b)
plot(b)

mvab_cc <- mvabund(matrix_dat)
cc_t1 <- manyglm(mvab_cc ~ loc_sys + cc_trt, data = wide_datw)
cc_t1
plot(cc_t1)
anova(cc_t1, nBoot = 999) 
summary(cc_t1, nBoot = 999)

# ---- does stability of biomass have an effect on the weedy community? ----- 
# only looking at rye cover crop data now

env_all <- wide_datw %>%
  ungroup() %>%
  mutate(loc_sys = dplyr::recode(loc_sys, "funcke_grain" = "funke_grain")) %>%
  left_join(., cc_env, by = c("loc_sys", "cc_trt")) %>%
  dplyr::select(loc_sys, cc_trt, rep, cc_mean:cc_stab) %>%
  left_join(., dat, by = c("loc_sys", "cc_trt", "rep"))

matrix_rye <- wide_datw %>%
  ungroup() %>%
  filter(cc_trt == "rye") %>%
  #unite(loc_trt_rep_id, loc_sys, cc_trt, rep, sep = "_", remove = TRUE)%>%
  # making into matrix
  #column_to_rownames("loc_trt_rep_id")
  dplyr::select(-c(loc_sys, cc_trt, rep))

rye_mvab <- mvabund(matrix_rye)

cc_t2 <- manyglm(rye_mvab ~ cc_stab, data = env_mvab)
plot(cc_t2) # ehh, not sure this looks very good
anova(cc_t2) # seems to say that stability of biomass matters, i.e. it effects the community composition
summary(cc_t2) # idk what's happening here

## try by doing dbRDA - arrow suggestive that it matters
cc.dbrda4 <- capscale(matrix_rye ~ env_mvab$cc_stab, distance = 'bray', data = env_mvab)
cc.dbrda4
plot(cc.dbrda4)
anova(cc.dbrda4)

## since biomass is not a treatment, fit biomass over PCoA
# regular PCoA (on subsetted data?)

matrye_bc <- vegdist(matrix_rye, method = "bray")
pcoa_matrye <- MASS::isoMDS(matrye_bc)
plot(pcoa_matrye$points, type = "n")
text(pcoa_matrye$points, labels = as.character(1:18))
f1 <- envfit(pcoa_matrye ~ cc_stab, data = env_mvab)
f1
plot(envfit(pcoa_matrye ~ cc_stab, data = env_mvab))
## try making this plot again in ggplot....


#---- model testing whether stability affects diversity of weedy community ------

# each site has multiple diversities....keeping each strip separate

cc_div_rye <- matrix_dat %>%
  mutate(shan = diversity(.)) %>%
  mutate(shan_hill= exp(shan))%>%
  bind_cols(env_all, .) %>%
  select(loc_sys:cc_stab, shan, shan_hill)

l1 <- lmer(shan_hill ~ cc_trt + (1|loc_sys), data = cc_div_rye)
summary(l1)
ggResidpanel::resid_panel(l1)
# Doesn't make a difference

cc_div_rye %>% # looks like weed community is more diverse in grain and not silage
  ggplot(aes(loc_sys, shan_hill))+
  geom_boxplot(aes(color = cc_trt))+
  geom_point(aes(color = cc_trt), size = 2, alpha = 0.2)+
  theme_bw()

# model testing whether stability affects number of weeds in community

cc_num <- cc_div_rye %>%
  left_join(., dat, by = c("loc_sys", "rep", "cc_trt"))

l2 <- lmer(tot ~ cc_trt + (1|loc_sys), data = cc_num)
summary(l2)
ggResidpanel::resid_panel(l2)

# better plot of this...
cc_num %>%
  ggplot(aes(loc_sys, tot))+
  geom_boxplot(aes(color = cc_trt))+
  theme_bw()

# # matrix using pi for each treatment (instead of averaging, since different treatments have different reps)
# 
# weed_pi <- wide_datw %>%
#   group_by(loc_sys, cc_trt)%>%
#   summarize_if(is.numeric, sum) %>%
#   select(-rep) %>%
#   ungroup()%>%
#   mutate(weed_tot = rowSums(.[3:21]))%>%
#   mutate_at(vars(ABUTH:UG), ~ ./weed_tot)
# 
# wmat_pi <- weed_pi %>%
#   unite(loc_trt, loc_sys, cc_trt, sep = "_", remove = TRUE) %>%
#   column_to_rownames("loc_trt") %>%
#   select(-weed_tot)

# ----- Figures ----- 

# map of sample sites - adapated from Gina
library(maps)
library(readxl)
locs <- read_excel("_data/raw/rd_site-locs.xlsx") %>% 
  mutate(system = str_to_title(system)) %>% 
  unite(coop_name, system, col = "loc_sys", remove = F)

iowa <- map_data("county") %>%
  filter(region == "iowa")

ggplot() +
  geom_polygon(data = iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = "white") +
  geom_jitter(data = locs, width = 0.1, color = "black", size = 6,
              aes(x = lon, y = lat,
                  fill = system,
                  pch = system)) +
  #coord_fixed(1.3)+
  labs(fill = NULL, pch = NULL) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = mycols[1:2]) + 
  #theme_pubclean() + 
  theme(panel.background = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.2, 0.85),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black"),
        legend.text = element_text(size = rel(1.2)),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())


# Biomass and stability - adapted from Gina
p1 <- ccbio %>%
  unite(loc_sys, location, crop_sys, sep = " ", remove = FALSE) %>%
  mutate(loc_sys = str_to_title(loc_sys),
         mytit = "Rye Biomass Over 10 Years") %>%
  ggplot(aes(year, ccbio_Mgha, group = loc_sys))+
  geom_line(aes(color = loc_sys, linetype = crop_sys), size = 2.5)+
  guides(linetype = F)+
  scale_color_manual(values = mycols)+
  labs(y = "Biomass(Mg/ha)",
       x = "Year",
       color = "Farm Site")+
  theme_bw()+
  facet_grid(. ~ mytit) +
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019))+
  theme(legend.direction  = "vertical",
        legend.background = element_rect(color = "black"),
        legend.position   = c(0.2, 0.75), 
        legend.text       = element_text(size = 12),
        legend.title      = element_text(size = 14),
        axis.title        = element_text(size = 14),
        axis.text         = element_text(size = 12),
        strip.text        = element_text(size = 16))

# Biomass mean and stability, adapted from Gina
p2 <- cc_env%>% 
  mutate(pos = "A",
         mytit = "Mean") %>% 
  ggplot(aes(pos, cc_mean)) +
  geom_point(aes(color = loc_sys, pch = crop_sys), size = 5) +
  scale_color_manual(values = mycols) +
  guides(color = F, pch = F) +
  coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  facet_grid(. ~ mytit) +
  theme(strip.text = element_text(size = 16),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12))

p3 <- cc_env %>% 
  mutate(pos = "A",
         mytit = "Signal to Noise") %>% 
  ggplot(aes(pos, cc_stab)) +
  geom_point(aes(color = loc_sys, pch = crop_sys), size = 5) +
  scale_color_manual(values = mycols) +
  guides(color = F, pch = F) +
  coord_cartesian(ylim = c(0, 3)) +
  theme_bw() +
  facet_grid(. ~ mytit) +
  theme(strip.text = element_text(size = 16),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12))

library(patchwork)

p1 + p2 + p3 + plot_layout(ncol = 3, width = c(2, 1, 1))

p2




