##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Aug 9 2019
# Last modified: Aug 22 
#                Aug 29 (added pca to species data, didn't work)
#                Sep 18 (LE, started nMDS)
#                Dec 30 2019 (Gina, outliers, rep nested)
#
# Purpose: explore processed (and tidy!) data
#
# Inputs: td_GHspecies, td_GHsum, td-all-ryebm2008-2019
#
# Outputs: 
#
# Notes:
#
#
####################################

rm(list = ls())
library(tidyverse)
library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(broom)
library(emmeans)
library(influence.ME)

# data --------------------------------------------------------------------

dat <- read_csv("_data/tidy/td-GHsum.csv") 
  
datsp <- read_csv("_data/tidy/td-GHspecies.csv") 

ccbio <- read_csv("_data/tidy/td-all-ryebm2008-2019.csv") %>%
  # stout starts 2010
  filter(year > 2009)

# dataset with outliers (id'd using cook's d) removed
datout <- dat %>% 
  filter(!(loc == "funcke" & rep == 4))

# look at all data --------------------------------------------------------------

# That stinking funcke one... 
dat %>%    
  ggplot(aes(cc_trt, totseeds_m2)) + 
  geom_point(size = 3, fill = "red", pch = 19, aes(color = as.factor(rep))) +
  facet_grid(cropsys~loc) + 
  labs(x = "Cover Crop Treatment", y = "Total Weed Seeds per m2") + 
  theme_bw()

# boyd was more diverse?
datsp %>%
  group_by(loc, cropsys, cc_trt, rep) %>%
  mutate(pct = seeds_m2 / sum(seeds_m2) * 100) %>%
  ggplot(aes(reorder(weed, -pct), pct)) +
  geom_col() + 
  facet_grid(loc ~ cc_trt) + 
  theme(axis.text.x = element_text(angle = 45))
  


# fit models no ratio --------------------------------------------------------------

# first, separate loc and sys, and make rep obvious it's nested
dat2 <- 
  dat %>%
  mutate(rep2 = paste(loc, rep, sep = "_")) %>% 
  mutate_if(is.character, as.factor)

xtabs(~ loc + cropsys, dat2)
# yeah yeah it's super unbalanced

#--cc as only fixed factor
m1a <- lmer(totseeds_m2 ~ cc_trt + (1|loc) + (1|rep2), data = dat2)
summary(m1a)
anova(m1a)
# definitely not sig

#--use influence.ME to see if there is a terrible point
#The function influence is the basis for all further steps:
infl <- influence.ME::influence(m1a, obs = TRUE)
#Calculate Cook's distance:
cookd <- cooks.distance(infl)
# all of rep 4 of funcke is high
dat2 %>% 
  mutate(cookd = as.vector(cookd)) %>% 
  ggplot(aes(rep, cookd)) + 
  geom_point(aes(color = cc_trt)) +
  facet_grid(.~(loc:cropsys))


#--do same thing but with cropsys included
m1b <- lmer(totseeds_m2 ~ cc_trt + cropsys + (1|loc) + (1|rep2), data = dat2)
summary(m1b)
anova(m1b)
# definitely not sig still

#--use influence.ME to see if there is a terrible point
#The function influence is the basis for all further steps:
infl <- influence.ME::influence(m1b, obs = TRUE)
#Calculate Cook's distance:
cookd <- cooks.distance(infl)
# all of rep 4 of funcke is high
dat2 %>% 
  mutate(cookd = as.vector(cookd)) %>% 
  ggplot(aes(rep, cookd)) + 
  geom_point(aes(color = cc_trt)) +
  facet_grid(.~(loc:cropsys))


#--try removing that stupid funcke value(s) from rep 4
dat4 <- 
  dat2 %>% 
  filter(!(loc == "funcke" & rep == 4))

#--cc trt and sys as fixed
m1 <- lmer(totseeds_m2 ~ cc_trt + (1|loc) + (1|rep2), data = dat4)
summary(m1)
anova(m1)
# welp there you go, it's significant
difflsmeans(m1)

# it's being driven by fucke. disadvantage of using raw numbers. use the ratio....
dat4 %>% 
  ggplot(aes(loc, totseeds_m2)) +
  geom_point(aes(color = cc_trt))

# 1367 seeds per m2 less. 
# Forcella et al. 1992 says corn belt densities range from 56-14000 seeds/sq foot. 
# https://www.canr.msu.edu/resources/weed_seedbank_dynamics_e2717



#--cc trt and sys as fixed, interaction is not sig so don't use it, use 'no outliers' data
m1 <- lmer(totseeds_m2 ~ cc_trt*cropsys + (1|loc) + (1|rep2), data = dat4)
summary(m1)
anova(m1)
m1noi <- lmer(totseeds_m2 ~ cc_trt + cropsys + (1|loc) + (1|rep2), data = dat4)
anova(m1, m1noi)
anova(m1noi)


difflsmeans(m1noi)


# make a ratio ------------------------------------------------------------
# include water hemp as a stand-alone, since people are real worried about it

datlnAM <- 
  datsp %>%
  filter(weed == "AMATU") %>%
  spread(cc_trt, value = seeds_m2) %>%
  mutate(ratAM = log(rye/no)) %>% 
  select(-weed)

datlnALL <- 
  datout %>%
  spread(cc_trt, value = totseeds_m2) %>%
  mutate(rat = (rye/no),
         lrat = log(rat))

datln <- 
  datlnAM %>%
  left_join(datlnALL, by = c("loc", "cropsys", "rep")) %>%
  select(loc, cropsys, rep, ratAM, rat) 



#--fit models w/ratio (outliers removed)
## I think this is my favorite....
# NOTE: only one obs for each rep, so can't include 'rep' in the model

# use a transformation in the actual model

mr1 <- lmer(log(rat) ~ cropsys + (1|loc), data = datlnALL)
summary(mr1)
mr1em <- emmeans(mr1, "cropsys") 
#summary(pigs.emm.s, infer = TRUE, null = log(35))
summary(mr1em, infer = T, type = "response", null = log(1))

confint(mr1em, level = .925, type = "response")

datlnALL %>% 
  ggplot(aes(cropsys, rat)) +
  geom_point(aes(color = loc)) +
  geom_hline(yintercept = 1)


#--should I include CC-biomass?

rbio <- read_csv("_data/tidy/td-all-ryebm2008-2019.csv") %>% 
  mutate(loc = location,
         cropsys = crop_sys) %>% 
  select(-location, -crop_sys)
datln

# Look at it --------------------------------------------------------


res1 %>%
  separate(loc_sys, into = c("loc", "sys"), remove = F) %>%
  
  ggplot(aes(reorder(loc_sys, estper), estper)) + 
  geom_point(aes(size = ccbio_mean)) + 
  geom_linerange(aes(ymin = ciloper, ymax = cihiper, x = loc_sys, color = sys), size = 2) + 
  geom_point(aes(size = ccbio_mean)) + 
  geom_hline(yintercept = 0) +
  coord_flip(ylim = c(-100, 100), xlim = c(1, 5)) + 
  scale_color_manual(values = c("gold2", "green4")) + 
  labs(x = NULL,
       y = "Percent Change in Germinable Weed Seeds",
       color = NULL,
       size = "Cover Crop Biomass (Mg/ha)\n10-year Mean") + 
  geom_text(aes(x = 4.8, y = -50), label = "Cover crops\nreduce weed seedbank") +
  geom_text(aes(x = 4.8, y = 50), label = "Cover crops\nincrease weed seedbank") + 
  theme_bw()


ggsave("_figs/resp-ratio-site.png") 


res2 %>%
  separate(loc_sys, into = c("loc", "sys"), remove = F) %>%
  
  ggplot(aes(reorder(loc_sys, estper), estper)) + 
  geom_point(aes(size = ccbio_Mgha)) + 
  geom_linerange(aes(ymin = ciloper, ymax = cihiper, x = loc_sys, color = sys), size = 2) + 
  geom_point(aes(size = ccbio_Mgha)) + 
  geom_hline(yintercept = 0) +
  coord_flip(ylim = c(-100, 100), xlim = c(1, 5)) + 
  scale_color_manual(values = c("gold2", "green4")) + 
  labs(x = NULL,
       y = "Percent Change in Germinable Weed Seeds",
       color = NULL,
       size = "Cover Crop Biomass (Mg/ha)\n10-year Mean") + 
  geom_text(aes(x = 4.8, y = -50), label = "Cover crops\nreduce weed seedbank") +
  geom_text(aes(x = 4.8, y = 50), label = "Cover crops\nincrease weed seedbank") + 
  theme_bw()


ggsave("_figs/resp-ratio-site.png") 




# Does the response have more to do with consistency? 
# Look at raw reductions rather than relative?
# Need to separate by species





# cover crop biomass stuff ------------------------------------------------


# I think it has to do with how quickly the biomass disappears. 
# More biomass means it disappears slowly. Water help germinates late. It needs soil temps of 24 C. 
# Can we estimate how fast the stuff decays?
# The rye needs to be suppressing weeds that germinate AFTER planting, bc you kill everything before planting
ccbio %>%
  ggplot(aes(year, ccbio_Mgha)) + 
  geom_point(aes(color = location, shape = crop_sys), size = 4) + 
  geom_line(aes(color = location, linetype = crop_sys))


ccbio1 <- 
  ccbio %>%
  group_by(location, crop_sys, cc_trt) %>%
  mutate(above1 = case_when(
    ccbio_Mgha > 1 ~ 1,
    TRUE ~ 0
  )) %>%
  summarise(ccbio_mean = mean(ccbio_Mgha,   na.rm = TRUE),
            ccbio_med  = median(ccbio_Mgha, na.rm = TRUE),
            ccbio_var  = sd(ccbio_Mgha,     na.rm = TRUE),
            ccbio_cv   = ccbio_var/ccbio_mean,
            ccbio_max  = max(ccbio_Mgha, na.rm = TRUE),
            nabove1 = sum(above1)) %>%
  mutate(locanon = recode(location,
                          boyd = "LocA",
                          funcke = "LocB",
                          stout = "LocC"),
         loc_sys = paste(locanon, crop_sys, sep = "_")) %>%
  ungroup()
# %>% # commenting out bc we not longer have ccbio_Mgha column
#  select(loc_sys, ccbio_Mgha)


# pca on weed abundance ---------------------------------------------------


# try pca (in the tidyverse style; https://tbradley1013.github.io/2018/02/01/pca-in-a-tidy-verse-framework/)
us_arrests_pca <- us_arrests %>% 
  nest() %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% select(-state), 
                                  center = TRUE, scale = F)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

var_exp <- us_arrests_pca %>% 
  unnest(pca_aug) %>% 
  summarize_at(.vars = vars(contains("PC")), .funs = funs(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

library(broom)
library(ggfortify)

# NOTE, I changed some column names so I am tweaking this
#datw %>% 
#  ungroup() %>%
#  select(-nmbr.m2) %>%
#  spread(weed, value = nmbr) %>%
datsp %>% 
  spread(weed, value = seeds_m2) %>% 
  nest(data = everything()) %>%
  mutate(pca = map(data, ~prcomp(.x %>% select(-loc, -cropsys, -cc_trt, -rep),
                                 center = T, scale = T)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y))) %>%
  unnest(pca_aug) %>%
  #summarise_at(.vars = vars(contains("PC")), .funs = funs(var)) %>%
  summarise_at(.vars = vars(contains("fittedPC")), .funs = list(~var(.))) %>%
  gather(key = pc, value = variance) %>%
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

# nmds indices on abundance ---------------------------------------

# Last updated: LE on 10/8/2019

# Each rep = a strip in the strip trial. First will keep them separate.

wide_datsp <- spread(datsp, key = weed, value = seeds_m2) %>%
  select(loc:rep, ABUTH:UG) %>%
  replace(., is.na(.), 0) %>%
  group_by(loc, cropsys, cc_trt, rep) %>%
  summarise_at(vars(ABUTH:UG), sum)

# consolidating columns into one name....
matrix_dat <- wide_datsp %>%
  unite(loc_trt_rep_id, loc, cropsys, cc_trt, rep, sep = "_", remove = TRUE) %>%
  # making into matrix
  column_to_rownames("loc_trt_rep_id")

library(vegan)  
mds_dat <- metaMDS(matrix_dat, distance = "bray", autotransform = FALSE, expand = FALSE)

plot(mds_dat, type = 't')
stressplot(mds_dat)

# want to link up another table 

dat.env <- wide_datsp %>%
  select(loc:rep) %>%
  unite(loc_trt, loc, cropsys, cc_trt, sep = "_", remove = FALSE)%>%
  #mutate(site = str_extract(loc_sys, "[A-z]+(?=_)"))%>%
  mutate(site = loc) %>% 
  mutate(crop = cropsys)
  #mutate(crop = str_extract(loc_sys, "(?<=_)[A-z]+"))

plot(mds_dat, type = 't')
ordihull(mds_dat, dat.env$site)
plot(mds_dat, type = 't')
ordihull(mds_dat, dat.env$loc_trt)

# seems like site matters more than treatment in terms of the weedy community...

## going to try and plot in ggplot
data.scores <- as.data.frame(scores(mds_dat)) %>%
  mutate(trt =  row.names(.)) %>%
  mutate(site = dat.env$site)%>%
  mutate(cc =   dat.env$cc_trt)%>%
  mutate(crop = dat.env$crop)

species.scores <- as.data.frame(scores(mds_dat, "species")) %>%
  mutate(species = rownames(.))

# I will figure out how to fix polygons later...for now it works at least! 
ggplot()+
  geom_polygon(data = data.scores, aes(NMDS1, NMDS2, fill = site, group = site), alpha = 0.3)+
  geom_text(data =  species.scores, aes(NMDS1, NMDS2, label = species), alpha = 0.5)+
  geom_point(data = data.scores, aes(NMDS1, NMDS2, colour = cc, shape = crop), size = 3)+
  #geom_text(data =  data.scores, aes(NMDS1, NMDS2, label = site))+
  theme_bw()

# Now combining reps - could average or do NMDS on relative abundances (pi)

# relative abundances of weeds...

weed_pi <- 
  wide_datsp %>%
  group_by(loc, cropsys, cc_trt)%>%
  summarize_if(is.numeric, sum) %>%
  select(-rep) %>%
  ungroup() %>%
  mutate(weed_tot = rowSums(.[4:21]))%>%
  mutate_at(vars(ABUTH:UG), ~ ./weed_tot)

weed_pi_mat <- weed_pi %>%
  unite(loc_trt, loc, cropsys, cc_trt, sep = "_", remove = TRUE) %>%
  column_to_rownames("loc_trt") %>%
  select(-weed_tot)

# NMDS doesn't work well with this few points - should be using metric methods
weed_pi_mds <- metaMDS(weed_pi_mat, distance = "bray", autotransform = FALSE, expand = FALSE)

# Instead trying metric scaling
weed_pi_bc <- vegdist(weed_pi_mat, method = "bray")
#PCoA
library(ape)
weed_pcoa <- ape::pcoa(sqrt(weed_pi_bc))
biplot(weed_pcoa)
# correspondance analysis?
weed_cca <- cca(weed_pi_mat)
plot(weed_cca, scaling = 1)

