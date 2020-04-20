##################################
# Author: Lydia English (lenglish@iastate.edu)
# Created: 4/20/2020
#
# Last modified: 
#
# Purpose: do 'official' diveristy/species specific calculations
#
#
# Notes:
####################################

library(tidyverse)
library(PFIweeds2020)
library(vegan)
library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(emmeans)
mycols <- c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02")

data("pfi_ghobsraw")


# ---- Univariate stat measures -----

# function to calculate species richness
rich <- function(x){rowSums(ifelse(x > 0, 1, 0))}

div_dat <- pfi_ghobsraw %>% 
  # same code as from `pfifun_sum_weedbyeu` except I'm not making df long
  dplyr::mutate_if(is.numeric, tidyr::replace_na, 0) %>%
  dplyr::group_by(site_name, field, sys_trt, cc_trt, rep, blockID) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  unite(site_name, sys_trt, col = "site_sys", remove = T) %>% 
  ungroup() %>%
  # calculate diveristy and spp richness
  mutate(shan_div  = diversity(.[, 6:23]),
         richness  = rich(.[, 6:23]),
         shan_hill = exp(shan_div),
         evenness  = shan_div/log(richness))

# model diversity, richness, and evenness

# 1. diversity

# first look at data
div_dat %>%
  ggplot(aes(site_sys, shan_hill))+
  geom_boxplot(aes(color = cc_trt))

div_dat %>%
  ggplot(aes(shan_hill))+
  geom_histogram()

d1 <- lmer(shan_hill ~ site_sys * cc_trt + (1|blockID), div_dat)
d2 <- lmer(shan_hill ~ site_sys + cc_trt + (1|blockID), div_dat)
anova(d1, d2) # get rid of interaction
summary(d2)
anova(d2) # mmmm doesn't look that interesting...
ggResidpanel::resid_panel(d2) # wow this looks bad! I don't know what to do about it....?

# 2. richness

div_dat %>%
  ggplot(aes(site_sys, richness))+
  geom_boxplot(aes(color = cc_trt))

r1 <- lmer(richness ~ site_sys * cc_trt + (1|blockID), div_dat)
r2 <- lmer(richness ~ site_sys + cc_trt + (1|blockID), div_dat)
anova(r1, r2) # keep interaction
# hmm singular fit? Maybe not a lot of variation across block...
summary(r1)
ggResidpanel::resid_panel(r1)

r1b <- glmer(richness ~ site_sys * cc_trt + (1|blockID), data = div_dat,
             family = poisson(link = "log"))
summary(r1b)
performance::check_overdispersion(r1b) # poisson ok! 

ggResidpanel::resid_compare(list(r1, r1b))

r1c <- lmer(log(richness) ~ site_sys * cc_trt + (1|blockID), div_dat)
r1d <- lmer(sqrt(richness)~ site_sys * cc_trt + (1|blockID), div_dat)

# hmmm I feel like log fits the best?
ggResidpanel::resid_compare(list(r1, r1b, r1c, r1d))

# compare the AICs of all the lmers?
performance::compare_performance(r1, r1c, r1d) # transformations are equally good...

# Going to go with r1c, for now...
# should I not account for block since there isn't much variation anyways?
r1c2 <- lm(log(richness) ~ site_sys*cc_trt, div_dat) 
summary(r1c) # same estimates and p-values, to be expected

r_means <- emmeans(r1c, pairwise ~ cc_trt|site_sys, type = "response")
r_means

# Funcke has response we'd imagine, rye increases richness of weeds
# Boyd silage has the opposite response, rye decreases the richness of weeds (because 
# it's the only one that lowers the number of weeds?)

# 3. evenness

div_dat %>%
  ggplot(aes(site_sys, evenness))+
  geom_boxplot(aes(color = cc_trt))

e1 <- lmer(evenness ~ site_sys * cc_trt + (1|blockID), div_dat)
summary(e1)

# with a logit transformation
e1b <- lmer(car::logit(evenness) ~ site_sys * cc_trt + (1|blockID), div_dat)
summary(e1b)

ggResidpanel::resid_compare(list(e1, e1b)) # fairly comprable...

# 4. # of waterhemp seeds

div_dat %>%
  ggplot(aes(site_sys, log(AMATU)))+
  geom_boxplot(aes(color = cc_trt))

a1 <- lmer(AMATU ~ site_sys * cc_trt + (1|blockID), div_dat)
a1b <- lmer(AMATU ~ site_sys + cc_trt + (1|blockID), div_dat)
anova(a1, a1b)  # get rid of interaction
summary(a1b)

ggResidpanel::resid_panel(a1b) # ooof looks bad

# try glmer
a1c <- glmer(AMATU ~ site_sys + cc_trt + (1|blockID), data = div_dat,
             family = poisson(link = "log"))
ggResidpanel::resid_panel(a1c)
performance::check_overdispersion(a1c) # overdispersed

a1d <- glmer.nb(AMATU ~ site_sys + cc_trt + (1|blockID), data = div_dat)
ggResidpanel::resid_panel(a1d) # does this look ok?....
summary(a1d)

# try log transform
div_dat <- 
  div_dat %>%
  mutate(AMATU = ifelse(AMATU == 0, 0.01, AMATU))

a1log <- lmer(log(AMATU) ~ site_sys + cc_trt + (1|blockID), div_dat)
summary(a1log)

a1log_means <- emmeans(a1log, ~ cc_trt|site_sys, type = "response")
a1log_means

# ---- Multivariate, NMDS ----------------

# option 1: going to combine fields B42 and B44 in Boyd_grain

env1 <- div_dat %>%
  group_by(site_sys, cc_trt, rep) %>%
  summarize_if(is.numeric, sum) %>%
  unite(site_sys, cc_trt, rep, col = "ID", remove = FALSE) %>%
  select(ID, site_sys, cc_trt, rep)

mat1 <- div_dat %>%
  group_by(site_sys, cc_trt, rep) %>%
  summarize_if(is.numeric, sum) %>%
  unite(site_sys, cc_trt, rep, col = "ID") %>%
  column_to_rownames("ID") %>%
  select(AMATU:UB)

nmds1 <- metaMDS(mat1, distance='bray',autotransform=F, expand=F)
# stress = 0.067, ok!
plot(nmds1)

# ggplot2

site_scores_1 <- as.data.frame(scores(nmds1)) %>%
  rownames_to_column(.) %>%
  # add in columns with useful info...
  left_join(env1, by = c("rowname" = "ID"))

spp_scores_1  <- as.data.frame(scores(nmds1, "species")) %>%
  rownames_to_column(., var = "speciesID")

# Makes polygons for site by treatment
hull_1 <- site_scores_1 %>% # dataframe of site scores
  unite("treatment", site_sys, cc_trt, remove = FALSE) %>%
  group_by(treatment) %>% # grouping variables: farm AND treatmnet
  slice(chull(NMDS1, NMDS2)) # points that polygons will connect

# nice plot
ggplot()+
  geom_point(data = site_scores_1, 
             aes(x = NMDS1, y = NMDS2, color = site_sys, shape = cc_trt)) +
  geom_text(data = spp_scores_1, 
            aes(x = NMDS1, y = NMDS2, label = speciesID), alpha = 0.5) + # Species as text - better!
  geom_polygon(data = hull_1, aes(x = NMDS1, y = NMDS2, fill = treatment), alpha = 0.5)+ # adding polygons from hull df
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

# option 2 - keep B44 and B42 separate? Will try again another time...






