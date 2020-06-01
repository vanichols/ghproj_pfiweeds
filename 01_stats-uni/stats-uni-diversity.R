# Gina
# 5/29/2020 based on Lydia's code

# ---- Getting data and loading packages ---- 
library(lme4) #--for mixed models
library(lmerTest) #--to get significances
library(vegan)
library(PFIweeds2020)
library(emmeans)
library(broom)


data("pfi_ghobsraw")
data("pfi_weedsplist")


dat <- 
  pfi_ghobsraw %>%
  group_by(site_name, sys_trt, cc_trt, blockID) %>%
  summarize_at(vars(AMATU:UB), ~sum(., na.rm = TRUE)) %>% 
  unite("site_sys", site_name, sys_trt, remove = FALSE) %>% 
  ungroup()


# ---- calculating richness and diversity ----
rich <- function(x){rowSums(ifelse(x > 0, 1, 0))}

div_rich <- 
  dat %>%
  mutate(shan_div = diversity(.[, 6:24]),
         shan_hill = exp(shan_div),
         richness  = rich(.[, 6:24]),
         evenness = shan_div/log(richness)) %>%
  select(-c(AMATU:UB))

write_csv(div_rich, "01_stats-uni/st_diversity-values.csv")

# ---- making long dataset ----

long_spp_list <- 
  dat %>%
  pivot_longer(cols = c(AMATU:UB), names_to = "code", values_to = "numSeedlings") %>%
  left_join(pfi_weedsplist, by = "code") %>%
  filter(numSeedlings > 0)
     
# ---- running univariate models ----

# i. diversity
# fixed effects model 
div1 <- lmer(shan_hill ~ site_sys*cc_trt + (1|blockID), data = div_rich)
anova(div1)
div1_stats <- emmeans(div1, specs = pairwise ~ cc_trt|site_sys)$contrasts %>% tidy() %>% mutate(metric = "shan_hill")

div_rich %>%
  ggplot(aes(site_sys, shan_hill)) +
  geom_boxplot(aes(color = cc_trt))


# ii. richness

rich1 <- lmer(richness ~ site_sys*cc_trt + (1|blockID), data = div_rich)
anova(rich1)   # significant interaction term since richness increased for Funcke and Stout
emmeans(rich1, pairwise ~ cc_trt|site_sys, type = "response")
# interestingly boyd richness is significanly lower in silage treatment...
rich1_stats <- emmeans(rich1, specs = pairwise ~ cc_trt|site_sys)$contrasts %>% tidy() %>% mutate(metric = "richness")


# iii. evenness

e1 <- lmer(evenness ~ site_sys*cc_trt + (1|blockID), data = div_rich)
anova(e1)   # significant interaction term since richness increased for Funcke and Stout
emmeans(e1, pairwise ~ cc_trt|site_sys, type = "response")
# interestingly boyd richness is significanly lower in silage treatment...
even1_stats <- emmeans(e1, specs = pairwise ~ cc_trt|site_sys)$contrasts %>% tidy() %>% mutate(metric = "evenness")


div_rich %>%
  ggplot(aes(site_sys, evenness))+
  geom_boxplot(aes(color = cc_trt))


stats_contrasts <- div1_stats %>% bind_rows(rich1_stats) %>% bind_rows(even1_stats)


stats_contrasts %>% write_csv("01_stats-uni/st_diversity-contrasts.csv")
