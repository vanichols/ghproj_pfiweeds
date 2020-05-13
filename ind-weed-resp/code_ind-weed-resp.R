##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 5/13/2020
#
# Purpose: look at ind weed responses, see which one is 'driving' overall resp
#
# Outputs: 
#
# Notes:
#
# Last modified:
#
####################################

rm(list = ls())
library(tidyverse)
#devtools::install_github("vanichols/PFIweeds2020", force = TRUE) ## <-- run if pkg changes
library(PFIweeds2020)


# data --------------------------------------------------------------------

#--use data and fucntion from package

pfi_ghobsraw

wdat <- pfifun_sum_weedbyeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = T)

adat <- pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = T)


# abs diffs -----------------------------------------------------------------

adiff <- 
  adat %>% 
  group_by(site_sys, cc_trt) %>% 
  summarise(mtot_m2 = mean(totseeds_m2)) %>% 
  pivot_wider(names_from = cc_trt, values_from = mtot_m2) %>% 
  mutate(diff_ryetono = rye - no) %>% 
  mutate(weed = "ALL")


wdiff <- 
  wdat %>% 
  group_by(site_sys, cc_trt, weed) %>% 
  summarise(mtot_m2 = mean(seeds_m2)) %>% 
  pivot_wider(names_from = cc_trt, values_from = mtot_m2) %>% 
  mutate(diff_ryetono = rye - no)


diff <- 
  adiff %>% 
  bind_rows(wdiff) %>% 
  mutate(clr_id = ifelse(diff_ryetono < 0, "neg", 
                         ifelse(diff_ryetono >0, "pos", "zero")))

#--get a factor order
myorder <- 
  wdiff %>% 
  group_by(weed) %>% 
  summarise(mdiff = abs(mean(diff_ryetono, na.rm = T))) %>%
  arrange(-mdiff) %>% 
  pull(weed)

diff %>% 
  mutate(weed = factor(weed, levels = c("ALL", myorder))) %>% 
  ggplot(aes(site_sys, diff_ryetono)) + 
  geom_col(aes(fill = clr_id)) +
  scale_fill_manual(values = c("neg" = "red", "pos" = "green4", "zero" = "gray70")) + 
  guides(fill = F) +
  facet_grid(site_sys~weed) 


diff %>% 
  mutate(weed = factor(weed, levels = c("ALL", myorder))) %>% 
  ggplot(aes(weed, site_sys)) + 
  geom_point(aes(color = clr_id, size = abs(diff_ryetono))) + 
  scale_x_discrete(position = "top") +
  scale_color_manual(values = c("neg" = "red", "pos" = "green4", "zero" = "gray90")) + 
  theme(axis.text.x = element_text(angle = 45))


diff %>% 
  mutate(weed = factor(weed, levels = c("ALL", myorder))) %>% 
  ggplot(aes(weed, site_sys, fill = diff_ryetono)) + 
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "darkred",
                       midpoint = 0,
                       mid = "white",
                      high = "green4",
                      na.value = "gray90") + 
  scale_x_discrete(position = "top") +
  theme(axis.text.x = element_text(angle = 45)) + 
  theme_minimal()

#--weedss on top
diff %>% 
  filter(weed != "ALL") %>% 
  mutate(weed = factor(weed, levels = c(myorder))) %>% 
  ggplot(aes(weed, site_sys)) + 
  geom_tile(color = "black", fill = "white") +
  geom_point(aes(color = clr_id, size = abs(diff_ryetono))) + 
  geom_text(aes(label = round(diff_ryetono, 0)),
            vjust = 2.5,
            color = "gray50",
            fontface = "italic",
            size = rel(3)) +
  scale_x_discrete(position = "top") +
  scale_color_manual(values = c("neg" = "red", "pos" = "green4", "zero" = "gray90")) + 
  guides(color = F, size = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0, vjust = -1)) + 
  labs(x = NULL, y = NULL) 



# relative diffs -----------------------------------------------------------------
#--this is stupid and useless

adiff_pct <- 
  adat %>% 
  group_by(site_sys, cc_trt) %>% 
  summarise(mtot_m2 = mean(totseeds_m2)) %>% 
  pivot_wider(names_from = cc_trt, values_from = mtot_m2) %>% 
  mutate(diff_ryetono_pct = (rye - no)/no * 100) %>% 
  mutate(weed = "ALL")


wdiff_pct <- 
  wdat %>% 
  group_by(site_sys, cc_trt, weed) %>% 
  summarise(mtot_m2 = mean(seeds_m2)) %>% 
  pivot_wider(names_from = cc_trt, values_from = mtot_m2) %>% 
  mutate(diff_ryetono_pct = (rye - no)/no * 100)


diff_pct <- 
  adiff_pct %>% 
  bind_rows(wdiff_pct) %>% 
  mutate(clr_id = ifelse(diff_ryetono_pct < 0, "neg", "pos"))

#--get a factor order
myorder <- 
  wdiff_pct %>%
  filter(!is.nan(diff_ryetono_pct),
         !is.infinite(diff_ryetono_pct)) %>% 
  group_by(weed) %>% 
  summarise(mdiff = abs(mean(diff_ryetono_pct, na.rm = T))) %>%
  arrange(-mdiff) %>% 
  pull(weed)

diff %>% 
  filter(weed %in% myorder) %>% 
  mutate(weed = factor(weed, levels = c("ALL", myorder))) %>% 
  ggplot(aes(site_sys, diff_ryetono_pct)) + 
  geom_col(aes(fill = clr_id)) +
  scale_fill_manual(values = c("neg" = "red", "pos" = "green4")) + 
  guides(fill = F) +
  facet_wrap(~weed) 
