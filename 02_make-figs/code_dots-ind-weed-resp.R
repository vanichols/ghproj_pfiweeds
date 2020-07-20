##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 5/13/2020
#
# Purpose: look at ind weed responses, see which one is 'driving' overall resp
#
# Outputs: 
#
# Notes: something isn't adding up in West...
#
# Last modified: 6/1/2020 (try log of seeds? some points are so tiny)
#
####################################

rm(list = ls())
library(tidyverse)
library(PFIweeds2020)


p_green <- "#619B44"
p_blue <- "#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FFC000"
p_gray <- "#E7E6E6"


# data --------------------------------------------------------------------

#--how many species were identified?
pfi_weedsplist %>% 
  filter(!is.na(scientific_name)) %>% 
  count()


#--use data and fucntion from package

pfi_ghobsraw

wdat <- pfifun_sum_weedbyeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = T) %>% 
  filter(seeds < 750) #--funcke point

adat <- pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = T) %>% 
  filter(totseeds < 750) #--funcke point


# abs diffs -----------------------------------------------------------------

adiff <- 
  adat %>% 
  group_by(site_sys, cc_trt) %>% 
  summarise(mtot_m2 = mean(totseeds_m2)) %>% 
  pivot_wider(names_from = cc_trt, values_from = mtot_m2) %>% 
  mutate(diff_ryetono = rye - no) %>% 
  mutate(weed = "Overall")


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
                         ifelse(diff_ryetono > 0, "pos", "zero"))) %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central",
                          "Boyd_silage" = "Central (Silage)",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"),
         site_id = factor(site_id, levels = rev(c("West", "Central (Silage)", "Central", "East"))),
         site_id2 = factor(site_id, levels = (c("West", "Central (Silage)", "Central", "East"))))


#--get a factor order
# myorder <- 
#   wdiff %>% 
#   group_by(weed) %>% 
#   summarise(mdiff = abs(mean(diff_ryetono, na.rm = T))) %>%
#   arrange(-mdiff) %>% 
#   pull(weed)

myorder <- 
  read_csv("02_make-figs/mf_weed-list-arranged.csv") %>% 
  pull(code)

#--weedss on top

diff %>% 
  mutate(weed = factor(weed, levels = c("Overall", myorder)),
         thick_id = ifelse(weed == "Overall", "thick", "thin"),
         difflog = log(abs(diff_ryetono)),
         diff_lab = round(diff_ryetono, 0),
         diff_lab2 = ifelse(diff_lab == 0, paste(""), diff_lab)) %>% 
  ggplot(aes(weed, site_id)) + 
  geom_tile(color = "black", aes(fill = thick_id)) +
  geom_point(aes(color = clr_id, 
                 #size = abs(diff_ryetono),
                 size = difflog
                 )) + 
  geom_text(aes(label = diff_lab2),
            vjust = 2.5,
            color = "gray50",
            fontface = "italic",
            size = rel(3)) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c("thick" = "gray80", "thin" = "white")) +
  scale_color_manual(values = c("neg" = p_orange, "pos" = p_gray, "zero" = "gray90")) + 
  #scale_size_continuous(range = c(1, 7)) +
  #scale_size_area(max_size = 7) + #--this makes a value of 0 a 0 point, can't see anything
  guides(color = F, size = F, fill = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0),
        axis.text.y = element_text(face = c("plain", "plain", "bold", "bold"))) + 
  labs(x = NULL, y = NULL) 

ggsave("02_make-figs/figs/fig_dots-ind-weed-resp1.png", width = 7, height = 3)

##--try coord flipped

diff %>% 
  mutate(weed = factor(weed, levels = c(rev(myorder), "Overall")),
         thick_id = ifelse(weed == "Overall", "thick", "thin"),
         difflog = log(abs(diff_ryetono)),
         diff_ryetono2 = ifelse(diff_ryetono == 0, " ", round(diff_ryetono))) %>% 
  ggplot(aes(site_id2, weed)) + 
  geom_tile(color = "black", aes(fill = thick_id)) +
  geom_point(aes(color = clr_id, 
                 size = abs(diff_ryetono),
                 #size = difflog
                 ),
             #alpha = 0.5
             ) + 
  geom_text(aes(label = diff_ryetono2, color = clr_id),
            vjust = 1.5,
            color = "gray70",
            fontface = "italic",
            size = rel(3.5)) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c("thick" = "gray80", "thin" = "white")) +
  scale_color_manual(values = c("neg" = p_pink, "pos" = p_green, "zero" = "gray90")) + 
  #scale_size_continuous(range = c(1, 7)) +
  scale_size(range = c(1, 9)) + #--this makes a value of 0 a 0 point, can't see anything
  guides(color = F, size = F, fill = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0)) + 
  labs(x = NULL, y = NULL) 

ggsave("02_make-figs/figs/fig_dots-ind-weed-resp2.png", width = 3, height = 7)


##--try different dot sizes

diff %>% 
  mutate(weed = factor(weed, levels = c(rev(myorder), "Overall")),
         thick_id = ifelse(weed == "Overall", "thick", "thin"),
         difflog = log(abs(diff_ryetono)),
         diff_ryetono2 = ifelse(diff_ryetono == 0, " ", round(diff_ryetono))) %>% 
  ggplot(aes(site_id2, weed)) + 
  geom_tile(color = "black", aes(fill = thick_id)) +
  geom_point(aes(color = clr_id, 
                 #size = (diff_ryetono),
                 size = diff_ryetono),
             alpha = 0.5,
             stroke = 2) + 
  geom_text(aes(label = diff_ryetono2),
            #vjust = 0,
            color = "black",
            fontface = "italic",
            size = rel(3.5)) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c("thick" = "gray80", "thin" = "white")) +
  scale_color_manual(values = c("neg" = p_pink, "pos" = p_green, "zero" = "gray90")) + 
  #scale_size_continuous(range = c(1, 7)) +
  scale_size_area(max_size = 5) + #--this makes a value of 0 a 0 point, can't see anything
  guides(color = F, size = F, fill = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0)) + 
  labs(x = NULL, y = NULL) 

ggsave("02_make-figs/figs/fig_dots-ind-weed-resp3.png", width = 3, height = 7)





#--old drafts

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
