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
p_blue <- "dodgerblue4"#"#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FAE549FD" #"#FFE100"
p_gray <- "#E7E6E6"
p_purp <- "#8B1C62"

scales::show_col(p_purp)

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
                 size = difflog)) + 
  geom_text(aes(label = diff_lab2),
            vjust = 2.5,
            color = "gray50",
            fontface = "italic",
            size = rel(3)) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c("thick" = "gray80", "thin" = "white")) +
  scale_color_manual(values = c("neg" = p_purp, "pos" = p_gray, "zero" = "gray90")) + 
  #scale_size_continuous(range = c(1, 7)) +
  #scale_size_area(max_size = 7) + #--this makes a value of 0 a 0 point, can't see anything
  guides(color = F, size = F, fill = F) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0),
        axis.text.y = element_text(face = c("plain", "plain", "bold", "bold"))) + 
  labs(x = NULL, y = NULL) 

ggsave("02_make-figs/figs/fig_dots-ind-weed-resp1.png", width = 7, height = 3)
ggsave("02_make-figs/manu/Fig3.jpg", width = 7, height = 3)
