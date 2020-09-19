##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 9/1/2020
#
# Purpose: make figure of weed abundance table
#
# Notes: 
# Last modified: 
#
####################################

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

library(PFIweeds2020)
library(ggpubr)
library(cowplot)


# weed lists ---------------------------------------------------------------

#--created in code_tbl-weed-list

dat_table <- read_csv("02_make-figs/mf_weed-list-arranged.csv")

dat_table_trial <- read_csv("02_make-figs/mf_weed-list-arranged-by-trial.csv")


# raw values, geom_tile ---------------------------------------------------

dat_by_trt_raw <- 
  pfi_ghobsraw %>% 
      pfifun_sum_weedbyeu() %>% 
      unite(site_name, field, sys_trt, col = "site_sys") %>% 
      select(site_sys, cc_trt, weed, seeds) %>%
      group_by(site_sys, cc_trt, weed) %>% 
      summarise(weed_trt_tot = sum(seeds)) %>% 
      group_by(site_sys, cc_trt) %>% 
      mutate(tot_trt = sum(weed_trt_tot),
             pct = round(weed_trt_tot/tot_trt*100, 2)) %>% 
  #--make things anonymous
  mutate(site_sys2 = case_when(
    site_sys == "Central_B42_grain" ~ "Central Grain_Maize",
    site_sys == "Central_B44_grain" ~ "Central Grain_Soybean",
    site_sys == "Central_B44_silage" ~ "Central Silage_Soybean",
    site_sys == "West_F_grain" ~ "West Grain_Soybean",
    site_sys == "East_S_grain" ~ "East Grain_Maize",
    site_sys == "overall" ~ "Overall_ ")
  ) %>%
  #--get sites/residues
  separate(site_sys2, into = c("site_sys2", "residue"), sep = "_") %>% 
  mutate(site_sys2 = factor(site_sys2, levels = c("Overall", "West Grain", "Central Silage", "Central Grain", "East Grain"))) %>%
  ungroup() %>% 
  mutate(cc_trt = recode(cc_trt,
                  "no" = "No Cover",
                  "rye" = "Winter Rye")) %>% 
  #--get scientific names
  left_join(pfi_weedsplist %>% rename("weed" = "code") %>% select(weed, scientific_name)) %>% 
  #--define order of weeds
  mutate(scientific_name = factor(scientific_name, levels = (dat_table$scientific_name))) %>% 
  #--make 0s NAs
  mutate(weed_trt_tot = ifelse(weed_trt_tot==0, NA, weed_trt_tot))


#--do them individually and patchwork them
dat_by_trt_raw %>% 
  ggplot(aes(cc_trt, scientific_name)) + 
  geom_tile(aes(fill = log(weed_trt_tot))) + 
  scale_fill_viridis_c(option = "cividis") +
  facet_grid(.~site_sys2+residue, scales = "free") + 
  theme(legend.key.size = unit(0.8, "lines"))



# patchwork them ----------------------------------------------------------

legend_title <- bquote(Seeds)

thesites <- 
  dat_by_trt_raw %>% 
  select(site_sys) %>% 
  distinct() %>% 
  pull() 


thesites <- c("West_F_grain",
    "Central_B44_silage",
    "Central_B44_grain",
    "Central_B42_grain",
    "East_S_grain")
  
thesites[1]


plot_fun_no_axis <- function(fun_site_sys = "Central_B42_grain", left_axis = "no"){
  
  #fun_site_sys <- "Central_B42_grain"
  #left_axis <- "no"
  
  plot_dat <- 
      dat_by_trt_raw %>% 
      filter(site_sys == fun_site_sys)
  
  plot_tmp <- 
    plot_dat %>% 
    ggplot(aes(cc_trt, scientific_name)) + 
    geom_tile(aes(fill = weed_trt_tot), color = "white") + 
    scale_fill_viridis_c(
      option = "plasma",
      guide = guide_colorbar(
        title = legend_title,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    geom_vline(xintercept = 1.5, color = "white") +
    facet_grid(.~site_sys2+residue, scales = "free") +
    theme_minimal() + 
    labs(x = NULL, y = NULL)
  
  if(left_axis == "no") 
  new_plot <- 
    plot_tmp +
    theme(legend.position = "top",
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          strip.text = element_text(face = "bold"),
          legend.key.size = unit(0.8, "lines"),
          legend.title = element_text(size = rel(0.8)),
          legend.text = element_text(size = rel(0.7)))
  else
  new_plot <- 
     plot_tmp +
    scale_fill_viridis_c(
      option = "plasma",
      breaks = c(400, 1200),
      guide = guide_colorbar(
        title = legend_title,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    theme(legend.position = "top",
           axis.text.y = element_text(face = "italic"),
           axis.text.x = element_text(angle = 45, vjust = 0.5),
           strip.text = element_text(face = "bold"),
           legend.key.size = unit(0.8, "lines"),
           legend.title = element_text(size = rel(0.9)),
           legend.text = element_text(size = rel(0.7))) + 
    labs(x = NULL, y = NULL)
  
  return(new_plot)
  
}

#--practice
plot_fun_no_axis(thesites[1], left_axis = "yes")
plot_fun_no_axis(thesites[1], left_axis = "no")

#--make plots
p1 <- plot_fun_no_axis(thesites[1], left_axis = "yes")
p2 <- plot_fun_no_axis(thesites[2], left_axis = "no")
p3 <- plot_fun_no_axis(thesites[3], left_axis = "no")
p4 <- plot_fun_no_axis(thesites[4], left_axis = "no")
p5 <- plot_fun_no_axis(thesites[5], left_axis = "no")

plot_grid(p1, p2, p3, p4, p5, 
          rel_widths = c(0.48, 0.25, 0.25, 0.25, 0.25),
          nrow = 1)


ggsave("02_make-figs/manu-new/fig2.jpg")

