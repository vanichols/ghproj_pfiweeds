##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 30 2019
# Last modified: Jan 8 2020 - Lydia trying to run things (see note)...
#                April 28 2020 - blowing things up and starting over....
#
# Purpose: make manuscript figs
#
# Inputs: 
#
# Outputs: 
#
# Notes: 
#
####################################

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)


# fig things --------------------------------------------------------------


cctrtpal <- c("darkolivegreen3", "lightsalmon4")

labseedsm2 = expression('Weed Seeds\n (1000s m'^"-2)")


# change in seedbank size -------------------------------------------------

# vision: bar graph on left, table on right with % and raw reductions?

sb_est <- read_csv("data/smy/sd_estimates.csv") %>% 
  filter(model == "pois")


fig_sb <- 
  sb_est %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"),
         cc_trt = recode(cc_trt, 
                         "no" = "None",
                         "rye" = "Rye Cover Crop")) %>%  
  ggplot(aes(reorder(site_id, -totseeds_m2, mean), totseeds_m2/1000, fill = cc_trt)) +
  geom_col(position = position_dodge(width = 0.9), 
           color = "black", size = 1.2) +
  geom_linerange(position = position_dodge(width = 0.9),
                 aes(ymin = se_lo/1000, ymax = se_hi/1000)) +
    labs(y = labseedsm2,
         x = NULL,
         fill = NULL, 
         color = NULL) +
    scale_fill_manual(values = c("None" = cctrtpal[2],
                                 "Rye Cover Crop" = cctrtpal[1])) +
    scale_color_manual(values = cctrtpal) +
    theme_bw() +
    theme(#legend.direction = "horizontal",
          #legend.position = "bottom",
          legend.justification = c(1, 1),
          legend.position = c(0.9, 0.9),
          legend.background = element_blank(),
          legend.key.width = unit(1.4, "cm"),
          legend.key.height = unit(0.5, "cm"),
          legend.key.size = unit(1, "cm"),
          axis.title.y = element_text(angle = 0, vjust = 0.5))
  
fig_sb
ggsave("figs/manu/fig1_bar-totseeds.png")

# table -------------------------------------------------------------------

sb_pvals <- read_csv("data/smy/sd_contrasts.csv") %>% 
    filter(model == "pois") %>% 
    select(site_sys, p.value)
  
library(gt)
  
dat_tbl <- 
  sb_est %>% 
    select(site_sys, cc_trt, totseeds_m2) %>% 
    pivot_wider(names_from = cc_trt,
                values_from = totseeds_m2) %>% 
    #--rename sites
    mutate(site_id = recode(site_sys,
                            "Boyd_grain" = "Central1",
                            "Boyd_silage" = "Central2",
                            "Funcke_grain" = "West",
                            "Stout_grain" = "East"),
           site_id = factor(site_id, levels = c("West", "Central2", "Central1", "East"))) %>% 
    mutate(Seeds = round(no - rye, 0),
           pct = paste0(round(Seeds/no * 100, 0), "%")) %>% 
    left_join(sb_pvals) %>% 
    mutate(p.value = round(p.value, 2)) %>% 
    select(-rye, -no, -site_sys) %>% 
  arrange(site_id)


tbl_sb <- 
  dat_tbl %>% 
  gt() %>% 
    tab_header(
      title = "Summary") %>%
    tab_spanner(
      label = "Reductions in Seed Bank",
      columns = vars(Seeds, pct)
    ) %>% 
    cols_label(
      Seeds = md("*Seeds*"),
      site_id = " ",
      pct = md("*Percent*"),
      p.value = md("*P-value*")
    ) %>% 
  cols_align(
    align = "center"
  )

gtsave(tbl_sb, "figs/manu/fig1_table-changes.png")
