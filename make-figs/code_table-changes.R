##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: may 13 2020
# Purpose: make manuscript table
#
# Last modified: 
#
#
####################################

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gt)

# table -------------------------------------------------------------------

sb_pvals <- read_csv("data/smy/sd_contrasts.csv") %>% 
    filter(model == "pois") %>% 
    select(site_sys, p.value)
  

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

gtsave(tbl_sb, "make-figs/fig1_table-changes.png")
