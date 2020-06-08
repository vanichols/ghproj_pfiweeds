##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: may 13 2020
# Purpose: make manuscript table
#
# Last modified: 6/2/2020 (these values seems wrong, need to redo and include CIs)
#
#
####################################

library(PFIweeds2020)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gt)


# raw stat output ---------------------------------------------------------

sb_contr <- read_csv("01_stats-uni/st_weedseed-contr.csv")
sb_est <- read_csv("01_stats-uni/st_weedseed-est.csv") 


# convert it --------------------------------------------------------------
#--this still isn't right.

sb_est %>%
  mutate(totseeds = exp(estimate),
         totseeds_m2 = pfifun_seedstom2conv(totseeds)) %>% 
  select(site_sys, cc_trt, totseeds_m2) %>% 
  pivot_wider(names_from = cc_trt,
              values_from = totseeds_m2) %>% 
  mutate(Seeds = -round(no - rye, 0),
         pct = paste0(round(Seeds/no * 100, 0), "%"))

  
  
  #--rename sites
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2 (Silage)",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"),
         site_id = factor(site_id, levels = c("West", "Central2 (Silage)", "Central1", "East"))) %>% 
  mutate(Seeds = -round(no - rye, 0),
         pct = paste0(round(Seeds/no * 100, 0), "%"))

# table -------------------------------------------------------------------


dat_tbl <- 
  sb_est %>% 
    %>% 
    left_join(sb_pvals) %>% 
    mutate(p.value = round(p.value, 2)) %>% 
  mutate(est_pct = (1 - exp(estimate))*100)
    select(-rye, -no, -site_sys) %>% 
  arrange(site_id)


tbl_sb <- 
  dat_tbl %>% 
  gt() %>% 
    tab_header(
      title = "Summary of Cover Crop Effects") %>%
    tab_spanner(
      label = "Changes in Seed Banks",
      columns = vars(Seeds, pct)
    ) %>% 
    cols_label(
      Seeds = md("*Seeds m<sup>-2*"),
      site_id = " ",
      pct = md("*Percent*"),
      p.value = md("*P-value*")
    ) %>% 
  cols_align(
    align = "center"
  )

tbl_sb

gtsave(tbl_sb, "make-figs/fig1_table-changes.png")
