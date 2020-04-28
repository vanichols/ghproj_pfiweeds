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
# Notes: Where are the dats and div datasets coming from??? I can't run those figures in this script... (from Lydia )
# ^ Perhaps it's nbd and you don't want those figures anyways but I'm just curious. 
#
####################################

library(dplyr)
library(ggplot2)
library(readxl)
library(ggpubr)
library(wesanderson)
library(patchwork)
library(maps)


# fig things --------------------------------------------------------------


mypal <- c("royalblue", (wes_palette("Zissou1", n = 5)[c(2,3,5)]))

mytheme1 <- theme(axis.text = element_text(size = rel(1.2)),
                 legend.text = element_text(size = rel(1.3)),
                 axis.title = element_text(size = rel(1.3)))


mytheme2 <- theme(legend.position = c(0.1, 0.9),
                 legend.justification = c(0,1),
                 legend.background = element_rect(color = "black"),
                 axis.text = element_text(size = rel(1.2)),
                 legend.text = element_text(size = rel(1.3)),
                 axis.title = element_text(size = rel(1.3)))


cctrtpal <- c("darkolivegreen3", "lightsalmon4")

labseedsm2 = expression('Weed Seeds m'^"-2")

# change in seedbank size -------------------------------------------------

# vision: bar graph on left, table on right with % and raw reductions?

sb_est <- read_csv("data/smy/sd_estimates.csv")


#fig_sb <- 
  sb_est %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"),
         cc_trt2 = recode(cc_trt2, 
                         "none" = "None",
                         "aryecc" = "Rye Cover Crop")) %>%  
  ggplot(aes(reorder(site_id, -totseeds_m2, mean), totseeds_m2, fill = cc_trt2)) +
  geom_col(position = position_dodge(width = 0.9), 
           color = "black", size = 1.2) +
  geom_linerange(position = position_dodge(width = 0.9),
                 aes(ymin = conf.low, ymax = conf.high)) +
    labs(y = labseedsm2,
         x = NULL,
         fill = NULL, 
         color = NULL) +
    scale_fill_manual(values = c("None" = cctrtpal[2],
                                 "Rye Cover Crop" = cctrtpal[1])) +
    scale_color_manual(values = cctrtpal) +
    theme_bw() +
    theme(legend.direction = "horizontal",
          legend.position = "bottom",
#          legend.justification = c(1, 1),
          axis.title.y = element_text(angle = 90, vjust = 0.5))
  

# table -------------------------------------------------------------------

  sb_pvals <- read_csv("data/smy/sd_contrasts.csv") %>% 
    select(-mod)
  
  library(gt)
  
  sb_est %>% 
    select(site_sys, cc_trt2, totseeds_m2) %>% 
    pivot_wider(names_from = cc_trt2,
                values_from = totseeds_m2) %>% 
    mutate(Seeds = none - aryecc,
           `%` = Seeds/none * 100) %>% 
    left_join(sb_pvals) %>% 
    select(-aryecc, -none) %>% 
    gt() %>% 
    tab_header(
      title = "Summary") %>%
    tab_spanner(
      label = "Reductions in Seed Bank",
      columns = vars(Seeds, `%`)
    ) 
  
    
    
    
    airquality_m <-
    airquality %>%
    mutate(Year = 1973L) %>%
    slice(1:10)
  
  # Create a display table using the `airquality`
  # dataset; arrange columns into groups
#  gt_tbl <-
    gt(data = airquality_m) %>%
    tab_header(
      title = "New York Air Quality Measurements",
      subtitle = "Daily measurements in New York City (May 1-10, 1973)"
    ) %>%
    tab_spanner(
      label = "Time",
      columns = vars(Year, Month, Day)
    ) %>%
    tab_spanner(
      label = "Measurement",
      columns = vars(Ozone, Solar.R, Wind, Temp)
    )
  