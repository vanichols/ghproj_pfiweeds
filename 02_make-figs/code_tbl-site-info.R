# Gina
# 5/29/2020

library(tidyverse)
library(ggrepel)
library(gt)
library(PFIweeds2020)


ccbio <- 
  pfi_mccbio %>% 
  group_by(site_name, sys_trt) %>% 
  summarise(mccbio_Mgha = mean(mccbio_Mgha))

skel <- 
  pfi_siteinfo %>%
  left_join(ccbio) %>% 
  mutate(site_name2 =
           case_when(
             grepl("Stout", site_name) ~ "East",
             (grepl("Boyd", site_name) & grepl("grain", sys_trt)) ~ "Central1",
             (grepl("Boyd", site_name) & grepl("silage", sys_trt)) ~ "Central2",
             grepl("Funcke", site_name) ~ "West"),
         site_name3 =
           case_when(
             grepl("Stout", site_name) ~ "East",
             grepl("Boyd", site_name) ~ "Central",
             grepl("Funcke", site_name) ~ "West")) %>%
  left_join(pfi_wea, by = c("site_name3" = "site_name")) %>% 
  select(site_name2, sys_trt, lat, lon, avgT_c, avgp_mm, mccbio_Mgha)


crop19 <- c("Soybean", "Maize", "Maize/\nSoybean", "Maize Silage/\nSoybean")
plotsize <- c("XxX", "XxX", "55 m x 3.8 m", "55 m x 3.8 m")
reps <- c("4", "4", "5", "5")
samps <- c("8", "8", "10", "5")
sampdate <- c("April 16 2019", "April 17 2019", "April 8-9 2019", "April 8-9 2019")


dat_tbl <- 
  skel %>%
  mutate_at(vars(mccbio_Mgha), list(~round(., 2))) %>% 
  mutate_at(vars(lat, lon), list(~round(., 1))) %>% 
  mutate_at(vars(avgT_c, avgp_mm), list(~round(., 0))) %>% 
  unite(lat, lon, col = "latlon", sep = ",") %>% 
  unite(avgT_c, avgp_mm, col = "wea", sep = ",", remove = FALSE) %>% 
  mutate(crop19 = crop19,
         plotsize = plotsize,
         reps = reps,
         samps = samps,
         sampdate = sampdate) %>% 
  mutate(site_name2 = factor(site_name2, levels = c("West", "Central1", "Central2", "East"))) %>% 
  arrange(site_name2) %>% select(-site_name2, -sys_trt, -samps) %>%
  select(latlon, crop19, plotsize, reps, sampdate, avgT_c, avgp_mm, mccbio_Mgha) %>% 
  rename("Latitude, Longitude" = latlon,
         "Mean Air Temp (degC)" = avgT_c,
         "Mean Precip (mm)" = avgp_mm,
         #"Mean Air Temp (degC) and Precip (mm)" = wea,
         "Crop In 2019" = crop19,
         "Plot Size" = plotsize,
         "Number of Replicates" = reps,
         "Sampling Date" = sampdate,
         "Mean Cover Crop Biomass (Mg/ha)" = mccbio_Mgha
  ) 


thevarnames <- names(dat_tbl)


tbl <- 
  dat_tbl %>%
  gt() %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", v_align = "middle")
    ),
    locations = cells_column_labels(vars(thevarnames))
  ) %>% 
 # tab_header(title = "Summary of Sampling Locations") %>%
  tab_row_group(group = "West",
                rows = 1) %>%
  tab_row_group(group = "Central",
                rows = 2:3) %>%
  tab_row_group(group = "East",
                rows = 4) %>% 
  cols_align(align = c("center")) %>% 
  tab_style(
    style = list(
      cell_text(style = "italic")),
    locations = cells_row_groups()
  )

tbl
gtsave(tbl, "02_make-figs/figs/table1-samp-locs.png")
