# Gina
# 5/29/2020
# 6/8/2020 need to add year of initiation!


library(tidyverse)
library(ggrepel)
library(gt)
library(PFIweeds2020)
library(janitor)

ccbio <- 
  read_csv("01_stats-ccbio/sc_ccbio-metrics.csv") %>% 
  select(site_sys, yr_span, ccbio_mean) %>% 
  pivot_wider(names_from = yr_span, values_from = ccbio_mean) %>% 
  clean_names() %>% 
  separate(site_sys, into = c("site", "sys_trt")) %>% 
  rename(site_name = site)

skel <- 
  pfi_siteinfo %>%
  pivot_longer(lat:lon) %>% 
  #--get lat/lon into degrees
  mutate(deg = (round(value, 0)),
         mins = abs((value - deg)),
         mins2 = round(mins*60, 0),
         deg_mins = paste0(abs(deg), "°",mins2, "'"),
         deg_mins2 = ifelse(name == "lat", paste0(deg_mins, "N"), paste0(deg_mins, "W"))) %>% 
  select(-(value:deg_mins)) %>% 
  pivot_wider(names_from = name, values_from = deg_mins2) %>% 
  left_join(ccbio) %>% 
  # rename sites
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
  # get ccbios together
 # mutate(mccbio_Mgha = paste(round(x5yr, 2), round(x10yr, 2), sep = ", ")) %>% 
  left_join(pfi_wea, by = c("site_name3" = "site_name")) %>% 
  select(site_name2, sys_trt, lat, lon, avgT_c, avgp_mm, x5yr, x10yr)


crop19 <- c("Soybean", "Maize", "Maize/\nSoybean", "Maize Silage/\nSoybean")
plotsize <- c("XxX", "XxX", "55m x 3.8m", "55m x 3.8m")
reps <- c("4", "4", "5", "5")
samps <- c("8", "8", "10", "5")
sampdate <- c("April 16 2019", "April 17 2019", "April 8-9 2019", "April 8-9 2019")


dat_tbl <- 
  skel %>%
  #--add year of init
  mutate(
    yrinit = case_when(
      grepl("West", site_name2) ~ 2008,
      grepl("East", site_name2) ~ 2009,
      (grepl("Central", site_name2) & grepl("silage", sys_trt)) ~ 2002,
      (grepl("Central", site_name2) & grepl("grain", sys_trt)) ~ 2009
    )
  ) %>% 
  mutate_at(vars(x5yr, x10yr), list(~round(., 2))) %>% 
  mutate_at(vars(avgT_c, avgp_mm), list(~round(., 0))) %>% 
  mutate(wea = paste0(avgT_c, "°C, ", avgp_mm, " mm")) %>% 
  unite(lat, lon, col = "latlon", sep = ", ") %>% 
  #--assigns values from vectors defined above
  mutate(crop19 = crop19,
         plotsize = plotsize,
         reps = reps,
         samps = samps,
         sampdate = sampdate) %>%
  mutate(site_name2 = factor(site_name2, levels = c("West", "Central1", "Central2", "East"))) %>% 
  arrange(site_name2) %>% select(-site_name2, -sys_trt, -samps) %>%
  #--select what I want to appear in table
  select(latlon, 
         yrinit,
         #wea,
         #crop19, 
         reps, plotsize,
         sampdate, 
         avgT_c, avgp_mm, 
         #mccbio_Mgha,
         x5yr,
         x10yr) %>% 
  rename("Air Temp (°C)" = avgT_c,
         "Precip (mm)" = avgp_mm,
         "5-year" = x5yr,
         "10-year" = x10yr)
  

thevarnames <- names(dat_tbl)


tbl <- 
  dat_tbl %>%
  gt() %>%
  tab_row_group(group = "West",
                rows = 1) %>%
  tab_row_group(group = "Central",
                rows = 2:3) %>%
  tab_row_group(group = "East",
                rows = 4) %>% 
  row_group_order(
    groups = c("West", "Central", "East")
  ) %>% 
  cols_align(align = c("center")) %>% 
  tab_style(
    style = list(
      cell_text(style = "italic")),
    locations = cells_row_groups()
  ) %>% 
  #--spanners
  tab_spanner(
    label = "30-year Mean Annual",
    columns = vars(`Air Temp (°C)`, `Precip (mm)`)
  ) %>% 
  tab_spanner(
    label = html("Mean Cover Crop Biomass (Mg ha<sup>-1)"),
    columns = vars(`5-year`, `10-year`)
    ) %>% 
  #--styles
  tab_style(
    style = list(
      cell_text(weight = "bold", v_align = "middle")
    ),
    locations = list(
      cells_column_labels(vars(latlon, yrinit, reps, plotsize, sampdate)),
      cells_column_spanners(vars(`30-year Mean Annual`, `Mean Cover Crop Biomass (Mg ha<sup>-1)`))
    )) %>%
  tab_style(
    style = list(
      cell_text(style = "italic", v_align = "middle")
    ),
    locations = list(
      cells_column_labels(vars(`Air Temp (°C)`, `Precip (mm)`, `5-year`, `10-year`))
      )
    ) %>% 
  #--labels
  cols_label(
    latlon = "Latitude, Longitude",
    yrinit = "Year Initiated",
    #wea = "30-year Mean Annual\nAir Temperature\n, Precipitation",
    plotsize = "Plot Size",
    reps = "Number of Replicates",
    sampdate = "Sampling Date",
    #avgT_C = "Air Temp&degC",
    #avgp_mm = "Precip (mm)"
    #mccbio_Mgha = html("Mean Five-, Ten-Year Cover Crop Biomass (Mg ha<sup>-1)")
  ) %>% 
  #--footnotes
  tab_footnote(
    footnote = "Grain-based rotation",
    locations = cells_body(columns = vars(latlon),
                           rows = c(1, 2, 4))
  ) %>% 
  tab_footnote(
    footnote = "Silage-based rotation",
    locations = cells_body(columns = vars(latlon),
                           rows = 3)
  )


tbl
#--doesn't work on desktop
gtsave(tbl, "02_make-figs/figs/table1-samp-locs.png")
