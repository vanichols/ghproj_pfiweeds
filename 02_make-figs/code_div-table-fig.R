# Gina
# 5/29/2020

library(tidyverse)
library(ggrepel)
library(gt)
library(PFIweeds2020)

div <- read_csv("01_stats-uni/st_diversity.csv")

fig_dat <- 
  div %>% 
  unite(site_name, sys_trt, col = "site_sys") %>% 
  select(-shan_div) %>% 
  pivot_longer(shan_hill:evenness) %>%
  filter(!is.nan(value)) %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2 (Silage)",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"),
         site_id = factor(site_id, 
                          levels = c("West", "Central2 (Silage)", "Central1", "East"))) 




# fig ---------------------------------------------------------------------

cctrtpal <- c("darkolivegreen3", "lightsalmon4")

fig_dat %>% 
  ggplot(aes(site_id, value)) + 
  #geom_boxplot(aes(color = cc_trt)) + 
  stat_summary(fun = "mean", geom = "point", 
               position = position_dodge2(width = 0.3),
               aes(color = cc_trt)) + 
  stat_summary(aes(color = cc_trt),
               position = position_dodge2(width = 0.3)) +
  scale_color_manual(values = rev(cctrtpal)) +
  facet_wrap(.~name, scales = "free", ncol=1)

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
