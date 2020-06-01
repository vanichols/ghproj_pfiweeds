# Gina
# 5/29/2020

library(tidyverse)
library(ggrepel)
library(gt)
library(PFIweeds2020)

div <- 
  read_csv("01_stats-uni/st_diversity.csv") %>% 
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



div_stats <- 
  read_csv("01_stats-uni/st_diversity-contrasts.csv") %>%
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2 (Silage)",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"),
         site_id = factor(site_id, 
                          levels = c("West", "Central2 (Silage)", "Central1", "East"))) %>% 
  arrange(site_id)




# table of stats ----------------------------------------------------------

div_table <- 
  div_stats %>% 
  select(site_id, metric, estimate, std.error, p.value) %>%
  mutate(est_se = paste0(round(-estimate, 2), " (", round(std.error, 2), ")"),
         p.value = round(p.value, 2)) %>% 
  select(-estimate, -std.error) %>% 
  pivot_wider(names_from = metric, values_from = c(est_se, p.value))
  

gt_div <- 
  div_table %>% 
  gt() %>% 
  tab_spanner(
    label = md("**Shannon Hill Diversity**"),
    columns = vars(est_se_shan_hill, p.value_shan_hill)
  ) %>% 
  tab_spanner(
    label = md("**Richness**"),
    columns = vars(est_se_richness, p.value_richness)
  ) %>% 
  tab_spanner(
    label = md("**Evenness**"),
    columns = vars(est_se_evenness, p.value_evenness)
  ) %>% 
  cols_label(
    est_se_shan_hill = md("Estimate *(SE)*"),
    p.value_shan_hill = md("*P-value*"),
    est_se_richness = md("Estimate *(SE)*"),
    p.value_richness = md("*P-value*"),
    est_se_evenness = md("Estimate *(SE)*"),
    p.value_evenness = md("*P-value*"),
    site_id = " "
) %>% 
  cols_align(
    align = "center"
  )

gt_div
gtsave(gt_div, "02_make-figs/figs/table2_div-stats.png")


# fig ---------------------------------------------------------------------

cctrtpal <- c("darkolivegreen3", "lightsalmon4")

div %>% 
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
