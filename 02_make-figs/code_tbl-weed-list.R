##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: June 5 2020
#
# Purpose: make manuscript figs
#
# Notes: 
# Last modified: 7/13/2020 (change UB/UG to UD/UM)
#
####################################

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

library(patchwork)
library(PFIweeds2020)



# constant themes ---------------------------------------------------------

mylegendtheme <- theme(legend.position = c(0.1, 0.9),
                       legend.justification = c(0,1),
                       legend.background = element_rect(color = "black"))

myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)),
                         strip.text = element_text(size = rel(1.3)))


p_green <- "#619B44"
p_blue <- "#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FFC000"
p_gray <- "#E7E6E6"

scales::show_col(p_green)


# weed list ---------------------------------------------------------------

dat_table <- 
  pfi_ghobsraw %>% 
  pfifun_sum_weedbyeu() %>% 
  group_by(weed) %>% 
  summarise(tot_seeds  = sum(seeds)) %>% 
  mutate(sum = sum(tot_seeds),
         pct = round(tot_seeds/sum*100, 2),
         pct2 = ifelse(pct < 0.1, "<0.10", pct),
         pct2 = paste0(pct2, "%")) %>% 
  arrange(-pct) %>% 
  rename(code = weed) %>% 
  left_join(pfi_weedsplist) %>% 
  unite(photo_path, functional_grp, col = "desc", sep = " ") %>% 
  select(code, scientific_name, common_name, desc, pct2) %>% 
  mutate(scientific_name = str_to_sentence(scientific_name),
         common_name = str_to_sentence(common_name),
         common_name = ifelse(common_name == "Water hemp", "Waterhemp", common_name),
         common_name = ifelse(common_name == "Marestail", "Horseweed", common_name),
         common_name = ifelse(common_name == "Nightshade", "Eastern black nightshade", common_name),
         common_name = ifelse(common_name == "Rye (cereal)", "Cereal rye", common_name),
         desc = ifelse(desc == "NA NA", NA, desc))

dat_table

write_csv(dat_table, "02_make-figs/mf_weed-list-arranged.csv")


# weed list, by trial ---------------------------------------------------------------

dat_table_trial <- 
  pfi_ghobsraw %>% 
  pfifun_sum_weedbyeu() %>% 
  unite(site_name, field, sys_trt, col = "site_sys") %>% 
  group_by(site_sys, weed) %>% 
  summarise(tot_seeds  = sum(seeds)) %>% 
  mutate(sum = sum(tot_seeds),
         pct = round(tot_seeds/sum*100, 2),
         pct2 = ifelse(pct < 0.1, "<0.10", pct),
         pct2 = paste0(pct2, "%")) %>% 
  arrange(-pct) %>% 
  select(site_sys, weed, pct2) %>% 
  pivot_wider(names_from = site_sys, values_from = pct2)

dat_table_trial

write_csv(dat_table_trial, "02_make-figs/mf_weed-list-arranged-by-trial.csv")



# is there a better way ---------------------------------------------------

dat_all <- 
  bind_rows(
  pfi_ghobsraw %>% 
  pfifun_sum_weedbyeu() %>% 
  unite(site_name, field, sys_trt, col = "site_sys") %>% 
  group_by(site_sys, weed) %>% 
  summarise(tot_seeds  = sum(seeds)) %>% 
  mutate(sum = sum(tot_seeds),
         pct = round(tot_seeds/sum*100, 2)) %>% 
  select(site_sys, weed, pct),
pfi_ghobsraw %>% 
  pfifun_sum_weedbyeu() %>% 
  group_by(weed) %>% 
  summarise(tot_seeds  = sum(seeds)) %>% 
  mutate(sum = sum(tot_seeds),
         pct = round(tot_seeds/sum*100, 2)) %>% 
  select(weed, pct) %>% 
  mutate(site_sys = "overall")
) %>% 
  mutate(weed = factor(weed, levels = rev(dat_table$code)))

dat_all %>% 
  mutate(site_sys2 = case_when(
    site_sys == "Boyd_B42_grain" ~ "Central Grain_Maize",
    site_sys == "Boyd_B44_grain" ~ "Central Grain_Soybean",
    site_sys == "Boyd_B44_silage" ~ "Central Silage_Soybean",
    site_sys == "Funcke_F_grain" ~ "West Grain_Soybean",
    site_sys == "Stout_S_grain" ~ "East Grain_Maize",
    site_sys == "overall" ~ "Overall_Overall")
    ) %>% 
  separate(site_sys2, into = c("site_sys2", "residue"), sep = "_") %>% 
  mutate(site_sys2 = factor(site_sys2, levels = c("Overall", "West Grain", "Central Silage", "Central Grain", "East Grain"))) %>% 
  ggplot(aes(residue, weed)) + 
  geom_tile(aes(fill = log(pct))) + 
  scale_fill_viridis_c(option = "cividis") +
  facet_grid(.~site_sys2, scales = "free")


# what if I break it up by cover croo too? --------------------------------


dat_by_trt <- 
  bind_rows(
  pfi_ghobsraw %>% 
      pfifun_sum_weedbyeu() %>% 
      unite(site_name, field, sys_trt, col = "site_sys") %>% 
      select(site_sys, cc_trt, weed, seeds) %>% 
      group_by(site_sys, cc_trt, weed) %>% 
  summarise(weed_trt_tot = sum(seeds)) %>% 
  group_by(site_sys, cc_trt) %>% 
  mutate(tot_trt = sum(weed_trt_tot),
         pct = round(weed_trt_tot/tot_trt*100, 2)) %>% 
      select(site_sys, cc_trt, weed, pct),
  #--overall cover crop treatemtns?
  pfi_ghobsraw %>% 
    pfifun_sum_weedbyeu() %>% 
    group_by(weed) %>% 
    summarise(tot_seeds  = sum(seeds)) %>% 
    mutate(sum = sum(tot_seeds),
           pct = round(tot_seeds/sum*100, 2)) %>% 
    select(weed, pct) %>% 
    mutate(site_sys = "overall",
           cc_trt = "Overall")
  )

dat_by_trt %>% 
  mutate(weed = factor(weed, levels = rev(dat_table$code))) %>% 
  mutate(site_sys2 = case_when(
    site_sys == "Boyd_B42_grain" ~ "Central Grain_Maize",
    site_sys == "Boyd_B44_grain" ~ "Central Grain_Soybean",
    site_sys == "Boyd_B44_silage" ~ "Central Silage_Soybean",
    site_sys == "Funcke_F_grain" ~ "West Grain_Soybean",
    site_sys == "Stout_S_grain" ~ "East Grain_Maize",
    site_sys == "overall" ~ "Overall_ ")
  ) %>% 
  separate(site_sys2, into = c("site_sys2", "residue"), sep = "_") %>% 
  mutate(site_sys2 = factor(site_sys2, levels = c("Overall", "West Grain", "Central Silage", "Central Grain", "East Grain"))) %>% 
  ggplot(aes(cc_trt, weed)) + 
  geom_tile(aes(fill = log(pct))) + 
  scale_fill_viridis_c(option = "cividis") +
  facet_grid(.~site_sys2+residue, scales = "free")


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
  mutate(scientific_name = factor(scientific_name, levels = rev(dat_table$scientific_name))) %>% 
  #--make 0s NAs
  mutate(weed_trt_tot = ifelse(weed_trt_tot==0, NA, weed_trt_tot))


#--do them individually and patchwork them
dat_by_trt_raw %>% 
  ggplot(aes(cc_trt, scientific_name)) + 
  geom_tile(aes(fill = log(weed_trt_tot))) + 
  scale_fill_viridis_c(option = "cividis") +
  facet_grid(.~site_sys2+residue, scales = "free")


library(ggpubr)

my_lab <- labs(x = NULL,
               y = NULL)

legend_title <- bquote(Seeds)

thesites <- 
  dat_by_trt_raw %>% 
  select(site_sys) %>% 
  distinct() %>% 
  pull() 


thesites <- c("West_F_Grain",
    "Central_B44_Silage",
    "Central_B44_Grain",
    "Central_B42_Grain",
    "East_S_Grain")
  
thesites[1]


plot_fun_no_axis <- function(fun_site_sys = "Central_B42_grain", left_axis = "no"){
  
  plot_dat <- 
      dat_by_trt_raw %>% 
      filter(site_sys == fun_site_sys)
  
  plot_tmp <- 
    plot_dat %>% 
    ggplot(aes(cc_trt, scientific_name)) + 
    geom_tile(aes(fill = weed_trt_tot)) + 
    scale_fill_viridis_c(
      option = "plasma",
      guide = guide_colorbar(
        title = legend_title,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    facet_grid(.~site_sys2+residue, scales = "free") +
    theme_minimal() 
  
  
  if(left_axis == "no") 
  
  (new_plot <- 
    plot_tmp +
    theme(legend.position = "top",
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          strip.text = element_text(face = "bold"),
          legend.title = element_text(size = rel(0.9)),
          legend.text = element_text(size = rel(0.7))) + 
    my_lab)
  
  else(
  
  (new_plot <- 
     plot_tmp +
     theme(legend.position = "top",
           #axis.text.y = element_blank(),
           axis.text.x = element_text(angle = 45, vjust = 0.5),
           strip.text = element_text(face = "bold"),
           legend.title = element_text(size = rel(0.9)),
           legend.text = element_text(size = rel(0.7))) + 
     my_lab)
  
  )
  
  return(new_plot)
  
}

#--practice
plot_fun_no_axis(thesites[1], left_axis = "yes")

#--make plots
p1 <- plot_fun_no_axis(thesites[1], left_axis = "yes")
p2 <- plot_fun_no_axis(thesites[2])
p3 <- plot_fun_no_axis(thesites[3])
p4 <- plot_fun_no_axis(thesites[4])
p5 <- plot_fun_no_axis(thesites[5])

library(cowplot)
plot_grid(p1, p2, p3, p4, p5, 
          rel_widths = c(0.47, 0.25, 0.25, 0.25, 0.25),
          nrow = 1)


ggsave("02_make-figs/manu/fig3_new.png")

# table -------------------------------------------------------------------

library(gt)


dat_table %>% 
  gt() %>% 
  tab_footnote(
    footnote = "Unknown dicotyldeon",
    locations = cells_body(columns = vars(code),
                           rows = code == "UD")
  ) %>% 
  fmt_missing(
    columns = 1:5,
    missing_text = "-"
  ) %>% 
  tab_footnote(
    footnote = "Unknown monocotyledon",
    locations = cells_body(columns = vars(code),
                           rows = code == "UM")
  ) %>% 
  cols_label(
    code = "Code",
    scientific_name = "Scientific Name",
    common_name = "Common Name",
    desc = "Description",
    pct2 = "Percent of Total Found"
  ) %>% 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(
      columns = vars(scientific_name)
    )
  ) %>% 
  cols_align(align = c("center")) -> mytable


mytable

#--doesn't run on work computer :(

gtsave(mytable, "02_make-figs/figs/tbl-weeds.png")  


  