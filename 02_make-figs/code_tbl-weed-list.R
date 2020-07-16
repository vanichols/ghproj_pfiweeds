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
         common_name = ifelse(common_name == "Rye (cereal)", "Cereal rye", common_name),
         desc = ifelse(desc == "NA NA", NA, desc))

dat_table

write_csv(dat_table, "02_make-figs/mf_weed-list-arranged.csv")

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


  