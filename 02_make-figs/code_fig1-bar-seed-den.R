##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 30 2019
# Last modified: Jan 8 2020 - Lydia trying to run things (see note)...
#                April 28 2020 - blowing things up and starting over....
#                june 3 2020 - change groupings based on Matt's feedback
#                june 22 2020 - use CIs, fix overall letters
#               8/31/2020 - update keeping corn/soy separate
#               9/2/2020 - make manu-new folder for revised figs
#               9/3/2020 - add avg ccbios to fig
#               9/9/2020 - correct widths/heights, move ccbio to bottom
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
library(stringr)

library(patchwork)
library(maps)
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
p_blue <- "dodgerblue4"#"#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FAE549FD" #"#FFE100"
p_gray <- "#E7E6E6"

scales::show_col(p_yellow)
scales::show_col("dodgerblue3")


# bar graph ---------------------------------------------------------------

#labseedsm2 = expression('Weed Seeds\n (1000s m'^"-2)")
labseedsm2 <- bquote("Weed Seeds (1000s"~m^-2~")")

#pfi_eus has sys_trt, site_name, and field

a <- pfi_eus

#--raw values
raws <-
  pfi_ghobsraw %>% 
  pfifun_sum_byeu() %>%
  ungroup() %>%
  left_join(pfi_eus) %>% 
  #--anonymize sites
  mutate(
    site = recode(
      site_name,
      "Boyd" = "Central",
      "Funcke" = "West",
      "Stout" = "East"
    )) %>% 
  mutate(
    crop_sys = str_to_title(sys_trt),
    site_sys = paste(site, crop_sys, sep = " "),
    #--get crop_2019 to crop_2018
    crop_2018 = case_when(
      (crop_2019 == "soy" & crop_sys == "Grain") ~ "Maize",
      (crop_2019 == "soy" & crop_sys == "Silage") ~ "Maize Silage",
      crop_2019 == "corn" ~ "Soybean")
  ) %>% 
  #--get the order I want
  mutate(
    site_sys = factor(site_sys, levels = c("West Grain", "Central Silage", "Central Grain", "East Grain")),
    cc_trt = recode(cc_trt,
                    "no" = "No Cover",
                    "rye" = "Winter Rye")
  ) #%>% 
#filter(totseeds_m2 < 15000)



##--stat data, don't worry about names of things yet

#--raw means 
sb_est <- 
  read_csv("01_stats-uni/st_weedseed-est.csv") %>% 
  mutate(totseeds_m2 = pfifun_seedstom2conv(rate),
         totseeds_se = pfifun_seedstom2conv(std.error),
         se_lo = totseeds_m2 - totseeds_se,
         se_hi = totseeds_m2 + totseeds_se)

#---Change
sb_chng <- 
  sb_est %>% 
  select(cc_trt, site_sys, totseeds_m2) %>% 
  pivot_wider(names_from = cc_trt, values_from = totseeds_m2) %>% 
  mutate(trt_eff = paste(round(ccrye - no, 0)))

#--sig of change plus ranges
sb_pval <- 
  read_csv("01_stats-uni/st_weedseed-contr.csv") %>% 
  mutate(rye_no = (round((ratio-1)*100, 0)),
         stderr = round(std.error * 100, 0),
         CI_lo = round((asymp.LCL - 1)*100, 0),
         CI_lo = ifelse(CI_lo <0, CI_lo, paste0("+", CI_lo)),
         CI_hi = round((asymp.UCL - 1)*100, 0),
         CI_hi = ifelse(CI_hi <0, CI_hi, paste0("+", CI_hi)),
         CIs = paste0("CI(", CI_lo, ",", CI_hi,"%)"),
         pct = paste0(rye_no, "%"),
         trt_eff_pct2 = paste0(rye_no, "%", "(±", stderr, "%)"),
         trt_eff_pct3 = paste0(rye_no, "%", ", CI(", CI_lo, ",", CI_hi,"%)"),
         p.value = paste0("p = ", round(p.value, 2)))


#--combine
table_changes <- 
  sb_chng %>% 
  left_join(sb_pval) %>% 
  left_join(sb_est %>% 
              group_by(site_sys) %>% 
              summarise(se_mx = max(se_hi)/1000)
  ) %>% 
  #--clean up names
  separate(site_sys,
           into = c("site", "field", "sys_trt"),
           remove = F) %>%
  left_join(pfi_eus) %>% 
  mutate(
    site = recode(
      site,
      "Boyd" = "Central",
      "Funcke" = "West",
      "Stout" = "East"
    ),
    crop_sys = str_to_title(sys_trt),
    site_sys = paste(site, crop_sys, sep = " "),
    crop_2018 = case_when(
      (crop_2019 == "soy" & crop_sys == "Grain") ~ "Maize",
      (crop_2019 == "soy" & crop_sys == "Silage") ~ "Maize Silage",
      crop_2019 == "corn" ~ "Soybean")
  ) %>% 
  #--get the order I want
  mutate(
    site_sys = factor(site_sys, levels = c("West Grain", "Central Silage", "Central Grain", "East Grain"))
  ) %>% 
  distinct() %>% 
  left_join(pfi_mccbio) %>%
  mutate(mccbio_Mgha = round(mccbio_Mgha, 2)) %>%
  left_join(
    pfi_ccbio %>%
      filter(year == 2019) %>%
      mutate(ccbio_Mgha = as.character(round(ccbio_Mgha, 2))) %>%
      mutate(ccbio_Mgha = ifelse(ccbio_Mgha == "0", 
                                 "0.10", ifelse(
                                   ccbio_Mgha == "0.3",
                                   "0.30", ccbio_Mgha))) %>%
      rename("ccbio19_Mgha" = "ccbio_Mgha")
  ) %>% 
  select(-blockID, -rep) %>% 
  distinct() %>% 
  filter(!is.na(ccbio19_Mgha))


table_changes


# create data for figure --------------------------------------------------------------

fig_dat <- 
  sb_est %>%
  #--make things nice
  separate(site_sys,
           into = c("site", "field", "crop_sys"),
           remove = F) %>%
  mutate(sys_trt = crop_sys,
         cc_trt = recode(cc_trt,
                         "ccrye" = "rye")) %>% 
  left_join(pfi_eus) %>% 
  mutate(
    crop_sys = str_to_title(crop_sys),
    #--need to redefine facets to site-sys
    site_sys = paste(site, crop_sys, sep = " "),
    #--get crop_2019 to crop_2018
    crop_2018 = case_when(
      (crop_2019 == "soy" & crop_sys == "Grain") ~ "Maize",
      (crop_2019 == "soy" & crop_sys == "Silage") ~ "Maize Silage",
      crop_2019 == "corn" ~ "Soybean")
  ) %>% 
  #--get the order I want
  mutate(
    site_sys = factor(site_sys, levels = c("West Grain", "Central Silage", "Central Grain", "East Grain")),
    cc_trt = recode(cc_trt,
                    "no" = "No Cover",
                    "rye" = "Winter Rye")
  ) %>%
  select(site_sys, crop_2018, cc_trt, totseeds_m2, se_lo, se_hi) %>% 
  distinct()

ccbio_dat <- 
  table_changes %>% 
  filter(cc_trt == "rye") %>% 
  select(site_sys, site, crop_2018, se_mx, mccbio_Mgha, ccbio19_Mgha) %>% 
  distinct() %>% 
  mutate(
    
    ccbio = (paste0(mccbio_Mgha, " Mg ha-1")))


# different facets --------------------------------------------------------


fig_dat %>% 
  distinct() %>% 
  ggplot(aes(cc_trt, totseeds_m2 / 1000)) +
  geom_col(color = "black",
           size = 0.9,
           alpha = 0.9,
           aes(fill = cc_trt)) +
  facet_grid(.~site_sys + crop_2018) +
  geom_jitter(data = raws %>% filter(totseeds_m2 < 15000), 
              aes(cc_trt, totseeds_m2/1000, 
                  color = cc_trt,
                  pch = cc_trt), 
              size = 2,
              fill = "gray80",
              alpha = 0.7, stroke = 1.2) +
  geom_point(data = raws %>% filter(totseeds_m2 > 15000), 
             aes(cc_trt, totseeds_m2/1000), 
             color = "red",
             pch = 24,
             size = 2,
             fill = "red",
             alpha = 0.7, stroke = 1.2) +
  geom_linerange(aes(ymin = se_lo / 1000, ymax = se_hi / 1000, alpha = cc_trt),
                 size = 1.2, color = "black") +
  geom_text(data = table_changes, 
            x = 1.5,
            aes(y = se_mx + 2.5, 
                label = paste0(trt_eff, " seeds"), fontface = "italic")) +
  geom_text(data = table_changes, 
            x = 1.5,
            aes(y = se_mx + 3.75, label = CIs), fontface = "italic") +
  geom_text(data = table_changes, 
            x = 1.5,
            aes(y = se_mx + 5, label = pct), fontface = "italic") +
  geom_text(data = ccbio_dat, 
            x = 1.5,
            aes(y = -1, label = ccbio), fontface = "bold") +
  scale_alpha_manual(values = c(1, 1)) +
  labs(y = labseedsm2,
       x = NULL,
       fill = "Cover Crop Treatment",
       color = "Cover Crop Treatment",
       shape = "Cover Crop Treatment") +
  guides(alpha = F
         #color = F
  ) +
  scale_fill_manual(values = c("No Cover" = p_yellow,
                               "Winter Rye" = p_blue)) +
  scale_color_manual(values = c("gray50", "gray50")) +
  scale_shape_manual(values = c("No Cover" = 21,
                                "Winter Rye" = 24)) +
  theme_bw() +
  facet_grid(. ~ site_sys+crop_2018, scales = "free") +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(0.99, 0.99),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.title = element_text(size = rel(1.2)),
    axis.text.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1.2)),
    legend.text = element_text(size = rel(1)))


ggsave("02_make-figs/manu-new/fig1.jpg", width = 7, height = 4.2)





# odl facets -------------------------------------

fig_dat %>% 
  ggplot(aes(reorder(crop_2018, totseeds_m2, mean), totseeds_m2 / 1000)) +
  geom_col(position = position_dodge(width = 0.9),
           color = "black",
           size = 0.9,
           alpha = 0.9,
           aes(fill = cc_trt)) +
  # geom_point(data = raws, aes(crop_sys, totseeds_m2/1000, color = cc_trt), 
  #            pch = 21, position = position_dodge(0.9), size = 3, fill = "white", alpha = 0.7, stroke = 1.2) +
  geom_point(data = raws %>% filter(totseeds_m2 < 15000), 
             aes(crop_2018, totseeds_m2/1000, 
                 color = cc_trt,
                 pch = cc_trt), 
             #pch = 21, 
             position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.1), 
             size = 2,
             fill = "gray80",
             alpha = 0.7, stroke = 1.2) +
  geom_point(data = raws %>% filter(totseeds_m2 > 15000), 
             aes(crop_2018, totseeds_m2/1000, 
                 color = cc_trt,
                 pch = cc_trt), 
             #pch = 21, 
             position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.01), 
             size = 2,
             fill = "red",
             alpha = 0.7, stroke = 1.2) +
  geom_linerange(position = position_dodge(width = 0.9),
                 aes(ymin = se_lo / 1000, ymax = se_hi / 1000, alpha = cc_trt),
                 size = 1.2, color = "black") +
  geom_text(data = table_changes, 
            aes(x = crop_2018, y = se_mx + 2.5, 
                label = paste0(trt_eff, " seeds"), fontface = "italic")) +
  geom_text(data = table_changes, 
            aes(x = crop_2018, y = se_mx + 3.25, label = CIs), fontface = "italic") +
  geom_text(data = table_changes, 
            aes(x = crop_2018, y = se_mx + 4, label = pct), fontface = "italic") +
  scale_alpha_manual(values = c(1, 1)) +
  labs(y = labseedsm2,
       x = "Previous Year's Crop",
       fill = "Cover Crop Treatment",
       color = "Cover Crop Treatment",
       shape = "Cover Crop Treatment") +
  guides(alpha = F
         #color = F
  ) +
  scale_fill_manual(values = c("No Cover" = p_yellow,
                               "Winter Rye" = p_blue)) +
  scale_color_manual(values = c("gray50", "gray50")) +
  scale_shape_manual(values = c("No Cover" = 21,
                                "Winter Rye" = 24)) +
  theme_bw() +
  facet_grid(. ~ site_sys, scales = "free") +
  myaxistexttheme +
  theme(#legend.direction = "horizontal",
    #legend.position = "top",
    legend.justification = c(1, 1),
    legend.position = c(0.99, 0.99),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.title = element_text(size = rel(1.4)),
    #legend.key.width = unit(1.4, "cm"),
    #legend.key.height = unit(0.5, "cm"),
    # legend.key.size = unit(1, "cm"),
    # axis.text.x = element_text(angle = 45,
    #                            vjust = 1))
  ) -> fig_sb

fig_sb

ggsave("02_make-figs/manu-new/fig1.jpg")