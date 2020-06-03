##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: Dec 30 2019
# Last modified: Jan 8 2020 - Lydia trying to run things (see note)...
#                April 28 2020 - blowing things up and starting over....
#                june 3 2020 - change groupings based on Matt's feedback
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



# map ---------------------------------------------------------------------



map_iowa <- as_tibble(map_data('state')) %>%
  filter(region == "iowa")

locs <- pfi_siteinfo %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = F) %>% 
  mutate(site_id = recode(site_sys,
                          "Boyd_grain" = "Central1",
                          "Boyd_silage" = "Central2\n(Silage)",
                          "Funcke_grain" = "West",
                          "Stout_grain" = "East"))


map_iowa <- as_tibble(map_data('state')) %>% 
  filter(region == "iowa")

map_county <- as_tibble(map_data('county')) %>% 
  filter(region == "iowa") 

map_county3 <- map_county %>% filter(subregion %in% c("boone", "greene", "washington"))

fig_map <- 
  ggplot() +
  geom_polygon(data = map_iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = "white") +
  geom_polygon(data = map_county, aes(x = long, y = lat, group = group), 
               color = "gray80", fill = NA) +
  geom_polygon(data = map_county3, aes(x = long, y = lat, group = group), 
               color = "gray80", fill = "gold", size = 2) +
  
  geom_text(aes(x = -95, y = 42.05), size = 5, label = "West") +
  geom_text(aes(x = -93.3, y = 42.05), size = 5, label = "Central") +
  geom_text(aes(x = -91.7, y = 41.60), size = 5, label = "East") +
  theme_minimal()  +
  mylegendtheme +
  myaxistexttheme +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  coord_quickmap() + 
  theme(panel.grid = element_blank())


fig_map
ggsave("make-figs/fig1_map.png")


# bar graph ---------------------------------------------------------------


cctrtpal <- c("darkolivegreen3", "lightsalmon4")

labseedsm2 = expression('Weed Seeds\n (1000s m'^"-2)")
labseedsm2 <- bquote("Weed Seeds (1000s"~m^-2~")")

#--raw values
sb_est <- read_csv("01_stats-uni/st_weedseed-est.csv") %>% 
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
         trt_eff_pct2 = paste0(rye_no, "%", "(±", stderr, "%)"),
         p.value = paste0("p = ", round(p.value, 2))) %>% 
    select(site_sys, p.value, trt_eff_pct2)
  

#--combine
table_changes <- 
  sb_chng %>% 
  left_join(sb_pval) %>% 
  left_join(sb_est %>% 
              group_by(site_sys) %>% 
              summarise(se_mx = max(se_hi)/1000)
  ) %>% 
  separate(site_sys,
           into = c("site", "crop_sys"),
                                  remove = F) %>%
  mutate(
    site = recode(
      site,
      "Boyd" = "Central",
      "Funcke" = "West",
      "Stout" = "East"
    ),
    site = factor(site, levels = c("West", "Central", "East")),
    crop_sys = str_to_title(crop_sys)
  ) %>% 
  select(site, crop_sys, se_mx, p.value, trt_eff, trt_eff_pct2)
  


# the figure --------------------------------------------------------------

fig_dat <- 
  sb_est %>%
  #--make things nice
  separate(site_sys,
           into = c("site", "crop_sys"),
           remove = F) %>%
  mutate(
    site = recode(
      site,
      "Boyd" = "Central",
      "Funcke" = "West",
      "Stout" = "East"
    ),
    site = factor(site, levels = c("West", "Central", "East")),
    crop_sys = str_to_title(crop_sys),
    cc_trt = recode(cc_trt,
                    "no" = "None",
                    "ccrye" = "Rye Cover Crop")) %>%
    select(site, cc_trt, crop_sys, totseeds_m2, se_lo, se_hi)

fig_dat %>% 
  ggplot(aes(reorder(crop_sys, totseeds_m2, mean), totseeds_m2 / 1000)) +
  geom_col(position = position_dodge(width = 0.9),
           color = "black",
           size = 1.2,
           aes(fill = cc_trt)) +
  geom_linerange(position = position_dodge(width = 0.9),
                 aes(ymin = se_lo / 1000, ymax = se_hi / 1000, alpha = cc_trt)) +
  geom_text(data = table_changes, aes(x = crop_sys, y = se_mx + 0.5, label = p.value), fontface = "italic") +
  geom_text(data = table_changes, aes(x = crop_sys, y = se_mx + 1, label = trt_eff), fontface = "italic") +
  geom_text(data = table_changes, aes(x = crop_sys, y = se_mx + 1.5, label = trt_eff_pct2), fontface = "italic") +
  scale_alpha_manual(values = c(1, 1)) +
  labs(y = labseedsm2,
       x = NULL,
       fill = NULL) +
  guides(alpha = F) +
  scale_fill_manual(values = c("None" = cctrtpal[2],
                               "Rye Cover Crop" = cctrtpal[1])) +
  scale_color_manual(values = cctrtpal) +
  theme_bw() +
  facet_grid(. ~ site, scales = "free") +
  myaxistexttheme +
  theme(#legend.direction = "horizontal",
        #legend.position = "top",
        legend.justification = c(1, 1),
        legend.position = c(0.99, 0.99),
        legend.background = element_rect(color = "black", fill = "white"),
        #legend.key.width = unit(1.4, "cm"),
        #legend.key.height = unit(0.5, "cm"),
        # legend.key.size = unit(1, "cm"),
        # axis.text.x = element_text(angle = 45,
        #                            vjust = 1))
  ) -> fig_sb
        
fig_sb

ggsave("02_make-figs/figs/fig1_bar-totseeds.png")

# put together ------------------------------------------------------------

#--total hack, doesn't work on desktop, works on laptop
#library(gridExtra)
#fig_sb + (fig_map /gridExtra::tableGrob(mtcars[1:10, c('mpg', 'disp')]))

layout <- c(
  area(t = 2, l = 1, b = 5, r = 4),
  area(t = 1, l = 3, b = 3, r = 5)
)

fig_sb + fig_map + 
  plot_layout(design = layout)


ggsave("02_make-figs/figs/fig1_bar-map.png")

fig_sb; print(fig_map, vp = viewport(0.8, 0.75, 0.4, 0.4))
