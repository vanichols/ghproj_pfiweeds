##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 5/22/2020
#
# Purpose: make stoch dom fig
#
# Last modified:
#                8/31/2020 (didn't get submitted?, update w/new field delineation)
#
# Notes:
####################################

rm(list = ls())
library(tidyverse)


p_green <- "#619B44"
p_blue <- "dodgerblue4"#"#46B2B5"
p_pink <- "#DC1A64"
p_orange <- "#FFA726"
p_yellow <- "#FAE549FD" #"#FFE100"
p_gray <- "#E7E6E6"
p_purp <- "#8B1C62"


# data --------------------------------------------------------------------


stodom <- read_csv("01_stats-stoch-dom/st_stoch-dom-res.csv")


# figure ------------------------------------------------------------------

# mylegendtheme <-   theme(#legend.direction = "horizontal",
#   #legend.position = "bottom",
#   legend.justification = c(1, 1),
#   legend.position = c(0.9, 0.9),
#   legend.background = element_blank(),
#   legend.key.width = unit(1.4, "cm"),
#   legend.key.height = unit(0.5, "cm"),
#   legend.key.size = unit(1, "cm"),
#   #axis.title.y = element_text(angle = 0, 
#   #                            vjust = 0.5, 
#   #                            hjust = -1)
# )



mylegendtheme <- theme(
  legend.justification = c(1, 1),
  legend.position = c(0.99, 0.99),
  legend.background = element_rect(color = "black", fill = "white"),
  legend.title = element_text(size = rel(1.4))
)


myaxistexttheme <- theme(axis.text = element_text(size = rel(1.1)),
                         legend.text = element_text(size = rel(1.2)),
                         axis.title = element_text(size = rel(1.2)))


#labseedsm2 = expression('Weed Seeds\n (1000s m'^"-2)")
labseedsm2 <- bquote("Weed Seed Density (1000s"~m^-2~")")


# fig ---------------------------------------------------------------------

library(ggridges)
library(scales)

#--inv cum diff
stodom %>% 
  pivot_longer(sc_nocum:sc_ryecum) %>% 
  mutate(name = recode(name, 
                       "sc_nocum" = "No Cover",
                       "sc_ryecum" = "Winter Rye")) %>%  
  select(seeds, name, value) %>% 
  mutate(inv_value = 1 - value) %>% 
  ggplot(aes(seeds/1000, inv_value, color = name, fill = name, group = name, shape = name)) + 
  ggridges::geom_density_line(stat = "identity", 
                              size = 0.5, 
                              #alpha = 0.7
                              ) +
  geom_line(size = 1) +
  geom_point(size = 2, color = "black") +
  scale_fill_manual(values = c("No Cover" = p_yellow,
                               "Winter Rye" = p_blue)) +
  scale_color_manual(values = c("No Cover" = p_yellow,
                                "Winter Rye" = p_blue)) +
  geom_vline(xintercept = 350/1000, linetype = "dashed") +
  theme_bw() + 
  scale_y_continuous(labels = label_percent()) +
  myaxistexttheme + 
  mylegendtheme +
  labs(x = labseedsm2,
       y = "Cumulative Probability",
       fill = "Cover Crop Treatment",
       color = "Cover Crop Treatment",
       shape = "Cover Crop Treatment")


ggsave("02_make-figs/manu-new/fig5.jpg", width = 6, height = 5)
