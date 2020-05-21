##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 5/22/2020
#
# Purpose: make stoch dom fig
#
# Last modified:
#
# Notes:
####################################

rm(list = ls())
library(tidyverse)

# data --------------------------------------------------------------------


stodom <- read_csv("01_stats-stoch-dom/st_stoch-dom-res.csv")


# figure ------------------------------------------------------------------

mylegendtheme <-   theme(#legend.direction = "horizontal",
  #legend.position = "bottom",
  legend.justification = c(1, 1),
  legend.position = c(0.9, 0.9),
  legend.background = element_blank(),
  legend.key.width = unit(1.4, "cm"),
  legend.key.height = unit(0.5, "cm"),
  legend.key.size = unit(1, "cm"),
  #axis.title.y = element_text(angle = 0, 
  #                            vjust = 0.5, 
  #                            hjust = -1)
)



myaxistexttheme <- theme(axis.text = element_text(size = rel(1.2)),
                         legend.text = element_text(size = rel(1.3)),
                         axis.title = element_text(size = rel(1.3)))


cctrtpal <- c("darkolivegreen3", "lightsalmon4")

labseedsm2 = expression('Weed Seeds\n (1000s m'^"-2)")
labseedsm2 <- bquote("Weed Seeds (1000s"~m^-2~")")


#--these are 'rough drafts'

#--cum diff
stodom %>% 
  pivot_longer(sc_nocum:sc_ryecum) %>% 
  select(seeds, name, value) %>% 
  ggplot(aes(seeds, value, color = name)) + 
  geom_point(size = 3) +
  geom_line(size = 1
  ) + 
  theme_bw() + 
  labs(x = "seeds/m2",
       y = "cumulative prob",
       title = "Un-transformed, outlier removed")

library(ggridges)

#--inv cum diff
stodom %>% 
  pivot_longer(sc_nocum:sc_ryecum) %>% 
mutate(name = recode(name, 
                  "sc_nocum" = "None",
                  "sc_ryecum" = "Rye Cover Crop")) %>%  
  select(seeds, name, value) %>% 
  mutate(inv_value = 1 - value) %>% 
  ggplot(aes(seeds, inv_value, color = name, fill = name, group = name)) + 
  geom_density_line(stat = "identity", size = 0.5, alpha = 0.3) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_fill_manual(values = c("None" = cctrtpal[2],
                               "Rye Cover Crop" = cctrtpal[1])) +
  scale_color_manual(values = c("None" = cctrtpal[2],
                                "Rye Cover Crop" = cctrtpal[1])) +
  geom_vline(xintercept = 350) +
  theme_bw() + 
  myaxistexttheme + 
  mylegendtheme +
  labs(x = "seeds/m2",
       y = "cumulative prob",
       title = "Un-transformed, outlier removed") 




stodom %>% 
  mutate(inv_value = 1-value,
         ) %>% 
  ggplot(aes(seeds, inv_value, color = name)) + 
  geom_point(size = 3) +
  #geom_line(size = 1) + 
  geom_smooth(size = 1, se = F) + 
#  geom_xspline(spline_shape=0.4, size=0.5) +
  geom_vline(xintercept = 350) + 
  theme_bw() + 
  labs(x = "seeds/m2",
       y = "cumulative prob",
       title = "Un-transformed, outlier removed") 



# splines? ----------------------------------------------------------------


set.seed(1492)
dat <- data.frame(x=c(1:10, 1:10, 1:10),
                  y=c(sample(15:30, 10), 2*sample(15:30, 10),
                      3*sample(15:30, 10)),
                  group=factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
)

ggplot(dat, aes(x, y, group=group, color=group)) +
  geom_point() +
  geom_line()

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point() +
  geom_line() +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
#  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
#  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-0.4, size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
#  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0.4, size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=1, size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0, size=0.5)

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-1, size=0.5)

