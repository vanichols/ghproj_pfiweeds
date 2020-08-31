##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 5/20/2020
#
# Purpose: do stochastic dominance analysis
#
# Last modified: 5/21/2020 (after mtg w/consultants)
#                8/31/2020 (keep field in there)
#
# Notes:
####################################

rm(list = ls())
library(tidyverse)
library(PFIweeds2020)
library(generalCorr)


# data --------------------------------------------------------------------

#--use data and fucntion from package

pfi_ghobsraw


########################## outlier removed ####################################

#--keep the field in there...

dat <- 
  pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, field, sys_trt, col = "site_sys", remove = T) %>% 
  select(-rep) %>% 
  filter(totseeds < 860) #remove outlier


# try stochastic dominance ------------------------------------------------



#--vectors of seed values for each treatment
xa <- dat %>% filter(cc_trt == "no") %>% select(totseeds_m2) %>% pull()
xb <- dat %>% filter(cc_trt == "rye") %>% select(totseeds_m2) %>% pull()
Ta = length(xa)
Tb = length(xb)
k = Ta + Tb 
pa0 = rep(1/Ta, Ta) #--probability of getting each value
pb0 = rep(1/Tb, Tb)

#--just creates a 'weighting' for the cumulative distribution
pra <- prelec2(n = Ta + Tb)


gina <- #--my equivalent of xpapb
  tibble(seeds = c(xa, xb)) %>% 
  mutate(prob_no = c(pa0, rep(0, Tb)),
         prob_rye = c(rep(0, Ta), pb0)) %>% 
  arrange(seeds) %>% 
  mutate(prob_no2 = prob_no * pra$pdif,
         prob_rye2 = prob_rye * pra$pdif) %>%
  filter(!is.na(seeds)) %>% 
  mutate(nocum = cumsum(prob_no2),
         ryecum = cumsum(prob_rye2)) %>% 
  mutate(mx_nocum = max(nocum),
         mx_ryecum = max(ryecum),
         sc_nocum = nocum/mx_nocum,
         sc_ryecum = ryecum/mx_ryecum) %>%
  mutate(probdiff = nocum-ryecum) %>% 
  select(seeds, sc_nocum, sc_ryecum)

gina

gina %>% 
  write_csv("01_stats-stoch-dom/st_stoch-dom-res.csv")

########################## full dataset ####################################

dat_full <- 
  pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = T) %>% 
  select(-field, -rep) #%>% 
  #filter(totseeds < 860) #remove outlier


# stochastic dominance ------------------------------------------------


#--vectors of seed values for each treatment
xa <- dat_full %>% filter(cc_trt == "no") %>% select(totseeds_m2) %>% pull()
xb <- dat_full %>% filter(cc_trt == "rye") %>% select(totseeds_m2) %>% pull()
Ta = length(xa)
Tb = length(xb)
k = Ta + Tb 
pa0 = rep(1/Ta, Ta) #--probability of getting each value
pb0 = rep(1/Tb, Tb)

#--just creates a 'weighting' for the cumulative distribution
pra <- prelec2(n = Ta + Tb)


gina_full <- #--my equivalent of xpapb
  tibble(seeds = c(xa, xb)) %>% 
  mutate(prob_no = c(pa0, rep(0, Tb)),
         prob_rye = c(rep(0, Ta), pb0)) %>% 
  arrange(seeds) %>% 
  mutate(prob_no2 = prob_no * pra$pdif,
         prob_rye2 = prob_rye * pra$pdif) %>%
  filter(!is.na(seeds)) %>% 
  mutate(nocum = cumsum(prob_no2),
         ryecum = cumsum(prob_rye2)) %>% 
  mutate(mx_nocum = max(nocum),
         mx_ryecum = max(ryecum),
         sc_nocum = nocum/mx_nocum,
         sc_ryecum = ryecum/mx_ryecum) %>%
  mutate(probdiff = nocum-ryecum) %>% 
  select(seeds, sc_nocum, sc_ryecum)

gina_full

gina_full %>% 
  write_csv("01_stats-stoch-dom/st_stoch-dom-res-full.csv")




# just looking at things --------------------------------------------------


#--cum diff
gina %>% 
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

#--inv cum diff
gina %>% 
  pivot_longer(sc_nocum:sc_ryecum) %>% 
  select(seeds, name, value) %>% 
  mutate(inv_value = 1-value) %>% 
  ggplot(aes(seeds, inv_value, color = name)) + 
  geom_point(size = 3) +
  geom_line(size = 1
  ) + 
  geom_vline(xintercept = 350) +
  theme_bw() + 
  labs(x = "seeds/m2",
       y = "cumulative prob",
       title = "Un-transformed, outlier removed") 

gina %>% 
  select(seeds, probdiff) %>% 
  ggplot(aes(seeds, probdiff)) + 
  geom_point(size = 3) +
  theme_bw() 

gina %>% 
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











xpapb = matrix(0, k, 3)


for (i in 1:Ta) {
  xpapb[i, 1] = xa[i]
  xpapb[i, 2] = pa0[i]
}
for (i in 1:Tb) {
  xpapb[Ta + i, 1] = xb[i]
  xpapb[Ta + i, 3] = pb0[i]
}


gina %>% 
  ggplot(aes(seeds, value, color = name)) + 
  geom_point(size = 3) +
  geom_line(size = 1
  ) + 
  theme_bw() + 
  labs(x = "seeds/m2",
       y = "cumulative prob",
       title = "Un-transformed, outlier removed") + 
  coord_cartesian(x = c(0, 500))


sm = sort_matrix(xpapb, 1)
pa = sm[, 2]
pb = sm[, 3]
wpa = pa * pra$pdif
wpb = pb * pra$pdif

gina %>% 
  mutate(prob_no2 = prob_no * pra$pdif,
         prob_rye2 = prob_rye * pra$pdif) %>%
  filter(!is.na(seeds)) %>% 
  mutate(nocum = cumsum(prob_no2),
         ryecum = cumsum(prob_rye2)) %>% 
  mutate(mx_nocum = max(nocum),
         mx_ryecum = max(ryecum),
         sc_nocum = nocum/mx_nocum,
         sc_ryecum = ryecum/mx_ryecum) %>%
  pivot_longer(sc_nocum:sc_ryecum) %>% 
  ggplot(aes(seeds, value, color = name)) + 
  geom_line(size = 3) + 
  theme_bw()

tibble(seeds = sm[,1],
       wpa = wpa,
       wpb = wpb)

dj = sm[, 1] - sm[1, 1]
list(wpa = wpa, wpb = wpb, dj = dj)
#}




#---old....
#--non-sqrt
wtdat <- wtdpapb(stochdat$no, stochdat$rye)
stochdom <- stochdom2(wtdat$dj, wtdat$wpa, wtdat$wpb)

stochdom$sd1b %>%
  as_tibble() %>% 
  mutate(n = 1:n()) %>% 
  ggplot(aes(n, value)) + 
  geom_line()

tibble(x1 = wtdat$wpa,
         x2 = wtdat$wpb) %>% 
  mutate(x1no = cumsum(x1),
         x2rye = cumsum(x2),
         rown = 1:n()) %>% 
  pivot_longer(x1no:x2rye) %>% 
  ggplot(aes(rown, value, color = name)) + 
  geom_line() + 
  labs(title = "Non-transformed data")

tibble(seeds = sm[,1],
       wpa = wpa,
       wpb = wpb) %>%
  mutate(nocum = cumsum(wpa),
         ryecum = cumsum(wpb)) %>% 
  pivot_longer(nocum:ryecum) %>% 
  ggplot(aes(seeds, value, color = name)) + 
  geom_line(size = 3) + 
  theme_bw()


#--example
set.seed(234);x=sample(1:30);y=sample(5:34)
w1=wtdpapb(x,y) #y should dominate x with mostly positive SDs
sd1 <- stochdom2(w1$dj, w1$wpa, w1$wpb)
