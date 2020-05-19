##################################
# Author: Gina Nichols (vnichols@iastate.edu)
# Created: 4/14/2020
#
# Last modified: 
#
# Purpose: do 'official' frequentist stats for manuscript, cleanly
#
# Outputs: sd_estimates, sd_contrasts
#
# Notes:
####################################

rm(list = ls())
library(tidyverse)
#devtools::install_github("vanichols/PFIweeds2020", force = TRUE) ## <-- run if pkg changes
library(PFIweeds2020)


# data --------------------------------------------------------------------

#--use data and fucntion from package

pfi_ghobsraw

dat <- pfifun_sum_byeu(pfi_ghobsraw) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = T) %>% 
  select(-field, -rep) %>% 
  filter(totseeds < 860)


dat %>% write_csv("data/misc-data-for-matt.csv")


# try stochastic dominance ------------------------------------------------
library(generalCorr)


#--wtf is waptb doing

#function (xa, xb) 
#{

xa <- dat %>% filter(cc_trt == "no") %>% select(totseeds_m2) %>% pull()
xb <- dat %>% filter(cc_trt == "rye") %>% select(totseeds_m2) %>% pull()

Ta = length(xa)
Tb = length(xb)
k = Ta + Tb #--why do this? no idea
pa0 = rep(1/Ta, Ta) #--probability of getting each value
pb0 = rep(1/Tb, Tb)


xpapb = matrix(0, k, 3)


for (i in 1:Ta) {
  xpapb[i, 1] = xa[i]
  xpapb[i, 2] = pa0[i]
}
for (i in 1:Tb) {
  xpapb[Ta + i, 1] = xb[i]
  xpapb[Ta + i, 3] = pb0[i]
}

pra = prelec2(n = Ta + Tb)
pra$pdif



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
  pivot_longer(sc_nocum:sc_ryecum)


gina %>% 
  ggplot(aes(seeds, value, color = name)) + 
  geom_point(size = 3) +
  geom_line(size = 1
            ) + 
  theme_bw() + 
  labs(x = "seeds/m2",
       y = "cumulative prob",
       title = "Un-transformed, outlier removed")



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
