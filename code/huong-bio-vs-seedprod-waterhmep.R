# created 4/14/2020
# purpose: look at relationship between waterhemp biomass and seed production
# note: using huong's data

library(tidyverse)
library(readxl)

dat <- read_excel("data/raw/huong_waterhemp-seeds.xlsx")


dat %>% 
  filter(Seed == 0) %>% 
  ggplot(aes(Biomass)) +
  geom_histogram()

# what are the units for Biomass?

dat %>% 
  ggplot(aes(Biomass)) + 
  geom_histogram() + 
  scale_x_continuous(limits = c(0, 100))

# a nickel is 5 grams. Biomass must be in grams. So <1 gram plants don't produce seeds. 

dat %>% 
  ggplot(aes(log(Biomass), log(Seed))) + 
  geom_point()


#--log-linear relationship. What does that mean?

#https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/

# Both dependent/response variable and independent/predictor variable(s) are log-transformed. 
# Interpret the coefficient as the percent increase in the dependent variable for every 1% increase in the independent variable. 
# Example: the coefficient is 0.198. For every 1% increase in the independent variable, our dependent variable increases by about 0.20%. 
# For x percent increase, calculate 1.x to the power of the coefficient, subtract from 1, and multiply by 100. 
# Example: For every 20% increase in the independent variable, our dependent variable increases by about (1.20 0.198 – 1) * 100 = 3.7 percent.


ldat <- 
  dat %>% 
  filter(Seed != 0) %>% 
  mutate(lbio = log(Biomass),
         lseed = log(Seed))

# Force the intercept through 0
lm1 <- lm(log(Seed) ~ log(Biomass), data = dat %>% filter(Seed != 0))
lm2 <- lm(lseed ~ lbio, data = ldat)

library(broom)

tidy(lm1$coefficients)
tidy(lm2$coefficients)

# So for eery 1% increase in Biomass, you get a 1.2% inc in number of seeds

ggplot(data = ldat, aes(x = lbio, y = lseed)) + 
  geom_point() + 
  geom_line(aes(y = fitted(lm2)), color = "red", size = 3)
