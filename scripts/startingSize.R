### Confirm that there were no height or stem diameter biases at start of trial

load("browseDFs.RData")

library(tidyverse)

t0 <- saplings %>% 
  select(Treat,HtCM,StDMM,uniqID,Habitat,Time) %>% 
  filter(Time==0)

# Note that including Habitat in the following model reveals that Open Grassland had marginally larger saplings on average, and Mallee smaller, and that if you allow for an interaction between Treatment and Habitat then partially protected saplings on open grasslands were larger to begin with, etc. The point is, there are starting diffs though none that are particularly troubling with regard to the design.

summary(height_mod <- lm(log(HtCM) ~ Treat -1, data = t0))

summary(stem_mod <- lm(log(StDMM) ~ Treat - 1, data = t0))
