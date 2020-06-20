##### Assorted density estimates for Western Grey Kangaroo #####
library(tidyverse)

# data entry from Taylor and Pegler (2016 Appendix 13, Pine Plains section) ----
yr <- 1993:2016
d_km2 <- c(7, 4.9, 3.5, 4.8, 5.4, 7.9, 15.59, 24.7, 26.3, 32.66, 11.4, NA, 7.76, 17.03, 11.22, 16, 4.3, 1.87, NA, 2.74, 1.82, 28.91, 25.02, 16.5)
d_CV <- c(rep(NA, 12), 38.15, 24.2, 30.42, 25.78, 30, 19, NA, 24, 39.77, 27, 
          23, 33)
l_CI <- c(rep(NA, 12), 3.6, 11.15, 6.04, 9.23, 2.41, 1.29, NA, 1.71, 0.72, 15.3,
          15.8, 7.89)
u_CI <- c(rep(NA, 12), 17.1, 26.45, 20.82, 27.72, 7.68, 2.72, NA, 4.37, 3.43,
          54.66, 39.63, 34.47)

# combine data
tp_wgk <- tibble(yr, d_km2, d_CV, l_CI, u_CI)

rm(yr, d_km2, d_CV, l_CI, u_CI)

# OEH data for western plains ----
yr <- 1997:2012
d_km2 <- c(4.33, 3.97, 4.5, 3.86, 4.53, 5.02, 2.72, 1.89, 1.31, 1.35, 1.55, 
           2.07, 1.49, 1.39, 1.02, 1.83)

oeh_wgk <- tibble(yr, d_km2)

rm(yr, d_km2)

# other sources ----
Cheal86 <- tibble(yr = c(1982, 1984), d_km2 = c(45, 12))
NESP <- tibble(yr = 2016.2, d_km2 = 100)

#Thackaringa <- 

# plot data ----
ggplot(tp_wgk, aes(yr, d_km2)) + ylim(0,110) + xlim(1980,2018) +
  geom_linerange(data = tp_wgk, aes(ymin = l_CI, ymax = u_CI)) + 
  scale_y_log10() +
  geom_point() + geom_line() + 
  geom_point(data = NESP, aes(yr, d_km2), colour = "red") +
  geom_point(data = Cheal86, aes(yr, d_km2), colour = "red") + 
  geom_line(data = oeh_wgk, mapping = aes(yr, d_km2), colour = "blue") +
  geom_point(data = oeh_wgk, mapping = aes(yr, d_km2), colour = "blue") +
  labs(y = "Estimated density n km^2") +
  annotate("text", x = 1990, y = 1, label = "NSW Western Plains (OEH 2016)",
           color = "blue")



