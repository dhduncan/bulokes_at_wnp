### rainfall data for the paper
library(lubridate)

# made this for Hopetoun originally, but Ami prefers Walpeup.

hptn <- read_csv("BOM_WalpeupResearch.csv")

hptn <- hptn[,3:5]
names(hptn)[3] <- "mm"

hptn_ref <- hptn[hptn$Year >= 1961 & hptn$Year <= 1990,]

anom_ref <- tibble(Month = 1:12, mean = NA, sd = NA)

for(i in 1:length(hptn$Month)) {
  
  anom_ref[i,"mean"] <- mean(hptn$mm[hptn$Month == i])
  anom_ref[i,"sd"] <- sd(hptn$mm[hptn$Month == i])
  
}


hptn$ym <- ymd(paste(hptn$Year, hptn$Month, "28", sep = '-'))

hptn <- left_join(hptn, anom_ref, by = "Month")

hptn <- hptn %>% 
  mutate(anom_pc = (mm - mean) / mean * 100,
         sd_pc = sd/mean * 100,
         experiment = if_else(ym > "2016-11-30" & ym < "2017-12-31", "active", "inactive"),
         anomaly = if_else(anom_pc < 0, "lower", "higher"))

obs_period <- data.frame(lower = ymd("2016-11-10"),
                         upper = ymd("2018-01-12"))

hptn_obs <- hptn %>% filter(Year > 2015) 
  
ggplot() + 
  #geom_point(hptn_obs, aes(ym, anom_pc)) + 
  geom_rect(data = obs_period, aes(xmin = lower, xmax = upper, 
                                   ymin = -100, ymax = 300), fill = "grey95") + 
  geom_hline(yintercept = 0, size = 0.25) +
  geom_line(data = hptn_obs, aes(ym, anom_pc), colour = "grey30", linetype = 3) + geom_point(data = hptn_obs, aes(ym, anom_pc, color = anom_pc), size = 2) + 
  labs(y = "Rainfall anomaly (%) recorded at Walpeup\ncompared with period 1961–1990",
       x = "Date") + 
  scale_shape_manual(values = c(19, 1)) + 
  scale_color_gradientn(colors = c("black", "grey", "red"), values = c(1,0.5,.15, 0), name = "Rainfall\nanomaly (%)") + 
  theme_bw()

#+ 
  #scale_color_gradient2(low = "red", mid = "grey", high = "black", midpoint = 0.4)

  



