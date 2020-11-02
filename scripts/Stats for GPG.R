# Notes for Good Practice Guide

rm(list=ls())
library(dplyr)
load("browseDFs.RData")

gpg_stats <- saplings_summary %>% group_by(Treat, context) %>% 
  filter(dead == 0) %>% 
  mutate(
    ht_incr = round(pmax(ht_change,0.5)/4, 3),
    dm_incr = round(dm_change/4, 3),
    ht_10y = end_ht + (10-4)/4 * ht_incr,
    ht_20y = end_ht + (20-4)/4 * ht_incr,
    ht_50y = end_ht + (50-4)/4 * ht_incr,
    dm_10y = end_dm + (10-4)/4 * dm_incr,
    dm_20y = end_dm + (20-4)/4 * dm_incr,
    dm_50y = end_dm + (50-4)/4 * dm_incr
  ) %>% 
  summarise(n = n(),
  m_ht_change = round(mean(ht_change, na.rm = TRUE),2),
  m_final_ht = round(mean(end_ht, na.rm = TRUE),2),
  m_dm_change = round(mean(dm_change, na.rm = TRUE),2),
  m_final_dm = round(mean(end_dm, na.rm = TRUE),2),
  m_ht_10y = round(1-ecdf(ht_10y)(130),2),
  m_ht_20y = round(1-ecdf(ht_20y)(130),2),
  m_ht_50y = round(1-ecdf(ht_50y)(130),2),
  m_dm_10y = round(1-ecdf(dm_10y)(35),2),
  m_dm_20y = round(1-ecdf(dm_20y)(35),2),
  m_dm_50y = round(1-ecdf(dm_50y)(35),2)
  )

library(ggplot2)

gpg_stats %>% 
 # filter(dead != 1) %>%
ggplot(aes(ht_20y)) + 
  geom_vline(xintercept=130) + 
  geom_hline(yintercept = 1) +
  geom_step(stat = "ecdf", colour = "red") +
  facet_grid(Treat~context, scales = "free")

