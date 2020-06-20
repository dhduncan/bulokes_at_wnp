(a <- saplings_summary %>% 
  group_by(Treat) %>%
  filter(state != 'D_nt') %>% 
  summarise(n_dead = sum(dead),
            n_browsed = sum(browsed),
            x_ht = mean(ht_change, na.rm=TRUE),
            sd_ht = sd(ht_change, na.rm=TRUE),
            x_dm = mean(dm_change, na.rm=TRUE),
            sd_dm = sd(dm_change, na.rm=TRUE),
            n = n()))

with(saplings_summary, table(state, Treat))

(b <- saplings_summary %>% 
        group_by(Treat) %>% 
        summarise(n_dead = sum(dead),
                  n_browsed = sum(browsed),
                  x_ht_change = mean(ht_change, na.rm=TRUE),
                  sd_ht_change = sd(ht_change, na.rm=TRUE),
                  x_dm_change = mean(dm_change, na.rm=TRUE),
                  sd_dm_change = sd(dm_change, na.rm=TRUE),
                  x_ht_1 = mean(start_ht, na.rm=TRUE),
                  x_sd_1 = mean(start_dm, na.rm=TRUE),
                  n = n()))


ggplot(saplings_summary, aes(x = dm_change, y = ht_change, colour=state)) + 
  geom_point(aes()) + 
  facet_grid(Treat~Context) + 
  geom_smooth(method = 'glm') + 
  geom_hline(yintercept = 0, colour = "grey") + 
  theme_minimal()  + 
  geom_vline(xintercept = 0, colour = "grey")
