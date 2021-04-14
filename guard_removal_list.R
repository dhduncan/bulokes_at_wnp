saplings_summary %>% 
  filter(Treat != "Open" & dead == 0) %>% 
  group_by(context, Treat, site) %>% 
  summarise(alive = n()) %>% 
  summarise(min = min(alive),
            max = max(alive))
            
for_sampling <- saplings_summary %>% 
  filter(Treat != "Open" & dead == 0) %>% 
  group_by(context, Treat, site) %>% 
  mutate(size_cut = if_else(end_dm <= median(end_dm, na.rm=TRUE), "Shorter", "Taller"))

#check balance
#ggplot(for_sampling, mapping = aes(size_cut, fill = Treat)) + geom_bar() + facet_wrap(~site)

for_action <- na.omit(for_sampling) %>% 
  group_by(context, site, Treat, size_cut) %>% 
  slice_sample(prop = 0.5) %>% 
  mutate(action = "Remove Guard")
  
#ggplot(for_action, mapping = aes(size_cut, fill = Treat)) + geom_bar() + facet_wrap(~site)

library(openxlsx)            
            
#write.xlsx(for_action, "remove_guard_list.xlsx")

for_sampling <- left_join(for_sampling, select(ungroup(for_action), uniqID, action), by = "uniqID")

write.xlsx(for_sampling, "remove_guard_list.xlsx", addWorksheet = TRUE)            
