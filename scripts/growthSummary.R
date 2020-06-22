#### Additional graphs for ESA
rm(list=ls())
library(tidyverse)
load("browseDFs.RData")

cbbPalette <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D000000", "#CC79A7")

saplings_summary$context <- factor(saplings_summary$context, 
                                   levels = c("Buloke woodland",
                                              "Open grassland",
                                              "Wattle dune",
                                              "Mallee"),
                          ordered = T)


tinyContext = c("Buloke woodland" = "Buloke\nWoodland",
                "Open grassland" = "Derived\nOpen Grassland",
                "Wattle dune" = "adj.\nWattle Dune",
                "Mallee" = "adj. Mallee\nWoodland")

#pdf(file="heightChangeSummary.pdf", width = 7.32, height = 1.35)

saplings_summary %>% group_by(Treat, context, site) %>% 
  summarise(ht_change = mean(ht_change, na.rm = TRUE)) %>% 
ggplot(aes(x = Treat, y = ht_change)) + 
  geom_hline(yintercept = 0, colour = "dark grey") +
  #geom_boxplot(show.legend = FALSE) +
  geom_jitter(aes(color = Treat), alpha = 0.9, width = 0.35,
              show.legend = F) + 
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", width = 0, size = 0.75, size=0.5) + 
  geom_point(stat = "summary", fun.y = mean, shape = 21, fill = "white", size = 1) +
  facet_grid(.~context, labeller = as_labeller(tinyContext)) + 
  scale_color_manual(values = cbbPalette) +
  scale_x_discrete(labels = c("Open" = "U", "Partial" = "PG", "Total" = "FG")) +
  labs(x = "Treatment", y = "Height change (cm) over experiment period") +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size=12, margin = margin(5,0,5,0)),
    axis.title.y = element_text(size=12, margin = margin(0,5,0,5)),
    axis.text = element_text(size = rel(1), margin = c(0,0,0,0)),
   # axis.text.x = element_text(hjust = 0.8, vjust = 0),
    text = element_text(size = 8),
    strip.background =element_blank(),
    strip.text = element_text(size = 12, margin = margin(2,2,2,2)),
   #  legend.text = element_text(size = rel(1.2), margin = margin(10,0,0,0)),
   #  legend.margin = margin(0,50,0,0),
   # legend.key.size =  unit(1,"cm"),
   plot.margin = margin(.1, .1, .1, .1),
 )

#dev.off()  


saplings_summary %>% group_by(Treat, context, site) %>% 
  summarise(end_ht = mean(end_ht, na.rm = TRUE)) %>% 
  ggplot(aes(x = Treat, y = end_ht)) + 
  geom_hline(yintercept = 0, colour = "dark grey") +
  #geom_boxplot(show.legend = FALSE) +
  geom_jitter(aes(color = Treat), alpha = 0.9, width = 0.35,
              show.legend = F) + 
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", width = 0, size = 0.75, size=0.5) + 
  geom_point(stat = "summary", fun.y = mean, shape = 21, fill = "white", size = 1) +
  facet_grid(.~context, labeller = as_labeller(tinyContext)) + 
  scale_color_manual(values = cbbPalette) +
  scale_x_discrete(labels = c("Open" = "U", "Partial" = "PG", "Total" = "FG")) +
  labs(x = "Treatment", y = "Height (cm) at end") +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size=12, margin = margin(5,0,5,0)),
    axis.title.y = element_text(size=12, margin = margin(0,5,0,5)),
    axis.text = element_text(size = rel(1), margin = c(0,0,0,0)),
    # axis.text.x = element_text(hjust = 0.8, vjust = 0),
    text = element_text(size = 8),
    strip.background =element_blank(),
    strip.text = element_text(size = 12, margin = margin(2,2,2,2)),
    #  legend.text = element_text(size = rel(1.2), margin = margin(10,0,0,0)),
    #  legend.margin = margin(0,50,0,0),
    # legend.key.size =  unit(1,"cm"),
    plot.margin = margin(.1, .1, .1, .1),
  )

saplings_summary %>% group_by(Treat, context, site) %>% 
  summarise(dm_change = mean(dm_change, na.rm = TRUE)) %>% 
  ggplot(aes(x = Treat, y = dm_change)) + 
  geom_hline(yintercept = 0, colour = "dark grey") +
  #geom_boxplot(show.legend = FALSE) +
  geom_jitter(aes(color = Treat), alpha = 0.9, width = 0.35,
              show.legend = F) + 
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", width = 0, size = 0.75, size=0.5) + 
  geom_point(stat = "summary", fun.y = mean, shape = 21, fill = "white", size = 1) +
  facet_grid(.~context, labeller = as_labeller(tinyContext)) + 
  scale_color_manual(values = cbbPalette) +
  scale_x_discrete(labels = c("Open" = "U", "Partial" = "PG", "Total" = "FG")) +
  labs(x = "Treatment", y = "Stem diameter change (mm)") +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size=12, margin = margin(5,0,5,0)),
    axis.title.y = element_text(size=12, margin = margin(0,5,0,5)),
    axis.text = element_text(size = rel(1), margin = c(0,0,0,0)),
    # axis.text.x = element_text(hjust = 0.8, vjust = 0),
    text = element_text(size = 8),
    strip.background =element_blank(),
    strip.text = element_text(size = 12, margin = margin(2,2,2,2)),
    #  legend.text = element_text(size = rel(1.2), margin = margin(10,0,0,0)),
    #  legend.margin = margin(0,50,0,0),
    # legend.key.size =  unit(1,"cm"),
    plot.margin = margin(.1, .1, .1, .1),
  )

saplings_summary %>% group_by(Treat, context, site) %>% 
  summarise(stem_dm = mean(end_dm, na.rm = TRUE)) %>% 
  ggplot(aes(x = Treat, y = stem_dm)) + 
  geom_hline(yintercept = 0, colour = "dark grey") +
  #geom_boxplot(show.legend = FALSE) +
  geom_jitter(aes(color = Treat), alpha = 0.9, width = 0.35,
              show.legend = F) + 
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", width = 0, size = 0.75, size=0.5) + 
  geom_point(stat = "summary", fun.y = mean, shape = 21, fill = "white", size = 1) +
  facet_grid(.~context, labeller = as_labeller(tinyContext)) + 
  scale_color_manual(values = cbbPalette) +
  scale_x_discrete(labels = c("Open" = "U", "Partial" = "PG", "Total" = "FG")) +
  labs(x = "Treatment", y = "Stem diameter at end (mm)") +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size=12, margin = margin(5,0,5,0)),
    axis.title.y = element_text(size=12, margin = margin(0,5,0,5)),
    axis.text = element_text(size = rel(1), margin = c(0,0,0,0)),
    # axis.text.x = element_text(hjust = 0.8, vjust = 0),
    text = element_text(size = 8),
    strip.background =element_blank(),
    strip.text = element_text(size = 12, margin = margin(2,2,2,2)),
    #  legend.text = element_text(size = rel(1.2), margin = margin(10,0,0,0)),
    #  legend.margin = margin(0,50,0,0),
    # legend.key.size =  unit(1,"cm"),
    plot.margin = margin(.1, .1, .1, .1),
  )

saplings_summary %>% group_by(Treat, context, site) %>% 
  summarise(stem_dm = mean(end_dm, na.rm = TRUE)) %>% 
  ggplot(aes(x = Treat, y = stem_dm)) + 
  geom_hline(yintercept = 0, colour = "dark grey") +
  #geom_boxplot(show.legend = FALSE) +
  geom_jitter(aes(color = Treat), alpha = 0.9, width = 0.35,
              show.legend = F) + 
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", width = 0, size = 0.75, size=0.5) + 
  geom_point(stat = "summary", fun.y = mean, shape = 21, fill = "white", size = 1) +
  facet_grid(.~context, labeller = as_labeller(tinyContext)) + 
  scale_color_manual(values = cbbPalette) +
  scale_x_discrete(labels = c("Open" = "U", "Partial" = "PG", "Total" = "FG")) +
  labs(x = "Treatment", y = "Stem diameter at end (mm)") +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size=12, margin = margin(5,0,5,0)),
    axis.title.y = element_text(size=12, margin = margin(0,5,0,5)),
    axis.text = element_text(size = rel(1), margin = c(0,0,0,0)),
    # axis.text.x = element_text(hjust = 0.8, vjust = 0),
    text = element_text(size = 8),
    strip.background =element_blank(),
    strip.text = element_text(size = 12, margin = margin(2,2,2,2)),
    #  legend.text = element_text(size = rel(1.2), margin = margin(10,0,0,0)),
    #  legend.margin = margin(0,50,0,0),
    # legend.key.size =  unit(1,"cm"),
    plot.margin = margin(.1, .1, .1, .1),
  )
