

require(grid)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

mortality <- ggplot(survival_site, aes(x = days, y = propAlive, colour = Treat)) +
  geom_line(aes(group = interaction(Site, Treat)), 
            alpha = 0.3) +
  geom_point(alpha = 0.5) + 
#  geom_smooth(method = 'gam', alpha = 0.4) +
  geom_point(data = survival_sum, aes(x=days, y = propAlive, color = Treat), size = 3) +
  geom_line(data = survival_sum, aes(x = days, y = propAlive, group = Treat), size = 3, alpha = 0.9) +
  labs(x = "Days elapsed of trial", y = "Proportion of saplings alive") + 
  theme_classic() + 
  scale_colour_manual(values=cbPalette) +
  facet_grid(~Habitat)

browse <- ggplot(survival_site, aes(x = days, y = propBrowsed, colour = Treat)) +
  geom_line(aes(group = interaction(Site, Treat)), alpha = 0.3) +
  geom_point(alpha = 0.5) +
  #geom_smooth(method = 'loess', alpha = 0.4, n = 10) +
  geom_point(data = survival_sum, aes(x=days, y = propBrowsed, color = Treat), size = 3) +
  geom_line(data = survival_sum, aes(x = days, y = propBrowsed,
                                     group = Treat),
            size = 3, alpha = 0.9) +
  labs(x = "Days elapsed of trial", y = "Proportion of saplings browsed") + 
  theme_classic() + 
  scale_colour_manual(values=cbPalette) +
  facet_grid(~Habitat)

png(filename = 'mort.png', width = 25.4, height = 15, units = 'cm', res = 200, pointsize = 18)
mortality
dev.off()

png(filename = 'browse.png', width = 25.4, height = 15, units = 'cm', res = 200, pointsize = 18)
browse
dev.off()
# png(filename = 'mortAndBrowse.png', width = 25.4, height = 15, units = 'cm', res = 200, pointsize = 18)
# grid.newpage()
# grid.draw(cbind(ggplotGrob(mortality), ggplotGrob(browse), size = "last"))
# dev.off()
