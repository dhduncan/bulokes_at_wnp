saplings_browse$state <- factor(saplings_browse$state, levels = c("A",  "A_wk", "A_b", "A_wk_b", "D", "D_b", "D_B", "D_nt"), ordered = TRUE)

labeller <- levels(saplings_browse$state)

stateCols <- c("A" = "chartreuse4", "A_wk" = "yellowgreen", "A_b" = "yellow3", "A_wk_b" = "cornsilk2", "D" = "black", "D_b" = "red1", "D_B" = "red3", "D_nt" = "grey")

for(s in unique(saplings_browse$Site)) {
  
  checkSite <- saplings_browse[saplings_browse$Site == s, ]
  
  png(paste("seedlingHistories/check_Browse_",s,".png",sep = ""), width = 1100, height = 900)
  print(ggplot(checkSite,aes(x=Time, y = as.numeric(state))) + 
          geom_line(color="grey") + 
          geom_point(aes(colour = state, shape = factor(Dead)), size=2) +
          scale_shape_manual(values = c(19, 8)) +
          scale_color_manual(values = stateCols) +
          labs(title = paste("Site: ",s), y = "Seedling status", x = "Visit #") +
          scale_y_continuous(breaks = 1:8, labels = labeller) +
          facet_wrap(~uniqID)
  )
  
  dev.off()
}  
