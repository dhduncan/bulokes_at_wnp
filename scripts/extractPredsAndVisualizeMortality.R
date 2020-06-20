# Suite of functions to get model predictions, and graph them over either the raw data or fitted values

# Get model predictions -----

get_gam_preds_mort <- function(mod_obj, df = st_cent_surv, 
                           q_roo = 0.5, q_rab = 0.5) {
  
  pred_df <- data.frame("elapsedDays"=rep(seq(min(df$elapsedDays),
                                              max(df$elapsedDays),
                                              length = 30), times = 3), 
                        "Treat" = rep(c("Open", "Partial", "Total"), 
                                      each = 30),
                        "Roo" = quantile(df$Roo[df$Time != 0], 
                                         probs = q_roo),
                        "Roo_site_mean" = quantile(df$Roo_site_mean[df$Time != 0],
                                                   probs = q_roo),
                        "Lagomrph" = quantile(df$Lagomrph[df$Time != 0], 
                                              probs = q_rab),
                        'Lagomrph_site_mean' = mean(df$Lagomrph_site_mean[df$Time != 0],
                                                    probs = q_rab),
                        "browsed" = 0,
                        "prev_browse" = 0,
                        "sand" = mean(df$sand),
                        "off" = log(pmax(rep(seq(0, 406, length = 30), 
                                             times = 3), 1)),
                        'PC1' = mean(df$PC1),
                        'PC2' = mean(df$PC2),
                        'uniqID' = '1.O10',
                        "site" = 1)
  
  context <- c('Buloke woodland', 'Wattle dune', 'Open grassland', 'Mallee')
  
  pred_df <- merge(pred_df, as.data.frame(context))
  
  pred_se <- as.data.frame(
    predict.gam(
      mod_obj, newdata = pred_df, type = 'response', 
      exclude = c('s(site)', 's(uniqID)'), 
      se.fit = TRUE
    )
  )
  
  cbind(pred_df, pred_se)
  
}

# Get fit from mortality model ----

get_mort_fit <- function(mod_obj, df = st_cent_surv) {
  
  if(length(mod_obj$fitted.values) != nrow(df)) {
    
    mod_fit <- dplyr::bind_cols(as.data.frame(1 - mod_obj$fitted.values), filter(df, Time != 0))
    
  } else {
    
    mod_fit <- dplyr::bind_cols(as.data.frame(1 - mod_obj$fitted.values), df)
    
  }
  
}

# Graph predictions over observed data or fitted values

pred_viz <- function(pred_df, fit_df = NULL, bg_type = c("obs", "none"), contrast = c("global", "context"))  {
 
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                 "#D55E00", "#CC79A7", "#999999")
  
  if(contrast == 'global') {
   
  if(bg_type == "obs") {
    
    print(ggplot(data = survival_site, 
                 aes(x= (days - mean(days)) / (2*sd(days)), 
                                    y = propAlive, color = Treat)) +
            geom_point(alpha = 0.15) +
            geom_line(aes(group = interaction(site, Treat)),
                      alpha = 0.15) +
            geom_line(data = pred_df, 
                      aes(x = elapsedDays, y = 1 - fit, colour = Treat, 
                          group = Treat), size = 1, alpha = 0.7) +
            geom_ribbon(data = pred_df,
                        aes(x = elapsedDays, y = 1 - fit,
                          ymin = (1 - fit - se.fit), 
                            ymax = (1 - fit + se.fit),
                            fill = Treat, colour = Treat), 
                        linetype = 'blank', alpha = 0.3) +
            theme_classic() + 
        theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12)) +
        labs(subtitle = unique(pred_df$context)[1]) +
        scale_colour_manual(values=cbPalette) +
            scale_fill_manual(values=cbPalette) +
            labs(x = "Days elapsed of trial", 
                 y = "Proportion of saplings alive")
    )
    
  } else #if(bg_type == "fit") {
    
  #   names(fit_df)[1] <- 'm_fit'
  #   
  #   print(
  #     ggplot(data = fit_df, aes(x = elapsedDays, y = m_fit, 
  #                        colour = Treat)) +
  #       geom_point(alpha = 0.15) +
  #       geom_line(aes(group = interaction(site, Treat)),
  #                 alpha = 0.15) +
  #       geom_line(data = pred_df, 
  #                     aes(x = elapsedDays, y = 1 - fit, colour = Treat, 
  #                         group = Treat), size = 1, alpha = 0.7) +
  #           geom_ribbon(data = pred_df, 
  #             aes(x = elapsedDays, 
  #             y = 1 - fit,ymin = (1 - fit - se.fit), 
  #                           ymax = (1 - fit + se.fit),
  #                           fill = Treat, colour = Treat), 
  #                       linetype = 'blank', alpha = 0.3) +
  #           theme_classic() + 
  #       theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12)) +
  #           scale_colour_manual(values=cbPalette) +
  #           scale_fill_manual(values=cbPalette) +
  #           labs(x = "Days elapsed of trial", 
  #                y = "Proportion of saplings alive")
  #   )
  #   
  # } else {
  {
    print(ggplot(pred_df, aes(x = elapsedDays, y = 1 - fit, colour = Treat)) +
            geom_line(aes(group = Treat), size = 1, alpha = 0.7) +
            geom_ribbon(aes(ymin = (1 - fit - se.fit), 
                            ymax = (1 - fit + se.fit),
                            fill = Treat, colour = Treat), 
                        linetype = 'blank', alpha = 0.3) +
            theme_classic() + 
            scale_colour_manual(values=cbPalette) +
            scale_fill_manual(values=cbPalette) +
        theme(axis.text = element_text(size = 12)) +
            labs(x = "Days elapsed of trial", 
                 y = "Proportion of saplings alive")
    )
    
  }
    } else {
    
      if(bg_type == 'obs') {
        
        print(ggplot(data = survival_site, 
                     aes(x= (days - mean(days)) / (2*sd(days)), 
                         y = propAlive, color = Treat)) +
                geom_point(alpha = 0.15) +
                geom_line(aes(group = interaction(site, Treat)),
                          alpha = 0.15) +
                geom_line(data = pred_df, 
                          aes(x = elapsedDays, y = 1 - fit, colour = Treat, 
                              group = Treat), size = 1, alpha = 0.7) +
                geom_ribbon(data = pred_df,
                            aes(x = elapsedDays, y = 1 - fit,
                                ymin = (1 - fit - se.fit), 
                                ymax = (1 - fit + se.fit),
                                fill = Treat, colour = Treat), 
                            linetype = 'blank', alpha = 0.3) +
                facet_grid(.~ context) +
                theme_classic() + 
                theme(axis.title = element_text(size = 12), axis.text = element_text(size = 12)) +
                scale_colour_manual(values=cbPalette) +
                scale_fill_manual(values=cbPalette) +
                labs(x = "Days elapsed of trial", 
                     y = "Proportion of saplings alive")
        )
            
      } else {  # no obs in background
        
        print(
          ggplot(data = pred_df, 
                     aes(x = elapsedDays, y = 1 - fit, colour = Treat)) +
                       geom_line(aes(group = Treat), size = 1, alpha = 0.7) +
                geom_ribbon(aes(ymin = (1 - fit - se.fit), 
                                ymax = (1 - fit + se.fit),
                                fill = Treat, colour = Treat), 
                            linetype = 'blank', alpha = 0.3) +
                facet_grid(.~ context) +
                theme_classic() + 
                theme(axis.title = element_text(size = 12), axis.text = element_text(size = 12)) +
                scale_colour_manual(values=cbPalette) +
                scale_fill_manual(values=cbPalette) +
                labs(x = "Days elapsed of trial", 
                     y = "Proportion of saplings alive")
        )
      } ## no obs
    } ## end context loop
} ## end pred viz function
