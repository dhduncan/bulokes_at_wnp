## glmmTMB testing the quadratic polynomial for dealing with non proportional hazard.

mod <- glmmTMB::glmmTMB(data = mort_data, dead ~ poly(off,2) * treat_class -treat_class + context * treatment + lagomorph + kangaroo + (1 | site), family = stats::binomial("cloglog"), offset = off)

mort_preds <- mort_pred_df(mort_data)
mort_preds$off <- log(pmax(mort_pred_set$days,0.5))
mort_preds$site <- 1

p <- predict(mod, newdata = mort_preds, type = "response")
p <- data.frame(p,mort_pred_set)




ggplot(data = p,
mapping = aes(x = days, y = 1 - p,
colour=treatment)) +
geom_line(aes(group = treatment), size=1) +
#geom_ribbon(aes(ymin = 1-mort_prob_lower, ymax = 1-mort_prob_upper,
#                fill=treatment, group = treatment),
#            alpha=0.1, linetype = 0,
#            show.legend = FALSE) +
facet_wrap(site~context) +
theme_minimal() +
labs(x = "Days elapsed", y = "Prob. of survival", colour = "Treatment") +
#theme(legend.position = 'none') +
scale_fill_manual(values = cbbPalette) +
scale_color_manual(values = cbbPalette)
