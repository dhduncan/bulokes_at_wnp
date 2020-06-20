library (greta)
#library(bayesplot)
library(MCMCvis)
library(tidyverse)
#library(future)

rm(list=ls())
source("scripts/gretaFunction_changedQuestionMark.R")


# load base data files ----

load("browseDFs.RData")

contextNameNarrow <- c(
  'Buloke woodland' = "Within buloke\nstand",
  'Open grassland' = "Open\ngrassy",
  'Mallee' = "Adj. mallee\nwoodland",
  'Wattle dune' = "Adj. wattle\ndune"
)

contextName <- c(
  'Buloke woodland' = "Within buloke stand",
  'Open grassland' = "Open grassy",
  'Mallee' = "Adj. mallee woodland",
  'Wattle dune' = "Adj. wattle dune"
)

treatName <- c(
  'Open' = "Open to graze",
  'Partial' = "Partial exclosure",
  'Total' = "Full exclosure"
)



# our browsing data ----

browse_CnS <- ungroup(browse_alt) %>%
  mutate_at(vars(HtCM, StDMM, sand:start_dm), 
            function(x) (x - mean(x, na.rm = TRUE)) / (2 * sd(x, na.rm = TRUE)))

real_data <- browse_CnS %>% 
  mutate_if(is.character, factor) %>% 
  mutate_at(vars(site, Time, Dead), funs(factor)) %>% 
  filter(Treat != 'Total') %>% 
  transmute(context, site, time = Time, treatment = droplevels(Treat), 
            days = elapsedDays, browsed = factor(browsed), 
            lagomorph = Lagomrph_site_mean, kangaroo = Roo_site_mean, 
            sand)

browse_lp <- ~ context * treatment + 
  lagomorph * treatment + 
  kangaroo * treatment

browse_design <- prep_design(glm_formula = browse_lp, data_obj = real_data)
browse_coefs <- prep_coefs(design_list = browse_design)

browse_probs <- pred_prob(design_list = browse_design, coef_list = browse_coefs, random_effect = TRUE, type = 're')

distribution(real_data$browsed) <- bernoulli(browse_probs)
#distribution(real_data$browsed) <- binomial(real_data$total, brows_prob)

browse_mod <- model(browse_coefs$coef, precision = "double") # can monitor whatever in here, e.g.,  site_sd 

# hack the initial values so that the intercept is small
#inits <- c(-5, rep(0, length(browse_mod$dag$example_parameters()) - 1))

coef <- browse_coefs$coef
coefs_init_mean <- c(-7, rep(0, length(coef)-1))
inits <- replicate(4, initials(coef = rnorm(length(coef), coefs_init_mean, 0.7)), simplify = FALSE)


# run greta model for browse hazard ----
#plan(multisession)
browse_draws <- mcmc(browse_mod,
              n_samples = 5000, 
              initial_values = inits)

coda::gelman.diag(browse_draws)


# squeeze inits on junk interactions
b_coefs_init_sd <- c(rep(0.6, length(coef)-1), rep(0.05,1))
browse_inits_2 <- replicate(4, initials(coef = rnorm(length(coef), coefs_init_mean, b_coefs_init_sd)), simplify = FALSE)

browse_draws_2 <- mcmc(browse_mod,
              n_samples = 5000, 
              initial_values = browse_inits_2)

# # i


coda::varnames(browse_draws)[1:length(browse_coefs$coef)] <- colnames(browse_design$matrix)

coda::varnames(browse_draws)[1:length(browse_coefs$coef)] <- c("intercept",
                                                               "adj_mallee",
                                                               "open_grassy",
                                                               "adj_dune",
                                                               "excl_partial",
                                                               "rabbit",
                                                               "kangaroo",
                                                               "partial*mallee",
                                                               "partial*grassy",
                                                               "partial*dune",
                                                               "partial*rabbit",
                                                               "partial*'roo")


# plot browse model result ----

plot(browse_draws)
coef_summary <- summary(browse_draws)[1]

#color_scheme_get()

#all_but_intercept <- coda::varnames(browse_draws)[-1]

#bayesplot::mcmc_intervals(browse_draws, 
#                          pars = all_but_intercept, 
#                          prob_outer = 0.95) + theme_minimal()

png(filename = "gfx/browseCoefPlot.png", width = 1400, height = 0.8*1080,
    units = "px")
par(mgp = c(3.1,0.75,0))
MCMCvis::MCMCplot(browse_draws, excl = "intercept", 
                  ref_ovl = T, rank = T, ax_sz = 1,
                  xlab = "Parameter estimate\n(cloglog scale)")
dev.off()

# make browse prediction set ----
df_browse_pred <- browse_pred_df(df = real_data)

predict_browse <- prep_design(glm_formula = browse_lp, 
                              data_obj = df_browse_pred)

predict_probs <- pred_prob(design_list = predict_browse, 
                           coef_list = browse_coefs, 
                           random_effect = FALSE, type = 're')

test <- calculate(predict_probs, browse_draws)

browse_prob_pred_summary <- summary(test)
browse_prob_pred_mean <- browse_prob_pred_summary$statistics[, "Mean"]
browse_prob_pred_CI <- browse_prob_pred_summary$quantiles[, c(1, 5)]

browse_output_df <- data.frame(df_browse_pred, browse_prob_pred_mean, browse_prob_pred_CI)

browse_output_df <- browse_output_df %>% 
  mutate(context = factor(context, 
                          levels = c("Buloke woodland", "Open grassland",
                                     "Wattle dune" , "Mallee")
                          )
  )
  
  
# plot predicted (adjusted) values ----
cbbPalette <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D000000", "#CC79A7")

# ggplot(data = browse_output_df, 
#        mapping = aes(x = days, 
#                      y = browse_prob_pred_mean, 
#                      colour=factor(treatment))) + 
#   facet_grid(treatment~context) + 
#   geom_line(aes(group = interaction(treatment,factor(lagomorph))), 
#             size=0.8) + 
#   geom_ribbon(aes(ymin = X2.5., ymax = X97.5., 
#                   group = interaction(treatment,factor(lagomorph)), 
#                   fill=factor(lagomorph)), alpha=0.1) + theme_bw()



lago_high <- browse_output_df %>% 
  filter(lagomorph == max(lagomorph)) %>% 
  group_by(treatment, context, days) %>% 
  summarise(browse_prob_pred_mean = mean(browse_prob_pred_mean),
            browse_prob_upper = mean(X97.5.),
            browse_prob_lower = mean(X2.5.))

lago_med <- browse_output_df %>% 
  filter(lagomorph == median(lagomorph)) %>% 
  group_by(treatment, context, days) %>% 
  summarise(browse_prob_pred_mean = mean(browse_prob_pred_mean),
            browse_prob_upper = mean(X97.5.),
            browse_prob_lower = mean(X2.5.))

lago_low <- browse_output_df %>% 
  filter(lagomorph == min(lagomorph)) %>% 
  group_by(treatment, context, days) %>% 
  summarise(browse_prob_pred_mean = mean(browse_prob_pred_mean),
            browse_prob_upper = mean(X97.5.),
            browse_prob_lower = mean(X2.5.))

dev.new()
png("gfx/browsePredFig.png", width = 1920, height = 0.8*1080, units = "px") 
ggplot(data = lago_med, aes(x = days, y = browse_prob_pred_mean, 
    colour=treatment, 
    group =treatment)) + 
  geom_ribbon(aes(ymin = browse_prob_lower, 
                  ymax = browse_prob_upper, 
                  group = treatment, 
                  fill=factor(treatment)), alpha=0.2, 
              show.legend = FALSE, linetype = 0) + 
  geom_line(size=2) + #scale_size(range = c(2,1), guide = FALSE) +
  #facet_grid(.~context)
  facet_grid(.~context, 
             labeller = labeller(context = as_labeller(contextName))) + 
  theme_minimal() +
  labs(x = "Days elapsed after planting", y = "Prob. browse damage to main stem", colour = "Treatment") +
  scale_fill_manual(values = cbbPalette) +
  scale_color_manual(values = cbbPalette) +
  theme(
  axis.title.x = element_text(size=40, margin = margin(20,0,20,0)),
  axis.title.y = element_text(size=40, margin = margin(0,20,0,50)),
  axis.text = element_text(size = rel(1)),
  #axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8),
  text = element_text(size = 30),
  strip.background =element_blank(),
  strip.text = element_text(size = 40, margin = margin(5,5,15,5)),
  legend.text = element_text(size = rel(1.2), margin = margin(10,0,0,0)),
  legend.margin = margin(0,50,0,0),
  legend.key.width = unit(1,"cm")
) +
guides(linetype = guide_legend(override.aes = list(size = 4)))
dev.off()


png("gfx/browsePredFig_2.png", width = 1920, height = 0.8*1080, units = "px")
browse_output_df %>% 
  group_by(treatment, context, kangaroo, days) %>% 
  summarise(browse_prob_pred_mean = mean(browse_prob_pred_mean)) %>% 
  ggplot(mapping = aes(x = days, y = browse_prob_pred_mean, 
    colour=treatment, group =interaction(factor(kangaroo),treatment))) + 
  facet_grid(.~context) + 
  geom_line(size=0.8) + theme_bw() +
  theme(
    axis.title.x = element_text(size=40, margin = margin(20,0,20,0)),
    axis.title.y = element_text(size=40, margin = margin(0,20,0,50)),
    axis.text = element_text(size = rel(1)),
    #axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8),
    text = element_text(size = 30),
    strip.background =element_blank(),
    strip.text = element_text(size = 40, margin = margin(5,5,15,5)),
    legend.text = element_text(size = rel(1.2)),
    legend.margin = margin(0,50,0,0),
    legend.key.width = unit(1,"cm")
  )
dev.off()


png("gfx/browsePredFig_3.png", width = 1920, height = 0.8*1080, units = "px")
browse_output_df %>% 
  filter(days==365) %>% 
  group_by(treatment, context, kangaroo) %>% 
  summarise(browse_prob_pred_mean = mean(browse_prob_pred_mean),
    lower = mean(X2.5.),
    upper = mean(X97.5.)) %>% 
  ggplot(mapping = aes(x = kangaroo, 
    y = browse_prob_pred_mean, 
    colour=treatment, group =treatment)) + 
  geom_line(size=0.8) + 
  theme_bw() + 
  geom_ribbon(aes(ymin=lower, ymax=upper), 
    alpha = 0.2, linetype = 0) + 
  labs(x = "Kangaroo pellets (standardised)",
    y = "Probability of main stem browse damage") +
  facet_grid(.~context, 
             labeller = labeller(context = as_labeller(contextNameNarrow))) +
  scale_fill_manual(values = cbbPalette) +
  scale_color_manual(values = cbbPalette) +
  theme(
    axis.title.x = element_text(size=40, margin = margin(20,0,20,0)),
    axis.title.y = element_text(size=40, margin = margin(0,20,0,50)),
    axis.text = element_text(size = rel(1)),
    #axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8),
    text = element_text(size = 30),
    strip.background =element_blank(),
    strip.text = element_text(size = 40, margin = margin(5,5,15,5)),
    legend.text = element_text(size = rel(1.2)),
    legend.margin = margin(0,50,0,0),
    legend.key.width = unit(1,"cm")
  ) 
dev.off()

png("gfx/browsePredFig_4.png", width = 1920, height = 0.8*1080, units = "px")
browse_output_df %>% 
  filter(days==365, context != "Open grassland") %>% # suppressing open grass
  group_by(treatment, context, lagomorph) %>% 
  summarise(mean = mean(browse_prob_pred_mean),
    lower = mean(X2.5.),
    upper = mean(X97.5.)) %>% 
  ggplot(mapping = aes(x = lagomorph, y = mean, 
    colour=treatment, group =treatment)) + 
  geom_line(size=2) + theme_minimal() + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = treatment), 
              alpha = 0.2, linetype = 0,
              show.legend = FALSE) + 
  labs(x = "Lagomorph pellets (standardised)",
    y = "Prob. of main stem browse damage") +
  facet_grid(.~context, 
             labeller = labeller(context = as_labeller(contextName))) + 
  scale_x_continuous(breaks=c(-0.25, 0, 0.25, 0.5), labels = c(-0.25, 0, "", 0.5))+
  scale_fill_manual(values = cbbPalette) +
  scale_color_manual(values = cbbPalette) +
  theme(
    axis.title.x = element_text(size=40, margin = margin(20,0,20,0)),
    axis.title.y = element_text(size=40, margin = margin(0,20,0,50)),
    axis.text = element_text(size = rel(1)),
    #axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8),
    text = element_text(size = 30),
    strip.background =element_blank(),
    strip.text = element_text(size = 40, 
                              margin = margin(5,5,15,5)),
    legend.text = element_text(size = rel(1.2), 
                               margin = margin(10,0,0,0)),
    legend.margin = margin(0,50,0,0),
    legend.key.width = unit(1,"cm")
  ) 
dev.off()



# mortality data ----

st_cent_surv <- ungroup(saplings_surv) %>%
  dplyr::select(-c(Fate:state, death_or_censor)) %>% 
  mutate_at(vars(sand, orgMat, Lagomrph, Roo, Lagomrph_site_mean, 
                 Roo_site_mean, stem_dm = start_dm),
            function(x) (x - mean(x, na.rm = TRUE)) / (2 * sd(x, na.rm = TRUE)))

mort_data <- st_cent_surv %>% 
  transmute(context, site, treatment = Treat, days = elapsedDays, dead = Dead, lagomorph = Lagomrph_site_mean, kangaroo = Roo_site_mean, sand = sand, pc1 = PC1, pc2 = PC2, time = Time)

# prepare greta model ----
# write formula for the linear predictor
mort_lp <- ~ context * treatment + 
  lagomorph * treatment + 
  kangaroo * treatment



mort_design <- prep_design(glm_formula = mort_lp, data_obj = mort_data)

mort_coefs <- prep_coefs(design_list = mort_design)

mort_probs <- pred_prob(design_list = mort_design, coef_list = mort_coefs,
                        type = 'response')


# error model
distribution(mort_data$dead) <- bernoulli(mort_probs)
#distribution(real_data$browsed) <- binomial(real_data$total, brows_prob)

mort_mod <- model(mort_coefs$coef, precision = "double")

# hack the initial values so that the intercept is small
#mort_inits <- c(-5, rep(0, length(mort_mod$dag$example_parameters()) - 1))
m_coef <- mort_coefs$coef
m_coefs_init_mean <- c(-7, rep(0, length(m_coef)-1))
mort_inits <- replicate(4, initials(m_coef = rnorm(length(m_coef), m_coefs_init_mean, 0.4)), simplify = FALSE)

mort_draws <- mcmc(mort_mod,
                    #warmup = 2000,
                    n_samples = 5000, 
                    #chains = 3,
                    initial_values = mort_inits)


# Can we squeeze the prior on bs interactions? ----
m_coefs_init_sd <- c(0.3, rep(0.6, length(m_coef)-3), rep(0.05,2))
mort_inits_2 <- replicate(4, initials(m_coef = rnorm(length(m_coef), m_coefs_init_mean, m_coefs_init_sd)), simplify = FALSE)

mort_draws_2 <- mcmc(mort_mod,
              n_samples = 5000, 
              initial_values = mort_inits_2)


# inspect mort model ----
coda::gelman.diag(mort_draws)

coda::varnames(mort_draws)[1:length(mort_coefs$coef)] <- mort_design$coef_names

coda::varnames(mort_draws)[1:length(mort_coefs$coef)] <- 
  c("intercept",
    "adj_mallee",
    "open_grassy",
    "adj_dune",
    "excl_partial",
    "excl_total",
    "rabbit",
    "kangaroo",
    "partial*mallee",
    "partial*grassy",
    "partial*dune",
    "total*mallee",
    "total*grassy",
    "total*dune",
    "partial*rabbit",
    "total*rabbit",
    "partial*roo",
    "total*roo")


# plot mortality model results ----

plot(mort_draws)
(coef_summary <- summary(mort_draws)[1])

#not_intercept <- coda::varnames(mort_draws)[-1] #MCMCvis accepts excl argument to do this directly

png(filename = "gfx/mortCoefPlot.png", 
    width = 1400, height = 0.8*1080, units = "px")
par(mgp=c(3,0.75,0))

MCMCvis::MCMCplot(mort_draws, 
                  excl = c("intercept"), 
                           #"partial*'roo",
                           #"total*'roo",
                           #"total*rabbit"), 
                  rank = T, xlab = "Param. estimate (cloglog scale)",
                  ref_ovl = T, ax_sz = 1)
dev.off()

# mortality prediction data set ----
mort_pred_set <- mort_pred_df(df = mort_data)

predict_mort <- prep_design(glm_formula = mort_lp, data_obj = mort_pred_set)
predict_probs <- pred_prob(design_list = predict_mort, coef_list = mort_coefs, random_effect = FALSE, type = 're')

mort_prob_pred_draws <- calculate(predict_probs, mort_draws)

mort_prob_pred_summary <- summary(mort_prob_pred_draws)
mort_prob_pred_mean <- mort_prob_pred_summary$statistics[, "Mean"]
mort_prob_pred_CI <- mort_prob_pred_summary$quantiles[, c(1, 5)]

mort_pred_plot_df <- data.frame(mort_pred_set, mort_prob_pred_mean, mort_prob_pred_CI)

mort_pred_plot_df$context <- factor(mort_pred_plot_df$context, 
                                    levels = c("Buloke woodland", 
                                               "Open grassland", 
                                               "Wattle dune", "Mallee"))

#mort_comb_preds <- rbind.data.frame(mort_pred_plot_df_M, mort_pred_plot_df, mort_pred_plot_df_L)

# main mortality prediction overview figure ----



# png("gfx/mortPredFig.png", width = 1920, height = 0.8*1080, units = "px")
mort_rab_high <- mort_pred_plot_df %>% 
  filter(lagomorph == max(lagomorph),
    kangaroo == median(kangaroo)) %>% 
  group_by(treatment, context, days) %>% 
  summarise(mort_prob_pred_mean = mean(mort_prob_pred_mean),
            mort_prob_upper = mean(X97.5.),
            mort_prob_lower = mean(X2.5.))

mort_rab_low <- mort_pred_plot_df %>% 
  filter(lagomorph == min(lagomorph), 
    kangaroo == median(kangaroo)) %>% 
  group_by(treatment, context, days) %>% 
  summarise(mort_prob_pred_mean = mean(mort_prob_pred_mean),
            mort_prob_upper = mean(X97.5.),
            mort_prob_lower = mean(X2.5.))


ggplot(data = mort_rab_high, 
       mapping = aes(x = days, y = 1 - mort_prob_pred_mean,
                     colour=treatment)) + 
  geom_line(aes(group = treatment), size=2) + 
  geom_ribbon(aes(ymin = 1-mort_prob_lower, ymax = 1-mort_prob_upper, 
                  fill=treatment, group = treatment), 
              alpha=0.1, linetype = 0,
              show.legend = FALSE) + 
  facet_grid(.~context) +
  theme_minimal() + 
  labs(x = "Days elapsed", y = "Prob. of survival", colour = "Treatment") + 
  #theme(legend.position = 'none') +
  scale_fill_manual(values = cbbPalette) +
  scale_color_manual(values = cbbPalette) #+
#   theme(
#     axis.title.x = element_text(size=40, margin = margin(20,0,20,0)),
#     axis.title.y = element_text(size=40, margin = margin(0,20,0,50)),
#     axis.text = element_text(size = rel(1)),
#     #axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8),
#     text = element_text(size = 30),
#     strip.background =element_blank(),
#     strip.text = element_text(size = 40, margin = margin(5,5,15,5)),
#     legend.text = element_text(size = rel(1.2), margin = margin(10,0,0,0)),
#     legend.margin = margin(0,50,0,0),
#     legend.key.size = unit(1, "cm"),
#     legend.key.width = unit(1,"cm"),
#     legend.spacing = unit(1, "cm")
#   ) 
# dev.off()


#### STOP HERE ####

# ## longerhanded ----
# design_matrix <- model.matrix(glm_formula, data = real_data)
# coef <- normal(0, 10, dim = ncol(design_matrix))
# log_offset <- log(pmax(real_data$days, 0.001))
# # site random effect
# n_site <- length(unique(real_data$site))
# site_sd <- normal(0, 3, truncation = c(0, Inf))
# site_coef <- normal(0, site_sd, dim = n_site)
# site_effect <- site_coef[real_data$site]
# 
# eta <- design_matrix %*% coef + site_effect + log_offset
# brows_prob <- icloglog(eta)
# distribution(real_data$browsed) <- bernoulli(browse_probs)
# #distribution(real_data$browsed) <- binomial(real_data$total, brows_prob)
# 
# mod <- model(browse_coefs$coef, precision = "double") # can monitor whatever in here, e.g.,  site_sd 
# 
# # hack the initial values so that the intercept is small
# inits <- c(-5, rep(0, length(mod$dag$example_parameters()) - 1))
# 
# # # in future you will be able to do:
# # inits <- list(coef = c(-5, rep(0, 14)))
# # # or pass a function so the chains start in different places:
# # inits <- function () {
# #   list(coef = rnorm(15, c(-5, rep(0, 14)), 1))
# # }
# 
# library(future)
# plan(multisession)
# draws <- mcmc(mod,
#               warmup = 2000,
#               n_samples = 2000, 
#               chains = 3,
#               initial_values = inits)
# 
# coda::gelman.diag(draws)
# 
# coda::varnames(draws)[1:length(browse_coefs$coef)] <- colnames(browse_design$matrix)
# plot(draws)
# coef_summary <- summary(draws)[1]
# 
# 
# #library(bayesplot)
# 
# bayesplot::mcmc_intervals(draws)
# 
# 
# # Nick's fake data ----
# set.seed(123)
# n <- 100
# n_treat <- 3
# n_site <- 17
# data <- data.frame(treatment = sample(letters[1:n_treat], n, replace = TRUE),
#                    site = sample(LETTERS[1:n_site], n, replace = TRUE))
# 
# data$treatment_id <- as.numeric(data$treatment)
# data$site_id <- as.numeric(data$site)
# data$lagomorph <- rnorm(n_site)[data$site_id]
# data$kangaroo <- rnorm(n_site)[data$site_id]
# data$days <- sample.int(405, n)
# site_contexts <- c("bw", "og", "mw", "wd")
# data$context <- sample(site_contexts, n_site, replace = TRUE)[data$site_id]
# data$total <- rbinom(n, 25, 0.9)
# data$browsed <- rbinom(n, data$total, 0.05)
# 
# 
# 
