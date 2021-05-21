
# packages ----
library(dplyr)
library(openxlsx)
library(tidyr)
library(ggplot2)


rm(list = ls())

# import data ----
data_path <- "~dduncan/OneDrive - The University of Melbourne/UM_projects/NESP7.6 Herbivore impacts on TS and TEC/BreakOutofBrowseTrap/data/"

st_crop <- read.xlsx(xlsxFile = paste0(data_path,"DATA_Dec2019_Wyperfeld_AW.xlsx"), sheet = 4)

sites <- read.xlsx(xlsxFile = paste0(data_path,"DATA_Dec2019_Wyperfeld_AW.xlsx"), sheet = "Sites")

# leave behind unnecessary fields
sites <-  sites %>% 
  select(Site, Habitat) %>% 
  rename(context = Habitat, site = Site) 

# insert leading 0 into m so that the ordering comes out right ----
st_crop <- st_crop %>% 
  mutate(uniqID = gsub(pattern = "(?<=\\d).(?=\\d{1}$)",
                       replacement = ".0",
                       uniqID, perl = TRUE))

# select a random sample of 10% of data to validate ----
# data_validation_set <- st_crop %>% 
#   filter(!is.na(Roo_pellets) | !is.na(Rabbit_pellets)) %>% 
#   select(uniqID, Site, Block, Location_m, Roo_pellets, Rabbit_pellets) %>% 
#   sample_frac(size = 0.1) %>% 
#   mutate(odd = if_else(Location_m %% 2 == 0, "evens", "odds")) %>%
#   arrange(Site, Block, odd, Location_m)

# Save the validation set ONCE ONLY
# write.xlsx(data_validation_set, 
#           paste0(data_path,"standing_crop_validation_set.xlsx"))

# adjust data for plotting ----

# replace "?" with an obviously silly numerical value

st_crop <- st_crop %>% 
  filter(!is.na(Roo_pellets) | !is.na(Rabbit_pellets)) %>%  
  mutate(Roo_pellets = ifelse(Roo_pellets == "?", 999, Roo_pellets),
         Rabbit_pellets = ifelse(Rabbit_pellets == "?", 999, Rabbit_pellets)) %>%
  mutate(Roo_pellets = as.numeric(Roo_pellets),
         Rabbit_pellets = as.numeric(Rabbit_pellets)) %>% 
  mutate(Roo_pellets = if_else(is.na(Roo_pellets), 0, Roo_pellets),
         Rabbit_pellets = if_else(is.na(Rabbit_pellets), 0, Rabbit_pellets))

# remove missing values with filter
# replace ? with dummy numeric value
# make numeric
# if count is currently NA, make it 0 otherwise leave it alone

# filter(st_crop, Roo_pellets != 999) %>% 
# ggplot(aes(x=Roo_pellets)) + geom_histogram() + facet_wrap(~Site) + 
#   ggtitle("Kangaroo pellet standing crop data per site,\nWyperfeld NP Dec 2019")
# 
# filter(st_crop, Rabbit_pellets != 999) %>% 
# ggplot(aes(x=Rabbit_pellets)) + geom_histogram() + facet_wrap(~Site) + 
#   ggtitle("Rabbit pellet standing crop data per site,\nWyperfeld NP Dec 2019")

# Summarise the pellet data to plot x time level for graphing and model building

# bring in the Accumulation plot data (for whole project)

file_path <- "~dduncan/OneDrive - The University of Melbourne/UM_projects/NESP1.2.2 BulokeWoodlands/Wyperfeld paper/browse/browse_6_2020-03-02.xlsx"

pellets <- read.xlsx(file_path, sheet = "ALL_P_DATA")

pellets <- rename(pellets, browser = Species,
                  site = Site) %>% 
  mutate(browser = factor(browser, levels = c("Goat", "Rabbit", "WGK"), labels = c("Goat", "Lagomorph", "Roo")))

# remove instances of NA where the pellet plot couldn't be relocated and had to be replaced (n=3)
pellets <- pellets[ !is.na(pellets$nPellets), ]

# Summarise the pellet data to plot x time level for graphing and model building
# This version retains different pellet groups via uniqID
plt_plot_kg <- pellets %>% 
  dplyr::select(-c(DateTprevious, DateTcurrent, rDayLog, pgID)) %>% 
  filter(browser == 'Roo') %>% 
  group_by(site, Transect, Plot, Time, browser, uniqID) %>%
  summarise(nGroups = max(nGroups),
            nPellets = sum(nPellets),
            rPellets = mean(rDay),
            obs = last(Surveyor),
            days = first(DaysAcc)) %>%
  mutate(link = paste0(site, ".",Time),
         tPlot = paste0(Transect, ".", Plot)) # add an identifier to enable join of pellet and sapling info

plt_plot_r <- pellets %>% 
  dplyr::select(-c(DateTprevious, DateTcurrent, rDayLog, pgID)) %>% 
  filter(browser == 'Lagomorph' & nPellets <= 40) %>% 
  group_by(site, Transect, Plot, Time, browser, uniqID) %>%
  summarise(nGroups = max(nGroups),
            nPellets = sum(nPellets),
            rPellets = mean(rDay),
            obs = last(Surveyor),
            days = first(DaysAcc)) %>%
  mutate(link = paste0(site, ".",Time),
         tPlot = paste0(Transect, ".", Plot))

plt_plot <- bind_rows(plt_plot_kg, plt_plot_r)

link_plt <- plt_plot %>% group_by(site, Time, link, browser) %>% 
  summarise(m_rPellets = mean(rPellets)) %>% 
  spread(key = browser, value = m_rPellets) %>% 
  group_by(site)

roo_density <- filter(ungroup(plt_plot), browser == 'Roo') %>%
  group_by(site, Time, link) %>%
  summarise(m_roo_nP = mean(nPellets),
            m_roo_nPG = mean(nGroups),
            days = mean(days)) %>% 
  mutate(roo_ha_nP = (m_roo_nP / (15.75 * days * 493))*10000,
         roo_ha_nPG = (m_roo_nPG / (15.75 * days * 89.5))*10000)

# Converting rabbit pellet density to rabbit density after Mutze et al 2014.
# Not yet implemented, 40% of the count from an uncleared area may be older than a year, but through decay and consumption only 77% may be retained.

# so (0.6 x count) = 0.77 of ____
# (.6*nPellets) * 1.298701

lgm_density <- filter(ungroup(plt_plot), browser == 'Lagomorph') %>%
  group_by(site, Time, link) %>%
  summarise(m_lgm_nP = mean(nPellets),
            days = mean(days)) %>% 
  mutate(m_nP_0.1m2_yr = ((m_lgm_nP / days) / 157.5 * 365), # convert to Mutze physical scale, and for 1 year's accumulation
         d_rab_mtz = -0.0008 * m_nP_0.1m2_yr^3 + 0.0565 * m_nP_0.1m2_yr^2 + 0.86*m_nP_0.1m2_yr)


link_plt <- left_join(
  link_plt, 
  roo_density[-c(1:2)], 
  by = 'link'
  ) 

link_plt <- left_join(
  link_plt, 
  select(ungroup(lgm_density), vars = -c(1:2,5)), 
  by = 'link'
  )

# join accumulation and standing crop 

# summarise standing crop data to site level
standing <- st_crop %>% 
  filter(Roo_pellets != 999) %>% # remove cells that had "?"
  group_by(Site) %>% 
  mutate(DO = if_else(Rabbit_pellets <= 40, Rabbit_pellets, NULL)) %>% #latrine
  summarise(
    m_roo_nP_0.1m2 = mean(Roo_pellets, na.rm = TRUE),
    roo_ha_nP = (m_roo_nP_0.1m2 / (0.1 * 365 * 493))*10000, # Coulson & Raines
    m_rab_nP_0.1m2 = mean(((.6 * DO) * 1.298701), na.rm = TRUE), # latrine adjustment
    d_rab_mtz = -0.0008 * m_rab_nP_0.1m2^3 + 0.0565 * m_rab_nP_0.1m2^2 + 0.86*m_rab_nP_0.1m2) %>% 
  rename(site = Site) %>% 
  mutate(method = "standing crop")
  
# extract accumulation set for T6 ----
accum_T6 <- filter(link_plt, Time==6) %>% 
  select(site, roo_ha_nP, d_rab_mtz) %>% 
  mutate(method = "accumulation")

# combine density estimates from the two different approaches
comb_meth <- bind_rows(select(standing, site, roo_ha_nP, d_rab_mtz, method), accum_T6)

comb_meth <- left_join(comb_meth, sites, by = 'site')

l_comb_meth  <-  comb_meth %>% 
  pivot_longer(c(roo_ha_nP, d_rab_mtz), names_to = "response", values_to = "estimate")

w_comb_meth <- l_comb_meth %>% 
  pivot_wider(names_from = method, values_from = estimate) %>% 
  rename(standing_crop = `standing crop`)

herbivore <- c("d_rab_mtz" =  "Lagomorph",
               "roo_ha_nP" = "Kangaroo")


save.image(file = "calibration2019.Rdata")

# summary graph (now executed in Rmarkdown doc) ----
# ggplot(w_comb_meth, aes(standing_crop+1e-3,accumulation+1e-3, 
#                         color = context)) + 
#   geom_point() + 
#   geom_abline(intercept = 0, slope = 1) + 
#   facet_wrap(~response, scales = "free", labeller = as_labeller(herbivore)) +
#   labs(x = 'Density (animals / ha) estimated from standing crop method (after Mutze et al 2014)',
#        y = 'Density (animals / ha) from accumulation\nmethod (i.e., Bennett et al 2019)') +
#   scale_y_log10() + scale_x_log10() +
#   theme(
#     aspect.ratio = 1
#   )
#   

library(arm)
mod_scale <- lmer(standing_crop ~ scale(accumulation):response-1 + (1|site), 
            data = w_comb_meth)
# removing line 18 from the above seriously improves the residuals distribution

library(MASS)



mod <- lmer(log(standing_crop+0.0001) ~ accumulation:response-1 + (1|site), 
                  data = w_comb_meth)

lmod_n18 <- lm(standing_crop ~ accumulation:response-1, 
             data = w_comb_meth[-18,])

lmod <- lm(standing_crop ~ accumulation:response-1, 
               data = w_comb_meth)


lmod_cube <- lm(standing_crop^(1/3) ~ scale(accumulation):response-1, 
           data = w_comb_meth[-18,])


# a box cox transform factor? 
bc <- boxcox(w_comb_meth$standing_crop ~ w_comb_meth$accumulation)
(lambda <- bc$x[which.max(bc$y)])
