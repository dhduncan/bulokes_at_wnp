# Prepare the grazing activity (pellet) and buloke sapling data
# Updated June 2020
# David Duncan

# Introductory notes ----
# Considerable pre-prep was done Excel-side by Ami Bennett, and myself, including coding up the status info with respect to browsing evidence.

rm(list=ls())

# Load required packages into session ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(openxlsx, tidyverse, RcppRoll)

## Read in Excel files ----

path_to_data <- "data/NESP1_2_2_buloke_survival_dec2021.xlsx"

# organise the site level data ----

# site level info
sites <- read.xlsx(path_to_data, sheet = "Sites")

names(sites)[names(sites)=="T0_seedling_plant_date"] <- 'T0_date'  # replace cumbersome name

sites <- sites %>% 
  rename(context = Habitat,
                          site = Site) # dplyr alternative to the above, change Habitat for 'context'

#sites$Site <- factor(sites$Site)


# point quadrats (200 hits)
site_lf_cover <- read.xlsx(path_to_data, sheet = "T0_Pointing_DATA")

site_lf_cover <- site_lf_cover %>%
  group_by(Site) %>% 
  summarise_at(
    vars(Bare_ground:Tree), list(cov = sum)
  ) %>% 
  rename(site = Site)


site_summ <- left_join(site_lf_cover[ , 1:16], sites[ , c(1:6, 20:25)], by = "site")

# that's it for
rm(site_lf_cover)

# buloke sapling data
read_saplings <- read.xlsx(path_to_data, 
                            sheet = "ALL_Seed_DATA") %>% 
  mutate("Dead" = as.numeric(Dead),
          "HtCM" = as.numeric(HtCM),
          "StDMM" = as.numeric(StDMM),
         "Browsed" = as.numeric(Browsed)
  )

read_saplings <- read_saplings %>% 
  select(site = 1, 4:7, 11, 13, 23, 24, 38)

# Further prep of the buloke sapling data ----

# firstly, remove the observation of sapling 46 at site 7, whose guard was knocked off and therefore could not be reliably assigned a treatment type for the last observation period. It is coded as Open at Time 4 ("7.O46", whereas previously it appears as "7.P46"), therefore we can remove it easily by the uniqID field.

read_saplings <- read_saplings[read_saplings$uniqID != "7.O46" | read_saplings$uniqID != "12.O61", ]

# join the sapling data to site data, leaving behind surplus fields

saplings <- left_join(read_saplings, 
                  sites[c(1, 6, 8:11, 13,14, 16, 17,18)],
                  by = "site")

rm(read_saplings)


# encode factors
#for (i in 1:3) {  # first three variables are factors
#  saplings[[i]] <- factor(saplings[[i]])
#}

# calculate a vector of days elapsed since each sapling was planted. AB had already done a version of this in the Excel sheet for the pellet data, `DaysAcc`, but I wanted to reproduce it with code for the sapling data.

saplings$elapsedDays <- 0 # create vector
# fill with series of ifelse() statements (thanks Nick Golding)
saplings$elapsedDays <- ifelse(saplings$Time == 1, 
                               saplings$T1_census_date - saplings$T0_date,
                               saplings$elapsedDays)
saplings$elapsedDays <- ifelse(saplings$Time == 2, 
                               saplings$T2_census_date - saplings$T0_date,
                               saplings$elapsedDays)
saplings$elapsedDays <- ifelse(saplings$Time == 3, 
                               saplings$T3_census_date - saplings$T0_date,
                               saplings$elapsedDays)
saplings$elapsedDays <- ifelse(saplings$Time == 4, 
                               saplings$T4_census_date - saplings$T0_date,
                               saplings$elapsedDays)
saplings$elapsedDays <- ifelse(saplings$Time == 5, 
                               saplings$T5_census_date - saplings$T0_date,
                               saplings$elapsedDays)
saplings$elapsedDays <- ifelse(saplings$Time == 6, 
                               saplings$T6_census_date - saplings$T0_date,
                               saplings$elapsedDays)
saplings$elapsedDays <- ifelse(saplings$Time == 7, 
                               saplings$T7_census_date - saplings$T0_date,
                               saplings$elapsedDays)
saplings$elapsedDays <- ifelse(saplings$Time == 8, 
                               saplings$T8_census_date - saplings$T0_date,
                               saplings$elapsedDays)



# Rationalise the date fields since the data are unique sapling x time combinations
saplings <- saplings %>% 
  mutate(
    date = case_when(
      Time == 0 ~ T0_date, 
      Time == 1 ~ T1_census_date, 
      Time == 2 ~ T2_census_date,
      Time == 3 ~ T3_census_date,
      Time == 4 ~ T4_census_date,
      Time == 5 ~ T5_census_date,
      Time == 6 ~ T6_census_date,
      Time == 7 ~ T7_census_date,
      Time == 8 ~ T8_census_date
    )
  )

# remove the old wide_format date fields by name pattern
saplings <- dplyr::select(saplings, -ends_with('_date'))

# add a link vector for joining to the pellet data
saplings <- saplings %>% 
  mutate(link = paste0(site, ".",Time))

# Fix state variable after last field visit ----
saplings <- arrange(saplings, uniqID, Time) %>% 
  mutate(state = if_else(Time == 4 & is.na(Fate), lag(state), state), # Will Morris gave me this solution to fix the state variable. It relies *absolutely* on the order of the df! The OR part I added (| Time == 4 & Dead == 0), and now I can't figure out why.
         now_browse = case_when(grepl('b', state) ~ 1,
                        grepl('B', state) ~ 1,
                        TRUE ~ 0)
         )
# Cut down version for survival and browse modeling ----

# This version has all individuals up to and including the first occasion when they get browsed (excludes very minor or marginal calls on browsing)
saplings_browse <- saplings %>% 
   group_by(uniqID) %>% 
   mutate( 
     cbrowse =  cumsum(cumsum(now_browse)),
     tBrowse = max(elapsedDays),
     bStatus = last(now_browse)) %>% 
  filter(cbrowse <= 1) %>% 
  mutate(
    dead_NB = sum(Dead[state=="D"])
  ) %>% 
  filter(Time <= 5 - dead_NB)

# alt browse set - retains inviduals after first appeared browsed

browse_alt <- saplings %>% 
  group_by(uniqID) %>% 
  mutate( 
    browsed =  if_else(now_browse == 0 & cumsum(now_browse) > 0, 
                       1, now_browse),
    cbrowse = cumsum(cumsum(now_browse)),
    tBrowse = first(elapsedDays[browsed ==1]),
    bStatus = last(now_browse)) %>% 
  #filter(cbrowse <= 1) %>% 
  mutate(
    dead_NB = sum(Dead[state=="D"])
  ) %>% 
  filter(Time <= 5 - dead_NB)


     
# 
saplings_surv <- saplings %>% 
  group_by(uniqID) %>% 
  mutate(death_or_censor = sum(Dead)) %>% 
  #filter(Time <= 5-death_or_censor) %>%  # remove the sapling after first noted dead
  mutate(
    tDeath = ifelse(sum(Dead) > 0, 
                    first(elapsedDays[Dead==1]), 
                    max(elapsedDays)),
    dStatus = last(Dead),
    prev_browse = if_else(grepl('b', lag(state)), 1, 0), # prev census only
    browsed = if_else(now_browse + prev_browse == 0, 0, 1) # either prev step or final
  )

# lag() in the prev_browse above looks for browsed in previous time step, not current! 


# reintroduce site data after having attached the PCA variables via `Visualize Site Variation.Rmd`
more_site_vars <- readRDS('data/site_variables_pca.rds')

more_site_vars <- rename(more_site_vars, site = Site) %>% 
  mutate(site = as.numeric(site))

saplings_surv <- left_join(x = saplings_surv,
                           y = more_site_vars[c(1:16)],
                           by = 'site')

saplings_browse <- left_join(x = saplings_browse,
                             y = more_site_vars[c(1:16)],
                             by = 'site')

browse_alt<- left_join(x = browse_alt,
                       y = more_site_vars[c(1:16)],
                       by = 'site')

## Prepare the pellet data ----
# herbivore poo data - to begin these data are per pellet group identified within each plot x time, so some level of summarising will be necessary.

accum <- read.xlsx(path_to_data, sheet = "ALL_P_DATA")

accum <- rename(accum, browser = Species,
                  site = Site) #%>% 

# recode_factor(browser, WGK = "Roo", Rabbit = "Lago")
accum$browser = factor(accum$browser, levels = c("Goat", "Rabbit", "WGK"), labels = c("Goat", "Lagomrph", "Roo"))

# remove instances of NA where the pellet plot couldn't be relocated and had to be replaced (n=3)
accum <- accum[ !is.na(accum$nPellets), ]

# Summarise the pellet data to plot x time level for graphing and model building

# This version retains different pellet groups via uniqID
plt_plot <- accum %>% 
  dplyr::select(-c(DateTprevious, DateTcurrent, rDayLog, pgID)) %>% 
  group_by(site, Transect, Plot, Time, browser, uniqID) %>%
  summarise(nGroups = max(nGroups),
            nPellets = sum(nPellets),
            rPellets = mean(rDay),
            obs = last(Surveyor),
            days = first(DaysAcc)) %>%
  mutate(link = paste0(site, ".",Time),
         tPlot = paste0(Transect, ".", Plot)) # add an identifier to enable join of pellet and sapling info

# join the site information to the pellet data
plt_plot <- left_join(plt_plot, sites[ , c(1,5,6, 19:26)], by = "site")

#plt_plot <- left_join(plt_plot, site_lf_cover, by = 'Site')

plt_plot <- left_join(plt_plot, more_site_vars[1:16], by = 'site')

# link site average nPellets per browser to sapling dataset ----

link_plt <- filter(plt_plot, !is.na(browser)) %>% 
  group_by(site, link, browser) %>% 
  summarise(m_rPellets = mean(rPellets)) %>% 
  spread(key = browser, value = m_rPellets) %>% 
  group_by(site) %>% 
  mutate_at(
    vars(Goat:Roo), funs(site_mean = round(mean(., na.rm = TRUE), 4))
  ) 

## introduce standing crop data from latter years ----
st_crop <- read.xlsx(path_to_data, sheet = "Standing_crop", cols = 1:9) %>% 
  filter(site != "?") %>% 
  mutate(site = as.numeric(site))

# a mode function, thanks to https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

link_st_crop <- na.omit(st_crop) %>% 
  group_by(site, Time) %>%
  mutate(
    #DO = NA,
    DO = case_when(Rabbit <= 40 ~ Rabbit)) %>% #latrine
  summarise(
    n_quad = n(),
    m_roo_nP_0.1m2_yr = mean(WGK, na.rm = TRUE),
    m_rab_nP_0.1m2_yr = mean(((.6 * DO) * 1.298701), na.rm = TRUE), # latrine adjustment
    #m_lgm_nP_0.1m2_yr = mean(Rabbit, na.rm = TRUE),
    m_goat_nP_0.1m2_yr = mean(Goat, na.rm = TRUE),
    obs = mode(Surveyor), # observer who did most of that site
    d_rab_mtz = -0.0008 * m_rab_nP_0.1m2_yr^3 + 0.0565 * m_rab_nP_0.1m2_yr^2 + 0.86*m_rab_nP_0.1m2_yr,
    roo_ha_nP = (m_roo_nP_0.1m2_yr / (0.1 * 365 * 493))*10000
  ) %>%
  mutate(link = paste0(site, ".",Time))

roo_density <- filter(ungroup(plt_plot), browser == 'Roo') %>%
  group_by(context, site, Time, link) %>%
  summarise(m_roo_nP = mean(nPellets),
         m_roo_nPG = mean(nGroups),
         days = mean(days)) %>% 
  mutate(roo_ha_nP = (m_roo_nP / (15.75 * days * 493))*10000,
         roo_ha_nPG = (m_roo_nPG / (15.75 * days * 89.5))*10000)

lgm_density <- filter(ungroup(plt_plot), browser == 'Lagomrph') %>%
  group_by(context, site, Time, link) %>%
  summarise(m_lgm_nP = mean(nPellets),
            days = mean(days)) %>% 
  mutate(m_nP_0.1m2_yr = ((m_lgm_nP / days) / 157.5 * 365),
         d_rab_mtz = -0.0008 * m_nP_0.1m2_yr^3 + 0.0565 * m_nP_0.1m2_yr^2 + 0.86*m_nP_0.1m2_yr)

link_plt <- left_join(link_plt, roo_density[-c(1:3)], by = 'link') #%>% 
  #mutate(Lagomrph_site_mean = if_else(is.na(Lagomrph_site_mean), 
  #                                    lead(Lagomrph_site_mean), 
  #                                    Lagomrph_site_mean),
  #       Roo_site_mean = if_else(is.na(Roo_site_mean), 
  #                               lead(Roo_site_mean), 
  #                               Roo_site_mean),
  #       m_roo_nPG = if_else(is.na(m_roo_nPG),
  #                           lead(m_roo_nPG),
  #                           m_roo_nPG),
  #       roo_ha_nP = if_else(is.na(roo_ha_nP),
  #                           lead(roo_ha_nP),
  #                           roo_ha_nP),
  #       roo_ha_nPG = if_else(is.na(roo_ha_nPG),
  #                            lead(roo_ha_nPG),
  #                            roo_ha_nPG)
  #)
  
link_plt <- left_join(link_plt, select(ungroup(lgm_density), vars = -c(1:3,5,6)), by = 'link') 
  


# join to site data for PCA, etc ----
#link_site <- distinct(link_plt[-c(2:5)]) 

# site_summ <- left_join(site_summ, link_site, by = "Site")

# save the site data object ----
# saveRDS(site_summ[-1], 'site_variables.RDS')

saplings_surv <- left_join(x = saplings_surv, 
                           y = link_plt[,- c(1,3,6)], 
                           by = "link") %>% 
  mutate(Lagomrph_site_mean = if_else(is.na(Lagomrph_site_mean), 
                                      lead(Lagomrph_site_mean), 
                                      Lagomrph_site_mean),
         Roo_site_mean = if_else(is.na(Roo_site_mean), 
                                 lead(Roo_site_mean), 
                                 Roo_site_mean),
         m_roo_nPG = if_else(is.na(m_roo_nPG),
                             lead(m_roo_nPG),
                             m_roo_nPG),
         roo_ha_nP = if_else(is.na(roo_ha_nP),
                             lead(roo_ha_nP),
                             roo_ha_nP),
         roo_ha_nPG = if_else(is.na(roo_ha_nPG),
                              lead(roo_ha_nPG),
                              roo_ha_nPG)
  )

saplings_browse <- left_join(saplings_browse, 
                             y = link_plt[, -c(1,3,6)], 
                             by = "link") %>% 
  mutate(Lagomrph_site_mean = if_else(is.na(Lagomrph_site_mean), 
                                      lead(Lagomrph_site_mean), 
                                      Lagomrph_site_mean),
         Roo_site_mean = if_else(is.na(Roo_site_mean), 
                                 lead(Roo_site_mean), 
                                 Roo_site_mean),
         m_roo_nPG = if_else(is.na(m_roo_nPG),
                             lead(m_roo_nPG),
                             m_roo_nPG),
         roo_ha_nP = if_else(is.na(roo_ha_nP),
                             lead(roo_ha_nP),
                             roo_ha_nP),
         roo_ha_nPG = if_else(is.na(roo_ha_nPG),
                              lead(roo_ha_nPG),
                              roo_ha_nPG)
  )


browse_alt <- left_join(browse_alt, 
                             y = link_plt[, -c(1,3,6)], 
                             by = "link") %>% 
  mutate(Lagomrph_site_mean = if_else(is.na(Lagomrph_site_mean), 
                                      lead(Lagomrph_site_mean), 
                                      Lagomrph_site_mean),
         Roo_site_mean = if_else(is.na(Roo_site_mean), 
                                 lead(Roo_site_mean), 
                                 Roo_site_mean),
         m_roo_nPG = if_else(is.na(m_roo_nPG),
                             lead(m_roo_nPG),
                             m_roo_nPG),
         roo_ha_nP = if_else(is.na(roo_ha_nP),
                             lead(roo_ha_nP),
                             roo_ha_nP),
         roo_ha_nPG = if_else(is.na(roo_ha_nPG),
                              lead(roo_ha_nPG),
                              roo_ha_nPG)
  )


# A version of pellet counts per plot per time browser for the attempts to directly model activity

#pellet_mod <- spread(plt_plot, key = link, value=)

# an early stuff around version of the model was greatly improved by narrowing down to the pellets of just one browser
#kang <- plt_plot %>% filter(Browser == "Roo") 

#rab <- plt_plot %>% filter(Browser == "Lagomrph")  # includes hares
  
# probably won't include goat, way too many zeroes I suspect. Nontheless:
#goat <- plt_plot %>% filter(Browser == "Goat")

# Prepare height and growth data ----

ids <- unique(saplings$uniqID)
get_final_ht <- function(id, df) {
  
  ht <- as_vector(df[df$uniqID == id, "HtCM"])
  ht <- na.omit(ht)
  
  if (length(ht) <= 1) {
    ans <- NA
    print(id)
  } else {
    ans <- ht[length(ht)]
  }
  ans
}


final_ht <- vapply(ids, get_final_ht, saplings, FUN.VALUE = 1)

final_ht <- data.frame(uniqID = ids, end_ht = final_ht)  

# All the height data is in the same columns (e.g., `HtCM`), to be distinguished via the sampling occasion (e.g., `HtCMM` at `Time==0` is the initial height, whereas `HtCM` at `Time == 4` is the height at the end, unless the plant died earlier, in which case its final height would have been recorded at `Time==2` or `Time==3`.

saplings_summary <- saplings %>% 
  group_by(site, context, Treat, uniqID) %>% 
  summarise(
    start_ht = first(HtCM),
    start_dm = first(StDMM),
    end_dm = last(StDMM),
    state = last(state),
    dead = last(Dead),
    browsed = last(now_browse)) 

saplings_summary <-  left_join(saplings_summary, final_ht, by = "uniqID")

saplings_summary <- saplings_summary %>% 
  mutate(
    ht_change = end_ht - start_ht,
    dm_change = end_dm - start_dm
  )

saplings_surv <- left_join(saplings_surv, 
                           dplyr::select(ungroup(saplings_summary), 
                                         c(uniqID, start_dm)), 
                           by = 'uniqID')

saplings_browse <- left_join(saplings_browse, 
                             dplyr::select(ungroup(saplings_summary), 
                                           c(uniqID, start_dm)), 
                             by = 'uniqID')

browse_alt <- left_join(browse_alt, 
                             dplyr::select(ungroup(saplings_summary), 
                                           c(uniqID, start_dm)), 
                             by = 'uniqID')
# # load BoM data ----
# AWAP_sm <- read.xlsx("bom_data.xlsx", sheet = 7) # soil moisture data AWAP
# 
# for(i in c(1:6,8:10)) { # for the rest the format is the same
#   assign(paste0("bom_",i), read.xlsx("bom_data.xlsx", sheet = i))
#   
# }
# 
# # the next two lines from https://stackoverflow.com/feeds/question/35387419
# dfs <- sapply(X = ls()[grep('bom_', ls())], is.data.frame)
# 
# bom_obs <- do.call(rbind.data.frame, mget(names(dfs))) # apparently bind_rows() would work here
# 
# # then back to my laborious own intuition
# bom_obs$station <- c(rep("Tutye", nrow(bom_1)), 
#                      rep("Rainbow4", nrow(bom_10)), 
#                      rep("Linga", nrow(bom_2)), 
#                      rep("Walp_R", nrow(bom_3)),
#                      rep("Walp", nrow(bom_4)),
#                      rep("Hopetoun", nrow(bom_5)),
#                      rep("Rainbow1", nrow(bom_6)),
#                      rep("Rainbow2", nrow(bom_8)),
#                      rep("Rainbow3", nrow(bom_9)))
# 
# # organise cumulative rainfall data ----
# 
# n_days <- c(30, 60, 90, 180, 360)   # 1-, 2-, 3-, 6- and 12-month moving window
# 
# bom_obs[,ncol(bom_obs)+seq(n_days)] <- NA
# 
# # god this is ugly, but seems to have produced a plausible result  
# for(i in seq(n_days)) {
#   
#   bom_obs[ ,3 + seq(n_days)[i]] <-  roll_sum(bom_obs$Rainfall_mm, n_days[i], align = "right", fill = NA)
#   
# } # roll_sum() is from RcppRoll package
# 
# # rename new columns - I wonder if the above could have been vectorized. 
# colnames(bom_obs)[3+seq(n_days)] <- paste0("cum_d", n_days)
# 
# # extract filtered set of obs pertaining to census dates ----
# bom_filter <- bom_obs %>%
#   dplyr::select(-Rainfall_mm, -cum_d360, -cum_d180) %>% 
#   filter(as.Date(Date) %in% unique(as.Date(as.vector(as.matrix(sites[,c(7:9,11,13,15,16)])))) & station == "Walp") 
# 
# # Attach those data to the saplings df
# saplings_surv <- 
#   left_join(saplings_surv, dplyr::select(bom_filter, -station), by = "Date")

# save the pellet data object ----
save(plt_plot, st_crop,  file = 'data/herb_activity.Rdata')

# remove the objects not required in the global environment ----
rm(ids, lgm_density, sites, site_summ, plt_plot, st_crop, accum, link_plt, final_ht, get_final_ht, roo_density)
# AWAP_sm, dfs, n_days, bom_1, bom_10, bom_2, bom_3, bom_4, bom_5, bom_6, bom_8, bom_9,

# save the remaining items in workspace for the browsing analysis ----
save.image(file = "data/browseDFs.Rdata")

