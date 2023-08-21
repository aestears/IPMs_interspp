#/////////////////////////
# Generate plot-level recruit data 
# Alice Stears
# 11 April 2023
#/////////////////////////

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(plantTracker)
library(sf)

# Read in Data ------------------------------------------------------------
AZn_demo_dorm1_buff5_buffG05_aggregated <- readRDS("./Data/trackSpp_outputData/AZn_buff5_dorm1_demoDat.RDS") %>% 
  mutate(Year = as.integer(Year))
AZs_demo_dorm1_buff5_buffG05_aggregated <- readRDS("./Data/trackSpp_outputData/AZs_buff5_dorm1_demoDat.RDS") %>% 
  mutate(Year = as.integer(Year))
NM_demo_dorm1_buff5_buffG05_aggregated <- readRDS("./Data/trackSpp_outputData/NM_buff5_dorm1_demoDat.RDS") %>% 
  mutate(Year = as.integer(Year))
MT_demo_dorm1_buff5_buffG05_aggregated <- readRDS("./Data/trackSpp_outputData/MT_buff5_dorm1_demoDat.RDS") %>% 
  mutate(Year = as.integer(Year))
ID_demo_dorm1_buff5_buffG05_aggregated <- readRDS("./Data/trackSpp_outputData/ID_buff5_dorm1_demoDat.RDS") %>% 
  mutate(Year = as.integer(Year))
CO_demo_dorm1_buff5_buffG05_aggregated <- readRDS("./Data/trackSpp_outputData/CO_buff5_dorm1_demoDat.RDS") %>% 
  mutate(Year = as.integer(Year))
KS_demo_dorm1_buff5_buffG05_aggregated <- readRDS("./Data/trackSpp_outputData/KS_buff5_dorm1_demoDat.RDS") %>% 
  mutate(Year = as.integer(Year))

# Generate plot-level recruit data ----------------------------------------
### AZn
AZn_recruits <- getRecruits(dat = AZn_demo_dorm1_buff5_buffG05_aggregated, byGenet = TRUE)

### AZs
AZs_recruits <- getRecruits(dat = AZs_demo_dorm1_buff5_buffG05_aggregated, byGenet = TRUE)

### NM
NM_recruits <- getRecruits(dat = NM_demo_dorm1_buff5_buffG05_aggregated, byGenet = TRUE)

### MT
MT_recruits <- getRecruits(dat = MT_demo_dorm1_buff5_buffG05_aggregated, byGenet = TRUE)

### ID
ID_recruits <- getRecruits(dat = ID_demo_dorm1_buff5_buffG05_aggregated, byGenet = TRUE)

### CO
CO_recruits <- getRecruits(dat = CO_demo_dorm1_buff5_buffG05_aggregated, byGenet = TRUE)

### KS
KS_recruits <- getRecruits(dat = KS_demo_dorm1_buff5_buffG05_aggregated, byGenet = TRUE)

## put together 
recruits_df <- rbind(AZn_recruits, AZs_recruits, CO_recruits, NM_recruits, ID_recruits, MT_recruits, KS_recruits)
recruits_df <- recruits_df %>% 
  rename(Year_t_recs = Year)
# calculate # of recruits per adult in previous year ----------------------

# for each site, get the number of adult (non recruit) individuals per year 
AZn_adults <- AZn_demo_dorm1_buff5_buffG05_aggregated %>% 
  st_drop_geometry() %>% 
  filter(recruit == 0 & !is.na(recruit)) %>% 
  group_by(Site, Quad, Species, Year) %>% 
  dplyr::summarize(n_adults = length(unique(trackID)),
                   mean_size_t = mean(basalArea_genet))
MT_adults <- MT_demo_dorm1_buff5_buffG05_aggregated %>% 
  st_drop_geometry() %>% 
  filter(recruit == 0 & !is.na(recruit)) %>% 
  group_by(Site, Quad, Species, Year) %>% 
  dplyr::summarize(n_adults = length(unique(trackID)),
                   mean_size_t = mean(basalArea_genet))
AZs_adults <- AZs_demo_dorm1_buff5_buffG05_aggregated %>% 
  st_drop_geometry() %>% 
  filter(recruit == 0 & !is.na(recruit)) %>% 
  group_by(Site, Quad, Species, Year) %>% 
  dplyr::summarize(n_adults = length(unique(trackID)),
                   mean_size_t = mean(basalArea_genet))
CO_adults <- CO_demo_dorm1_buff5_buffG05_aggregated %>% 
  st_drop_geometry() %>% 
  filter(recruit == 0 & !is.na(recruit)) %>% 
  group_by(Site, Quad, Species, Year) %>% 
  dplyr::summarize(n_adults = length(unique(trackID)),
                   mean_size_t = mean(basalArea_genet))
NM_adults <- NM_demo_dorm1_buff5_buffG05_aggregated %>% 
  st_drop_geometry() %>% 
  filter(recruit == 0 & !is.na(recruit)) %>% 
  group_by(Site, Quad, Species, Year) %>% 
  dplyr::summarize(n_adults = length(unique(trackID)),
                   mean_size_t = mean(basalArea_genet))
ID_adults <- ID_demo_dorm1_buff5_buffG05_aggregated %>% 
  st_drop_geometry() %>% 
  filter(recruit == 0 & !is.na(recruit)) %>% 
  group_by(Site, Quad, Species, Year) %>% 
  dplyr::summarize(n_adults = length(unique(trackID)),
                   mean_size_t = mean(basalArea_genet))
KS_adults <- KS_demo_dorm1_buff5_buffG05_aggregated %>% 
  st_drop_geometry() %>% 
  filter(recruit == 0 & !is.na(recruit)) %>% 
  group_by(Site, Quad, Species, Year) %>% 
  dplyr::summarize(n_adults = length(unique(trackID)),
                   mean_size_t = mean(basalArea_genet))

## put together 
adults_df <- rbind(AZn_adults, AZs_adults, CO_adults, NM_adults, ID_adults, MT_adults, KS_adults)

adults_df <- adults_df %>% 
  rename(Year_t_adults = Year) %>% 
  mutate(Year_tplus1 = Year_t_adults+1)

## determine number of adults in previous year for all recruit combos (time lag of 1)

recsPerAdult_lag1 <- recruits_df %>% 
  full_join(adults_df, by = c("Site" = "Site", "Quad" = "Quad", "Species" = "Species", "Year_t_recs" = "Year_tplus1" ))
# when recruits is 'NA', replace with a 0
recsPerAdult_lag1[is.na(recsPerAdult_lag1$recruits), "recruits"] <- 0
#except when last year of sampling...
recsPerAdult_lag1[recsPerAdult_lag1$Year_t_recs==2021 & recsPerAdult_lag1$Site == "AZn", "recruits"] <- NA
recsPerAdult_lag1[recsPerAdult_lag1$Year_t_recs==2002 & recsPerAdult_lag1$Site == "AZs", "recruits"] <- NA
recsPerAdult_lag1[recsPerAdult_lag1$Year_t_recs==2011 & recsPerAdult_lag1$Site == "CO", "recruits"] <- NA
recsPerAdult_lag1[recsPerAdult_lag1$Year_t_recs==1981 & recsPerAdult_lag1$Site == "NM", "recruits"] <- NA
recsPerAdult_lag1[recsPerAdult_lag1$Year_t_recs==1974 & recsPerAdult_lag1$Site == "ID", "recruits"] <- NA
recsPerAdult_lag1[recsPerAdult_lag1$Year_t_recs==1946 & recsPerAdult_lag1$Site == "MT", "recruits"] <- NA
recsPerAdult_lag1[recsPerAdult_lag1$Year_t_recs==1973 & recsPerAdult_lag1$Site == "KS", "recruits"] <- NA

# when n_adults is 'NA', replace with a .1 so we don't lose the data.... should do something else? 
recsPerAdult_lag1[is.na(recsPerAdult_lag1$n_adults), "n_adults"] <- 1

# calculate the number of recruits per adult in the previous year
recsPerAdult_lag1$recsPerAdult <- as.numeric(recsPerAdult_lag1$recruits/recsPerAdult_lag1$n_adults)

# remove values for non-matching years
recsPerAdult_lag1[is.na(recsPerAdult_lag1$Year_t_adults), "recsPerAdult"] <- NA


plot(recsPerAdult_lag1$n_adults, recsPerAdult_lag1$recruits)
