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

