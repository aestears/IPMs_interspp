#/////////////////////////
# Vital Rate Models: Survival 
# Alice Stears
# 22 June 2023
#/////////////////////////

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(plantTracker)
library(sf)
library(mapview)
library(rstan)
library(bayesplot)

# Load data from previous script ------------------------------------------
source("./AnalysisScripts/02_RecruitData.R")

# decide which species to use ---------------------------------------------
# get Hesperostipa comata species from three sites into one data.frame
MT_hesCom <- MT_demo_dorm1_buff5_buffG05_aggregated[MT_demo_dorm1_buff5_buffG05_aggregated$Species == "Hesperostipa comata",]

CO_hesCom <- CO_demo_dorm1_buff5_buffG05_aggregated[CO_demo_dorm1_buff5_buffG05_aggregated$Species == "Stipa comata",]

ID_hesCom <- ID_demo_dorm1_buff5_buffG05_aggregated[ID_demo_dorm1_buff5_buffG05_aggregated$Species == "Hesperostipa comata",]

hesCom <- rbind(MT_hesCom, CO_hesCom, ID_hesCom) 
hesCom$Species <- "Hesperostipa comata"


# Prepare data for survival model --------------------------------------------
hesCom_surv <- hesCom[!is.na(hesCom$survives_tplus1) & 
                        !is.na(hesCom$age)  ,]

## prepare data for Stan model
modMat <- model.matrix(~ basalArea_genet + age , data = hesCom_surv)
data <- with(hesCom_surv, 
             list(y = survives_tplus1, x = modMat, K = ncol(modMat), N = nrow(hesCom_surv)))

## run Stan model: 
survStan <- stan_model("./AnalysisScripts/StanModels/SurvModel.stan")

survStan_fit <- sampling(survStan, data = data, chains = 3, iter = 5000)
