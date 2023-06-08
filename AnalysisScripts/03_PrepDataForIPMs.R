#/////////////////////////
# Prepare demographic data for IPM analysis
# Alice Stears
# 11 April 2023
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

# visualize survival and growth for this species
survDat <- hesCom[is.na(hesCom$survives_tplus1) != TRUE,]
survMod <- glm(survives_tplus1 ~ basalArea_genet + Site + as.factor(age), data = survDat, family = "binomial")

ggplot(data = hesCom, aes(x = basalArea_genet, y = survives_tplus1)) + 
  geom_jitter(aes(col = as.factor(age)), height = .01, alpha = .5) + 
  geom_smooth(aes(col = as.factor(age)),se = FALSE, method = "glm", method.args = list(family = "binomial"))

# Prepare data for survival model -----------------------------------------
## basic survival model glmer(survives_tplus1 ~ basalArea_genet + Site + age + (1|Year), family = "binomial")
amod <- aov(survives_tplus1 ~ basalArea_genet, hesCom)

# Prepare data for growth model --------------------------------------------
## basic growth model: size_tplus1 ~ normal(alpha + (Beta_1 * basalArea_genet) + (Beta_2 * age), sigma)
# remove data for individuals that didn't survive
hesCom_growth <- hesCom[hesCom$survives_tplus1 == 1 & !is.na(hesCom$survives_tplus1) & 
                          !is.na(hesCom$age)  ,]
## prepare data for Stan model:
modMat <- model.matrix(~ basalArea_genet + age , data = hesCom_growth)
data <- with(hesCom_growth, 
             list(y = size_tplus1, x = modMat, K = ncol(modMat), N = nrow(hesCom_growth)))

## run Stan model: 
growthStan <- stan_model("./AnalysisScripts/StanModels/growthModel.stan")

growthStan_fit <- sampling(growthStan, data = data, chains = 4, iter = 8000)

## evaluate Stan model: 
traceplot(growthStan_fit)

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

