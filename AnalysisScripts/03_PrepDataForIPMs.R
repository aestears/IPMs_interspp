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
newData <- data.frame("basalArea_genet" <- seq(from = min(hesCom$basalArea_genet), 
                                               to = max(hesCom$basalArea_genet), 
                                               length.out = length(hesCom$basalArea_genet)))
newData$survives_tplus1 <-  predict(survMod, 
        newdata = newData, 
        type = "response")



ggplot(data = hesCom, aes(x = basalArea_genet, y = survives_tplus1)) + 
  geom_jitter(aes(col = as.factor(age)), height = .01, alpha = .5) + 
  geom_smooth(aes(col = as.factor(age)),se = FALSE, method = "glm", method.args = list(family = "binomial"))

# Prepare data for survival model -----------------------------------------
## basic survival model glmer(survives_tplus1 ~ basalArea_genet + Site + age + (1|Year), family = "binomial")
amod <- aov(survives_tplus1 ~ basalArea_genet, hesCom)



# Prepare data for growth data --------------------------------------------
## basic growth model: size_tplus1 ~ normal(alpha + Beta * basalArea_genet, sigma)

## Stan model: 
