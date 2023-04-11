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

# decide which species to use ---------------------------------------------
# get Hesperostipa comata species from three sites into one data.frame
MT_hesCom <- MT_demo_dorm1_buff5_buffG05_aggregated[MT_demo_dorm1_buff5_buffG05_aggregated$Species == "Hesperostipa comata",]

CO_hesCom <- CO_demo_dorm1_buff5_buffG05_aggregated[CO_demo_dorm1_buff5_buffG05_aggregated$Species == "Stipa comata",]

ID_hesCom <- ID_demo_dorm1_buff5_buffG05_aggregated[ID_demo_dorm1_buff5_buffG05_aggregated$Species == "Hesperostipa comata",]

hesCom <- rbind(MT_hesCom, CO_hesCom, ID_hesCom) 
hesCom$Species <- "Hesperostipa comata"

# visualize survival and growth for this species

survMod <- glm(survives_tplus1 ~ basalArea_genet + age, data = hesCom, family = "binomial")
newData <- data.frame("basalArea_genet" <- seq(from = min(hesCom$basalArea_genet), 
                                               to = max(hesCom$basalArea_genet), 
                                               length.out = length(hesCom$basalArea_genet)))
newData$survives_tplus1 <-  predict(survMod, 
        newdata = newData, 
        type = "response")

plot(jitter(survives_tplus1) ~ basalArea_genet, data = hesCom)
lines(survives_tplus1 ~ basalArea_genet, data = newData, col = "red")

ggplot(data = hesCom, aes(x = basalArea_genet, y = survives_tplus1)) + 
  geom_jitter(aes(col = Site), height = .01, alpha = .5) + 
  geom_smooth(aes(col = Site), method = "glm", method.args = list(family = "binomial"))
