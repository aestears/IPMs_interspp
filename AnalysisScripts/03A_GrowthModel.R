#/////////////////////////
# Vital Rate Models: Growth
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
library(brms)

# Load data from previous script ------------------------------------------
source("./AnalysisScripts/02_RecruitData.R")

# Prepare real data for growth model --------------------------------------------
# decide which species to use 
# get Hesperostipa comata species from three sites into one data.frame
MT_hesCom <- MT_demo_dorm1_buff5_buffG05_aggregated[MT_demo_dorm1_buff5_buffG05_aggregated$Species == "Hesperostipa comata",]

CO_hesCom <- CO_demo_dorm1_buff5_buffG05_aggregated[CO_demo_dorm1_buff5_buffG05_aggregated$Species == "Stipa comata",]

ID_hesCom <- ID_demo_dorm1_buff5_buffG05_aggregated[ID_demo_dorm1_buff5_buffG05_aggregated$Species == "Hesperostipa comata",]

hesCom <- rbind(MT_hesCom, CO_hesCom, ID_hesCom) 
hesCom$Species <- "Hesperostipa comata"
## basic growth model: size_tplus1 ~ normal(alpha + (Beta_1 * basalArea_genet) + (Beta_2 * age), sigma)
# remove data for individuals that didn't survive
hesCom_growth <- hesCom[hesCom$survives_tplus1 == 1 & !is.na(hesCom$survives_tplus1) & 
                          !is.na(hesCom$age)  ,]
# log transform size data
hesCom_growth$size_t_log <- log(hesCom_growth$basalArea_genet)
hesCom_growth$size_tplus1_log <- log(hesCom_growth$size_tplus1)

plot(size_tplus1_log ~ size_t_log, data = hesCom_growth)


# change age to a factor
hesCom_growth$age_fac <- as.factor(hesCom_growth$age)

# Prepare simulated data for growth model ---------------------------------
# only has size_t as a predictor, not age
# predictor variables
# basal area in the current year
simDat <- data.frame(basalArea_genet = rlnorm(n = 500, meanlog = .0001, sdlog = .6))/10
hist(simDat$basalArea_genet)
# log-transform data
simDat$size_t_log <- log(simDat$basalArea_genet)

# response variable
# basal area in the next year
simDat$size_tplus1_log <- -2.5 + .687 * simDat$size_t_log + rnorm(n = nrow(simDat), mean = 0, sd = .05)
plot(simDat$size_tplus1_log, simDat$size_t_log)


# Prepare simulated data for Stan model ---------------------------------------------

# modMat <- model.matrix(~ size_t_log , data = simDat)
# data <- with(simDat, 
#              list(y = size_tplus1_log, x = as.matrix(modMat[,2]), K = 1, N = nrow(simDat)))

data <- brms::make_standata(formula = size_tplus1_log ~ size_t_log, data = simDat, family = gaussian(),
                    prior = c(set_prior("normal(0,1000", class = "b")))

# Run simulated data Stan model ----------------------------------------------------------
#brms::make_stancode(formula = size_tplus1_log ~ size_t_log, data = simDat, family = gaussian(),
                   # prior = c(set_prior("normal(0,1000", class = "b")))

growthStan <- stan_model("./AnalysisScripts/StanModels/growthModel.stan")

growthStan_fit_sim <- sampling(growthStan, data = data, chains = 2, iter = 2000, warmup = 1000)

## evaluate Stan model: 
traceplot(growthStan_fit_sim)

print(growthStan_fit_sim, probs = c(0.025, 0.975))

summary(glm(simDat$size_tplus1_log ~ simDat$size_t_log))

# Prepare real data for Stan model ---------------------------------------------

# modMat <- model.matrix(~ size_t_log + age_fac, data = hesCom_growth)
# data <- with(hesCom_growth, 
#              list(y = size_tplus1_log, x = modMat[,2:3], K = ncol(modMat[,2:3]), N = nrow(hesCom_growth)))
data <- brms::make_standata(formula = size_tplus1_log ~ size_t_log, data = hesCom_growth, family = gaussian(),
                            prior = c(set_prior("normal(0,1000", class = "b")))

# Run simulated data Stan model ----------------------------------------------------------

growthStan <- stan_model("./AnalysisScripts/StanModels/growthModel.stan")

growthStan_fit_real <- sampling(growthStan, data = data, chains = 2, iter = 2000, warmup = 1000)

## evaluate Stan model: 
traceplot(growthStan_fit_real)

print(growthStan_fit_real)

summary(glm(hesCom_growth$size_tplus1_log ~ hesCom_growth$size_t_log))
