#/////////////////////////
# Vital Rate Models: Survival
# Alice Stears
# 18 July 2023
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

# Prepare real data for growth model --------------------------------------------
# decide which species to use 
# get Hesperostipa comata species from three sites into one data.frame
MT_hesCom <- MT_demo_dorm1_buff5_buffG05_aggregated[MT_demo_dorm1_buff5_buffG05_aggregated$Species == "Hesperostipa comata",]

CO_hesCom <- CO_demo_dorm1_buff5_buffG05_aggregated[CO_demo_dorm1_buff5_buffG05_aggregated$Species == "Stipa comata",]

ID_hesCom <- ID_demo_dorm1_buff5_buffG05_aggregated[ID_demo_dorm1_buff5_buffG05_aggregated$Species == "Hesperostipa comata",]

hesCom <- rbind(MT_hesCom, CO_hesCom, ID_hesCom) 
hesCom$Species <- "Hesperostipa comata"

## basic survival model: survives_tplus1 ~ binomial(alpha + (Beta_1 * basalArea_genet) + (Beta_2 * age), sigma)
# remove data for individuals that didn't survive
hesCom_surv <- hesCom[!is.na(hesCom$survives_tplus1) & 
                          !is.na(hesCom$age)  ,]
# log transform size data
hesCom_surv$size_t_log <- log(hesCom_surv$basalArea_genet)

plot(survives_tplus1 ~ size_t_log, data = hesCom_surv)

# change age to a factor
hesCom_surv$age_fac <- as.factor(hesCom_surv$age)

# Prepare simulated data for growth model ---------------------------------
# predictor variables
# basal area in the current year
simDat <- data.frame(basalArea_genet = rlnorm(n = 500, meanlog = -10, sdlog = 1))
hist(simDat$basalArea_genet)
# log-transform data
simDat$size_t_log <- log(simDat$basalArea_genet)
hist(simDat$size_t_log)

# response variable
# survival to the next year
survMod_test <- glm(hesCom_surv$survives_tplus1 ~ hesCom_surv$size_t_log, family = "binomial")
# generate the linear predicted values
linPreds_sim <- 4.64 + .427 * simDat$size_t_log + rnorm(n = nrow(simDat), mean = 0, sd = .05)
# translate the predicted values to proportions using an inverse logit link function
preds_sim <- 1/(1+exp(-linPreds_sim))
# generate binomial response variables using the predicted proportions
simDat$survives_tplus1 <- as.integer(rbinom(n = nrow(simDat), size = 1, prob = preds_sim) )

plot(simDat$survives_tplus1~ simDat$size_t_log)

# Prepare simulated data for Stan model ---------------------------------------------
# 
# modMat <- model.matrix(~ size_t_log , data = simDat)
# data <- with(simDat, 
#              list(y = survives_tplus1, x = as.matrix(modMat[,2]), K = as.integer(1), N = nrow(simDat)))

data <- brms::make_standata(formula = survives_tplus1 ~ size_t_log, data = simDat, family = bernoulli(),
                            prior = c(set_prior("normal(0,1000", class = "b")))

# Run simulated data Stan model ----------------------------------------------------------

survStan <- stan_model("./AnalysisScripts/StanModels/survModel.stan")

survStan_fit_sim <- sampling(survStan,  warmup = 1000, data = data, chains = 2, iter = 2000)

## evaluate Stan model: 
traceplot(survStan_fit_sim)
print(survStan_fit_sim, probs = c(0.025, 0.975))

summary(glm(simDat$survives_tplus1 ~ simDat$size_t_log , family = "binomial"))

# Prepare real data for Stan model ---------------------------------------------
# 
# modMat <- model.matrix(~ size_t_log, data = hesCom_surv)
# data <- with(hesCom_surv, 
#              list(y = survives_tplus1, x = as.matrix(modMat[,2]), K = 1, N = nrow(hesCom_surv)))
# data$prior_only <- 0

# use brms
data <- brms::make_standata(formula = survives_tplus1 ~ size_t_log, data = hesCom_surv, family = bernoulli(),
                    prior = c(set_prior("normal(0,1000", class = "b")))

# Run simulated data Stan model ----------------------------------------------------------
# used brms to make the stan code
# brms::make_stancode(formula = survives_tplus1 ~ size_t_log, data = hesCom_surv, family = bernoulli(), 
#                     prior = c(set_prior("normal(0,1000", class = "b")))

survStan <- stan_model("./AnalysisScripts/StanModels/survModel.stan")

survStan_fit_real <- sampling(survStan, warmup = 1000, data = data, chains = 2, iter = 2000)

## evaluate Stan model: 
traceplot(survStan_fit_real)
print(survStan_fit_real, probs = c(0.025, 0.975))

summary(glm(hesCom_surv$survives_tplus1 ~ hesCom_surv$size_t_log , family = "binomial"))


# testing brms model w/ real data ------------------------------------------------------


survBrms_fit_real <- brm(formula = survives_tplus1 ~ size_t_log,
            data = hesCom_surv, family = bernoulli(),
            prior = c(set_prior("normal(0,1000)", class = "b")),
            warmup = 1000, iter = 2000, chains = 2,
            control = list(adapt_delta = 0.95))

