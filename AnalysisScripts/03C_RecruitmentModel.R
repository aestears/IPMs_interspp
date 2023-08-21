#/////////////////////////
# Vital Rate Models: Recruitment
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
hesCom_recs <- recsPerAdult_lag1[recsPerAdult_lag1$Species %in% c("Hesperostipa comata", "Stipa comata"),]

hesCom_recs$Species <- "Hesperostipa comata"

# log transform size data
hesCom_recs$mean_size_t_log <- log(hesCom_recs$mean_size_t)

# # Prepare simulated data for growth model ---------------------------------
# # only has size_t as a predictor, not age
# # predictor variables
# # basal area in the current year
# simDat <- data.frame(basalArea_genet = rlnorm(n = 500, meanlog = .0001, sdlog = .6))/10
# hist(simDat$basalArea_genet)
# # log-transform data
# simDat$size_t_log <- log(simDat$basalArea_genet)
# 
# # response variable
# # basal area in the next year
# simDat$size_tplus1_log <- -2.5 + .687 * simDat$size_t_log + rnorm(n = nrow(simDat), mean = 0, sd = .05)
# plot(simDat$size_tplus1_log, simDat$size_t_log)
# 
# Prepare simulated data for Stan model ---------------------------------------------

# # modMat <- model.matrix(~ size_t_log , data = simDat)
# # data <- with(simDat, 
# #              list(y = size_tplus1_log, x = as.matrix(modMat[,2]), K = 1, N = nrow(simDat)))
# 
# data <- brms::make_standata(formula = size_tplus1_log ~ size_t_log, data = simDat, family = gaussian(),
#                             prior = c(set_prior("normal(0,1000", class = "b")))
# 
# # Run simulated data Stan model ----------------------------------------------------------
# #brms::make_stancode(formula = size_tplus1_log ~ size_t_log, data = simDat, family = gaussian(),
# # prior = c(set_prior("normal(0,1000", class = "b")))
# 
# growthStan <- stan_model("./AnalysisScripts/StanModels/growthModel.stan")
# 
# growthStan_fit_sim <- sampling(growthStan, data = data, chains = 2, iter = 2000, warmup = 1000)
# 
# ## evaluate Stan model: 
# traceplot(growthStan_fit_sim)
# 
# print(growthStan_fit_sim, probs = c(0.025, 0.975))
# 
# summary(glm(simDat$size_tplus1_log ~ simDat$size_t_log))

# Prepare real data for Stan model ---------------------------------------------
# use a gamma distribution??

data <- brms::make_standata(formula = recsPerAdult ~ mean_size_t_log, data = hesCom_recs, family = hurdle_gamma(),
                            prior = c(set_prior("normal(0,1000", class = "b")))

# Run simulated data Stan model ----------------------------------------------------------
# brms::make_stancode(formula = recsPerAdult ~ mean_size_t_log, data = hesCom_recs, family = hurdle_gamma(),
#                     prior = c(set_prior("normal(0,1000", class = "b")))

recStan <- stan_model("./AnalysisScripts/StanModels/recModel.stan")

recStan_fit_real <- sampling(recStan, data = data, chains = 2, iter = 2000, warmup = 1000)

## evaluate Stan model: 
traceplot(recStan_fit_real)

print(recStan_fit_real)

