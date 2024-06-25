library(nimble)
library(stringr)
library(tidyr)
library(dplyr)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load("Missing_pred_repo_example.RData")

data <- data$HumanPresence

constants <- 

model <<- nimbleCode({
  for(j in 1:ngrdyrs) {
    ## Human presence ##
    HumanPresence[j] ~ dbern(pred.HumanPresence[j])
    logit(pred.HumanPresence[j]) <- BETA0.HumanPresence +
      BETA.TrailTotm.HumanPresence * X.beta[j, ind.TrailTotm] +
      BETA.RoadTotm.HumanPresence * X.beta[j, ind.RoadTotm] +
      BETA.Prp_MotRestricted.HumanPresence * X.beta[j, ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.HumanPresence * X.beta[j, ind.Prp_HorseRestricted]
    #_____ GOF _____#
    LLobs.HumanPresence[j] <- (HumanPresence[j] * pred.HumanPresence[j]) +
      ((1 - HumanPresence[j]) * (1 - pred.HumanPresence[j]))
    X.sim.HumanPresence[j] ~ dbern(pred.HumanPresence[j])
    LLsim.HumanPresence[j] <- (X.sim.HumanPresence[j] * pred.HumanPresence[j]) +
      ((1 - X.sim.HumanPresence[j]) * (1 - pred.HumanPresence[j]))
    #_______________#
  }

  ### Prior distributions ###
  BETA0.HumanPresence ~ dnorm(0, 0.66667)
  BETA.TrailTotm.HumanPresence ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.HumanPresence ~ dnorm(0, 0.66667)
  BETA.Prp_HorseRestricted.HumanPresence ~ dnorm(0, 0.66667)
  BETA.RoadTotm.HumanPresence ~ dnorm(0, 0.66667)
  
  #_______ GOF _______#
  dev.obs.HumanPresence <- -2 * sum(LLobs.HumanPresence[1:ngrdyrs])
  dev.sim.HumanPresence <- -2 * sum(LLsim.HumanPresence[1:ngrdyrs])
  test.HumanPresence <- step(dev.sim.HumanPresence - dev.obs.HumanPresence)
  #___________________#
})