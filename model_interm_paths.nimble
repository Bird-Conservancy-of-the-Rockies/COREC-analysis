logdens.gamma <- nimbleFunction(
    run = function(x = double(), a = double(), b = double()) {
        lp <- dgamma(x, a, b, log = TRUE)
        returnType(double())
        return(lp)
    }
)

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
  
  for(j in 1:ngrdyrs.hpresent) {
    ## Traffic volume where humans are present ##
    Traffic[j] ~ dgamma(shape.Traffic, rate.Traffic[j])
    log(pred.Traffic[j]) <- BETA0.Traffic +
      BETA.TrailTotm.Traffic * X.beta[ind.hpresent[j], ind.TrailTotm] +
      BETA.RoadTotm.Traffic * X.beta[ind.hpresent[j], ind.RoadTotm] +
      BETA.Prp_MotRestricted.Traffic * X.beta[ind.hpresent[j], ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.Traffic * X.beta[ind.hpresent[j], ind.Prp_HorseRestricted]
    rate.Traffic[j] <- shape.Traffic / pred.Traffic[j]
    #_____ GOF _____#
    LLobs.Traffic[j] <- logdens.gamma(Traffic[j], shape.Traffic, rate.Traffic[j])
    X.sim.Traffic[j] ~ dgamma(shape.Traffic, rate.Traffic[j])
    LLsim.Traffic[j] <- logdens.gamma(X.sim.Traffic[j], shape.Traffic, rate.Traffic[j])
    #_______________#
    
    ## Traffic diel timing where humans are present ##
    TOD_mean[j] ~ dgamma(shape.TOD_mean, rate.TOD_mean[j])
    log(pred.TOD_mean[j]) <- BETA0.TOD_mean +
      BETA.TrailTotm.TOD_mean * X.beta[ind.hpresent[j], ind.TrailTotm] +
      BETA.RoadTotm.TOD_mean * X.beta[ind.hpresent[j], ind.RoadTotm] +
      BETA.Prp_MotRestricted.TOD_mean * X.beta[ind.hpresent[j], ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.TOD_mean * X.beta[ind.hpresent[j], ind.Prp_HorseRestricted]
    rate.TOD_mean[j] <- shape.TOD_mean / pred.TOD_mean[j]
    #_____ GOF _____#
    LLobs.TOD_mean[j] <- logdens.gamma(TOD_mean[j], shape.TOD_mean, rate.TOD_mean[j])
    X.sim.TOD_mean[j] ~ dgamma(shape.TOD_mean, rate.TOD_mean[j])
    LLsim.TOD_mean[j] <- logdens.gamma(X.sim.TOD_mean[j], shape.TOD_mean, rate.TOD_mean[j])
    #_______________#
  }
  
  for(j in 1:ngrdyrs.DOY_Speed) {
    ## Traffic seasonal timing where humans are present ##
    X.latent.Traffic_DOY_mn[j] ~ dbern(X.latent.prob)
    Traffic_DOY_mn[j] ~ dgamma(shape.Traffic_DOY_mn, rate.Traffic_DOY_mn[j])
    log(pred.Traffic_DOY_mn[j]) <- BETA0.Traffic_DOY_mn +
      BETA.TrailTotm.Traffic_DOY_mn * X.beta[ind.DOY_Speed[j], ind.TrailTotm] +
      BETA.RoadTotm.Traffic_DOY_mn * X.beta[ind.DOY_Speed[j], ind.RoadTotm] +
      BETA.Prp_MotRestricted.Traffic_DOY_mn * X.beta[ind.DOY_Speed[j], ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.Traffic_DOY_mn * X.beta[ind.DOY_Speed[j], ind.Prp_HorseRestricted] +
      BETA.latent.Traffic_DOY_mn * X.latent.Traffic_DOY_mn[j]
    rate.Traffic_DOY_mn[j] <- shape.Traffic_DOY_mn / pred.Traffic_DOY_mn[j]
    #_____ GOF _____#
    LLobs.Traffic_DOY_mn[j] <- logdens.gamma(Traffic_DOY_mn[j], shape.Traffic_DOY_mn, rate.Traffic_DOY_mn[j])
    X.sim.Traffic_DOY_mn[j] ~ dgamma(shape.Traffic_DOY_mn, rate.Traffic_DOY_mn[j])
    LLsim.Traffic_DOY_mn[j] <- logdens.gamma(X.sim.TOD_mean[j], shape.Traffic_DOY_mn, rate.Traffic_DOY_mn[j])
    #_______________#

    ## Traffic speed where humans are present ##
    Speed[j] ~ dgamma(shape.Speed, rate.Speed[j])
    log(pred.Speed[j]) <- BETA0.Speed +
      BETA.TrailTotm.Speed * X.beta[ind.DOY_Speed[j], ind.TrailTotm] +
      BETA.Prp_MotRestricted.Speed * X.beta[ind.DOY_Speed[j], ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.Speed * X.beta[ind.DOY_Speed[j], ind.Prp_HorseRestricted] +
      BETA.RoadTotm.Speed * X.beta[ind.DOY_Speed[j], ind.RoadTotm]
    rate.Speed[j] <- shape.Speed / pred.Speed[j]
    #_____ GOF _____#
    LLobs.Speed[j] <- logdens.gamma(Speed[j], shape.Speed, rate.Speed[j])
    X.sim.Speed[j] ~ dgamma(shape.Speed, rate.Speed[j])
    LLsim.Speed[j] <- logdens.gamma(X.sim.Speed[j], shape.Speed, rate.Speed[j])
    #_______________#
  }

  ### Prior distributions ###
  BETA0.HumanPresence ~ dnorm(0, 0.66667)
  BETA.TrailTotm.HumanPresence ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.HumanPresence ~ dnorm(0, 0.66667)
  BETA.Prp_HorseRestricted.HumanPresence ~ dnorm(0, 0.66667)
  BETA.RoadTotm.HumanPresence ~ dnorm(0, 0.66667)

  BETA0.Traffic ~ dnorm(0, 0.66667)
  BETA.TrailTotm.Traffic ~ dnorm(0, 0.66667)
  BETA.RoadTotm.Traffic ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.Traffic ~ dnorm(0, 0.66667)
  BETA.Prp_HorseRestricted.Traffic ~ dnorm(0, 0.66667)
  shape.Traffic ~ dgamma(1, 1)

  BETA0.TOD_mean ~ dnorm(0, 0.66667)
  BETA.TrailTotm.TOD_mean ~ dnorm(0, 0.66667)
  BETA.RoadTotm.TOD_mean ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.TOD_mean ~ dnorm(0, 0.66667)
  BETA.Prp_HorseRestricted.TOD_mean ~ dnorm(0, 0.66667)
  shape.TOD_mean ~ dgamma(1, 0.01)

  BETA0.Traffic_DOY_mn ~ dnorm(0, 0.66667)
  BETA.TrailTotm.Traffic_DOY_mn ~ dnorm(0, 0.66667)
  BETA.RoadTotm.Traffic_DOY_mn ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.Traffic_DOY_mn ~ dnorm(0, 0.66667)
  BETA.Prp_HorseRestricted.Traffic_DOY_mn ~ dnorm(0, 0.66667)
  BETA.latent.Traffic_DOY_mn ~ T(dnorm(0, 0.66667), 0, )
  shape.Traffic_DOY_mn ~ dgamma(1, 0.1)
  X.latent.prob ~ dunif(0, 1)

  BETA0.Speed ~ dnorm(0, 0.66667)
  BETA.TrailTotm.Speed ~ dnorm(0, 0.66667)
  BETA.RoadTotm.Speed ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.Speed ~ dnorm(0, 0.66667)
  BETA.Prp_HorseRestricted.Speed ~ dnorm(0, 0.66667)
  shape.Speed ~ dgamma(1, 0.1)

  #_______ GOF _______#
  dev.obs.HumanPresence <- -2 * sum(LLobs.HumanPresence[1:ngrdyrs])
  dev.sim.HumanPresence <- -2 * sum(LLsim.HumanPresence[1:ngrdyrs])
  test.HumanPresence <- step(dev.sim.HumanPresence - dev.obs.HumanPresence)
  
  dev.obs.Traffic <- -2 * sum(LLobs.Traffic[1:ngrdyrs.hpresent])
  dev.sim.Traffic <- -2 * sum(LLsim.Traffic[1:ngrdyrs.hpresent])
  test.Traffic <- step(dev.sim.Traffic - dev.obs.Traffic)
  
  #dev.obs.Traffic_manual <- -2 * sum(LLobs.Traffic_manual[1:ngrdyrs.hpresent])
  #dev.sim.Traffic_manual <- -2 * sum(LLsim.Traffic_manual[1:ngrdyrs.hpresent])
  #test.Traffic_manual <- step(dev.sim.Traffic_manual - dev.obs.Traffic_manual)
  
  dev.obs.TOD_mean <- -2 * sum(LLobs.TOD_mean[1:ngrdyrs.hpresent])
  dev.sim.TOD_mean <- -2 * sum(LLsim.TOD_mean[1:ngrdyrs.hpresent])
  test.TOD_mean <- step(dev.sim.TOD_mean - dev.obs.TOD_mean)
  
  dev.obs.Traffic_DOY_mn <- -2 * sum(LLobs.Traffic_DOY_mn[1:ngrdyrs.DOY_Speed])
  dev.sim.Traffic_DOY_mn <- -2 * sum(LLsim.Traffic_DOY_mn[1:ngrdyrs.DOY_Speed])
  test.Traffic_DOY_mn <- step(dev.sim.Traffic_DOY_mn - dev.obs.Traffic_DOY_mn)
  
  dev.obs.Speed <- -2 * sum(LLobs.Speed[1:ngrdyrs.DOY_Speed])
  dev.sim.Speed <- -2 * sum(LLsim.Speed[1:ngrdyrs.DOY_Speed])
  test.Speed <- step(dev.sim.Speed - dev.obs.Speed)
  #___________________#
  })