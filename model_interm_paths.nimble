model <<- nimbleCode({
  for(j in 1:ngrdyrs) {
    ## Human presence ##
    X.Eco.raw[j, ind.HumanPresence] ~ dbern(pred.HumanPresence[j])
    logit(pred.HumanPresence[j]) <- BETA0.HumanPresence +
      BETA.TrailTotm.HumanPresence * X.Eco[j, ind.TrailTotm] +
      BETA.RoadTotm.HumanPresence * X.Eco[j, ind.RoadTotm] +
      BETA.Prp_MotRestricted.HumanPresence * X.Eco[j, ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.HumanPresence * X.Eco[j, ind.Prp_HorseRestricted]
    #_____ GOF _____#
    LLobs.HumanPresence[j] <- logdensity.bern(X.Eco.raw[j, ind.HumanPresence],
      pred.HumanPresence[j])
    X.sim.HumanPresence[j] ~ dbern(pred.HumanPresence[j])
    LLsim.HumanPresence[j] <- logdensity.bern(X.sim.HumanPresence[j],
      pred.HumanPresence[j])
    #_______________#
  }
  
  for(j in 1:ngrdyrs.hpresent) {
    ## Traffic volume where humans are present ##
    LogTraffic[j] <- exp(X.Eco.raw[ind.hpresent[j], ind.LogTrafficNoZeros])
    LogTraffic[j] ~ dgamma(shape.LogTraffic, shape.LogTraffic / pred.LogTraffic[j])
    log(pred.LogTraffic[j]) <- BETA0.LogTraffic +
      BETA.TrailTotm.LogTraffic * X.Eco[ind.hpresent[j], ind.TrailTotm] +
      BETA.RoadTotm.LogTraffic * X.Eco[ind.hpresent[j], ind.RoadTotm] +
      BETA.Prp_MotRestricted.LogTraffic * X.Eco[ind.hpresent[j], ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.LogTraffic * X.Eco[ind.hpresent[j], ind.Prp_HorseRestricted]
    #_____ GOF _____#
    LLobs.LogTraffic[j] <- logdensity.gamma(LogTraffic[j],
      shape.LogTraffic, shape.LogTraffic / pred.LogTraffic[j])
    X.sim.LogTraffic[j] ~ dgamma(shape.LogTraffic, shape.LogTraffic / pred.LogTraffic[j])
    LLsim.LogTraffic[j] <- logdensity.gamma(X.sim.LogTraffic[j],
      shape.LogTraffic, shape.LogTraffic / pred.LogTraffic[j])
    #_______________#
    
    ## Traffic diel timing where humans are present ##
    X.Eco.raw[ind.hpresent[j], ind.TOD_mean] ~ dgamma(shape.TOD_mean, shape.TOD_mean / pred.TOD_mean[j])
    log(pred.TOD_mean[j]) <- BETA0.TOD_mean +
      BETA.TrailTotm.TOD_mean * X.Eco[ind.hpresent[j], ind.TrailTotm] +
      BETA.RoadTotm.TOD_mean * X.Eco[ind.hpresent[j], ind.RoadTotm] +
      BETA.Prp_MotRestricted.TOD_mean * X.Eco[ind.hpresent[j], ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.TOD_mean * X.Eco[ind.hpresent[j], ind.Prp_HorseRestricted]
    #_____ GOF _____#
    LLobs.TOD_mean[j] <- logdensity.gamma(X.Eco.raw[ind.hpresent[j], ind.TOD_mean],
      shape.TOD_mean, shape.TOD_mean / pred.TOD_mean[j])
    X.sim.TOD_mean[j] ~ dgamma(shape.TOD_mean, shape.TOD_mean /
      pred.TOD_mean[j])
    LLsim.TOD_mean[j] <- logdensity.gamma(X.sim.TOD_mean[j],
      shape.TOD_mean, shape.TOD_mean / pred.TOD_mean[j])
    #_______________#
  }
  
  for(j in 1:ngrdyrs.DOY_Speed) {
    ## Traffic seasonal timing where humans are present ##
    X.Eco.raw[ind.DOY_Speed[j], ind.Traffic_DOY_mn] ~
      dgamma(shape.Traffic_DOY_mn, shape.Traffic_DOY_mn / pred.Traffic_DOY_mn[j])
    log(pred.Traffic_DOY_mn[j]) <- BETA0.Traffic_DOY_mn +
      BETA.TrailTotm.Traffic_DOY_mn * X.Eco[ind.DOY_Speed[j], ind.TrailTotm] +
      BETA.RoadTotm.Traffic_DOY_mn * X.Eco[ind.DOY_Speed[j], ind.RoadTotm] +
      BETA.Prp_MotRestricted.Traffic_DOY_mn * X.Eco[ind.DOY_Speed[j], ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.Traffic_DOY_mn * X.Eco[ind.DOY_Speed[j], ind.Prp_HorseRestricted]
    #_____ GOF _____#
    LLobs.Traffic_DOY_mn[j] <- logdensity.gamma(X.Eco.raw[ind.DOY_Speed[j], ind.Traffic_DOY_mn],
      shape.Traffic_DOY_mn, shape.Traffic_DOY_mn / pred.Traffic_DOY_mn[j])
    X.sim.Traffic_DOY_mn[j] ~ dgamma(shape.Traffic_DOY_mn, shape.Traffic_DOY_mn / pred.Traffic_DOY_mn[j])
    LLsim.Traffic_DOY_mn[j] <- logdensity.gamma(X.sim.Traffic_DOY_mn[j],
      shape.Traffic_DOY_mn, shape.Traffic_DOY_mn / pred.Traffic_DOY_mn[j])
    #_______________#

    ## Traffic speed where humans are present ##
    X.Eco.raw[ind.DOY_Speed[j], ind.Speed] ~ dgamma(shape.Speed, shape.Speed / pred.Speed[j])
    log(pred.Speed[j]) <- BETA0.Speed +
      BETA.TrailTotm.Speed * X.Eco[ind.DOY_Speed[j], ind.TrailTotm] +
      BETA.Prp_MotRestricted.Speed * X.Eco[ind.DOY_Speed[j], ind.Prp_MotRestricted] +
      BETA.Prp_HorseRestricted.Speed * X.Eco[ind.DOY_Speed[j], ind.Prp_HorseRestricted] +
      BETA.RoadTotm.Speed * X.Eco[ind.DOY_Speed[j], ind.RoadTotm]
    #_____ GOF _____#
    LLobs.Speed[j] <- logdensity.gamma(X.Eco.raw[ind.DOY_Speed[j], ind.Speed],
      shape.Speed, shape.Speed / pred.Speed[j])
    X.sim.Speed[j] ~ dgamma(shape.Speed, shape.Speed / pred.Speed[j])
    LLsim.Speed[j] <- logdensity.gamma(X.sim.Speed[j], shape.Speed, shape.Speed / pred.Speed[j])
    #_______________#
  }

  ### Prior distributions ###
  BETA0.HumanPresence ~ dnorm(0, 0.66667)
  BETA.TrailTotm.HumanPresence ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.HumanPresence ~ dnorm(0, 0.66667)
  BETA.Prp_HorseRestricted.HumanPresence ~ dnorm(0, 0.66667)
  BETA.RoadTotm.HumanPresence ~ dnorm(0, 0.66667)

  BETA0.LogTraffic ~ dnorm(0, 0.66667)
  BETA.TrailTotm.LogTraffic ~ dnorm(0, 0.66667)
  BETA.RoadTotm.LogTraffic ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.LogTraffic ~ dnorm(0, 0.66667)
  BETA.Prp_HorseRestricted.LogTraffic ~ dnorm(0, 0.66667)
  shape.LogTraffic ~ dgamma(1, 1)

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
  shape.Traffic_DOY_mn ~ dgamma(1, 0.1)

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
  
  dev.obs.LogTraffic <- -2 * sum(LLobs.LogTraffic[1:ngrdyrs.hpresent])
  dev.sim.LogTraffic <- -2 * sum(LLsim.LogTraffic[1:ngrdyrs.hpresent])
  test.LogTraffic <- step(dev.sim.LogTraffic - dev.obs.LogTraffic)
  
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