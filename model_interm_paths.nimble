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
      BETA.Prp_MotRestricted.HumanPresence * X.beta[j, ind.Prp_MotRestricted]
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
      BETA.Prp_MotRestricted.Traffic * X.beta[ind.hpresent[j], ind.Prp_MotRestricted]
    rate.Traffic[j] <- shape.Traffic / pred.Traffic[j]
    #_____ GOF _____#
    LLobs.Traffic[j] <- logdens.gamma(Traffic[j], shape.Traffic, rate.Traffic[j])
    X.sim.Traffic[j] ~ dgamma(shape.Traffic, rate.Traffic[j])
    LLsim.Traffic[j] <- logdens.gamma(X.sim.Traffic[j], shape.Traffic, rate.Traffic[j])
    #_______________#
  }
  
  for(j in 1:ngrdyrs.Speed) {
    ## Traffic speed where humans are present ##
    Speed[j] ~ dgamma(shape.Speed, rate.Speed[j])
    log(pred.Speed[j]) <- BETA0.Speed +
      BETA.TrailTotm.Speed * X.beta[ind.SpeedPresent[j], ind.TrailTotm] +
      BETA.RoadTotm.Speed * X.beta[ind.SpeedPresent[j], ind.RoadTotm] +
      BETA.Prp_MotRestricted.Speed * X.beta[ind.SpeedPresent[j], ind.Prp_MotRestricted]
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
  BETA.RoadTotm.HumanPresence ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.HumanPresence ~ dnorm(0, 0.66667)

  BETA0.Traffic ~ dnorm(0, 0.66667)
  BETA.TrailTotm.Traffic ~ dnorm(0, 0.66667)
  BETA.RoadTotm.Traffic ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.Traffic ~ dnorm(0, 0.66667)
  shape.Traffic ~ dgamma(1, 1)

  BETA0.Speed ~ dnorm(0, 0.66667)
  BETA.TrailTotm.Speed ~ dnorm(0, 0.66667)
  BETA.RoadTotm.Speed ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.Speed ~ dnorm(0, 0.66667)
  shape.Speed ~ dgamma(1, 0.1)

  #_______ GOF _______#
  dev.obs.HumanPresence <- -2 * sum(LLobs.HumanPresence[1:ngrdyrs])
  dev.sim.HumanPresence <- -2 * sum(LLsim.HumanPresence[1:ngrdyrs])
  test.HumanPresence <- step(dev.sim.HumanPresence - dev.obs.HumanPresence)
  
  dev.obs.Traffic <- -2 * sum(LLobs.Traffic[1:ngrdyrs.hpresent])
  dev.sim.Traffic <- -2 * sum(LLsim.Traffic[1:ngrdyrs.hpresent])
  test.Traffic <- step(dev.sim.Traffic - dev.obs.Traffic)
  
  dev.obs.Speed <- -2 * sum(LLobs.Speed[1:ngrdyrs.Speed])
  dev.sim.Speed <- -2 * sum(LLsim.Speed[1:ngrdyrs.Speed])
  test.Speed <- step(dev.sim.Speed - dev.obs.Speed)
  #___________________#
  })