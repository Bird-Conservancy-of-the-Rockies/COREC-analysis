model <<- nimbleCode({
    ########################
    # Bird community model #
    ########################
    
    ##~~~~~~~~~~~ Prior distributions for basic parameters ~~~~~~~~~~~~##
    # Perceptibility parameters
    zeta0.mu ~ dnorm(4.5, 0.666667)
    zeta0.sd ~ dgamma(1, 1)
    b.mu ~ dunif(0, 20)
    b.sd ~ dgamma(1, 1)
    
    # Availability parameters
    theta0.mu ~ dnorm(0, 0.666667)
    theta0.sd ~ dgamma(1, 1)

    # Ecological parameters (occupancy and abundance)
    beta0.mu ~ dnorm(0, 1)
    beta0.sd ~ dgamma(1, 1)
    beta0.sd.yr ~ dgamma(1, 1)

    # Covariate effects
    for(k in 1:n.Xpp) {
      zetaVec.mu[k] ~ dnorm(0, 1)
      zetaVec.sd[k] ~ dgamma(1, 1)
    }

    for(k in 1:n.Xpa) {
      thetaVec.mu[k] ~ dnorm(0, 1)
      thetaVec.sd[k] ~ dgamma(1, 1)
    }
    
    for(k in 1:n.Xbeta) {
      betaVec.mu[k] ~ dnorm(0, 1)
      betaVec.sd[k] ~ dgamma(1, 1)
    }

    for(s in 1:nspp) { ## Start species loop ##
    
      ##~~~~~~~~~~~ Species-specific parameters ~~~~~~~~~~~~##
      zeta0[s] ~ dnorm(zeta0.mu, pow(zeta0.sd, -2))
      b[s] ~ T(dnorm(b.mu, pow(b.sd, -2)), 0, 20)
      for(k in 1:n.Xpp) {
        zetaVec[s, k] ~ dnorm(zetaVec.mu[k], pow(zetaVec.sd[k], -2))
      }
      theta0[s] ~ dnorm(theta0.mu, pow(theta0.sd, -2))
      for(k in 1:n.Xpa) {
        thetaVec[s, k] ~ dnorm(thetaVec.mu[k], pow(thetaVec.sd[k], -2))
      }

      beta0[s] ~ dnorm(beta0.mu, pow(beta0.sd, -2))
      for(k in 1:n.Xbeta) {
        betaVec[s, k] ~ dnorm(betaVec.mu[k], pow(betaVec.sd[k], -2))
      }
      for(t in 1:nyear) {
        dev.beta0[s, t] ~ dnorm(0, pow(beta0.sd.yr, -2))
      }

      for(j in 1:ngrdyrs) { # Start gridXyear observation loop
        ##~~~~~~~~~~~~~~~ Observation models ~~~~~~~~~~~~~~~~~~~##
        # Distance sampling
        
        if(n.Xpp > 1) {
          log(a[s, j]) <- zeta0[s] + inprod(zetaVec[s, 1:n.Xpp], X.pp[j, 1:n.Xpp])
        }
        if(n.Xpp == 1) {
          log(a[s, j]) <- zeta0[s] + zetaVec[s, 1] * X.pp[j, 1]
        }

        for(d in 1:nD)  {
          pp_d[s, j, d] <- 1 - exp(-pow(((breaks[s, d] + breaks[s, d + 1]) / 2) / a[s, j], -b[s]))
          pi_pp[s, j, d] <- pp_d[s, j, d] * area.prop[d]
          pi_pp_c[s, j, d] <- pi_pp[s, j, d] / pp[s, j]
          }
        pp[s, j] <- sum(pi_pp[s, j, 1:nD]) # Probability of detection at all

        # Time-removal probabilities
    
        logit(p_a[s, j]) <- theta0[s] + inprod(thetaVec[s, 1:n.Xpa], X.pa[j, 1:n.Xpa])
    
        for(k in 1:K)  {
          pi_pa[s, j, k] <- p_a[s, j] * pow(1 - p_a[s, j], (k - 1))
          pi_pa_c[s, j, k] <- pi_pa[s, j, k] / pa[s, j] # Conditional probabilities of availability
          }
        pa[s, j] <- sum(pi_pa[s, j, 1:K]) # Probability of ever available
        
        prob_n[s, j] <- pp[s, j] * pa[s, j] * (effort[j] / 16)
        n[s, j] ~ dbin(prob_n[s, j], N[s, j])
        
        ##~~~~~~~~~~~~~~ State process abundance model ~~~~~~~~~~~~~~##
        log(lambda[s, j]) <- beta0[s] + dev.beta0[s, yearInd[j]] + inprod(betaVec[s, 1:n.Xbeta], X.beta[j, 1:n.Xbeta])
        N[s, j] ~ dpois(lambda[s, j]) # Abundance state
        
      } # End gridXyear loop
    } # End species loop
    
    for(i in 1:nDet) { ## Start loop through detections (i.e., occassions when the species was detected) ##
    
      ##~~~~~~~~~~~~~~~ Multinomial distance and removal processes ~~~~~~~~~~~~~~~~~~~##
      dclass[i, 1:nD] ~ dmulti(pi_pp_c[spp.ind[i], det.ind[i], 1:nD], n[spp.ind[i], det.ind[i]])
      tint[i, 1:K] ~ dmulti(pi_pa_c[spp.ind[i], det.ind[i], 1:K], n[spp.ind[i], det.ind[i]])
    }
  
  ###############
  # Path models #
  ###############
  
  #~~~~~~~~~~ Prior distributions ~~~~~~~~~~~~~#
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

  BETA0.Speed ~ dnorm(0, 0.66667)
  BETA.Prp_MotRestricted.Speed ~ dnorm(0, 0.66667)
  shape.Speed ~ dgamma(1, 0.1)
  
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
    LLobs.Traffic[j] <- log((pow(rate.Traffic[j], shape.Traffic) *
      Traffic[j] * exp(-1 * rate.Traffic[j] * Traffic[j]))) -
      loggam(shape.Traffic)
    X.sim.Traffic[j] ~ dgamma(shape.Traffic, rate.Traffic[j])
    LLsim.Traffic[j] <- log((pow(rate.Traffic[j], shape.Traffic) *
      X.sim.Traffic[j] * exp(-1 * rate.Traffic[j] * X.sim.Traffic[j]))) -
      loggam(shape.Traffic)
    #_______________#
  }
  
  for(j in 1:ngrdyrs.Speed) {
    ## Traffic speed where humans are present ##
    Speed[j] ~ dgamma(shape.Speed, rate.Speed[j])
    log(pred.Speed[j]) <- BETA0.Speed +
      BETA.Prp_MotRestricted.Speed * X.beta[ind.SpeedPresent[j], ind.Prp_MotRestricted]
    rate.Speed[j] <- shape.Speed / pred.Speed[j]
    #_____ GOF _____#
    LLobs.Speed[j] <- log((pow(rate.Speed[j], shape.Speed) *
      Speed[j] * exp(-1 * rate.Speed[j] * Speed[j]))) -
      loggam(shape.Speed)
    X.sim.Speed[j] ~ dgamma(shape.Speed, rate.Speed[j])
    LLsim.Speed[j] <- log((pow(rate.Speed[j], shape.Speed) *
      X.sim.Speed[j] * exp(-1 * rate.Speed[j] * X.sim.Speed[j]))) -
      loggam(shape.Speed)
    #_______________#
  }
  
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