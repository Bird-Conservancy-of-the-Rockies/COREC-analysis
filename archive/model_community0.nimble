model <<- nimbleCode({
    ##~~~~~~~~~~~ Prior distributions for basic parameters ~~~~~~~~~~~~##
    
    # Perceptibility parameters
    zeta0.mu ~ dnorm(4.5, 0.666667)
    zeta0.sd ~ dgamma(1, 1)
    b.mu ~ dunif(0, 20)
    b.sd ~ dgamma(1, 1)
    
    # Availability parameters
    theta0.mu ~ dnorm(0, 0.666667)
    theta0.sd ~ dgamma(1, 1)

    # Ecological parameters (occupancy, abundance, and their dynamics)
    alpha0.mu ~ dnorm(0, 0.66667)
    alpha0.sd ~ dgamma(1, 1)
    alpha0.sd.yr ~ dgamma(1, 1)
    
    beta0.mu ~ dnorm(0, 1)
    beta0.sd ~ dgamma(1, 1)
    beta0.sd.yr ~ dgamma(1, 1)
    
    rho.ab ~ dunif(-1, 1) # Correlation between occupancy and abundance among species

    # Covariate effects
    #for(k in 1:n.Xpp) {
    #  zetaVec.mu[k] ~ dnorm(0, 1)
    #  zetaVec.sd[k] ~ dgamma(1, 1)
    #}

    #for(k in 1:n.Xpa) {
    #  thetaVec.mu[k] ~ dnorm(0, 1)
    #  thetaVec.sd[k] ~ dgamma(1, 1)
    #}
    
    #for(k in 1:n.Xalpha) {
    #  alphaVec.mu[k] ~ dnorm(0, 1)
    #  alphaVec.sd[k] ~ dgamma(1, 1)
    #}
    
    #for(k in 1:n.Xbeta) {
    #  betaVec.mu[k] ~ dnorm(0, 1)
    #  betaVec.sd[k] ~ dgamma(1, 1)
    #}

    for(s in 1:nspp) { ## Start species loop ##
    
      ##~~~~~~~~~~~ Species-specific parameters ~~~~~~~~~~~~##
      zeta0[s] ~ dnorm(zeta0.mu, pow(zeta0.sd, -2))
      b[s] ~ T(dnorm(b.mu, pow(b.sd, -2)), 0, 20)
      #for(k in 1:n.Xpp) {
      #  zetaVec[s, k] ~ dnorm(zetaVec.mu[k], pow(zetaVec.sd[k], -2))
      #}
      theta0[s] ~ dnorm(theta0.mu, pow(theta0.sd, -2))
      #for(k in 1:n.Xpa) {
      #  thetaVec[s, k] ~ dnorm(thetaVec.mu[k], pow(thetaVec.sd[k], -2))
      #}

      alpha0[s] ~ dnorm(alpha0.mu, pow(alpha0.sd, -2))
      #for(k in 1:n.Xalpha) {
      #  alphaVec[s, k] ~ dnorm(alphaVec.mu[k], pow(alphaVec.sd[k], -2))
      #}
      beta0[s] ~ dnorm(beta0.mu + (rho.ab * beta0.sd / alpha0.sd) * (alpha0[s] - alpha0.mu),
        pow(beta0.sd, -2) / (1 - pow(rho.ab, 2)))
      #for(k in 1:n.Xbeta) {
      #  betaVec[s, k] ~ dnorm(betaVec.mu[k], pow(betaVec.sd[k], -2))
      #}
      for(t in 1:nyear) {
        dev.alpha0[s, t] ~ dnorm(0, pow(alpha0.sd.yr, -2))
        dev.beta0[s, t] ~ dnorm(0, pow(beta0.sd.yr, -2))
      }

      for(j in 1:ngrdyrs) { # Start gridXyear observation loop
        ##~~~~~~~~~~~~~~~ Observation models ~~~~~~~~~~~~~~~~~~~##
        # Distance sampling
        
        log(a[s, j]) <- zeta0[s]# + inprod(zetaVec[s, 1:n.Xpp], X.pp[j, 1:n.Xpp])

        for(d in 1:nD)  {
          pp_d[s, j, d] <- 1 - exp(-pow(((breaks[d] + breaks[d + 1]) / 2) / a[s, j], -b[s]))
          pi_pp[s, j, d] <- pp_d[s, j, d] * area.prop[d]
          pi_pp_c[s, j, d] <- pi_pp[s, j, d] / pp[s, j]
          }
        pp[s, j] <- sum(pi_pp[s, j, 1:nD]) # Probability of detection at all

        # Time-removal probabilities
    
        logit(p_a[s, j]) <- theta0[s]# + inprod(thetaVec[s, 1:n.Xpa], X.pa[j, 1:n.Xpa])
    
        for(k in 1:K)  {
          pi_pa[s, j, k] <- p_a[s, j] * pow(1 - p_a[s, j], (k - 1))
          pi_pa_c[s, j, k] <- pi_pa[s, j, k] / pa[s, j] # Conditional probabilities of availability
          }
        pa[s, j] <- sum(pi_pa[s, j, 1:K]) # Probability of ever available
        
        prob_n[s, j] <- pp[s, j] * pa[s, j] * (effort[j] / 16)
        n[s, j] ~ dbin(prob_n[s, j], N[s, j])
        
        ##~~~~~~~~~~~~~~ State process occupancy model ~~~~~~~~~~~~~~##
        logit(psi[s, j]) <- alpha0[s] + dev.alpha0[s, yearInd[j]]# + inprod(alphaVec[s, 1:n.Xalpha], X.alpha[j, 1:n.Xalpha])
        z[s, j] ~ dbern(psi[s, j]) # Occupancy state
        
        ##~~~~~~~~~~~~~~ State process abundance model ~~~~~~~~~~~~~~##
        log(lambda[s, j]) <- beta0[s] + dev.beta0[s, yearInd[j]]# + inprod(betaVec[s, 1:n.Xbeta], X.beta[j, 1:n.Xbeta])
        N[s, j] ~ dpois(lambda[s, j] * z[s, j]) # Abundance state
        
      } # End gridXyear loop
    } # End species loop
    
    for(i in 1:nDet) { ## Start loop through detections (i.e., occassions when the species was detected) ##
    
      ##~~~~~~~~~~~~~~~ Multinomial distance and removal processes ~~~~~~~~~~~~~~~~~~~##
      dclass[i, 1:nD] ~ dmulti(pi_pp_c[spp.ind[i], det.ind[i], 1:nD], n[spp.ind[i], det.ind[i]])
      tint[i, 1:K] ~ dmulti(pi_pa_c[spp.ind[i], det.ind[i], 1:K], n[spp.ind[i], det.ind[i]])
    }
  })