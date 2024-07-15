sd.init <- 0.5

beta0.init <- apply(n, 1, function(x) mean(x[which(x > 0)])) %>% log
beta0.init[which(is.na(beta0.init))] <- -5
beta0.mu.init <- mean(beta0.init)
beta0.sd.init <- sd(beta0.init)

inits <- list(N = N.init,
              
              beta0.mu = beta0.mu.init, beta0.sd = 1.3, beta0 = beta0.init, #rho.ab = 0.5,
              betaVec.mu = rep(0, n.Xbeta), betaVec.sd = rep(sd.init, n.Xbeta), betaVec = matrix(0, nspp, n.Xbeta),
              dev.beta0 = matrix(0, nspp, nyear), beta0.sd.yr = 0.2,
              
              theta0.mu = 0, theta0.sd = sd.init, theta0 = rep(0, nspp),
              thetaVec.mu = rep(0, n.Xpa), thetaVec.sd = rep(sd.init, n.Xpa), thetaVec = matrix(0, nspp, n.Xpa),
              
              zeta0.mu = 4.5, zeta0.sd = sd.init, zeta0 = rep(4.5, nspp),
              b.mu = 3.16, b.sd = 0.005, b = rep(3.16, nspp),
              zetaVec.mu = rep(0, n.Xpp), zetaVec.sd = rep(sd.init, n.Xpp), zetaVec = matrix(0, nspp, n.Xpp))

inits.path <- list(BETA0.HumanPresence = 0, BETA.TrailTotm.HumanPresence = 0,
                   BETA.Prp_MotRestricted.HumanPresence = 0, BETA.Prp_HorseRestricted.HumanPresence = 0,
                   BETA.RoadTotm.HumanPresence = 0,
                   
                   BETA0.Traffic = 0, BETA.TrailTotm.Traffic = 0,
                   BETA.RoadTotm.Traffic = 0, BETA.Prp_MotRestricted.Traffic = 0,
                   BETA.Prp_HorseRestricted.Traffic = 0, shape.Traffic =1,
                   
                   BETA0.TOD_mean = 0, BETA.TrailTotm.TOD_mean = 0, 
                   BETA.RoadTotm.TOD_mean = 0, BETA.Prp_MotRestricted.TOD_mean = 0,
                   BETA.Prp_HorseRestricted.TOD_mean = 0, shape.TOD_mean = 1,
                   
                   BETA0.Traffic_DOY_mn = 0, BETA.TrailTotm.Traffic_DOY_mn = 0,
                   BETA.RoadTotm.Traffic_DOY_mn = 0, BETA.Prp_MotRestricted.Traffic_DOY_mn = 0,
                   BETA.Prp_HorseRestricted.Traffic_DOY_mn = 0, shape.Traffic_DOY_mn = 1,
                   
                   BETA.latent.Traffic_DOY_mn = 0.1, X.latent.prob = 0.5,
                   X.latent.Traffic_DOY_mn = rep(0, ngrdyrs.DOY_Speed),
                   
                   BETA0.Speed = 0, BETA.TrailTotm.Speed = 0, BETA.RoadTotm.Speed = 0,
                   BETA.Prp_MotRestricted.Speed = 0, BETA.Prp_HorseRestricted.Speed = 0,
                   shape.Speed = 1)

if(mod.nam == "interm_paths") {
  inits <- inits.path
} else if(mod.nam == "path") {
  inits <- c(inits, inits.path)
}

if(any(is.na(unlist(inits)) | unlist(inits) %in% c(Inf, -Inf))) {
  ind.undefined <- which(is.na(unlist(inits)) | unlist(inits) %in% c(Inf, -Inf))
  print(str_c("Warning: Initial values are undefined for ", str_c(names(unlist(inits)), collapse = ", "), "."))
  }