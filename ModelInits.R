sd.init <- 0.5

alpha0.init <- apply(n, 1, function(x) sum(x > 0) / length(x)) %>% logit
alpha0.init[which(alpha0.init == -Inf)] <- -10
alpha0.mu.init <- mean(alpha0.init)
alpha0.sd.init <- sd(alpha0.init)

beta0.init <- apply(n, 1, function(x) mean(x[which(x > 0)])) %>% log
beta0.init[which(is.na(beta0.init))] <- -5
beta0.mu.init <- mean(beta0.init)
beta0.sd.init <- sd(beta0.init)

inits <- list(z = z.init, N = N.init,
              
              alpha0.mu = alpha0.mu.init, alpha0.sd = alpha0.sd.init, alpha0 = alpha0.init,
              alphaVec.mu = rep(0, n.Xalpha), alphaVec.sd = rep(sd.init, n.Xalpha), alphaVec = matrix(0, nspp, n.Xalpha),

              beta0.mu = beta0.mu.init, beta0.sd = 1.3, beta0 = beta0.init, rho.ab = 0.5,
              betaVec.mu = rep(0, n.Xbeta), betaVec.sd = rep(sd.init, n.Xbeta), betaVec = matrix(0, nspp, n.Xbeta),
              dev.beta0 = matrix(0, nspp, nyear), beta0.sd.yr = 0.2,
              
              theta0.mu = 0, theta0.sd = sd.init, theta0 = rep(0, nspp),
              thetaVec.mu = rep(0, n.Xpa), thetaVec.sd = rep(sd.init, n.Xpa), thetaVec = matrix(0, nspp, n.Xpa),
              
              zeta0.mu = 4.5, zeta0.sd = sd.init, zeta0 = rep(4.5, nspp),
              b.mu = 3.16, b.sd = 0.005, b = rep(3.16, nspp),
              zetaVec.mu = rep(0, n.Xpp), zetaVec.sd = rep(sd.init, n.Xpp), zetaVec = matrix(0, nspp, n.Xpp))

if(any(is.na(unlist(inits)) | unlist(inits) %in% c(Inf, -Inf))) {
  ind.undefined <- which(is.na(unlist(inits)) | unlist(inits) %in% c(Inf, -Inf))
  print(str_c("Warning: Initial values are undefined for ", str_c(names(unlist(inits)), collapse = ", "), "."))
  }