ind.spp.keep <- 1:10
spp.ind.keep <- which(constants$spp.ind %in% ind.spp.keep)
data$n <- data$n[ind.spp.keep,]
constants$nspp <- length(ind.spp.keep)
constants$spp.ind <- constants$spp.ind[spp.ind.keep]
constants$nDet <- length(spp.ind.keep)
data$dclass <- data$dclass[spp.ind.keep,]
data$tint <- data$tint[spp.ind.keep,]
constants$det.ind <- constants$det.ind[spp.ind.keep]

inits$z <- inits$z[ind.spp.keep,]
inits$N <- inits$N[ind.spp.keep,]
inits$alpha0 <- inits$alpha0[ind.spp.keep]
inits$dev.alpha0 <- inits$dev.alpha0[ind.spp.keep,]
inits$beta0 <- inits$beta0[ind.spp.keep]
inits$dev.beta0 <- inits$dev.beta0[ind.spp.keep,]
inits$theta0 <- inits$theta0[ind.spp.keep]
inits$zeta0 <- inits$zeta0[ind.spp.keep]
inits$b <- inits$b[ind.spp.keep]
inits$alphaVec <- inits$alphaVec[ind.spp.keep,]
inits$betaVec <- inits$betaVec[ind.spp.keep,]
inits$zetaVec <- as.matrix(inits$zetaVec[ind.spp.keep,])
inits$thetaVec <- inits$thetaVec[ind.spp.keep,]

model <- nimbleModel(code = model, name = "model", constants = constants,
                     data = data, inits = inits)
Cmodel <- compileNimble(model)
modelConf <- configureMCMC(model, thin = nt)
modelConf$setMonitors(parameters)
modelMCMC <- buildMCMC(modelConf)
CmodelMCMC <- compileNimble(modelMCMC, project = model)
CmodelMCMC$run(200, reset = FALSE)
