# List covariates #
Eco.vars <- c("HumanPresence", "LogTrafficNoZeros", "Traffic_DOY_mn", "Speed", "TOD_mean",
              "Shrubland", "PinyonJuniper", "ConiferForest", "Aspen", "OakWoodland",
              "GrasslandMeadow", "Mesic", "Alpine")
Eco.vars.quad <- c(F, F, T, F, T,
                   F, F, F, F, F,
                   F, F, F)

pa.vars <- c("LogTraffic", "Survey_DOY", "Survey_tssr")
pa.vars.quad <- c(F, T, T)
pp.vars <- c("LogTraffic")

# List parameters to save
parameters <- c("zeta0.mu", "zeta0", "b.mu", "b.sd", "b",
                "zetaVec.mu", "zetaVec.sd", "zetaVec",
                "zeta0.sd", "zeta0.sd.yr", "dev.zeta0",
                
                "theta0.mu", "theta0.sd", "theta0",
                "theta0.sd.yr", "dev.theta0",
                "thetaVec.mu", "thetaVec.sd", "thetaVec",
                
                "alpha0.mu", "alpha0.sd", "alpha0",
                "alphaVec.mu", "alphaVec.sd", "alphaVec",
                "dev.alpha0", "alpha0.sd.yr",

                "beta0.mu", "beta0.sd", "beta0", "rho.ab",
                "betaVec.mu", "betaVec.sd", "betaVec",
                "dev.beta0", "beta0.sd.yr")

