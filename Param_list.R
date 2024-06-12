# List covariates #
Eco.vars <- c("HumanPresence", "LogTrafficNoZeros", "Traffic_DOY_mn", "Speed", "TOD_mean",
              "TrailTotm", "RoadTotm", "Prp_MotRestricted", "Prp_HorseRestricted",
              "Shrubland", "PinyonJuniper", "ConiferForest", "Aspen", "OakWoodland",
              "GrasslandMeadow", "Mesic")
Eco.vars.quad <- c(F, F, T, F, T,
                   F, F, F, F,
                   F, F, F, F, F,
                   F, F)

alpha.vars <- c("Shrubland", "PinyonJuniper", "ConiferForest", "Aspen", "OakWoodland",
                "GrasslandMeadow", "Mesic")
beta.vars <- c("HumanPresence", "LogTrafficNoZeros", "Traffic_DOY_mn", "Speed", "TOD_mean",
               "TrailTotm", "RoadTotm", "Prp_MotRestricted", "Prp_HorseRestricted")

pa.vars <- c("LogTraffic", "Survey_DOY", "Survey_tssr")
pa.vars.quad <- c(F, F, T)
pp.vars <- c("LogTraffic")

# List parameters to save
parameters <- c("zeta0.mu", "zeta0.sd", "zeta0", "b.mu", "b.sd", "b",
                "zetaVec.mu", "zetaVec.sd", "zetaVec",
                
                "theta0.mu", "theta0.sd", "theta0",
                "thetaVec.mu", "thetaVec.sd", "thetaVec",
                
                "alpha0.mu", "alpha0.sd", "alpha0",
                "alphaVec.mu", "alphaVec.sd", "alphaVec",

                "beta0.mu", "beta0.sd", "beta0", "rho.ab",
                "betaVec.mu", "betaVec.sd", "betaVec",
                "dev.beta0", "beta0.sd.yr")

