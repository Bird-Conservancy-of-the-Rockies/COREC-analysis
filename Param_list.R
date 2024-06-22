# List covariates #
Mangmt.vars <- c("TrailTotm", "RoadTotm", "Prp_MotRestricted", "Prp_HorseRestricted")
Mangmt.vars.quad <- c(F, F, F, F)
Human.vars <- c("HumanPresence", "LogTrafficNoZeros", "Traffic_DOY_mn", "Speed", "TOD_mean")
Human.vars.quad <- c(F, F, T, F, T)
Hab.vars <- c("Shrubland", "PinyonJuniper", "ConiferForest", "Aspen", "OakWoodland",
              "GrasslandMeadow", "Mesic", "Alpine")
Hab.vars.quad <- c(F, F, F, F, F,
                   F, F, F)

beta.vars <- c(Mangmt.vars, Human.vars, Hab.vars)

pa.vars <- c("LogTraffic", "Survey_DOY", "Survey_tssr")
pa.vars.quad <- c(F, F, T)
pp.vars <- c("LogTraffic")

# List parameters to save
parameters <- c("zeta0.mu", "zeta0.sd", "zeta0", "b.mu", "b.sd", "b",
                "zetaVec.mu", "zetaVec.sd", "zetaVec",
                
                "theta0.mu", "theta0.sd", "theta0",
                "thetaVec.mu", "thetaVec.sd", "thetaVec",
                
                "beta0.mu", "beta0.sd", "beta0",
                "betaVec.mu", "betaVec.sd", "betaVec")#,
                #"dev.beta0", "beta0.sd.yr")