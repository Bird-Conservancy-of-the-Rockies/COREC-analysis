# List covariates #
Mangmt.vars <- c("TrailTotm", "RoadTotm", "Prp_MotRestricted")
Mangmt.vars.quad <- c(F, F, F)
Human.vars <- c("HumanPresence", "LogTrafficNoZeros", "Speed")
Human.vars.quad <- c(F, F, T)
Hab.vars <- c("Shrubland", "ConiferForest", "Aspen", "OakWoodland", # "PinyonJuniper" dropped to reduce multicollinearity and improve convergence
              "GrasslandMeadow", "Mesic", "Alpine")
Hab.vars.quad <- c(F, F, F, F,# F,
                   F, F, F)

beta.vars <- c(Mangmt.vars, Human.vars, Hab.vars)

pa.vars <- c("LogTraffic", "Survey_DOY", "Survey_tssr")
pa.vars.quad <- c(F, F, T)
pp.vars <- c("LogTraffic")

# List parameters to save
parameters.bird <- c("zeta0.mu", "zeta0.sd", "zeta0", "b.mu", "b.sd", "b",
                "zetaVec.mu", "zetaVec.sd", "zetaVec",
                
                "theta0.mu", "theta0.sd", "theta0",
                "thetaVec.mu", "thetaVec.sd", "thetaVec",
                
                "beta0.mu", "beta0.sd", "beta0",
                "betaVec.mu", "betaVec.sd", "betaVec",
                "dev.beta0", "beta0.sd.yr")

parameters.path <- c("BETA0.HumanPresence", "BETA.TrailTotm.HumanPresence",
                     "BETA.Prp_MotRestricted.HumanPresence", "BETA.RoadTotm.HumanPresence",
                     
                     "BETA0.Traffic", "BETA.TrailTotm.Traffic", "BETA.RoadTotm.Traffic",
                     "BETA.Prp_MotRestricted.Traffic", "shape.Traffic",
                     
                     "BETA0.Speed", "BETA.Prp_MotRestricted.Speed",
                     "shape.Speed",

                     "test.HumanPresence", "test.Traffic", "test.Speed")

if(mod.nam == "community") {
  parameters <- parameters.bird
} else if(mod.nam == "interm_paths") {
  parameters <- parameters.path
} else {
  parameters <- c(parameters.bird, parameters.path) %>% unique()
}
