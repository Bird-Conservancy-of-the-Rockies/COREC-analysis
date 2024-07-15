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
parameters.bird <- c("zeta0.mu", "zeta0.sd", "zeta0", "b.mu", "b.sd", "b",
                "zetaVec.mu", "zetaVec.sd", "zetaVec",
                
                "theta0.mu", "theta0.sd", "theta0",
                "thetaVec.mu", "thetaVec.sd", "thetaVec",
                
                "beta0.mu", "beta0.sd", "beta0",
                "betaVec.mu", "betaVec.sd", "betaVec",
                "dev.beta0", "beta0.sd.yr")

parameters.path <- c("BETA0.HumanPresence", "BETA.TrailTotm.HumanPresence",
                     "BETA.Prp_MotRestricted.HumanPresence", "BETA.Prp_HorseRestricted.HumanPresence",
                     "BETA.RoadTotm.HumanPresence",
                     
                     "BETA0.Traffic", "BETA.TrailTotm.Traffic", "BETA.RoadTotm.Traffic",
                     "BETA.Prp_MotRestricted.Traffic", "BETA.Prp_HorseRestricted.Traffic",
                     "shape.Traffic",
                     
                     "BETA0.TOD_mean", "BETA.TrailTotm.TOD_mean", "BETA.RoadTotm.TOD_mean",
                     "BETA.Prp_MotRestricted.TOD_mean", "BETA.Prp_HorseRestricted.TOD_mean",
                     "shape.TOD_mean",
                     
                     "BETA0.Traffic_DOY_mn", "BETA.TrailTotm.Traffic_DOY_mn",
                     "BETA.RoadTotm.Traffic_DOY_mn", "BETA.Prp_MotRestricted.Traffic_DOY_mn",
                     "BETA.Prp_HorseRestricted.Traffic_DOY_mn", "shape.Traffic_DOY_mn",
                     
                     "BETA0.Speed", "BETA.TrailTotm.Speed", "BETA.RoadTotm.Speed",
                     "BETA.Prp_MotRestricted.Speed", "BETA.Prp_HorseRestricted.Speed",
                     "shape.Speed",
                     
                     "test.HumanPresence", "test.Traffic", "test.TOD_mean",
                     "test.Traffic_DOY_mn", "test.Speed",
                     
                     "X.sim.Traffic_DOY_mn", "BETA.latent.Traffic_DOY_mn",
                     "X1.latent.prob", "X2.latent.prob")

if(mod.nam == "community") {
  parameters <- parameters.bird
} else if(mod.nam == "interm_paths") {
  parameters <- parameters.path
} else {
  parameters <- c(parameters.bird, parameters.path) %>% unique()
}
