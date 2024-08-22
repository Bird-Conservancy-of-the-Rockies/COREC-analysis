## Management scenarios (values on real scale) ##
# Baseline: Trail density = 0, Road density = 0, OHV = mean, Horse = mean
# High trail density, no restrictions: Trail density = 95th %-ile, Road density = 0, OHV = 0, Horse = 0
# High trail density, OHVs restricted: Trail density = 95th %-ile, Road density = 0, OHV = 95th %-ile, Horse = 0
# High trail density, horses restricted: Trail density = 95th %-ile, Road density = 0, OHV = 0, Horse = 95th %-ile
# High road density: Trail density = 0, Road density = 95th %-ile, OHV = mean, Horse = mean

## Create tables to store predicted x-values for each management scenario ##
X.pred.baseline <- X.pred.hitrail_norest <- X.pred.hitrail_noOHV <-
  X.pred.hitrail_noHorse <- X.pred.hiroad <-
  matrix(NA, nrow = nsims, ncol = ncol(X.beta))
dimnames(X.pred.baseline)[[2]] <-
  dimnames(X.pred.hitrail_norest)[[2]] <- dimnames(X.pred.hitrail_noOHV)[[2]] <-
  dimnames(X.pred.hitrail_noHorse)[[2]] <- dimnames(X.pred.hiroad)[[2]] <-
  dimnames(X.beta)[[2]]
X.pred.baseline[, Hab.vars] <- X.pred.hitrail_norest[, Hab.vars] <-
  X.pred.hitrail_noOHV[, Hab.vars] <- X.pred.hitrail_noHorse[, Hab.vars] <-
  X.pred.hiroad[, Hab.vars] <- 0

# Get scaling factors for all covariates #
cov.mn <- (covariates %>% select(TrailTotm:Prp_HorseRestricted, HumanPresence, LogTrafficNoZeros,
                                 Speed, Shrubland, ConiferForest:Alpine) %>%
             summarise_all(function(x) mean(x, na.rm = TRUE)) %>% data.matrix)[1,]
cov.sd <- (covariates %>% select(TrailTotm:Prp_HorseRestricted, HumanPresence, LogTrafficNoZeros,
                                 Speed, Shrubland, ConiferForest:Alpine) %>%
             summarise_all(function(x) sd(x, na.rm = TRUE)) %>% data.matrix)[1,]

# Derive predicted x-values for each scenario #
# Function for calculating predicted human mobility values given a management scenario:
CalcPredValues <- function(X.pred, X.mangmt) {
  X.pred[, "TrailTotm"] <- X.mangmt["TrailTotm"]
  X.pred[, "RoadTotm"] <- X.mangmt["RoadTotm"]
  X.pred[, "Prp_MotRestricted"] <- X.mangmt["Prp_MotRestricted"]
  X.pred[, "Prp_HorseRestricted"] <- X.mangmt["Prp_HorseRestricted"]
  HP.raw <- (FunctionsBCR::expit(mod$mcmcOutput$BETA0.HumanPresence +
                                   mod$mcmcOutput$BETA.TrailTotm.HumanPresence * X.pred[, "TrailTotm"] +
                                   mod$mcmcOutput$BETA.RoadTotm.HumanPresence * X.pred[, "RoadTotm"] +
                                   mod$mcmcOutput$BETA.Prp_MotRestricted.HumanPresence * X.pred[, "Prp_MotRestricted"] +
                                   mod$mcmcOutput$BETA.Prp_HorseRestricted.HumanPresence * X.pred[, "Prp_HorseRestricted"]))
  X.pred[, "HumanPresence"] <- HP.raw %>%
    (function(x) (x - cov.mn["HumanPresence"]) / cov.sd["HumanPresence"])
  X.pred[, "LogTrafficNoZeros"] <-
    (mod$mcmcOutput$BETA0.Traffic +
       mod$mcmcOutput$BETA.TrailTotm.Traffic * X.pred[, "TrailTotm"] +
       mod$mcmcOutput$BETA.RoadTotm.Traffic * X.pred[, "RoadTotm"] +
       mod$mcmcOutput$BETA.Prp_MotRestricted.Traffic * X.pred[, "Prp_MotRestricted"] +
       mod$mcmcOutput$BETA.Prp_HorseRestricted.Traffic * X.pred[, "Prp_HorseRestricted"]) %>%
    (function(x) (x - cov.mn["LogTrafficNoZeros"]) / cov.sd["LogTrafficNoZeros"]) * HP.raw
  X.pred[, "Speed"] <-
    exp(mod$mcmcOutput$BETA0.Speed +
          mod$mcmcOutput$BETA.Prp_MotRestricted.Speed * X.pred[, "Prp_MotRestricted"]) %>%
    (function(x) (x - cov.mn["Speed"]) / cov.sd["Speed"]) * HP.raw
  X.pred[, "Speed2"] <- X.pred[, "Speed"] ^ 2
  return(X.pred)
}

# Baseline:
X.mangmt <- c((0 - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"],
              (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
              Prp_MotRestricted = 0, Prp_HorseRestricted = 0)
X.pred.baseline <- CalcPredValues(X.pred.baseline, X.mangmt)

# High trail density with no restrictions:
X.mangmt <- c(TrailTotm = as.numeric((cov.sd["TrailTotm"] - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"]),
              (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
              (0 - cov.mn["Prp_MotRestricted"]) / cov.sd["Prp_MotRestricted"],
              (0 - cov.mn["Prp_HorseRestricted"]) / cov.sd["Prp_HorseRestricted"])
X.pred.hitrail_norest <- CalcPredValues(X.pred.hitrail_norest, X.mangmt)

# High trail density with OHVs restricted:
X.mangmt <- c(TrailTotm = as.numeric((cov.sd["TrailTotm"] - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"]),
              (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
              (cov.sd["Prp_MotRestricted"] - cov.mn["Prp_MotRestricted"]) / cov.sd["Prp_MotRestricted"],
              (0 - cov.mn["Prp_HorseRestricted"]) / cov.sd["Prp_HorseRestricted"])
X.pred.hitrail_noOHV <- CalcPredValues(X.pred.hitrail_noOHV, X.mangmt)

# High trail density with horses restricted:
X.mangmt <- c(TrailTotm = as.numeric((cov.sd["TrailTotm"] - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"]),
              (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
              (0 - cov.mn["Prp_MotRestricted"]) / cov.sd["Prp_MotRestricted"],
              (cov.sd["Prp_HorseRestricted"] - cov.mn["Prp_HorseRestricted"]) / cov.sd["Prp_HorseRestricted"])
X.pred.hitrail_noHorse <- CalcPredValues(X.pred.hitrail_noHorse, X.mangmt)

# High road density:
X.mangmt <- c((0 - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"],
              RoadTotm = as.numeric((cov.sd["RoadTotm"] - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"]),
              Prp_MotRestricted = 0, Prp_HorseRestricted = 0)
X.pred.hiroad <- CalcPredValues(X.pred.hiroad, X.mangmt)

rm(CalcPredValues, X.mangmt)

## Derive scenario abundance estimates ##
nsim <- dim(mod$mcmcOutput)[1]
Scenarios <- c("Baseline", "HighTrail_norest", "HighTrail_noOHV", "HighTrail_noHorse", "HighRoad")
X.pred <- list(X.pred.baseline, X.pred.hitrail_norest, X.pred.hitrail_noOHV, X.pred.hitrail_noHorse,
               X.pred.hiroad)
names(X.pred) <- Scenarios
N.pred <- array(NA, dim = c(nsim, length(Spp), length(Scenarios)))
dimnames(N.pred) <- list(NULL, Spp, Scenarios)

for(i in 1:length(Spp)) {
  beta0 <- mod$mcmcOutput$beta0[,i]
  beta1 <- mod$mcmcOutput$betaVec[,i,]
  
  for(scn in Scenarios) {
    N.pred[, i, scn] <- exp(beta0 + apply(beta1 * X.pred[[scn]][, dimnames(X.beta)[[2]]], 1, sum))
  }
}
rm(i)
