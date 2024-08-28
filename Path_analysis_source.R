## Management scenarios (values on real scale) ##
# Baseline: Trail density = 0, Road density = 0, OHV = mean, Horse = mean
# High trail density, no restrictions: Trail density = 95th %-ile, Road density = 0, OHV = 0, Horse = 0
# High trail density, OHVs restricted: Trail density = 95th %-ile, Road density = 0, OHV = 95th %-ile, Horse = 0
# High trail density, horses restricted: Trail density = 95th %-ile, Road density = 0, OHV = 0, Horse = 95th %-ile
# High road density: Trail density = 0, Road density = 95th %-ile, OHV = mean, Horse = mean

## Create tables to store predicted x-values for each management scenario ##
X.pred.baseline <- X.pred.hitrail_avgOHV <- X.pred.hitrail_maxOHV <- X.pred.hitrail_noOHV <-
  X.pred.hiroad <- matrix(NA, nrow = nsims, ncol = ncol(X.beta))
dimnames(X.pred.baseline)[[2]] <- dimnames(X.pred.hitrail_avgOHV)[[2]] <-
  dimnames(X.pred.hitrail_maxOHV)[[2]] <- dimnames(X.pred.hitrail_noOHV)[[2]] <-
  dimnames(X.pred.hiroad)[[2]] <- dimnames(X.beta)[[2]]
X.pred.baseline[, Hab.vars] <- X.pred.hitrail_avgOHV[, Hab.vars] <- X.pred.hitrail_maxOHV[, Hab.vars] <-
  X.pred.hitrail_noOHV[, Hab.vars] <- X.pred.hiroad[, Hab.vars] <- 0

# Get scaling factors for all covariates #
cov.mn <- (covariates %>% select(TrailTotm:Prp_MotRestricted, HumanPresence, LogTrafficNoZeros,
                                 Speed, Shrubland, ConiferForest:Alpine) %>%
             summarise_all(function(x) mean(x, na.rm = TRUE)) %>% data.matrix)[1,]
cov.sd <- (covariates %>% select(TrailTotm:Prp_MotRestricted, HumanPresence, LogTrafficNoZeros,
                                 Speed, Shrubland, ConiferForest:Alpine) %>%
             summarise_all(function(x) sd(x, na.rm = TRUE)) %>% data.matrix)[1,]

# Derive predicted x-values for each scenario #
#__________________ Functions for calculating predicted values _________________#
CalcPredValues <- function(X.pred, X.mangmt) {
  X.pred[, "TrailTotm"] <- X.mangmt["TrailTotm"]
  X.pred[, "RoadTotm"] <- X.mangmt["RoadTotm"]
  X.pred[, "Prp_MotRestricted"] <- X.mangmt["Prp_MotRestricted"]
  HP.raw <- (FunctionsBCR::expit(mod$mcmcOutput$BETA0.HumanPresence +
                                   mod$mcmcOutput$BETA.TrailTotm.HumanPresence * X.pred[, "TrailTotm"] +
                                   mod$mcmcOutput$BETA.RoadTotm.HumanPresence * X.pred[, "RoadTotm"] +
                                   mod$mcmcOutput$BETA.Prp_MotRestricted.HumanPresence * X.pred[, "Prp_MotRestricted"]))
  X.pred[, "HumanPresence"] <- HP.raw %>%
    (function(x) (x - cov.mn["HumanPresence"]) / cov.sd["HumanPresence"])
  X.pred[, "LogTrafficNoZeros"] <-
    (mod$mcmcOutput$BETA0.Traffic +
       mod$mcmcOutput$BETA.TrailTotm.Traffic * X.pred[, "TrailTotm"] +
       mod$mcmcOutput$BETA.RoadTotm.Traffic * X.pred[, "RoadTotm"] +
       mod$mcmcOutput$BETA.Prp_MotRestricted.Traffic * X.pred[, "Prp_MotRestricted"]) %>%
    (function(x) (x - cov.mn["LogTrafficNoZeros"]) / cov.sd["LogTrafficNoZeros"]) * HP.raw
  X.pred[, "Speed"] <-
    exp(mod$mcmcOutput$BETA0.Speed +
          mod$mcmcOutput$BETA.Prp_MotRestricted.Speed * X.pred[, "Prp_MotRestricted"]) %>%
    (function(x) (x - cov.mn["Speed"]) / cov.sd["Speed"]) * HP.raw
  X.pred[, "Speed2"] <- X.pred[, "Speed"] ^ 2
  return(X.pred)
}

N.pred.calc <- function(X.pred, spp) {
  spp.ind <- which(Spp %in% spp)
  N.pred <- matrix(NA, nrow = nsims, ncol = length(spp.ind),
                   dimnames = list(NULL, Spp[spp.ind]))
  for(i in 1:length(spp.ind)) {
    beta0 <- mod$mcmcOutput$beta0[,spp.ind[i]]
    beta1 <- mod$mcmcOutput$betaVec[,spp.ind[i],]
    N.pred[,i] <- exp(beta0 + apply(beta1 * X.pred, 1, sum))
  }
  N.pred <- N.pred[,spp]
  return(N.pred)
}
#________________________________________________________________________________#

# Baseline:
X.mangmt <- c((0 - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"],
              (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
              Prp_MotRestricted = 0)
X.pred.baseline <- CalcPredValues(X.pred.baseline, X.mangmt)

# High trail density with average OHV restriction:
X.mangmt <- c(TrailTotm = as.numeric((cov.sd["TrailTotm"] - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"]),
              (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
              Prp_MotRestricted = 0)
X.pred.hitrail_avgOHV <- CalcPredValues(X.pred.hitrail_avgOHV, X.mangmt)

# High trail density with no OHV restriction:
X.mangmt <- c(TrailTotm = as.numeric((cov.sd["TrailTotm"] - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"]),
              (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
              (0 - cov.mn["Prp_MotRestricted"]) / cov.sd["Prp_MotRestricted"])
X.pred.hitrail_maxOHV <- CalcPredValues(X.pred.hitrail_maxOHV, X.mangmt)

# High trail density with OHVs restricted:
X.mangmt <- c(TrailTotm = as.numeric((cov.sd["TrailTotm"] - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"]),
              (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
              (1 - cov.mn["Prp_MotRestricted"]) / cov.sd["Prp_MotRestricted"])
X.pred.hitrail_noOHV <- CalcPredValues(X.pred.hitrail_noOHV, X.mangmt)

# High road density:
X.mangmt <- c((0 - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"],
              RoadTotm = as.numeric((cov.sd["RoadTotm"] - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"]),
              Prp_MotRestricted = 0)
X.pred.hiroad <- CalcPredValues(X.pred.hiroad, X.mangmt)

## Derive scenario abundance estimates ##
nsim <- dim(mod$mcmcOutput)[1]
Scenarios <- c("Baseline", "HighTrail_avgOHV", "HighTrail_maxOHV", "HighTrail_noOHV", "HighRoad")
X.pred <- list(X.pred.baseline, X.pred.hitrail_avgOHV, X.pred.hitrail_maxOHV, X.pred.hitrail_noOHV,
               X.pred.hiroad)
names(X.pred) <- Scenarios
N.pred <- array(NA, dim = c(nsim, length(Spp), length(Scenarios)))
dimnames(N.pred) <- list(NULL, Spp, Scenarios)
for(scn in Scenarios) {
  N.pred[,,scn] <- N.pred.calc(X.pred[[scn]], Spp)
}

for(i in 1:length(Spp)) {
  beta0 <- mod$mcmcOutput$beta0[,i]
  beta1 <- mod$mcmcOutput$betaVec[,i,]
  
}
rm(i, X.pred.baseline, X.pred.hitrail_avgOHV, X.pred.hitrail_maxOHV, X.pred.hitrail_noOHV, X.pred.hiroad)

## Series for plotting full relationships ##
if(!exists("series")) series <- FALSE
if(series) {
  n.series <- 10
  
  # Trail density series #
  x.trail = seq(quantile(X.beta[, "TrailTotm"], probs = 0.01, type = 8),
                quantile(X.beta[, "TrailTotm"], probs = 0.99, type = 8),
                length.out = n.series)
  X.trail.series <- array(NA, c(nsims, ncol(X.beta), n.series, 2))
  dimnames(X.trail.series)[[2]] <- dimnames(X.beta)[[2]]
  N.trail.series <- array(NA, c(nsims, length(Spp), n.series, 2))
  dimnames(X.trail.series)[[4]] <- dimnames(N.trail.series)[[4]] <- c("total", "unexplained")
  dimnames(N.trail.series)[[2]] <- Spp
  X.trail.series[,Hab.vars,,] <- 0
  for(i in 1:n.series) {
    X.mangmt <- c(TrailTotm = x.trail[i],
                  (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
                  Prp_MotRestricted = 0)
    X.trail.series[,,i,"total"] <- X.trail.series[,,i,"unexplained"] <-
      CalcPredValues(X.trail.series[,,i,"total"], X.mangmt)
    N.trail.series[,,i,"total"] <- N.pred.calc(X.trail.series[,,i,"total"], Spp)
    X.trail.series[,c(Human.vars, "Speed2"),i,"unexplained"] <- 0
    N.trail.series[,,i,"unexplained"] <- N.pred.calc(X.trail.series[,,i,"unexplained"], Spp)
  }

  # Road density series #
  x.road = seq(quantile(X.beta[, "RoadTotm"], probs = 0.01, type = 8),
                quantile(X.beta[, "RoadTotm"], probs = 0.99, type = 8),
                length.out = n.series)
  X.road.series <- array(NA, c(nsims, ncol(X.beta), n.series, 2))
  dimnames(X.road.series)[[2]] <- dimnames(X.beta)[[2]]
  N.road.series <- array(NA, c(nsims, length(Spp), n.series, 2))
  dimnames(X.road.series)[[4]] <- dimnames(N.road.series)[[4]] <- c("total", "unexplained")
  dimnames(N.road.series)[[2]] <- Spp
  X.road.series[,Hab.vars,,] <- 0
  for(i in 1:n.series) {
    X.mangmt <- c((0 - cov.mn["TrailTotm"]) / cov.sd["TrailTotm"],
                  RoadTotm = x.road[i],
                  Prp_MotRestricted = 0)
    X.road.series[,,i,"total"] <- X.road.series[,,i,"unexplained"] <-
      CalcPredValues(X.road.series[,,i,"total"], X.mangmt)
    N.road.series[,,i,"total"] <- N.pred.calc(X.road.series[,,i,"total"], Spp)
    X.road.series[,c(Human.vars, "Speed2"),i,"unexplained"] <- 0
    N.road.series[,,i,"unexplained"] <- N.pred.calc(X.road.series[,,i,"unexplained"], Spp)
  }

  # OHV restriction series #
  x.OHV = seq(min(X.beta[, "Prp_MotRestricted"]), max(X.beta[, "Prp_MotRestricted"]),
               length.out = n.series)
  X.OHV.series <- array(NA, c(nsims, ncol(X.beta), n.series, 2))
  dimnames(X.OHV.series)[[2]] <- dimnames(X.beta)[[2]]
  N.OHV.series <- array(NA, c(nsims, length(Spp), n.series, 2))
  dimnames(X.OHV.series)[[4]] <- dimnames(N.OHV.series)[[4]] <- c("total", "unexplained")
  dimnames(N.OHV.series)[[2]] <- Spp
  X.OHV.series[,Hab.vars,,] <- 0
  for(i in 1:n.series) {
    X.mangmt <- c(TrailTotm = 0,
                  (0 - cov.mn["RoadTotm"]) / cov.sd["RoadTotm"],
                  Prp_MotRestricted = x.OHV[i])
    X.OHV.series[,,i,"total"] <- X.OHV.series[,,i,"unexplained"] <-
      CalcPredValues(X.OHV.series[,,i,"total"], X.mangmt)
    N.OHV.series[,,i,"total"] <- N.pred.calc(X.OHV.series[,,i,"total"], Spp)
    X.OHV.series[,c(Human.vars, "Speed2"),i,"unexplained"] <- 0
    N.OHV.series[,,i,"unexplained"] <- N.pred.calc(X.OHV.series[,,i,"unexplained"], Spp)
  }
}

rm(CalcPredValues, X.mangmt)
