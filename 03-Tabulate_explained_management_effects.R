library(mcmcOutput)
library(stringr)
library(dplyr)
library(R.utils)
library(ggplot2)
library(cowplot)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load("Data_compiled.RData")

#__________ Script inputs _____________#
mod.nam <- "path"
git.repo <- "COREC-analysis/"
mod <- R.utils::loadObject(str_c("mod_", mod.nam))
nsims <- dim(mod$mcmcOutput)[1]
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
#______________________________________#

## Stratum covariate values ##
source(str_c(git.repo, "Path_analysis_source.R"))

tab.effect.total <- tab.effect.explained <- read.csv("Spp_total_management_effects.csv", header = TRUE, stringsAsFactors = FALSE)
spp.mech <- tab.effect.total$Spp

# Explained_effect <- function() {
#   N.pred.explained <- exp(beta0 + apply(beta1 * X.pred.explained[, dimnames(X.beta)[[2]]], 1, sum))
#   effect.exp <- log(N.pred.explained / N.pred[,spp.ind,"Baseline"])
#   p.eff.exp <- sum(effect.exp > 0) / nsims
# }
#for(spp in spp.mech) {
spp.ind <- which(Spp %in% spp.mech)
beta0 <- mod$mcmcOutput$beta0[,spp.ind]
beta1 <- mod$mcmcOutput$betaVec[,spp.ind,]

# Trail density
effect.tot <- log(N.pred[,spp.ind,"HighTrail_norest"] / N.pred[,spp.ind,"Baseline"])
effect.unexpl <- beta1[, , which(dimnames(X.beta)[[2]] == "TrailTotm")]
effect.expl <- effect.tot - effect.unexpl
tab.effect.explained$HighTrail.md <- apply(effect.expl, 2, median)
tab.effect.explained$HighTrail.lo <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.explained$HighTrail.hi <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.explained <- tab.effect.explained %>%
  mutate(HighTrail.supp = ifelse(HighTrail.lo > 0, "pos", ifelse(HighTrail.hi < 0, "neg", "none")))

# OHV Restriction
effect.tot <- log(N.pred[,spp.ind, "HighTrail_noOHV"] / N.pred[,spp.ind,"HighTrail_norest"])
effect.unexpl <- beta1[, , which(dimnames(X.beta)[[2]] == "Prp_MotRestricted")]
effect.expl <- effect.tot - effect.unexpl
tab.effect.explained$NoOHV.md <- apply(effect.expl, 2, median)
tab.effect.explained$NoOHV.lo <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.explained$NoOHV.hi <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.explained <- tab.effect.explained %>%
  mutate(NoOHV.supp = ifelse(NoOHV.lo > 0, "pos", ifelse(NoOHV.hi < 0, "neg", "none")))

# Horse Restriction
effect.tot <- log(N.pred[,spp.ind, "HighTrail_noHorse"] / N.pred[,spp.ind,"HighTrail_norest"])
effect.unexpl <- beta1[, , which(dimnames(X.beta)[[2]] == "Prp_HorseRestricted")]
effect.expl <- effect.tot - effect.unexpl
tab.effect.explained$NoHorse.md <- apply(effect.expl, 2, median)
tab.effect.explained$NoHorse.lo <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.explained$NoHorse.hi <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.explained <- tab.effect.explained %>%
  mutate(NoHorse.supp = ifelse(NoHorse.lo > 0, "pos", ifelse(NoHorse.hi < 0, "neg", "none")))

# Road density
effect.tot <- log(N.pred[,spp.ind,"HighRoad"] / N.pred[,spp.ind,"Baseline"])
effect.unexpl <- beta1[, , which(dimnames(X.beta)[[2]] == "RoadTotm")]
effect.expl <- effect.tot - effect.unexpl
tab.effect.explained$HighRoad.md <- apply(effect.expl, 2, median)
tab.effect.explained$HighRoad.lo <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.explained$HighRoad.hi <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.explained <- tab.effect.explained %>%
  mutate(HighRoad.supp = ifelse(HighRoad.lo > 0, "pos", ifelse(HighRoad.hi < 0, "neg", "none")))

write.csv(tab.effect.explained, "Spp_explained_management_effects.csv", row.names = T)
