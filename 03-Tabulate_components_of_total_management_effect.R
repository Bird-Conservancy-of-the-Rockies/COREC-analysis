library(mcmcOutput)
library(stringr)
library(dplyr)
library(R.utils)
library(ggplot2)
library(cowplot)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load("data/Data_compiled.RData")

#__________ Script inputs _____________#
mod.nam <- "path"
git.repo <- "COREC-analysis/"
mod <- R.utils::loadObject(str_c("mod_", mod.nam))
nsims <- dim(mod$mcmcOutput)[1]
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
source(str_c(git.repo, "Functions_source.R"))
source(str_c(git.repo, "Cluster_sizes.R"))
#______________________________________#

## Stratum covariate values ##
source(str_c(git.repo, "Path_analysis_source.R"))

tab.effect.total <- tab.effect.explained <- tab.effect.unexplained <-
  read.csv("data/Spp_total_management_effects.csv", header = TRUE, stringsAsFactors = FALSE)
spp.mech <- tab.effect.total$Spp

# Trail density
effect.tot <- log(N.pred[, spp.mech, "HighTrail_avgOHV"] / N.pred[, spp.mech, "Baseline"])
X.pred.alt <- X.pred[["HighTrail_avgOHV"]]
X.pred.alt[, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")] <-
  X.pred[["Baseline"]][, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")]
N.pred.alt <- N.pred.calc(X.pred.alt, spp.mech)
effect.unexpl <- log(N.pred.alt / N.pred[, spp.mech, "Baseline"])
tab.effect.unexplained$HighTrail.md <- apply(effect.unexpl, 2, median)
tab.effect.unexplained$HighTrail.lo <- apply(effect.unexpl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.unexplained$HighTrail.hi <- apply(effect.unexpl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.unexplained <- tab.effect.unexplained %>%
  mutate(HighTrail.supp = ifelse(HighTrail.lo > 0, "pos", ifelse(HighTrail.hi < 0, "neg", "none")))
effect.expl <- effect.tot - effect.unexpl
tab.effect.explained$HighTrail.md <- apply(effect.expl, 2, median)
tab.effect.explained$HighTrail.lo <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.explained$HighTrail.hi <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.explained <- tab.effect.explained %>%
  mutate(HighTrail.supp = ifelse(HighTrail.lo > 0, "pos", ifelse(HighTrail.hi < 0, "neg", "none")))

# OHV Restriction
effect.tot <- log(N.pred[, spp.mech, "HighTrail_noOHV"] / N.pred[, spp.mech, "HighTrail_maxOHV"])
X.pred.alt <- X.pred[["HighTrail_noOHV"]]
X.pred.alt[, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")] <-
  X.pred[["HighTrail_maxOHV"]][, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")]
N.pred.alt <- N.pred.calc(X.pred.alt, spp.mech)
effect.unexpl <- log(N.pred.alt / N.pred[, spp.mech, "HighTrail_maxOHV"])
tab.effect.unexplained$NoOHV.md <- apply(effect.unexpl, 2, median)
tab.effect.unexplained$NoOHV.lo <- apply(effect.unexpl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.unexplained$NoOHV.hi <- apply(effect.unexpl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.unexplained <- tab.effect.unexplained %>%
  mutate(NoOHV.supp = ifelse(NoOHV.lo > 0, "pos", ifelse(NoOHV.hi < 0, "neg", "none")))
effect.expl <- effect.tot - effect.unexpl
tab.effect.explained$NoOHV.md <- apply(effect.expl, 2, median)
tab.effect.explained$NoOHV.lo <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.explained$NoOHV.hi <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.explained <- tab.effect.explained %>%
  mutate(NoOHV.supp = ifelse(NoOHV.lo > 0, "pos", ifelse(NoOHV.hi < 0, "neg", "none")))

# Road density
effect.tot <- log(N.pred[, spp.mech, "HighRoad"] / N.pred[, spp.mech, "Baseline"])
X.pred.alt <- X.pred[["HighRoad"]]
X.pred.alt[, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")] <-
  X.pred[["Baseline"]][, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")]
N.pred.alt <- N.pred.calc(X.pred.alt, spp.mech)
effect.unexpl <- log(N.pred.alt / N.pred[, spp.mech, "Baseline"])
tab.effect.unexplained$HighRoad.md <- apply(effect.unexpl, 2, median)
tab.effect.unexplained$HighRoad.lo <- apply(effect.unexpl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.unexplained$HighRoad.hi <- apply(effect.unexpl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.unexplained <- tab.effect.unexplained %>%
  mutate(HighRoad.supp = ifelse(HighRoad.lo > 0, "pos", ifelse(HighRoad.hi < 0, "neg", "none")))
effect.expl <- effect.tot - effect.unexpl
tab.effect.explained$HighRoad.md <- apply(effect.expl, 2, median)
tab.effect.explained$HighRoad.lo <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.1, type = 8))
tab.effect.explained$HighRoad.hi <- apply(effect.expl, 2, function(x) quantile(x, prob = 0.9, type = 8))
tab.effect.explained <- tab.effect.explained %>%
  mutate(HighRoad.supp = ifelse(HighRoad.lo > 0, "pos", ifelse(HighRoad.hi < 0, "neg", "none")))

write.csv(tab.effect.explained, "data/Spp_explained_management_effects.csv", row.names = T)
write.csv(tab.effect.unexplained, "data/Spp_unexplained_management_effects.csv", row.names = T)
