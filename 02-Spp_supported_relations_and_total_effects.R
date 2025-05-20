library(stringr)
library(tidyr)
library(dplyr)
library(mcmcOutput)
library(FunctionsBCR)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load("data/Data_compiled.RData")

#_____ Script inputs _____#
git.repo <- "COREC-analysis/"
mod.nam <- "path"
mod <- R.utils::loadObject(str_c("mod_", mod.nam))
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
source(str_c(git.repo, "Cluster_sizes.R"))
#________________________#

nsamp <- dim(mod$mcmcOutput)[1]

source(str_c(git.repo, "Functions_source.R"))

covs <- dimnames(X.beta)[[2]]

## List species with any supported covariate relationships ##
spp.relations <- c()
for(cov in covs) {
  beta.ind <- which(dimnames(X.beta)[[2]] == cov)
  beta <- mod$mcmcOutput$betaVec[,,beta.ind]
  dat <- tabulate_community_effect(Spp, beta, BCIpercent = 80)
  spp.relations <- c(spp.relations, dat$Spp[which(dat$beta.supp != "none")]) %>% unique
}

## List species with supported total management effects ##
nsims <- nsamp
source(str_c(git.repo, "Path_analysis_source.R"))

# Tabulate total effects #
tab.effect.total <- data.frame(Spp = Spp)

## Tabulate values to plot ##
# Trail density #
effect <- log(N.pred[,,"HighTrail_avgOHV"] / N.pred[,,"Baseline"])
tab.effect.total <- tab.effect.total %>%
  mutate(HighTrail.md = apply(effect, 2, median),
         HighTrail.lo = apply(effect, 2, function(x) quantile(x, prob = 0.1, type = 8)),
         HighTrail.hi = apply(effect, 2, function(x) quantile(x, prob = 0.9, type = 8))) %>%
  mutate(HighTrail.supp = ifelse(HighTrail.hi < 0, "neg", ifelse(HighTrail.lo > 0, "pos", "none")))

# OHV #
effect <- log(N.pred[,,"HighTrail_noOHV"] / N.pred[,,"HighTrail_maxOHV"])
tab.effect.total <- tab.effect.total %>%
  mutate(NoOHV.md = apply(effect, 2, median),
         NoOHV.lo = apply(effect, 2, function(x) quantile(x, prob = 0.1, type = 8)),
         NoOHV.hi = apply(effect, 2, function(x) quantile(x, prob = 0.9, type = 8))) %>%
  mutate(NoOHV.supp = ifelse(NoOHV.hi < 0, "neg", ifelse(NoOHV.lo > 0, "pos", "none")))

# Roads #
effect <- log(N.pred[,,"HighRoad"] / N.pred[,,"Baseline"])
tab.effect.total <- tab.effect.total %>%
  mutate(HighRoad.md = apply(effect, 2, median),
         HighRoad.lo = apply(effect, 2, function(x) quantile(x, prob = 0.1, type = 8)),
         HighRoad.hi = apply(effect, 2, function(x) quantile(x, prob = 0.9, type = 8))) %>%
  mutate(HighRoad.supp = ifelse(HighRoad.hi < 0, "neg", ifelse(HighRoad.lo > 0, "pos", "none")))
rm(effect)

spp.relations <- spp.relations %>%
  c(tab.effect.total %>% filter(HighTrail.supp != "none" | NoOHV.supp != "none" |
                                               HighRoad.supp != "none") %>%
  pull(Spp)) %>%
  unique %>% sort

tab.effect.total <- tab.effect.total %>%
  filter(Spp %in% spp.relations)

write.csv(tab.effect.total, "data/Spp_total_management_effects.csv", row.names = FALSE)
R.utils::saveObject(spp.relations, "data/Spp_results")
