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

######################################
# Tabulate mechanistic contributions #
######################################

tab.effect.total <- read.csv("Spp_total_management_effects.csv", header = TRUE, stringsAsFactors = FALSE)
spp.mech <- tab.effect.total$Spp
cols <- c("Trail.total", "Trail.p",
          "HumTraff_Trail_contrib", "HumTraff_Trail_p",# "HumTraff_Trail_hypSupp",
          "OHV.total", "OHV.p",
          "HumTraff_OHV_contrib", "HumTraff_OHV_p",# "HumTraff_OHV_hypSupp",
          "Speed_OHV_contrib", "Speed_OHV_p",# "Speed_OHV_hypSupp",
          "Horse.total", "Horse.p",
          "HumTraff_Horse_contrib", "HumTraff_Horse_p",# "HumTraff_Horse_hypSupp",
          "Road.total", "Road.p",
          "HumTraff_Road_contrib", "HumTraff_Road_p")#, "HumTraff_Road_hypSupp")
out <- matrix("", nrow = length(spp.mech), ncol = length(cols),
              dimnames = list(spp.mech, cols))

# Total effects #
out[, "Trail.total"] <- str_c(round(tab.effect.total$HighTrail.md, digits = 1),
                                 " (", round(tab.effect.total$HighTrail.lo, digits = 1), ", ",
                                 round(tab.effect.total$HighTrail.hi, digits = 1), ")")
out[, "Trail.p"] <- (log(N.pred[,,"HighTrail_norest"] / N.pred[,,"Baseline"]) %>%
  apply(2, function(x) sum(x > 0) / length(x)))[spp.mech] %>% round(digits = 2)
out[, "Road.total"] <- str_c(round(tab.effect.total$HighRoad.md, digits = 1),
                                 " (", round(tab.effect.total$HighRoad.lo, digits = 1), ", ",
                                 round(tab.effect.total$HighRoad.hi, digits = 1), ")")
out[, "Road.p"] <- (log(N.pred[,,"HighRoad"] / N.pred[,,"Baseline"]) %>%
                       apply(2, function(x) sum(x > 0) / length(x)))[spp.mech] %>% round(digits = 2)
out[, "OHV.total"] <- str_c(round(tab.effect.total$NoOHV.md, digits = 1),
                                 " (", round(tab.effect.total$NoOHV.lo, digits = 1), ", ",
                                 round(tab.effect.total$NoOHV.hi, digits = 1), ")")
out[, "OHV.p"] <- (log(N.pred[,,"HighTrail_noOHV"] / N.pred[,,"HighTrail_norest"]) %>%
                       apply(2, function(x) sum(x > 0) / length(x)))[spp.mech] %>% round(digits = 2)
out[, "Horse.total"] <- str_c(round(tab.effect.total$NoHorse.md, digits = 1),
                                 " (", round(tab.effect.total$NoHorse.lo, digits = 1), ", ",
                                 round(tab.effect.total$NoHorse.hi, digits = 1), ")")
out[, "Horse.p"] <- (log(N.pred[,,"HighTrail_noHorse"] / N.pred[,,"HighTrail_norest"]) %>%
                       apply(2, function(x) sum(x > 0) / length(x)))[spp.mech] %>% round(digits = 2)

Contribution.fn <- function(Base_scn) {
  N.pred.alternative <- exp(beta0 + apply(beta1 * X.pred.alternative[, dimnames(X.beta)[[2]]], 1, sum))
  effect.alt <- log(N.pred.alternative / N.pred[,spp.ind, Base_scn])
  p.eff.alt <- sum(effect.alt > 0) / nsims
  contribution <<- contribution <- ((effect.tot - effect.alt) / effect.tot) * 100
  p.cont <<- sum(contribution > 0) / length(contribution)
}
for(spp in spp.mech) {
  spp.ind <- which(Spp == spp)
  beta0 <- mod$mcmcOutput$beta0[,spp.ind]
  beta1 <- mod$mcmcOutput$betaVec[,spp.ind,]
  
  # Human traffic contribution to trail effect
  effect.tot <- log(N.pred[,spp.ind,"HighTrail_norest"] / N.pred[,spp.ind,"Baseline"])
  X.pred.alternative <- X.pred.hitrail_norest
  X.pred.alternative[, c("HumanPresence", "LogTrafficNoZeros")] <-
    X.pred.baseline[, c("HumanPresence", "LogTrafficNoZeros")]
  Contribution.fn(Base_scn = "Baseline")
  out[spp, "HumTraff_Trail_contrib"] <- FunctionsBCR::BCI(contribution, BCIpercent = 80)
  out[spp, "HumTraff_Trail_p"] <- round(p.cont, digits = 2)

  # Contributions to OHV effects
  effect.tot <- log(N.pred[,spp.ind,"HighTrail_noOHV"] / N.pred[,spp.ind,"HighTrail_norest"])
    # Human traffic contribution
  X.pred.alternative <- X.pred.hitrail_noOHV
  X.pred.alternative[, c("HumanPresence", "LogTrafficNoZeros")] <-
    X.pred.hitrail_norest[, c("HumanPresence", "LogTrafficNoZeros")]
  Contribution.fn(Base_scn = "HighTrail_norest")
  out[spp, "HumTraff_OHV_contrib"] <- FunctionsBCR::BCI(contribution, BCIpercent = 80)
  out[spp, "HumTraff_OHV_p"] <- round(p.cont, digits = 2)
    # Speed contribution
  X.pred.alternative <- X.pred.hitrail_noOHV
  X.pred.alternative[, "Speed"] <- X.pred.hitrail_norest[, "Speed"]
  Contribution.fn(Base_scn = "HighTrail_norest")
  out[spp, "Speed_OHV_contrib"] <- FunctionsBCR::BCI(contribution, BCIpercent = 80)
  out[spp, "Speed_OHV_p"] <- round(p.cont, digits = 2)
  
  # Human traffic contribution to horse effect
  effect.tot <- log(N.pred[,spp.ind,"HighTrail_noHorse"] / N.pred[,spp.ind,"HighTrail_norest"])
  X.pred.alternative <- X.pred.hitrail_noHorse
  X.pred.alternative[, c("HumanPresence", "LogTrafficNoZeros")] <-
    X.pred.hitrail_norest[, c("HumanPresence", "LogTrafficNoZeros")]
  Contribution.fn(Base_scn = "HighTrail_norest")
  out[spp, "HumTraff_Horse_contrib"] <- FunctionsBCR::BCI(contribution, BCIpercent = 80)
  out[spp, "HumTraff_Horse_p"] <- round(p.cont, digits = 2)
  
  # Human traffic contribution to road effect
  effect.tot <- log(N.pred[,spp.ind,"HighRoad"] / N.pred[,spp.ind,"Baseline"])
  X.pred.alternative <- X.pred.hiroad
  X.pred.alternative[, c("HumanPresence", "LogTrafficNoZeros")] <-
    X.pred.baseline[, c("HumanPresence", "LogTrafficNoZeros")]
  Contribution.fn(Base_scn = "Baseline")
  out[spp, "HumTraff_Road_contrib"] <- FunctionsBCR::BCI(contribution, BCIpercent = 80)
  out[spp, "HumTraff_Road_p"] <- round(p.cont, digits = 2)
}

write.csv(out, "Spp_mech_contributions.csv", row.names = T)
