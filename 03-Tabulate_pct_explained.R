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

###########
# Species #
###########

tab.effect.total <- read.csv("data/Spp_total_management_effects.csv", header = TRUE, stringsAsFactors = FALSE)
spp.mech <- tab.effect.total$Spp
cols <- c("Trail.total", "Trail.p", "Trail.pctExplained", "Trail.pexp.p",
          "OHV.total", "OHV.p", "OHV.pctExplained", "OHV.pexp.p",
          "Road.total", "Road.p", "Road.pctExplained", "Road.pexp.p")
out <- matrix("", nrow = length(spp.mech), ncol = length(cols),
              dimnames = list(spp.mech, cols))

## Total effects ##
out[, "Trail.total"] <- str_c(round(tab.effect.total$HighTrail.md, digits = 2),
                                 " (", round(tab.effect.total$HighTrail.lo, digits = 2), ", ",
                                 round(tab.effect.total$HighTrail.hi, digits = 2), ")")
out[, "Trail.p"] <- (log(N.pred[,,"HighTrail_avgOHV"] / N.pred[,,"Baseline"]) %>%
  apply(2, function(x) sum(x > 0) / length(x)))[spp.mech] %>% round(digits = 2)
out[, "Road.total"] <- str_c(round(tab.effect.total$HighRoad.md, digits = 2),
                                 " (", round(tab.effect.total$HighRoad.lo, digits = 2), ", ",
                                 round(tab.effect.total$HighRoad.hi, digits = 2), ")")
out[, "Road.p"] <- (log(N.pred[,,"HighRoad"] / N.pred[,,"Baseline"]) %>%
                       apply(2, function(x) sum(x > 0) / length(x)))[spp.mech] %>% round(digits = 2)
out[, "OHV.total"] <- str_c(round(tab.effect.total$NoOHV.md, digits = 2),
                                 " (", round(tab.effect.total$NoOHV.lo, digits = 2), ", ",
                                 round(tab.effect.total$NoOHV.hi, digits = 2), ")")
out[, "OHV.p"] <- (log(N.pred[,,"HighTrail_noOHV"] / N.pred[,,"HighTrail_maxOHV"]) %>%
                       apply(2, function(x) sum(x > 0) / length(x)))[spp.mech] %>% round(digits = 2)

## Percent of total effects explained ##
  # Trail density
effect.tot <- log(N.pred[, spp.mech, "HighTrail_avgOHV"] / N.pred[, spp.mech, "Baseline"])
X.pred.alt <- X.pred[["HighTrail_avgOHV"]]
X.pred.alt[, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")] <-
  X.pred[["Baseline"]][, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")]
N.pred.alt <- N.pred.calc(X.pred.alt, spp.mech)
effect.expl <- log(N.pred[, spp.mech, "HighTrail_avgOHV"] / N.pred.alt)
pct.explained <- (effect.expl / effect.tot) * 100
out[, "Trail.pctExplained"] <- pct.explained %>%
  apply(2, function(x) FunctionsBCR::BCI(x, ndig = 0, BCIpercent = 80, flag.sig = TRUE))
out[, "Trail.pexp.p"] <- apply(pct.explained, 2, function(x) round(sum(x > 0) / length(x), digits = 2))

  # OHV Restriction
effect.tot <- log(N.pred[, spp.mech, "HighTrail_noOHV"] / N.pred[, spp.mech, "HighTrail_maxOHV"])
X.pred.alt <- X.pred[["HighTrail_noOHV"]]
X.pred.alt[, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")] <-
  X.pred[["HighTrail_maxOHV"]][, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")]
N.pred.alt <- N.pred.calc(X.pred.alt, spp.mech)
effect.expl <- log(N.pred[, spp.mech, "HighTrail_noOHV"] / N.pred.alt)
pct.explained <- (effect.expl / effect.tot) * 100
out[, "OHV.pctExplained"] <- pct.explained %>%
  apply(2, function(x) FunctionsBCR::BCI(x, ndig = 0, BCIpercent = 80, flag.sig = TRUE))
out[, "OHV.pexp.p"] <- apply(pct.explained, 2, function(x) round(sum(x > 0) / length(x), digits = 2))

# Road density
effect.tot <- log(N.pred[, spp.mech, "HighRoad"] / N.pred[, spp.mech, "Baseline"])
X.pred.alt <- X.pred[["HighRoad"]]
X.pred.alt[, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")] <-
  X.pred[["Baseline"]][, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")]
N.pred.alt <- N.pred.calc(X.pred.alt, spp.mech)
effect.expl <- log(N.pred[, spp.mech, "HighRoad"] / N.pred.alt)
pct.explained <- (effect.expl / effect.tot) * 100
out[, "Road.pctExplained"] <- pct.explained %>%
  apply(2, function(x) FunctionsBCR::BCI(x, ndig = 0, BCIpercent = 80, flag.sig = TRUE))
out[, "Road.pexp.p"] <- apply(pct.explained, 2, function(x) round(sum(x > 0) / length(x), digits = 2))

write.csv(out, "Pct_man_effects_explained_species.csv", row.names = T)

#############
# Community #
#############

## Define groups ##
spp_assignments <- read.csv("data/Species_list_assigned.csv", header = TRUE, stringsAsFactors = FALSE)
groups <- list(
  Community = Spp,
  Specialist = spp_assignments %>% filter(Hab_specialist) %>% pull(BirdCode),
  Generalist = spp_assignments %>% filter(!Hab_specialist) %>% pull(BirdCode),
  Migratory = spp_assignments %>% filter(Migratory) %>% pull(BirdCode),
  NonMigratory = spp_assignments %>% filter(!Migratory) %>% pull(BirdCode),
  Large = spp_assignments %>% filter(Mass > median(Mass)) %>% pull(BirdCode),
  Small = spp_assignments %>% filter(Mass < median(Mass)) %>% pull(BirdCode),
  HumComm = spp_assignments %>% filter(HumanCommensal) %>% pull(BirdCode),
  NonHumComm = spp_assignments %>% filter(!HumanCommensal) %>% pull(BirdCode),
  Omnivore = spp_assignments %>% filter(Omnivore) %>% pull(BirdCode),
  NonOmnivore = spp_assignments %>% filter(!Omnivore) %>% pull(BirdCode),
  Insectivore = spp_assignments %>% filter(Insectivore) %>% pull(BirdCode),
  NonInsectivore = spp_assignments %>% filter(!Insectivore) %>% pull(BirdCode),
  Ground = spp_assignments %>% filter(Ground) %>% pull(BirdCode),
  NonGround = spp_assignments %>% filter(!Ground) %>% pull(BirdCode),
  SGCN = spp_assignments %>% filter(SGCN) %>% pull(BirdCode)
)

cols <- c("Trail.total", "Trail.p", "Trail.pctExplained", "Trail.pexp.p",
          "OHV.total", "OHV.p", "OHV.pctExplained", "OHV.pexp.p",
          "Road.total", "Road.p", "Road.pctExplained", "Road.pexp.p")
out <- matrix("", nrow = length(groups), ncol = length(cols),
              dimnames = list(names(groups), cols))

for(g in names(groups)) {
  spp <- groups[[g]]
  
  # Trail density
  D.treatment <- N.pred[, spp, "HighTrail_avgOHV"] %>%
    apply(1, HillShannon)
  D.reference <- N.pred[, spp, "Baseline"] %>%
    apply(1, HillShannon)
  effect.tot <- log(D.treatment / D.reference)
  out[g, "Trail.total"] <- FunctionsBCR::BCI(effect.tot, BCIpercent = 80, ndig = 2, flag.sig = TRUE)
  out[g, "Trail.p"] <- round(sum(effect.tot > 0) / nsim, digits = 2)
  X.pred.alt <- X.pred[["HighTrail_avgOHV"]]
  X.pred.alt[, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")] <-
    X.pred[["Baseline"]][, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")]
  N.pred.alt <- N.pred.calc(X.pred.alt, spp)
  D.alt <- N.pred.alt %>% apply(1, HillShannon)
  effect.expl <- log(D.treatment / D.alt)
  pct.explained <- (effect.expl / effect.tot) * 100
  out[g, "Trail.pctExplained"] <- FunctionsBCR::BCI(pct.explained, ndig = 0, BCIpercent = 80, flag.sig = TRUE)
  out[g, "Trail.pexp.p"] <- round(sum(pct.explained > 0) / length(pct.explained), digits = 2)
  
  # OHV Restriction
  D.treatment <- N.pred[, spp, "HighTrail_noOHV"] %>% apply(1, HillShannon)
  D.reference <- N.pred[, spp, "HighTrail_maxOHV"] %>% apply(1, HillShannon)
  effect.tot <- log(D.treatment / D.reference)
  out[g, "OHV.total"] <- FunctionsBCR::BCI(effect.tot, BCIpercent = 80, ndig = 2, flag.sig = TRUE)
  out[g, "OHV.p"] <- round(sum(effect.tot > 0) / nsim, digits = 2)
  X.pred.alt <- X.pred[["HighTrail_noOHV"]]
  X.pred.alt[, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")] <-
    X.pred[["HighTrail_maxOHV"]][, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")]
  N.pred.alt <- N.pred.calc(X.pred.alt, spp)
  D.alt <- N.pred.alt %>% apply(1, HillShannon)
  effect.expl <- log(D.treatment / D.alt)
  pct.explained <- (effect.expl / effect.tot) * 100
  out[g, "OHV.pctExplained"] <- FunctionsBCR::BCI(pct.explained, ndig = 0, BCIpercent = 80, flag.sig = TRUE)
  out[g, "OHV.pexp.p"] <- round(sum(pct.explained > 0) / length(pct.explained), digits = 2)
  
  # Road density
  D.treatment <- N.pred[, spp, "HighRoad"] %>% apply(1, HillShannon)
  D.reference <- N.pred[, spp, "Baseline"] %>% apply(1, HillShannon)
  effect.tot <- log(D.treatment / D.reference)
  out[g, "Road.total"] <- FunctionsBCR::BCI(effect.tot, BCIpercent = 80, ndig = 2, flag.sig = TRUE)
  out[g, "Road.p"] <- round(sum(effect.tot > 0) / nsim, digits = 2)
  X.pred.alt <- X.pred[["HighRoad"]]
  X.pred.alt[, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")] <-
    X.pred[["Baseline"]][, c("HumanPresence", "LogTrafficNoZeros", "Speed", "Speed2")]
  N.pred.alt <- N.pred.calc(X.pred.alt, spp)
  D.alt <- N.pred.alt %>% apply(1, HillShannon)
  effect.expl <- log(D.treatment / D.alt)
  pct.explained <- (effect.expl / effect.tot) * 100
  out[g, "Road.pctExplained"] <- FunctionsBCR::BCI(pct.explained, ndig = 0, BCIpercent = 80, flag.sig = TRUE)
  out[g, "Road.pexp.p"] <- round(sum(pct.explained > 0) / length(pct.explained), digits = 2)
}

write.csv(out, "Pct_man_effects_explained_community.csv", row.names = T)
