library(stringr)
library(tidyr)
library(dplyr)
library(mcmcOutput)
library(ggplot2)
library(cowplot)
library(FunctionsBCR)
theme_set(theme_bw())

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load(str_c("Data_compiled.RData"))

#_____ Script inputs _____#
git.repo <- "COREC-analysis/"
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
#________________________#

management.covs <- c("TrailTotm", "RoadTotm", "Prp_MotRestricted", "Prp_HorseRestricted")
human.covs <- c("HumanPresence", "LogTrafficNoZeros", "Speed", "Traffic_DOY_mn", "TOD_mean")

rmat <- pmat <- nmat <- matrix(NA, nrow = length(human.covs), ncol = length(management.covs),
                               dimnames = list(human.covs, management.covs))
for(i in 1:length(human.covs)) for(j in 1:length(management.covs)) {
  x <- CovIndMat[, human.covs[i]]
  y <- CovIndMat[, management.covs[j]]
  ctest <- cor.test(x, y)
  rmat[i, j] <- ctest$estimate
  pmat[i, j] <- ctest$p.value
  nmat[i, j] <- sum(!is.na(x) & !is.na(x))
}

tab.out <- matrix("", nrow = length(human.covs), ncol = length(management.covs),
                  dimnames = list(human.covs, management.covs))
for(i in 1:length(tab.out)) {
  sig.val <- pmat[i] %>% (function(x) ifelse(x > 0.05, "", ifelse(x <= 0.05 & x > 0.01, "*", "**")))
  tab.out[i] <- str_c(round(rmat[i], digits = 3), sig.val, " (", nmat[i], ")")
}
write.csv(tab.out, "Management_human_correlations.csv", row.names = TRUE)
