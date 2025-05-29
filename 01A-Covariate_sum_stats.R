library(stringr)
library(tidyr)
library(dplyr)
library(FunctionsBCR)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load(str_c("data/Data_compiled.RData"))

covariates$Speed <- covariates$Speed * 1.609 # Convert mi per hour to km per hour
sumStats <- FunctionsBCR::SumStats_df(covariates,
                          vars = c("TrailTotm", "RoadTotm", "Prp_MotRestricted",
                                   "HumanPresence", "LogTrafficNoZeros", "Speed",
                                   "Shrubland", "ConiferForest", "Aspen", "OakWoodland",
                                   "GrasslandMeadow", "Mesic", "Alpine", "PinyonJuniper",
                                   "LogTraffic", "Survey_DOY", "Survey_tssr"),
                          binary = 4)
write.csv(sumStats, "CovSumStats.csv", row.names = TRUE)
