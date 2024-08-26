library(dplyr)
library(stringr)
library(GetDataBCR)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")

#_______ Script inputs _______#
years <- 2021:2023
dat_human <- read.csv("data/HumanTraffic_covariates.csv", header = TRUE, stringsAsFactors = FALSE)
grid.list <- sort(unique(dat_human$TransectNum))
#_____________________________#

grab <- BirdData_IMBCR(select.cols = c('BirdCode',
                                       'Species',
                                       'CL_Count'),
                       Year.filter = years,
                       TransectNum.filter = grid.list) %>%
  filter(!str_sub(BirdCode, 1, 2) == "UN")
spp.exclude <- read.csv("data/Species_list0.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  filter(Exclude == 1) %>%
  pull(BirdCode)
# 'Species_list0.csv' was generated with an initial version of this script.
  # Species listed in that preliminary file were visually inspected and marked for exclusion.
grab %>% filter(!BirdCode %in% spp.exclude) %>%
  dplyr::group_by(BirdCode, Species) %>%
  summarise(Detections = sum(CL_Count)) %>%
  write.csv(str_c("data/Species_list.csv"), row.names = F)

# Reviewed preliminary list, added exclude column, and renamed to 'Species_list0.csv'.
