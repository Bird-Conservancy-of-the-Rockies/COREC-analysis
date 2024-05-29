library(dplyr)
library(stringr)
library(GetDataBCR)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")

#_______ Script inputs _______#
years <- 2021:2023
# List grid cell IDs
file_list <- list.files("data/AllDataRDS_19April2024") %>%
  (function(x) x[which(str_detect(x, "ResidencyTime"))])
dat <- readRDS(str_c("data/AllDataRDS_19April2024/", file_list[1]))
for(i in 2:length(file_list)) dat <- dat %>% bind_rows(readRDS(str_c("data/AllDataRDS_19April2024/", file_list[i])))
grid.list <- dat %>% pull(grid_transect) %>% unique()
rm(dat)
#_____________________________#

grab <- BirdData_IMBCR(select.cols = c('BirdCode',
                                       'Species',
                                       'CL_Count'),
                       Year.filter = years,
                       TransectNum.filter = grid.list) %>%
  filter(!str_sub(BirdCode, 1, 2) == "UN")
grab %>% dplyr::group_by(BirdCode, Species) %>%
  summarise(Detections = sum(CL_Count)) %>%
  write.csv(str_c("Species_list.csv"), row.names = F)

# Reviewed preliminary list, added exclude column, and renamed to 'Species_list0.csv'.
