setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")

library(dplyr)
library(stringr)
library(GetDataBCR)

# Compile and expore covariates:
# 1. Human traffic - ping rate x residency time.
# 2. Speed - Mean speed of pings.
# 3. Time of day - mean.
# 4. Date (day of year) - mean.

#_______ Script inputs _______#
years <- 2021:2023
# List grid cell IDs
grid.list <- readRDS("data/AllDataRDS_19April2024/ResidencyTime_updt4_16May2024.rds") %>%
  pull(grid_transect) %>% unique
#_____________________________#

## Gather & collate data ##
# Point ping data #
dat_pnt_pings <- readRDS("data/AllDataRDS_19April2024/HMD_IMBCRGrids_100m_noBuild5m_Counts_23May2024.rds") %>%
  ungroup() %>% rename(TransectNum = Site, Year = year)

# Time of day #
dat_TOD <- readRDS("data/AllDataRDS_19April2024/HMD_IMBCRGrids_100m_noBuild5m_TOD_23May2024.rds") %>%
  ungroup() %>% rename(TransectNum = Site)

# Res time #
dat_res_time <- readRDS("data/AllDataRDS_19April2024/ResidencyTime_updt4_16May2024.rds")

# Movement rates #
file_list <- list.files("data/AllDataRDS_19April2024") %>%
  (function(x) x[which(str_detect(x, "MovementRates"))])
dat_MoveRates <- readRDS(str_c("data/AllDataRDS_19April2024/", file_list[1]))
for(i in 2:length(file_list)) dat_MoveRates <- dat_MoveRates %>%
  bind_rows(readRDS(str_c("data/AllDataRDS_19April2024/", file_list[i])))
dat_MoveRates <- dat_MoveRates %>% filter(speed_mi_per_hr_orig != 0) # Zero-speed records do not represent movement, and so are deemed uninformative.

# Samples #
dat_samples <- SampleUnits("TransectNum", TransectNum.filter = grid.list, Year.filter = years)

## Compile potential covariates ##
dat <- dat_samples %>%
  left_join(dat_pnt_pings %>%
              group_by(TransectNum) %>%
              summarise(Users_per_day = sum(UniqueUsers) / 93),
            by = "TransectNum") %>%
  mutate(Users_per_day =
           ifelse(is.na(Users_per_day), 0, Users_per_day)) %>%
  left_join(dat_MoveRates %>%
              filter(CAT2 != "Building") %>%
              group_by(TransectNu) %>%
              summarise(Date_mn = mean(jul),
                        Speed = mean(speed_mi_per_hr_orig)) %>%
              ungroup() %>%
              rename(TransectNum = TransectNu), by = "TransectNum") %>%
  left_join(dat_res_time %>%
              rename(TransectNum = grid_transect) %>%
              group_by(TransectNum) %>%
              summarise(ResTime_mn = mean(mean_time_in_grid_100m, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(ResTime_mn = ifelse(is.na(ResTime_mn), mean(ResTime_mn, na.rm = TRUE), ResTime_mn)), # Human traffic is virtually the same if I impute with the min or mean. Picked the mean.
            by = "TransectNum") %>%
  left_join(dat_TOD %>%
              group_by(TransectNum) %>%
              summarise(TOD_mean = mean(TOD_mean)) %>%
              ungroup(), by = "TransectNum") %>%
  mutate(HumanTraffic = Users_per_day * ResTime_mn) %>%
  select(-Users_per_day) %>% select(-ResTime_mn)
write.csv(dat, "data/HumanTraffic_covariates.csv", row.names = FALSE)
