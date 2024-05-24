setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")

library(dplyr)
library(stringr)
library(GetDataBCR)

# Compile and expore covariates:
# 1. Human traffic - ping rate x residency time. Scales: 50, 100, 250, and 500. Time frames: May, June, July, All months.
# 2. Speed - Mean speed of pings. Scale: 100m on grid cell. Time frames: May, June, July, All months.
# 3. Time of day - Mode. Scale: 50, 100, 250, and 500. Time frames: May, June, July, All months.
# 4. Human traffic x Speed - Mean speed of pings. Scale: 100m on grid cell. Time frames: May, June, July, All months.
# 5. Human traffic x Time of day - Mode. Scale: 50, 100, 250, and 500. Time frames: May, June, July, All months.

# Additional summaries:
# 1. Histogram of dates (day of year) across all pings.
# 2. Proportion of activity types (building, vehicle, hiking, etc.)

#_______ Script inputs _______#
years <- 2021:2023
# List grid cell IDs
file_list <- list.files("data/AllDataRDS_19April2024") %>%
  (function(x) x[which(str_detect(x, "ResidencyTime"))])
dat <- readRDS(str_c("data/AllDataRDS_19April2024/", file_list[1]))
for(i in 2:length(file_list)) dat <- dat %>% bind_rows(readRDS(str_c("data/AllDataRDS_19April2024/", file_list[i])))
grid.list <- unique(dat$grid_transect)
rm(dat)

#n_points_per_grid <- read.csv("data/AllDataRDS_19April2024/PointCounts_HMD_Analysis.csv", header = TRUE, stringsAsFactors = FALSE) %>%
#  group_by(TransectNu) %>% summarise(n = n())
#***16 points for every grid cell in this file so we can just assume 16 without referencing this file.
grids_w_points <- read.csv("data/AllDataRDS_19April2024/PointCounts_HMD_Analysis.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  pull(TransectNu) %>% unique()
#_____________________________#

# Compare grid cells in extraction to master list (Not needed after initial check.)
# file_list <- list.files("data/AllDataRDS_19April2024") %>%
#   (function(x) x[which(str_detect(x, "ResidencyTime"))])
# dat <- readRDS(str_c("data/AllDataRDS_19April2024/", file_list[1]))
# for(i in 2:length(file_list)) dat <- dat %>% bind_rows(readRDS(str_c("data/AllDataRDS_19April2024/", file_list[i])))
# grid_extract <- dat %>% pull(grid_transect) %>% unique()
# grid_master <- read.csv("data/COREC grids_list_all.csv", header = TRUE, stringsAsFactors = FALSE) %>% pull(TransectNum) %>% unique()
# grid_extract[which(!grid_extract %in% grid_master)]
# grid_master[which(!grid_master %in% grid_extract)]

## Gather & collate data ##
collate.fn <- function(file.stub) { # General function for gathering and combining datasets provided by M. Ditmer
  file_list <- list.files("data/AllDataRDS_19April2024") %>%
    (function(x) x[which(str_detect(x, file.stub))])
  dat <- readRDS(str_c("data/AllDataRDS_19April2024/", file_list[1]))
  for(i in 2:length(file_list)) dat <- dat %>%
    bind_rows(readRDS(str_c("data/AllDataRDS_19April2024/", file_list[i])))
  return(dat)
}

# Point ping data #
dat_pnt_pings <- collate.fn("Point_Pings_Daily") %>% ungroup() %>%
  mutate(TransectNum = str_split(Site, PointID, simplify = TRUE)[,2]) %>%
  rename(Year = year)
# Notes:
# - PointIDs identify cell phone pings, not survey points
# - Mark says buffers are on points not grid cells, which is confusing....
# - Thinking these are compiled within point count buffers.

# Time of day #
dat_TOD <- collate.fn("TOD_PointLevel_Monthly") %>% ungroup()
# Notes:
# - Thinking these are compiled within point count buffers.

# Res time #
# dat_res_time <- collate.fn("ResidencyTime_Transect_Monthly") %>% ungroup() %>%
#   rename(TransectNum = grid_transect,
#          Year = year) %>%
#   mutate(Year = as.integer(Year))
dat_res_time <- readRDS("data/AllDataRDS_19April2024/ResidencyTime_updt4_16May2024.rds")
# Notes:
# - Thinking these are compiled within 100m of grid cell boundaries.

# Movement rates #
dat_MoveRates <- collate.fn("MovementRates_Transect_Monthly") %>% ungroup()
# Notes:
# - Thinking these are compiled within 100m of grid cell boundaries.

# Samples #
dat_samples <- SampleUnits("TransectNum", TransectNum.filter = grid.list, Year.filter = years)
# effort_per_grid <- SampleUnits(c("TransectNum", "Point", "Year"), TransectNum.filter = grid.list, Year.filter = years) %>% # average points per grid. Using this temporarily until I get the point list from Mark.
#   group_by(TransectNum, Year) %>%
#   summarise(n = n()) %>%
#   ungroup() %>% group_by(TransectNum) %>%
#   summarise(n = mean(n))
# npnts_per_grid <- effort_per_grid$n
# names(npnts_per_grid) <- effort_per_grid$TransectNum

## Compile potential covariates ##
#_________Discarded code_________#
# Point level #
# dat <- dat_samples
# buffers <- sort(unique(dat_pnt_pings$buffer))
# months <- c("May", "All")
# for(buff in buffers) for(mnth in months) {
#   col.nam <- str_c("User_count_", mnth, "_pnt", buff, "m")
#   dat.join <- dat_pnt_pings
#   if(mnth == "All") {
#     dat.join <- dat.join %>% filter(buffer == buff)
#   } else {
#     dat.join <- dat.join %>% filter(buffer == buff & month == mnth)
#   }
#   dat.join <- dat.join %>%
#     group_by(TransectNum) %>%
#     summarise(User_count = sum(UniqueUsers))
#   dat <- dat %>% left_join(dat.join, by = "TransectNum") %>%
#     mutate(User_count = ifelse(is.na(User_count), 0, User_count)) %>%
#     mutate(User_count = ifelse(TransectNum %in% grids_w_points, User_count, NA))
#   names(dat)[which(names(dat) == "User_count")] <- col.nam
# }
# dat.join <- dat_MoveRates %>%
#   filter(CAT2 != "Building") %>%
#   group_by(TransectNu) %>%
#   #summarise(User_count_All_grd100m = n()) %>%
#   summarise(User_count_All_grd100m = n()) %>%
#   ungroup() %>%
#   rename(TransectNum = TransectNu)
# dat <- dat %>%
#   left_join(dat.join, by = "TransectNum") %>%
#   mutate(User_count_All_grd100m =
#            ifelse(is.na(User_count_All_grd100m), 0, User_count_All_grd100m))
# cor(dat %>% select(User_count_May_pnt50m:User_count_All_grd100m), use = "complete")
#_____________________________#

dat <- dat_samples %>%
  left_join(dat_MoveRates %>%
              filter(CAT2 != "Building") %>%
              group_by(TransectNu) %>%
              summarise(Users_per_day = n() / 93,
                        Date_mn = mean(jul),
                        Speed = mean(speed_mi_per_hr_orig)) %>%
              ungroup() %>%
              rename(TransectNum = TransectNu), by = "TransectNum") %>%
  mutate(Users_per_day =
           ifelse(is.na(Users_per_day), 0, Users_per_day),
         ) %>%
  left_join(dat_res_time %>%
              rename(TransectNum = grid_transect) %>%
              group_by(TransectNum) %>%
              summarise(ResTime_mn = mean(mean_time_in_grid_100m, na.rm = TRUE)) %>%
              ungroup(), by = "TransectNum") %>%
  left_join(dat_TOD %>% filter(buffer == 500) %>%
              rename(TransectNum = TransectNu) %>%
              group_by(TransectNum) %>%
              summarise(TOD_mean = mean(TOD_mean)) %>%
              ungroup(), by = "TransectNum") %>%
  mutate(HumanTraffic = ifelse(Users_per_day == 0, 0, Users_per_day * ResTime_mn)) %>%
  select(-Users_per_day) %>% select(-ResTime_mn)
write.csv(dat, "data/HumanTraffic_covariates.csv", row.names = FALSE)
