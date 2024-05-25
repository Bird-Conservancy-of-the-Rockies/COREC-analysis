library(dplyr)
library(stringr)
library(GetDataBCR)
library(lubridate)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")

#_______ Script inputs _______#
years <- 2021:2023
Spp_list <- read.csv("Species_list.csv", header = TRUE, stringsAsFactors = FALSE)
dat_human <- read.csv("data/HumanTraffic_covariates.csv", header = TRUE, stringsAsFactors = FALSE)
grid.list <- sort(unique(dat_human$TransectNum))
trunc.pct <- 0.95 # Distance quantile at which detections are truncated for distance sampling (and thus defining point count plot radius)
nG <- 10 # number of distance categories
#_____________________________#

Spp <- Spp_list$BirdCode

## Samples ##
samples <- SampleUnits_IMBCR(select.cols = c("TransectNum", "Point", "Year", "DATE", "PointVisitStartTime",
                                          "PointLatitude", "PointLongitude"),
                          Year.filter = years, TransectNum.filter = grid.list) %>%
  rename(Date = DATE) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(TZ = "Mountain") %>%
  mutate(HR = PointVisitStartTime %>% str_sub(1, -3) %>% str_pad(width = 2, side = "left", pad = "0")) %>%
  mutate(MIN = PointVisitStartTime %>% str_sub(-2, -1) %>% str_pad(width = 2, side = "left", pad = "0")) %>%
  mutate(Time = str_c(HR, MIN, "00", sep = ":")) %>%
  mutate(dateTime = str_c(Date, Time, sep = " ")) %>%
  mutate(Time_ssr = FunctionsBCR::tssr(PointLatitude, PointLongitude, dateTime, format = "%Y-%m-%d %H:%M:%OS")) %>%
  group_by(TransectNum, Year, Date, TZ) %>%
  summarise(Effort = n(),
            Survey_tssr = mean(Time_ssr, na.rm = TRUE)) %>%
  ungroup()

## Bird data ##
detections <- BirdData_IMBCR(select.cols = c('TransectNum',
                                       'Point',
                                       'Year',
                                       'BirdCode',
                                       'radialDistance',
                                       'TimePeriod',
                                       'CL_ID',
                                       'CL_Count'),
                       Year.filter = years,
                       TransectNum.filter = grid.list,
                       BirdCode.filter = Spp_list$BirdCode)

# Consolidate clusters #
# detections %>% filter(!is.na(CL_ID)) %>% # Check within-cluster range of time periods
#   dplyr::group_by(TransectNum, Point, BirdCode, Year, CL_ID) %>%
#   summarize(TimeRange = max(TimePeriod) - min(TimePeriod)) %>%
#   #View
#   pull(TimeRange) %>% max # Should be zero.

# detections %>% filter(!is.na(CL_ID)) %>% # Check within-cluster range of distances
#   dplyr::group_by(TransectNum, Point, BirdCode, Year, TimePeriod, CL_ID) %>%
#   summarize(DistDiff = max(radialDistance) - min(radialDistance)) %>%
#   #View
#   pull(DistDiff) %>% max # Should be <= 20

det.proc <- detections %>% filter(is.na(CL_ID)) %>%
  select(TransectNum, Point, Year, TimePeriod, BirdCode, radialDistance,
         CL_Count) %>%
  bind_rows(
    detections %>% filter(!is.na(CL_ID)) %>%
      dplyr::group_by(TransectNum, Point, Year,
                      TimePeriod, BirdCode, CL_ID) %>%
      summarize(radialDistance = mean(radialDistance),
                CL_Count = sum(CL_Count)) %>%
      select(-CL_ID)
  ) %>%
  mutate(radialDistance = ifelse(radialDistance == 0, 0.01, radialDistance)) %>%
  mutate(Grid_year = str_c(TransectNum,
                           Year, sep = "-")) %>%
  mutate(TimePeriod = ifelse(TimePeriod %in% c(1, 2), 1, # Collapsing to 2-min intervals
                             ifelse(TimePeriod %in% c(3, 4), 2, 3))) %>%
  select(Grid_year, BirdCode, radialDistance, TimePeriod, CL_Count)

# Review cluster size by species #
# spp_max_clusters <- det.proc %>% group_by(BirdCode) %>% summarise(max_cluster = max(CL_Count))
# View(spp_max_clusters %>% filter(max_cluster > 6))
# spp_max_clusters %>% filter(max_cluster > 6) %>%
#   write.csv("Spp_w_big_clusters.csv", row.names = FALSE)

# Filter out large clusters that are likely migrants (probably don't need this but checking with Matthew...) #
# for(sp in 1:length(Spp_NGP_all)) if(Spp_NGP_all[sp] %in% Spp_NGP_migratory) {
#   grab.proc <- grab.proc %>% filter(!(BirdCode == Spp_NGP_all[sp] & CL_Count > 6))
# }

# Derive parameters for distance sampling #
cutoff <- quantile(det.proc$radialDistance, trunc.pct, na.rm=TRUE)
ind.keep <- which(det.proc$radialDistance <= cutoff)
ind.drop <- which(det.proc$radialDistance > cutoff)
detection.trim.summary <- data.frame(Spp = Spp,
                                     total = (det.proc$BirdCode %>%
                                                tapply(.,.,length))[Spp],
                                     n.keep = (det.proc$BirdCode[ind.keep] %>%
                                                 tapply(.,.,length))[Spp],
                                     n.drop = (det.proc$BirdCode[ind.drop] %>%
                                                 tapply(.,.,length))[Spp]) %>%
  mutate(prp.keep = round(n.keep / total, digits = 3),
         prp.drop = round(n.drop / total, digits = 3))
#View(det.trim.log)
rm(ind.drop, ind.keep)

det.proc.cutoff <- det.proc %>% filter(radialDistance <= cutoff)
area.circle <- as.numeric(pi * (cutoff / 1000) ^ 2) # area of point count circle in km^2
breaks <- seq(0, cutoff, length.out = nG + 1) # breaks for distance categories
area.band <- (pi * breaks[-1]^2) - (pi * breaks[-(nG+1)]^2) # area of each distance category
area.prop <- area.band / sum(area.band)
det.proc.cutoff <- det.proc.cutoff %>%
  mutate(dclass = ceiling(radialDistance / breaks[2])) %>%
  select(Grid_year:radialDistance, dclass, TimePeriod, CL_Count)

# Check correlations between distance and time period #
# cor(det.proc.cutoff$dclass, det.proc.cutoff$TimePeriod, method = "spearman") # Community wide correlation: r = 0.05234036
detection.trim.summary <- detection.trim.summary %>%
  left_join(det.proc.cutoff %>%
              group_by(BirdCode) %>%
              summarise(Dist_time_cor = suppressWarnings(cor(dclass, TimePeriod, method = "spearman"))),
            by = c("Spp" = "BirdCode"))
# View(detection.trim.summary %>% arrange(Dist_time_cor))
# View(detection.trim.summary %>% arrange(prp.drop))

# library(ggplot2)
# theme_set(theme_bw())
# sp <- "GRSG"
# dat <- grab.proc.cutoff %>% filter(BirdCode == sp & !radialDistance >= cutoff) %>%
#   mutate(dclass = ceiling(radialDistance / breaks[2])) %>%
#   select(Point_year:TimePeriod, dclass, CL_Count)
# ggplot(dat, aes(x = dclass, y = TimePeriod)) +
#   geom_jitter(alpha = 0.3) +
#   ggtitle(sp)

detects.all <- det.proc %>%
  mutate(dclass = ceiling(radialDistance / breaks[2])) %>%
  select(Grid_year:radialDistance, dclass, TimePeriod, CL_Count)
detects.cutoff <- detects.all %>% filter(dclass <= 10)
detects.drop <- detects.all %>% filter(dclass > 10)
rm(detects.all, det.proc, det.proc.cutoff)

# List all grid_year IDs
gridXyears.list <- samples %>%
  mutate(Grid_year = str_c(TransectNum,
                           Year, sep = "-")) %>%
  pull(Grid_year) %>% unique() %>% sort()

## Covariates ##
# Human traffic #
covariates <- samples %>% left_join(dat_human, by = "TransectNum") %>%
  rename(Traffic_DOY_mn = Date_mn)

# Vegetation classes #
grab <- VegData(select.cols = c("TransectNum",
                                "Point",
                                "Year",
                                'primaryHabitat'),
                Year.filter = years,
                TransectNum.filter = grid.list) %>%
  group_by(TransectNum) %>%
  summarise(Shrubland       = sum(primaryHabitat %in% c("SA", "SH", "DS"))       / n(),
            PinyonJuniper   = sum(primaryHabitat     == "PJ")                    / n(),
            ConiferForest   = sum(primaryHabitat %in% c("SF", "MC", "LP", "PP")) / n(),
            Aspen           = sum(primaryHabitat     == "AS")                    / n(),
            OakWoodland     = sum(primaryHabitat     == "OA")                    / n(),
            GrasslandMeadow = sum(primaryHabitat %in% c("GR", "MM", "HM"))       / n(),
            Mesic           = sum(primaryHabitat %in% c("RI", "OW", "WE"))       / n(),
            Alpine          = sum(primaryHabitat     == "AT")                    / n())
covariates <- covariates %>%
  left_join(grab, by = "TransectNum")
rm(grab)

# Survey timing #
library(lubridate)
covariates <- covariates %>%
  mutate(Date = Date %>% ymd) %>%
  mutate(Survey_DOY = yday(Date)) %>%
  select(TransectNum:Date, Survey_DOY, Effort:Alpine)

## Compile detection data arrays ##
n.mat <- matrix(0, nrow = length(Spp), ncol = length(gridXyears.list))
for(sp in 1:length(Spp)) {
  dat <- detects.cutoff %>% filter(BirdCode == Spp[sp])
  n.mat[sp,] <- data.frame(Grid_year = gridXyears.list, stringsAsFactors = F) %>%
    left_join(
      dat %>%
        dplyr::group_by(Grid_year) %>%
        summarise(Count = n()),
      by = "Grid_year"
    ) %>%
    mutate(Count = ifelse(is.na(Count), 0, Count)) %>%
    pull(Count)
}
dimnames(n.mat) <- list(Spp, gridXyears.list)
rm(dat, sp)

dtobs <- detects.cutoff %>%
  filter(Grid_year %in% gridXyears.list) %>%
  mutate(GridYrInd = match(Grid_year, gridXyears.list),
         SppInd = match(BirdCode, Spp)) %>%
  select(SppInd, GridYrInd, CL_Count, dclass, TimePeriod) %>%
  as.matrix()

## Compile covariate matrix ##
covariates <- covariates %>%
  mutate(Grid_year = str_c(TransectNum, Year, sep = "-"),
         HumanPresence = ifelse(HumanTraffic > 0, 1, 0),
         LogTrafficNoZeros = ifelse(HumanTraffic == 0, NA, log(HumanTraffic)),
         LogTraffic = log(HumanTraffic + 0.1),
         GrdYrInd = Grid_year %>% as.factor %>% as.integer,
         GridInd = TransectNum %>% as.factor %>% as.integer,
         YearInd = Year %>% as.factor %>% as.integer) %>%
  select(Grid_year, TransectNum, Year, GrdYrInd:YearInd, Date:Survey_tssr, HumanPresence:LogTraffic,
         Traffic_DOY_mn:TOD_mean, Shrubland:Alpine)
CovIndMat <- data.frame(Grid_year = gridXyears.list, stringsAsFactors = F) %>%
  left_join(covariates, by = "Grid_year") %>%
  select(GrdYrInd:YearInd, Effort, Survey_DOY:Alpine) %>%
  data.matrix

## Clean & save workspace ##
save.image("Data_compiled.RData")
