library(dplyr)
library(stringr)
library(GetDataBCR)
library(psych)

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

dat_traffic <- read.csv("data/HumanTraffic_covariates.csv", header = TRUE, stringsAsFactors = FALSE)
#_____________________________#

## Frequencies ##
grab <- VegData(select.cols = c("TransectNum",
                                "Year",
                                'primaryHabitat',
                                'HabitatCommonName',
                                'o_canopy_percent',
                                'o_mean_height',
                                'shrub_cover',
                                'shrub_mean_height'),
                Year.filter = years,
                TransectNum.filter = grid.list) %>%
  rename(CanCov = o_canopy_percent,
         CanHt = o_mean_height,
         ShrubCov = shrub_cover,
         ShrubHt = shrub_mean_height)
grab %>% dplyr::group_by(primaryHabitat, HabitatCommonName) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  write.csv(str_c("Primary_habitats.csv"), row.names = F)

# Correlate veg categories with human traffic #
veg.classes <- list(
  SageShrubland = "SA",
  PinyonJuniper = "PJ",
  SpruceFir = "SF",
  Aspen = "AS",
  OakWoodland = "OA",
  Shrubland = c("SA", "SH", "DS"),
  ConiferForest = c("PJ", "SF", "MC", "LP", "PP"),
  DecidForest = c("AS", "OA", "DW"),
  GrasslandMeadow = c("GR", "MM", "HM"),
  Mesic = c("RI", "OW", "WE"),
  AnthroUrban = c("UR", "AR"),
  HighStructure = c("PJ", "SF", "AS", "OA", "MC",
                    "LP", "II", "PP", "DW")
)
dat <- grab %>% select(TransectNum, Year) %>% distinct() %>%
  left_join(dat_traffic %>% select(TransectNum, HumanTraffic), by = "TransectNum") %>%
  mutate(LogTraffic = log(HumanTraffic + 0.001))
for(i in 1:length(veg.classes)) {
  dat <- dat %>%
    left_join(
      grab %>% group_by(TransectNum) %>%
        summarise(PVeg = sum(primaryHabitat %in% veg.classes[[i]]) / n()),
      by = c("TransectNum")
    )
  names(dat)[which(names(dat) == "PVeg")] <- names(veg.classes)[i]
}
dat <- dat %>% left_join(
  grab %>% group_by(TransectNum, Year) %>%
    summarise(CanCov = mean(CanCov, na.rm = TRUE),
              CanHt = mean(CanHt, na.rm = TRUE),
              ShrubCov = mean(ShrubCov, na.rm = TRUE),
              ShrubHt = mean(ShrubHt, na.rm = TRUE)),
  by = c("TransectNum", "Year")
)
pdf(file = "HumTraffic_veg_relations.pdf",   # The directory you want to save the file in
    width = 11, # The width of the plot in inches
    height = 11,  # The height of the plot in inches
    paper="USr")
pairs.panels(dat %>% select(HumanTraffic:ShrubHt),
             method = "pearson",
             hist.col = "#00AFBB",
             cex.cor = 1)
dev.off()

# Select veg variables and save #
# dat <- dat %>% select(TransectNum, Year, )