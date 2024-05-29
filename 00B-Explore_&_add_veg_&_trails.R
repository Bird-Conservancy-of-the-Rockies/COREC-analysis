library(dplyr)
library(stringr)
library(GetDataBCR)
library(psych)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")

#_______ Script inputs _______#
years <- 2021:2023
dat_traffic <- read.csv("data/HumanTraffic_covariates.csv", header = TRUE, stringsAsFactors = FALSE)
grid.list <- dat_traffic %>% pull(TransectNum)
#_____________________________#

## Primary habitat frequencies ##
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

# Get trail densities #
dat_trails <- read.table("data/COrecGridsTrailDensity.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t") %>%
  rename(TransectNum = TransNum) %>%
  mutate(TrailTotm = ifelse(is.na(TrailTotm), 0, TrailTotm),
         RoadTotm = ifelse(is.na(RoadTotm), 0, RoadTotm)) %>%
  mutate(Prp_MotRestricted = ifelse(is.na(atvohvrm), 0, atvohvrm) / TrailTotm,
         Prp_HorseRestricted = ifelse(is.na(horsesRestm), 0, horsesRestm) / TrailTotm) %>%
  mutate(Prp_MotRestricted = ifelse(is.na(Prp_MotRestricted), mean(Prp_MotRestricted, na.rm = TRUE), Prp_MotRestricted),
         Prp_HorseRestricted = ifelse(is.na(Prp_HorseRestricted), mean(Prp_HorseRestricted, na.rm = TRUE), Prp_HorseRestricted)) %>%
  select(TransectNum:RoadTotm, Prp_MotRestricted, Prp_HorseRestricted)
# Only 19 had any proportion of trails with no horses allowed, so only including prp motorized restricted

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
  mutate(HumanPresence = HumanTraffic > 0,
         LogTraffic = ifelse(HumanTraffic == 0, log(mean(HumanTraffic)), log(HumanTraffic))) %>%
  select(-HumanTraffic) %>%
  left_join(dat_trails, by = "TransectNum")
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
    width = 40, # The width of the plot in inches
    height = 40,  # The height of the plot in inches
    paper="USr")
pairs.panels(dat %>% select(HumanPresence:ShrubHt),
             method = "pearson",
             hist.col = "#00AFBB",
             cex.cor = 1,
             cex.labels = 0.5)
dev.off()

# Select veg variables and save #
# dat <- dat %>% select(TransectNum, Year, )