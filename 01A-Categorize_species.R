library(stringr)
library(tidyr)
library(dplyr)
library(FunctionsBCR)
library(GetDataBCR)

#setwd("~/COREC")
setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load("data/Data_compiled.RData")

Spp_list <- Spp_list %>%
  left_join(BirdData(select.cols = c("BirdCode", "ScientificName"), group_by = TRUE),
            by = "BirdCode") %>%
  rename(Sum_counts = Detections) %>%
  select(BirdCode, Species, ScientificName, Sum_counts) %>%
  mutate(Hab_specialist = as.logical(NA))#,
         #Migratory = as.logical(NA),
         #Ground_NestOrForage = as.logical(NA),
         #Mass = as.numeric(NA),
         #BodyLarge = as.logical(NA),
         #Human_commensal = as.logical(NA),
         #Insectivore = as.logical(NA),
         #SGCN = as.logical(NA))

# Compile and joing AVONET traits
dat_AVONET <- read.csv("C:/Users/quresh.latif/files/data/AVONET/AVONET2_Ebird.csv",
                       header = TRUE, stringsAsFactors = FALSE)
AVONET_fields <- dat_AVONET %>%
  mutate(Migratory = Migration %in% c(2, 3),
         HumanCommensal = Habitat == "Human Modified",
         Omnivore = Trophic.Level == "Omnivore",
         Insectivore = Trophic.Niche == "Invertivore") %>%
  select(Species2, Migratory, Mass, HumanCommensal, Omnivore, Insectivore)
Spp_list <- Spp_list %>%
  left_join(AVONET_fields, by = c("ScientificName" = "Species2"))

# Add data listing ground nesters/foragers
dat_4FRI <- read.csv("Spp_ground_4FRI.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(Ground = str_detect(Foraging, "Gr") | str_detect(Nesting, "OG"))
dat_BBN <- read.csv("Spp_ground_BBN.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(Ground = str_detect_any(Forage, c("GG", "GI", "GO")) | str_detect(Nest.layer, "GR"))
  # Additional species not listed in Saab and Powell (2005) or Birds of the World and not listed in the above references:
SaabPowell2005_BOW_ground_spp <- c("American Pipit", "Brown-capped Rosy-Finch", "Burrowing Owl",
                                   "Chukar", "Common Poorwill", "Dusky Grouse", "European Starling",
                                   "Fox Sparrow", "Greater Sage-Grouse", "Gunnison Sage-Grouse",
                                   "House Sparrow", "Killdeer", "Lark Bunting", "Northern Harrier",
                                   "Ovenbird", "Rock Pigeon", "Sagebrush Sparrow", "Sage Thrasher",
                                   "Savannah Sparrow", "Scaled Quail", "Sharp-tailed Grouse",
                                   "Veery", "White-crowned Sparrow", "Wilson's Snipe", "Wild Turkey",
                                   "White-tailed Ptarmigan")
SaabPowell2005_BOW_nongrnd_spp <- c("American Goldfinch", "American Dipper", "American Kestrel",
                                    "Bank Swallow", "Black-billed Magpie", "Belted Kingfisher",
                                    "Blue Jay", "Cassia Crossbill", "Cliff Swallow", "Common Grackle",
                                    "Cooper's Hawk", "Common Yellowthroat", "Eastern Kingbird",
                                    "Eastern Phoebe", "Ferruginous Hawk", "Great Horned Owl",
                                    "Golden Eagle", "Gray Catbird", "Least Flycatcher",
                                    "Loggerhead Shrike", "Northern Saw-whet Owl", "Peregrine Falcon",
                                    "Pine Grosbeak", "Red-tailed Hawk", "Red-winged Blackbird",
                                    "Sharp-shinned Hawk", "Swainson's Hawk", "Turkey Vulture",
                                    "Western Flycatcher", "Willow Flycatcher", "Yellow-breasted Chat")
# sum(!Spp_list$ScientificName %in% c(dat_4FRI$species, dat_BBN$Taxonomic.name) &
#       !Spp_list$Species %in% c(dat_4FRI$common_name, dat_BBN$Common.name))
Spp_list <- Spp_list %>%
  mutate(Ground = ifelse(Species %in% (dat_4FRI %>% filter(Ground) %>% pull(common_name)) |
                           ScientificName %in% (dat_4FRI %>% filter(Ground) %>% pull(species)) |
                           Species %in% (dat_BBN %>% filter(Ground) %>% pull(Common.name)) |
                           ScientificName %in% (dat_BBN %>% filter(Ground) %>% pull(Taxonomic.name)) |
                           Species %in% SaabPowell2005_BOW_ground_spp, TRUE,
                         ifelse(Species %in% (dat_4FRI %>% filter(!Ground) %>% pull(common_name)) |
                                  ScientificName %in% (dat_4FRI %>% filter(!Ground) %>% pull(species)) |
                                  Species %in% (dat_BBN %>% filter(!Ground) %>% pull(Common.name)) |
                                  ScientificName %in% (dat_BBN %>% filter(!Ground) %>% pull(Taxonomic.name)) |
                                  Species %in% SaabPowell2005_BOW_nongrnd_spp, FALSE, NA)))

# SGCN
#~~~~~~~~~For 2015 list~~~~~~~~~~~~~#
# SGCN_spp <- read.csv("C:/Users/quresh.latif/files/data/CPW SWAP Species List.csv", header = TRUE) %>%
#   filter(Group == "Birds")
# Spp_list <- Spp_list %>%
#   mutate(SGCN = Species %in% SGCN_spp$Common_Name |
#            ScientificName %in% SGCN_spp$Species)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

SGCN_spp <- read.csv("C:/Users/quresh.latif/files/data/CO Bird SGCN 2025.csv", header = TRUE) %>%
  filter(Category != "SGIN")
Spp_list <- Spp_list %>%
  mutate(SGCN = Species %in% SGCN_spp$Common |
           ScientificName %in% SGCN_spp$Taxonomic)

write.csv(Spp_list, "data/Species_list_assigned_HabSpec0.csv", row.names = FALSE)
