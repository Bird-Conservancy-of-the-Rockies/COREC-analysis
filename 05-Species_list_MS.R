## Generates species list table for manuscript supplement

library(stringr)
library(dplyr)
library(purrr)
library(readr)

#setwd("~/COREC")
setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")

Spp_list <- read_csv("data/Species_list_assigned.csv",
                     show_col_types = FALSE, progress = FALSE) %>%
  mutate(Small = Mass < median(Mass)) %>%
  mutate(Group = pmap_chr(list(ifelse(Hab_specialist, "HS", NA),
                               ifelse(Migratory, "M", NA),
                               ifelse(Small, "S", NA),
                               ifelse(HumanCommensal, "HC", NA),
                               ifelse(Omnivore, NA, "DS"),
                               ifelse(Insectivore, "I", NA),
                               ifelse(Ground, "G", NA),
                               ifelse(SGCN, "SGCN", NA)),
                          ~ paste(na.omit(c(...)), collapse = ","))) %>%
  select(Species, ScientificName, BirdCode, Sum_counts, Group)
write.csv(Spp_list, "Spp_list_MS.csv", row.names = FALSE)
