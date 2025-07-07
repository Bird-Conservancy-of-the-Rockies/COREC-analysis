library(stringr)
library(tidyr)
library(dplyr)
library(mcmcOutput)
library(ggplot2)
library(cowplot)
library(FunctionsBCR)
theme_set(theme_bw())

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load("data/Data_compiled.RData")

#_____ Script inputs _____#
git.repo <- "COREC-analysis/"
mod.nam <- "path"
mod <- R.utils::loadObject(str_c("mod_", mod.nam))
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
#________________________#

nsamp <- dim(mod$mcmcOutput)[1]

source(str_c(git.repo, "Functions_source.R"))

################################
## Species recreation effects ##
################################
covs <- dimnames(X.beta)[[2]]
cov.labs <- c("Trail density", "Road density", "Proportion no OHVs",
              "Humans detected", "Cond. traffic intensity", "Traffic speed",
              "Shrubland", "Conifer forest", "Aspen", "Oak woodland",
              "Grassland or meadow", "Mesic", "Alpine", "Traffic speed^2")
names(cov.labs) <- covs

## Retrieve species to plot ##
spp.plot <- R.utils::loadObject("data/Spp_results")
spp.plot1 <- spp.plot[1:floor(length(spp.plot) / 2)]
spp.plot2 <- spp.plot[ceiling(length(spp.plot) / 2):length(spp.plot)]

## Generate plots ##
ncov.relations.supported <- 0
for(cov in covs) {
  beta.ind <- which(dimnames(X.beta)[[2]] == cov)
  beta <- mod$mcmcOutput$betaVec[,,beta.ind]
  dat <- tabulate_community_effect(Spp, beta, BCIpercent = 80) %>%
    filter(Spp %in% spp.plot) %>%
    mutate(index = rank(index))
  ncov.relations.supported <- ncov.relations.supported + sum(dat$beta.supp != "none")

  # p <- plot_community_effects(dat = dat, min.y = min(dat$beta.lo), max.y = max(dat$beta.hi),
  #                             pnam = "beta", vnam = cov.labs[cov])
  # assign(str_c("p_", cov), p)
  
  dat1 <- dat %>% filter(Spp %in% spp.plot1)
  p <- plot_community_effects(dat = dat1, min.y = min(dat$beta.lo), max.y = max(dat$beta.hi),
                              pnam = "beta", vnam = cov.labs[cov])
  assign(str_c("p1_", cov), p)
  
  dat2 <- dat %>% filter(Spp %in% spp.plot2) %>%
    mutate(index = index - min(index) + 1)
  p <- plot_community_effects(dat = dat2, min.y = min(dat$beta.lo), max.y = max(dat$beta.hi),
                              pnam = "beta", vnam = cov.labs[cov])
  assign(str_c("p2_", cov), p)
}

# Management effects #
p <- ggdraw() + 
  draw_plot(p1_TrailTotm,           x = 0.05,      y = 0, width = 0.3166667, height = 1) +
  draw_plot(p1_RoadTotm,            x = 0.3666667, y = 0, width = 0.3166667, height = 1) +
  draw_plot(p1_Prp_MotRestricted,   x = 0.6833333, y = 0, width = 0.3166667, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0.5)

save_plot("Figure_Managmt_cov_effects_set1.jpg", p, ncol = 2, nrow = 4.5, dpi = 300)

p <- ggdraw() + 
  draw_plot(p2_TrailTotm,           x = 0.05,      y = 0, width = 0.3166667, height = 1) +
  draw_plot(p2_RoadTotm,            x = 0.3666667, y = 0, width = 0.3166667, height = 1) +
  draw_plot(p2_Prp_MotRestricted,   x = 0.6833333, y = 0, width = 0.3166667, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0.5)

save_plot("Figure_Managmt_cov_effects_set2.jpg", p, ncol = 2, nrow = 4.5, dpi = 300)

# Human mobility #
p <- ggdraw() + 
  draw_plot(p1_HumanPresence,     x = 0.05,   y = 0, width = 0.2375, height = 1) +
  draw_plot(p1_LogTrafficNoZeros, x = 0.2875, y = 0, width = 0.2375, height = 1) +
  draw_plot(p1_Speed,             x = 0.5250, y = 0, width = 0.2375, height = 1) +
  draw_plot(p1_Speed2,            x = 0.7625, y = 0, width = 0.2375, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0)

save_plot("Figure_Human_cov_effects_set1.jpg", p, ncol = 3, nrow = 4.5, dpi = 300)

p <- ggdraw() + 
  draw_plot(p2_HumanPresence,     x = 0.05,   y = 0, width = 0.2375, height = 1) +
  draw_plot(p2_LogTrafficNoZeros, x = 0.2875, y = 0, width = 0.2375, height = 1) +
  draw_plot(p2_Speed,             x = 0.5250, y = 0, width = 0.2375, height = 1) +
  draw_plot(p2_Speed2,            x = 0.7625, y = 0, width = 0.2375, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0)

save_plot("Figure_Human_cov_effects_set2.jpg", p, ncol = 3, nrow = 4.5, dpi = 300)

# Habitat #
p <- ggdraw() +
  draw_plot(p1_Shrubland,       x = 0.05,      y = 0, width = 0.1357143, height = 1) +
  draw_plot(p1_ConiferForest,   x = 0.1857143, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p1_Aspen,           x = 0.3214286, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p1_OakWoodland,     x = 0.4571429, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p1_GrasslandMeadow, x = 0.5928571, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p1_Mesic,           x = 0.7285714, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p1_Alpine,          x = 0.8642857, y = 0, width = 0.1357143, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0)

save_plot("Figure_Habitat_cov_effects_set1.jpg", p, ncol = 5, nrow = 4.5, dpi = 300)

p <- ggdraw() +
  draw_plot(p2_Shrubland,       x = 0.05,      y = 0, width = 0.1357143, height = 1) +
  draw_plot(p2_ConiferForest,   x = 0.1857143, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p2_Aspen,           x = 0.3214286, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p2_OakWoodland,     x = 0.4571429, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p2_GrasslandMeadow, x = 0.5928571, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p2_Mesic,           x = 0.7285714, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p2_Alpine,          x = 0.8642857, y = 0, width = 0.1357143, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0)

save_plot("Figure_Habitat_cov_effects_set2.jpg", p, ncol = 5, nrow = 4.5, dpi = 300)
