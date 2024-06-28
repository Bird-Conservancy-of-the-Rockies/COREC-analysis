library(stringr)
library(tidyr)
library(dplyr)
library(mcmcOutput)
library(ggplot2)
library(cowplot)
library(FunctionsBCR)
theme_set(theme_bw())

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load(str_c("Data_compiled.RData"))

#_____ Script inputs _____#
git.repo <- "COREC-analysis/"
mod.nam <- "community"
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

## Identify species to plot ##
spp.plot <- c()
for(cov in covs) {
  beta.ind <- which(dimnames(X.beta)[[2]] == cov)
  beta <- mod$mcmcOutput$betaVec[,,beta.ind]
  dat <- tabulate_community_effect(Spp, beta)
  spp.plot <- c(spp.plot, dat$Spp[which(dat$beta.supp != "none")]) %>% unique
}
  
## Generate plots ##
for(cov in covs) {
  beta.ind <- which(dimnames(X.beta)[[2]] == cov)
  beta <- mod$mcmcOutput$betaVec[,,beta.ind]
  dat <- tabulate_community_effect(Spp, beta) %>%
    filter(Spp %in% spp.plot) %>%
    mutate(index = rank(index))
  
  p <- plot_community_effects(dat = dat, min.y = min(dat$beta.lo), max.y = max(dat$beta.hi),
                              pnam = "beta", vnam = cov)
  assign(str_c("p_", cov), p)
}

# Management effects #
p <- ggdraw() + 
  draw_plot(p_TrailTotm,           x = 0.05,   y = 0, width = 0.2375, height = 1) +
  draw_plot(p_RoadTotm,            x = 0.2875, y = 0, width = 0.2375, height = 1) +
  draw_plot(p_Prp_HorseRestricted, x = 0.5250, y = 0, width = 0.2375, height = 1) +
  draw_plot(p_Prp_MotRestricted,   x = 0.7625, y = 0, width = 0.2375, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0)

save_plot("Figure_Managmt_effects.jpg", p, ncol = 3, nrow = 5.5, dpi = 300)

# Human mobility #
p <- ggdraw() + 
  draw_plot(p_HumanPresence,     x = 0.05,      y = 0, width = 0.1357143, height = 1) +
  draw_plot(p_LogTrafficNoZeros, x = 0.1857143, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p_Speed,             x = 0.3214286, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p_TOD_mean,          x = 0.4571429, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p_TOD_mean2,         x = 0.5928571, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p_Traffic_DOY_mn,    x = 0.7285714, y = 0, width = 0.1357143, height = 1) +
  draw_plot(p_Traffic_DOY_mn2,   x = 0.8642857, y = 0, width = 0.1357143, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0)

save_plot("Figure_Human_effects.jpg", p, ncol = 5, nrow = 5.5, dpi = 300)

# Habitat #
p <- ggdraw() + 
  draw_plot(p_Shrubland,       x = 0.05,    y = 0, width = 0.11875, height = 1) +
  draw_plot(p_PinyonJuniper,   x = 0.16875, y = 0, width = 0.11875, height = 1) +
  draw_plot(p_ConiferForest,   x = 0.28750, y = 0, width = 0.11875, height = 1) +
  draw_plot(p_Aspen,           x = 0.40625, y = 0, width = 0.11875, height = 1) +
  draw_plot(p_OakWoodland,     x = 0.52500, y = 0, width = 0.11875, height = 1) +
  draw_plot(p_GrasslandMeadow, x = 0.64375, y = 0, width = 0.11875, height = 1) +
  draw_plot(p_Mesic,           x = 0.76250, y = 0, width = 0.11875, height = 1) +
  draw_plot(p_Alpine,          x = 0.88125, y = 0, width = 0.11875, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0)

save_plot("Figure_Habitat_effects.jpg", p, ncol = 5, nrow = 5.5, dpi = 300)
