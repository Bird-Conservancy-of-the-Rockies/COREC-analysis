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
mod <- R.utils::loadObject("mod_community")
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

# Set 2 #
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

# ## 5-year trend ##
# # Summary table of trends #
# col.nams <- c("Species", "Treatment_type", "Untreated", "Treated", "p")
# Spp_trends <- matrix(NA, nrow = 0, ncol = 5,
#                      dimnames = list(NULL, col.nams))
# 
# for(species in Spp) {
#   ind.spp <- which(Spp == species)
# 
#   ind.EA <- which(str_detect(grid.list, "NFWF-EA"))
#   ind.yrs <- which(year >= 2018)
#   ind.EA <- which(Cov_grid[,"gridID"] %in% ind.EA & Cov_grid[,"YearID"] %in% ind.yrs)
#   ind.GZ <- which(str_detect(grid.list, "NFWF-GZ"))
#   ind.GZ <- which(Cov_grid[,"gridID"] %in% ind.GZ & Cov_grid[,"YearID"] %in% ind.yrs)
#   
#   dplt.view_Trt_trend.fn(ind.EA = ind.EA, ind.GZ = ind.GZ, nsamp = nsamp,
#                          alpha0 = mod$mcmcOutput$alpha0[,ind.spp],
#                          alphaVec = mod$mcmcOutput$alphaVec[,ind.spp,],
#                          X.alpha = X.alpha,
#                          DELTA0 = mod$mcmcOutput$DELTA0[,ind.spp],
#                          DELTAVec = mod$mcmcOutput$DELTAVec[,ind.spp,],
#                          X.DELTA = X.DELTA,
#                          ETA0 = mod$mcmcOutput$ETA0[,ind.spp],
#                          ETAVec = mod$mcmcOutput$ETAVec[,ind.spp,],
#                          X.ETA = X.ETA,
#                          beta0 = mod$mcmcOutput$beta0[,ind.spp],
#                          betaVec = mod$mcmcOutput$betaVec[,ind.spp,],
#                          X.beta = X.beta,
#                          delta0 = mod$mcmcOutput$delta0[,ind.spp],
#                          deltaVec = mod$mcmcOutput$deltaVec[,ind.spp,],
#                          X.delta = X.delta)
#   assign(str_c("trend.EA.", species), trend.EA)
#   assign(str_c("trend.GZ.", species), trend.GZ)
#   assign(str_c("dat.plt.EA.", species), dat.plt.EA)
#   assign(str_c("dat.plt.GZ.", species), dat.plt.GZ)
#   trend.EA <- c(species, "Conservation easement", trend.EA)
#   trend.GZ <- c(species, "Grazing management", trend.GZ)
#   Spp_trends <- rbind(Spp_trends, trend.EA, trend.GZ)
# }
# 
# write.csv(Spp_trends, str_c("Treatment_effects_trend_community_model_", data.set, ".csv"), row.names = F)
# 
# # Plot trends where differences are supported #
# Spp_trends <- data.frame(Spp_trends) %>%
#   mutate(p = as.numeric(p))
# Spp.plot <- Spp_trends %>% filter(p < 0.1 | p > 0.9) %>%
#   pull(Species) %>% unique
# 
# for(species in Spp.plot) {
#   dat.plt.EA <- eval(as.name(str_c("dat.plt.EA.", species))) %>%
#     rename(Treatment = Trt_EA) %>%
#     mutate(Treatment = as.character(Treatment)) %>%
#     mutate(Treatment = factor(Treatment, levels = c(0, 100)))
#   p <- ggplot(dat.plt.EA, aes(x = Year, y = N.md)) +
#     geom_line(aes(color = Treatment), linewidth = 2) +
#     geom_errorbar(aes(ymin = N.lo, ymax = N.hi, color = Treatment), width = 0.1,
#                   position = position_dodge(width = 0.1)) +
#     geom_point(aes(color = Treatment), size = 5,
#                position = position_dodge(width = 0.1)) +
#     scale_color_manual(values = c("#999999", "#000000")) +
#     ylab("Bird density (per km sqr)") +
#     theme(axis.text.x = element_text(size=25),
#           axis.text.y = element_text(size=25))
#   save_plot(str_c("Plot_EA_trend_effect_", species, "_", data.set, ".jpg"), p, ncol = 1, nrow = 1, dpi = 300)
#   
#   dat.plt.GZ <- eval(as.name(str_c("dat.plt.GZ.", species))) %>%
#     rename(Treatment = Trt_GZ) %>%
#     mutate(Treatment = as.character(Treatment)) %>%
#     mutate(Treatment = factor(Treatment, levels = c(0, 100)))
#   p <- ggplot(dat.plt.GZ, aes(x = Year, y = N.md)) +
#     geom_line(aes(color = Treatment), linewidth = 2) +
#     geom_errorbar(aes(ymin = N.lo, ymax = N.hi, color = Treatment), width = 0.1,
#                   position = position_dodge(width = 0.1)) +
#     geom_point(aes(color = Treatment), size = 5,
#                position = position_dodge(width = 0.1)) +
#     scale_color_manual(values = c("#999999", "#000000")) +
#     ylab("Bird density (per km sqr)") +
#     theme(axis.text.x = element_text(size=25),
#           axis.text.y = element_text(size=25))
#   save_plot(str_c("Plot_GZ_trend_effect_", species, "_", data.set, ".jpg"), p, ncol = 1, nrow = 1, dpi = 300)
# }

