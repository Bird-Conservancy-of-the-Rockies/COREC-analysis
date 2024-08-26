library(mcmcOutput)
library(stringr)
library(dplyr)
library(R.utils)
library(ggplot2)
library(cowplot)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load("data/Data_compiled.RData")

#__________ Script inputs _____________#
mod.nam <- "path"
git.repo <- "COREC-analysis/"
mod <- R.utils::loadObject(str_c("mod_", mod.nam))
nsims <- dim(mod$mcmcOutput)[1]
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
#______________________________________#

## Stratum covariate values ##
source(str_c(git.repo, "Path_analysis_source.R"))

######################
# Plot total effects #
######################

Spp_results <- R.utils::loadObject("data/Spp_results")
tab.effect.total <- read.csv("data/Spp_total_management_effects.csv", header = TRUE, stringsAsFactors = FALSE)
dat.plot <- tab.effect.total %>% filter(Spp %in% Spp_results) %>%
  mutate(index = rev(1:n()))

## Plot effects ##
# Trail density #
min.y <- min(dat.plot$HighTrail.lo)
max.y <- max(dat.plot$HighTrail.hi)
p.trail <- ggplot(dat = dat.plot, aes(x = index, y = HighTrail.md)) +
  geom_errorbar(aes(ymin = HighTrail.lo, ymax = HighTrail.hi, color = HighTrail.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = HighTrail.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot$index, labels = dat.plot$Spp, expand=c(0, 1)) +
  xlab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y)) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  ylab("Trail density") +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  guides(color = "none")

# OHV restriction #
min.y <- min(dat.plot$NoOHV.lo)
max.y <- max(dat.plot$NoOHV.hi)
p.OHV <- ggplot(dat = dat.plot, aes(x = index, y = NoOHV.md)) +
  geom_errorbar(aes(ymin = NoOHV.lo, ymax = NoOHV.hi, color = NoOHV.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = NoOHV.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot$index, labels = dat.plot$Spp, expand=c(0, 1)) +
  xlab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y)) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  ylab("OHV restriction") +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  guides(color = "none")

# Horse restriction #
min.y <- min(dat.plot$NoHorse.lo)
max.y <- max(dat.plot$NoHorse.hi)
p.horse <- ggplot(dat = dat.plot, aes(x = index, y = NoHorse.md)) +
  geom_errorbar(aes(ymin = NoHorse.lo, ymax = NoHorse.hi, color = NoHorse.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = NoHorse.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot$index, labels = dat.plot$Spp, expand=c(0, 1)) +
  xlab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y)) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  ylab("Horse restriction") +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  guides(color = "none")

# Road density #
min.y <- min(dat.plot$HighRoad.lo)
max.y <- max(dat.plot$HighRoad.hi)
p.road <- ggplot(dat = dat.plot, aes(x = index, y = HighRoad.md)) +
  geom_errorbar(aes(ymin = HighRoad.lo, ymax = HighRoad.hi, color = HighRoad.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = HighRoad.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot$index, labels = dat.plot$Spp, expand=c(0, 1)) +
  xlab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y)) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  ylab("Road density") +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  guides(color = "none")
rm(min.y, max.y)

p <- ggdraw() + 
  draw_plot(p.trail, x = 0.05,   y = 0, width = 0.2375, height = 1) +
  draw_plot(p.road,  x = 0.2875, y = 0, width = 0.2375, height = 1) +
  draw_plot(p.horse, x = 0.5250, y = 0, width = 0.2375, height = 1) +
  draw_plot(p.OHV,   x = 0.7625, y = 0, width = 0.2375, height = 1) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0)

save_plot("Figure_Management_effects_total.jpg", p, ncol = 3, nrow = 7, dpi = 300)