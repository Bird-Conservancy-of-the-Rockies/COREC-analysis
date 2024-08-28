library(mcmcOutput)
library(stringr)
library(dplyr)
library(R.utils)
library(ggplot2)
library(cowplot)
library(FunctionsBCR)
theme_set(theme_bw())

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

source(str_c(git.repo, "Functions_source.R"))

################################
# Tabulate values for plotting #
################################

Spp_results <- spp.plot <- R.utils::loadObject("data/Spp_results")
tab.effect.total <- read.csv("data/Spp_total_management_effects.csv", header = TRUE, stringsAsFactors = FALSE)
dat.plot.total <- tab.effect.total %>% filter(Spp %in% Spp_results) %>%
  mutate(index = rev(1:n()))
tab.effect.explained <- read.csv("data/Spp_explained_management_effects.csv", header = TRUE, stringsAsFactors = FALSE)
dat.plot.explained <- tab.effect.explained %>% filter(Spp %in% Spp_results) %>%
  mutate(index = rev(1:n()))
tab.effect.unexplained <- read.csv("data/Spp_unexplained_management_effects.csv", header = TRUE, stringsAsFactors = FALSE)
dat.plot.unexplained <- tab.effect.unexplained %>% filter(Spp %in% Spp_results) %>%
  mutate(index = rev(1:n()))

#################
# Trail density #
#################
min.y <- min(dat.plot.total$HighTrail.lo, dat.plot.explained$HighTrail.lo, dat.plot.unexplained$HighTrail.lo)
max.y <- max(dat.plot.total$HighTrail.hi, dat.plot.explained$HighTrail.hi, dat.plot.unexplained$HighTrail.hi)
breaks.y <- seq(min.y, max.y, length.out = 4)[-c(1, 4)] %>% round(digits = 1)

p.trail.total <- ggplot(dat = dat.plot.total, aes(x = index, y = HighTrail.md)) +
  geom_errorbar(aes(ymin = HighTrail.lo, ymax = HighTrail.hi, color = HighTrail.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = HighTrail.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.total$index, labels = dat.plot.total$Spp, expand=c(0, 1)) +
  xlab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y), breaks = breaks.y) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  ylab(NULL) +
  #theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  theme(plot.margin = margin(0,0,0,0)) +
  guides(color = "none") +
  ggtitle("Total")

p.trail.explained <- ggplot(dat = dat.plot.explained, aes(x = index, y = HighTrail.md)) +
  geom_errorbar(aes(ymin = HighTrail.lo, ymax = HighTrail.hi, color = HighTrail.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = HighTrail.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.explained$index, expand=c(0, 1)) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y), breaks = breaks.y) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0,0,0,0)) +
  theme(axis.ticks.length.y = unit(0, "pt")) +
  guides(color = "none") +
  ggtitle("Explained")

p.trail.unexplained <- ggplot(dat = dat.plot.unexplained, aes(x = index, y = HighTrail.md)) +
  geom_errorbar(aes(ymin = HighTrail.lo, ymax = HighTrail.hi, color = HighTrail.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = HighTrail.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.unexplained$index, expand=c(0, 1)) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y), breaks = breaks.y) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0,0,0,0)) +
  theme(axis.ticks.length.y = unit(0, "pt")) +
  guides(color = "none") +
  ggtitle("Unexplained")

p.trail <- ggdraw() + 
  draw_plot(p.trail.total,       x = 0,    y = 0.05, width = 0.5,  height = 0.95) +
  draw_plot(p.trail.explained,   x = 0.5,  y = 0.05, width = 0.25, height = 0.95) +
  draw_plot(p.trail.unexplained, x = 0.75, y = 0.05, width = 0.25, height = 0.95) +
  draw_plot_label("Trail density effect", x = 0.5, y = 0.03, size = 30, hjust = 0.5, vjust = 1)

################
# Road density #
################
min.y <- min(dat.plot.total$HighRoad.lo, dat.plot.explained$HighRoad.lo, dat.plot.unexplained$HighRoad.lo)
max.y <- max(dat.plot.total$HighRoad.hi, dat.plot.explained$HighRoad.hi, dat.plot.unexplained$HighRoad.hi)
breaks.y <- seq(min.y, max.y, length.out = 4)[-c(1, 4)] %>% round(digits = 1)

p.road.total <- ggplot(dat = dat.plot.total, aes(x = index, y = HighRoad.md)) +
  geom_errorbar(aes(ymin = HighRoad.lo, ymax = HighRoad.hi, color = HighRoad.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = HighRoad.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.total$index, labels = dat.plot.total$Spp, expand=c(0, 1)) +
  xlab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y), breaks = breaks.y) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  ylab(NULL) +
  #theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  theme(plot.margin = margin(0,0,0,0)) +
  guides(color = "none") +
  ggtitle("Total")

p.road.explained <- ggplot(dat = dat.plot.explained, aes(x = index, y = HighRoad.md)) +
  geom_errorbar(aes(ymin = HighRoad.lo, ymax = HighRoad.hi, color = HighRoad.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = HighRoad.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.explained$index, expand=c(0, 1)) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y), breaks = breaks.y) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0,0,0,0)) +
  theme(axis.ticks.length.y = unit(0, "pt")) +
  guides(color = "none") +
  ggtitle("Explained")

p.road.unexplained <- ggplot(dat = dat.plot.unexplained, aes(x = index, y = HighRoad.md)) +
  geom_errorbar(aes(ymin = HighRoad.lo, ymax = HighRoad.hi, color = HighRoad.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = HighRoad.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.unexplained$index, expand=c(0, 1)) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y), breaks = breaks.y) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0,0,0,0)) +
  theme(axis.ticks.length.y = unit(0, "pt")) +
  guides(color = "none") +
  ggtitle("Unexplained")

p.road <- ggdraw() + 
  draw_plot(p.road.total,       x = 0,    y = 0.05, width = 0.5,  height = 0.95) +
  draw_plot(p.road.explained,   x = 0.5,  y = 0.05, width = 0.25, height = 0.95) +
  draw_plot(p.road.unexplained, x = 0.75, y = 0.05, width = 0.25, height = 0.95) +
  draw_plot_label("Road density effect", x = 0.5, y = 0.03, size = 30, hjust = 0.5, vjust = 1)

###################
# OHV Restriction #
###################
min.y <- min(dat.plot.total$NoOHV.lo, dat.plot.explained$NoOHV.lo, dat.plot.unexplained$NoOHV.lo)
max.y <- max(dat.plot.total$NoOHV.hi, dat.plot.explained$NoOHV.hi, dat.plot.unexplained$NoOHV.hi)
breaks.y <- seq(min.y, max.y, length.out = 4)[-c(1, 4)] %>% round(digits = 1)

p.OHV.total <- ggplot(dat = dat.plot.total, aes(x = index, y = NoOHV.md)) +
  geom_errorbar(aes(ymin = NoOHV.lo, ymax = NoOHV.hi, color = NoOHV.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = NoOHV.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.total$index, labels = dat.plot.total$Spp, expand=c(0, 1)) +
  xlab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y), breaks = breaks.y) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  ylab(NULL) +
  #theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  theme(plot.margin = margin(0,0,0,0)) +
  guides(color = "none") +
  ggtitle("Total")

p.OHV.explained <- ggplot(dat = dat.plot.explained, aes(x = index, y = NoOHV.md)) +
  geom_errorbar(aes(ymin = NoOHV.lo, ymax = NoOHV.hi, color = NoOHV.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = NoOHV.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.explained$index, expand=c(0, 1)) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y), breaks = breaks.y) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0,0,0,0)) +
  theme(axis.ticks.length.y = unit(0, "pt")) +
  guides(color = "none") +
  ggtitle("Explained")

p.OHV.unexplained <- ggplot(dat = dat.plot.unexplained, aes(x = index, y = NoOHV.md)) +
  geom_errorbar(aes(ymin = NoOHV.lo, ymax = NoOHV.hi, color = NoOHV.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = NoOHV.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.unexplained$index, expand=c(0, 1)) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(lim = c(min.y, max.y), breaks = breaks.y) +
  scale_color_manual(values = c("#0072B2", "#000000", "#D55E00")) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.ticks.y=element_blank()) +
  theme(plot.margin = margin(0,0,0,0)) +
  theme(axis.ticks.length.y = unit(0, "pt")) +
  guides(color = "none") +
  ggtitle("Unxplained")

p.OHV <- ggdraw() + 
  draw_plot(p.OHV.total,       x = 0,    y = 0.05, width = 0.5,  height = 0.95) +
  draw_plot(p.OHV.explained,   x = 0.5,  y = 0.05, width = 0.25, height = 0.95) +
  draw_plot(p.OHV.unexplained, x = 0.75, y = 0.05, width = 0.25, height = 0.95) +
  draw_plot_label("OHV restriction effect", x = 0.5, y = 0.03, size = 30, hjust = 0.5, vjust = 1)

#######################
# Put it all together #
#######################

p <- ggdraw() + 
  draw_plot(p.trail, x = 0.0300000, y = 0, width = 0.3166667, height = 0.99) +
  draw_plot(p.road,  x = 0.3533333, y = 0, width = 0.3166667, height = 0.99) +
  draw_plot(p.OHV,   x = 0.6766667, y = 0, width = 0.3166667, height = 0.99) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0.5)

save_plot("Figure_Management_effects.jpg", p, ncol = 3.5, nrow = 7, dpi = 300)
