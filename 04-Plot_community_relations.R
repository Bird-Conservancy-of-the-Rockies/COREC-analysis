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
source(str_c(git.repo, "Functions_source.R"))
#______________________________________#

#########################
# Define species groups #
#########################

spp_assignments <- read.csv("data/Species_list_assigned.csv", header = TRUE, stringsAsFactors = FALSE)
groups <- list(
  community = Spp,
  migratory = spp_assignments %>% filter(Migratory) %>% pull(BirdCode),
  large = spp_assignments %>% filter(Mass > median(Mass)) %>% pull(BirdCode),
  HumComm = spp_assignments %>% filter(HumanCommensal) %>% pull(BirdCode),
  insectivore = spp_assignments %>% filter(Insectivore) %>% pull(BirdCode),
  ground = spp_assignments %>% filter(Ground) %>% pull(BirdCode),
  SGCN = spp_assignments %>% filter(SGCN) %>% pull(BirdCode)
)

################################
# Tabulate values for plotting #
################################
series <- TRUE # Turns on component of 'Path_analysis_source.R' that generates values to plot here.
source(str_c(git.repo, "Path_analysis_source.R"))

# Trail density #
dat.plt = data.frame(TrailDensity = x.trail * cov.sd["TrailTotm"] + cov.mn["TrailTotm"])

cov.ind <- which(dimnames(X.beta)[[2]] %in% c("TrailTotm", "Prp_MotRestricted", "Prp_HorseRestricted"))
for(g in names(groups)) {
  spp.ind <- which(Spp %in% groups[[g]])
  D <- matrix(NA, nrow = nsims, ncol = nrow(dat.plt))
  for(i in 1:nrow(dat.plt)) {
    X.pred <- dat.plt %>% select(TrailTotm, Prp_MotRestricted, Prp_HorseRestricted) %>%
      slice(i) %>% data.matrix
    X.pred <- matrix(rep(X.pred, nsims), nrow = length(cov.ind), ncol = nsims) %>% t()
    N <- N.pred.calc(X.pred, cov.ind)
    D[,i] <- apply(N, 1, HillShannon)
  }
  dat.plt$v.md <- apply(D, 2, median)
  dat.plt$v.lo <- apply(D, 2, function(x) quantile(x, prob = 0.1, type = 8))
  dat.plt$v.hi <- apply(D, 2, function(x) quantile(x, prob = 0.9, type = 8))
  names(dat.plt)[which(names(dat.plt) %in% c("v.md", "v.lo", "v.hi"))] <-
    str_c(g, c(".md", ".lo", ".hi"))
}
write.csv(dat.plt, "data/Dat_plot_community_trail_density.csv", row.names = FALSE)


##################
# Generate plots #
##################

## Trail density ##
dat.plt <- read.csv("data/Dat_plot_community_trail_density.csv", header = TRUE, stringsAsFactors = FALSE)

# Community #
p.trail.community <- ggplot(dat = dat.plt, aes(x = index, y = community.md)) +
  geom_ribbon() +
  geom_line()






#################
# Trail density #
#################
min.y <- min(dat.plot.total$HighTrail.lo, dat.plot.explained$HighTrail.lo, dat.plot.unexplained.TrailTotm$beta.lo)
max.y <- max(dat.plot.total$HighTrail.hi, dat.plot.explained$HighTrail.hi, dat.plot.unexplained.TrailTotm$beta.hi)
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

p.trail.unexplained <- ggplot(dat = dat.plot.unexplained.TrailTotm, aes(x = index, y = beta.md)) +
  geom_errorbar(aes(ymin = beta.lo, ymax = beta.hi, color = beta.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = beta.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.unexplained.TrailTotm$index, expand=c(0, 1)) +
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
min.y <- min(dat.plot.total$HighRoad.lo, dat.plot.explained$HighRoad.lo, dat.plot.unexplained.RoadTotm$beta.lo)
max.y <- max(dat.plot.total$HighRoad.hi, dat.plot.explained$HighRoad.hi, dat.plot.unexplained.RoadTotm$beta.hi)
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

p.road.unexplained <- ggplot(dat = dat.plot.unexplained.RoadTotm, aes(x = index, y = beta.md)) +
  geom_errorbar(aes(ymin = beta.lo, ymax = beta.hi, color = beta.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = beta.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.unexplained.RoadTotm$index, expand=c(0, 1)) +
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
min.y <- min(dat.plot.total$NoOHV.lo, dat.plot.explained$NoOHV.lo, dat.plot.unexplained.Prp_MotRestricted$beta.lo)
max.y <- max(dat.plot.total$NoOHV.hi, dat.plot.explained$NoOHV.hi, dat.plot.unexplained.Prp_MotRestricted$beta.hi)
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

p.OHV.unexplained <- ggplot(dat = dat.plot.unexplained.Prp_MotRestricted, aes(x = index, y = beta.md)) +
  geom_errorbar(aes(ymin = beta.lo, ymax = beta.hi, color = beta.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = beta.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.unexplained.Prp_MotRestricted$index, expand=c(0, 1)) +
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

p.OHV <- ggdraw() + 
  draw_plot(p.OHV.total,       x = 0,    y = 0.05, width = 0.5,  height = 0.95) +
  draw_plot(p.OHV.explained,   x = 0.5,  y = 0.05, width = 0.25, height = 0.95) +
  draw_plot(p.OHV.unexplained, x = 0.75, y = 0.05, width = 0.25, height = 0.95) +
  draw_plot_label("OHV restriction effect", x = 0.5, y = 0.03, size = 30, hjust = 0.5, vjust = 1)

#####################
# Horse restriction #
#####################
min.y <- min(dat.plot.total$NoHorse.lo, dat.plot.explained$NoHorse.lo, dat.plot.unexplained.Prp_HorseRestricted$beta.lo)
max.y <- max(dat.plot.total$NoHorse.hi, dat.plot.explained$NoHorse.hi, dat.plot.unexplained.Prp_HorseRestricted$beta.hi)
breaks.y <- seq(min.y, max.y, length.out = 4)[-c(1, 4)] %>% round(digits = 1)

p.horse.total <- ggplot(dat = dat.plot.total, aes(x = index, y = NoHorse.md)) +
  geom_errorbar(aes(ymin = NoHorse.lo, ymax = NoHorse.hi, color = NoHorse.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = NoHorse.supp)) +
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

p.horse.explained <- ggplot(dat = dat.plot.explained, aes(x = index, y = NoHorse.md)) +
  geom_errorbar(aes(ymin = NoHorse.lo, ymax = NoHorse.hi, color = NoHorse.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = NoHorse.supp)) +
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

p.horse.unexplained <- ggplot(dat = dat.plot.unexplained.Prp_HorseRestricted, aes(x = index, y = beta.md)) +
  geom_errorbar(aes(ymin = beta.lo, ymax = beta.hi, color = beta.supp),
                linewidth = 1, width = 0) +
  geom_point(size = 2.5, aes(color = beta.supp)) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_x_continuous(breaks = dat.plot.unexplained.Prp_HorseRestricted$index, expand=c(0, 1)) +
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

p.horse <- ggdraw() + 
  draw_plot(p.horse.total,       x = 0,    y = 0.05, width = 0.5,  height = 0.95) +
  draw_plot(p.horse.explained,   x = 0.5,  y = 0.05, width = 0.25, height = 0.95) +
  draw_plot(p.horse.unexplained, x = 0.75, y = 0.05, width = 0.25, height = 0.95) +
  draw_plot_label("Horse restriction effect", x = 0.5, y = 0.03, size = 30, hjust = 0.5, vjust = 1)

#######################
# Put it all together #
#######################

p <- ggdraw() + 
  draw_plot(p.trail, x = 0.0300, y = 0, width = 0.24, height = 0.99) +
  draw_plot(p.road,  x = 0.2725, y = 0, width = 0.24, height = 0.99) +
  draw_plot(p.horse, x = 0.5150, y = 0, width = 0.24, height = 0.99) +
  draw_plot(p.OHV,   x = 0.7575, y = 0, width = 0.24, height = 0.99) +
  draw_plot_label("Species", x = 0, y = 0.5, size = 40, angle = 90, hjust = 0)

save_plot("Figure_Management_effects.jpg", p, ncol = 5, nrow = 7, dpi = 300)
