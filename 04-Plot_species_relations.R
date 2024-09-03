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
source(str_c(git.repo, "Cluster_sizes.R"))
#______________________________________#

spp.plot <- R.utils::loadObject("data/Spp_results") # Retrieve species to plot
spp.names <- Spp_list$Species
names(spp.names) <- Spp_list$BirdCode
spp.names <- spp.names[spp.plot]
for(sp in 1:length(spp.names)) spp.names[sp] <- str_replace(spp.names[sp], "'", "\\\\'")

source(str_c(git.repo, "Tabulate_species_plotting_values.R"))

# Standard plots #
v.stub <- c("trail", "road", "ohv", "speed")
v.clabs <- c("TrailDensity", "RoadDensity", "OHVRestrict", "Speed")
v.xlabs <- c("Trail density", "Road density", "Prp. trails no OHV", "Traffic speed")
names(v.clabs) <- names(v.xlabs) <- v.stub
for(sp in spp.plot) {
  ymax.str <- str_c("max(c(dat.plt.trail$", sp, ".hi, dat.plt.road$", sp, ".hi, dat.plt.ohv$", sp,
                    ".hi, dat.plt.traffic$", sp, ".hi, dat.plt.speed$", sp, ".hi))")
  ymax <- eval(parse(text = ymax.str))
  ymin.str <- str_c("min(c(dat.plt.trail$", sp, ".lo, dat.plt.road$", sp, ".lo, dat.plt.ohv$", sp,
                    ".lo, dat.plt.traffic$", sp, ".lo, dat.plt.speed$", sp, ".lo))")
  ymin <- eval(parse(text = ymin.str))
  y.marg <- (ymax - ymin) / 100
  ymax <- ymax + y.marg
  ymin <- ymin - y.marg
  for(v in v.stub) {
    p.str <- str_c(str_c("p", v, sp, sep = "."),
                   "<- ggplot(dat = dat.plt.", v,", aes(x = ", v.clabs[v],", y = ", sp, ".md)) +",
                   "geom_ribbon(aes(ymin = ", sp, ".lo, ymax = ", sp, ".hi), linewidth = 0.5, alpha = 0.3) +",
                   "geom_line(linewidth = 1) +",
                   "xlab('", v.xlabs[v], "') + ylab(NULL) +",
                   "ylim(", ymin,", ", ymax+1, ")")
    eval(parse(text = p.str))
  }
}

# Traffic plots
n.breaks <- 5
x.breaks <- seq(min(dat.plt.traffic$LogTrafficVol), max(dat.plt.traffic$LogTrafficVol), length.out = n.breaks)
x.labs.vals <- exp(x.breaks)
ndig <- c(3, 2, 0, -2, -4)
x.labs.vals <- as.character(round(x.labs.vals, digits = ndig))
x.labs.vals[1] <- "0"
dat.plt.HP0 <- (dat.plt.traffic %>% filter(HumanPresence == 0))
dat.plt.HP1 <- (dat.plt.traffic %>% filter(HumanPresence == 1))
for(sp in spp.plot) {
  ymax.str <- str_c("max(dat.plt.trail$", sp, ".hi, dat.plt.road$", sp, "hi, dat.plt.ohv$", sp,
                    ".hi, dat.plt.traffic$", sp, ".hi, dat.plt.speed$", sp, ".hi)")
  ymax <- eval(parse(text = ymax.str))
  ymin.str <- str_c("min(dat.plt.trail$", sp, ".lo, dat.plt.road$", sp, "lo, dat.plt.ohv$", sp,
                    ".lo, dat.plt.traffic$", sp, ".lo, dat.plt.speed$", sp, ".lo)")
  ymin <- eval(parse(text = ymin.str))
  y.marg <- (ymax - ymin) / 100
  ymax <- ymax + y.marg
  ymin <- ymin - y.marg
  p.str <- str_c(str_c("p.traffic", sp, sep = "."),
                 "<- ggplot(dat = dat.plt.traffic, aes(x = LogTrafficVol, y = ", sp, ".md)) +",
                 "geom_ribbon(data = dat.plt.HP1, aes(x = LogTrafficVol, ymin = ", sp,
                 ".lo, ymax = ", sp, ".hi), linewidth = 0.5, alpha = 0.3) +",
                 "geom_line(data = dat.plt.HP1, aes(x = LogTrafficVol, y = ", sp,
                 ".md), linewidth = 1) +",
                 "geom_errorbar(data = dat.plt.HP0, aes(x = LogTrafficVol,",
                 "ymin = ", sp, ".lo, ymax = ", sp,".hi), linewidth = 1) +",
                 "geom_point(data = dat.plt.HP0, aes(x = LogTrafficVol, y = ", sp,
                 ".md), size = 3) +",
                 "scale_x_continuous(breaks = x.breaks, labels = x.labs.vals)+",
                 "xlab('Traffic volume') + ylab(NULL)+",
                 "ylim(", ymin,", ", ymax, ")")
  eval(parse(text = p.str))
}

for(sp in spp.plot) {
  p.str <- str_c("p <- ggdraw() +",
    "draw_plot(p.trail.", sp, ",     x = 0.0500000, y = 0.475, width = 0.3166667, height = 0.475) +",
    "draw_plot(p.road.", sp, ",      x = 0.3666667, y = 0.475, width = 0.3166667, height = 0.475) +",
    "draw_plot(p.ohv.", sp, ",       x = 0.6833333, y = 0.475, width = 0.3166667, height = 0.475) +",
    "draw_plot(p.traffic.", sp, ",   x = 0.050,     y = 0,     width = 0.475,     height = 0.475) +",
    "draw_plot(p.speed.", sp, ",     x = 0.525,     y = 0,     width = 0.475,     height = 0.475) +",
    "draw_plot_label('Birds per km sqr', x = 0, y = 0.5, size = 20, angle = 90, hjust = 0.5) +",
    "draw_plot_label('", spp.names[sp], "', x = 0.5, y = 1, size = 20, hjust = 0.5)")
  eval(parse(text = p.str))
  
  save_plot(str_c("species_relations/", sp, "_relations.jpg"), p, ncol = 1.5, nrow = 2, dpi = 200)
}

## Compare total and unexplained series ##
v.stub <- c("trail", "road", "ohv")
v.clabs <- c("TrailDensity", "RoadDensity", "OHVRestrict")
v.xlabs <- c("Trail density", "Road density", "Prp. trails no OHV")
names(v.clabs) <- names(v.xlabs) <- v.stub
for(sp in spp.plot) {
  ymax.str <- str_c("max(c(dat.plt.trail.series$", sp, ".hi, dat.plt.road.series$", sp, ".hi, dat.plt.ohv.series$", sp, ".hi))")
  ymax <- eval(parse(text = ymax.str))
  ymin.str <- str_c("min(c(dat.plt.trail.series$", sp, ".lo, dat.plt.road.series$", sp, ".lo, dat.plt.ohv.series$", sp, ".lo))")
  ymin <- eval(parse(text = ymin.str))
  y.marg <- (ymax - ymin) / 100
  ymax <- ymax + y.marg
  ymin <- ymin - y.marg
  for(v in v.stub) {
    p.str <- str_c(str_c("p", v, sp, sep = "."),
                   "<- ggplot(dat = dat.plt.", v,".series, aes(x = ", v.clabs[v],", y = ", sp, ".md)) +",
                   "geom_ribbon(aes(ymin = ", sp, ".lo, ymax = ", sp, ".hi, linetype = Series, alpha = Series), linewidth = 0.5) +",
                   "geom_line(aes(linetype = Series), linewidth = 1) +",
                   "scale_linetype_manual(values = c('dashed', 'solid')) +",
                   "scale_alpha_manual(values = c(0.1, 0.3)) +",
                   "xlab('", v.xlabs[v], "') + ylab(NULL) +",
                   "ylim(", ymin,", ", ymax+1, ")")
    if(v != "trail") {
      p.str <- str_c(p.str, "+guides(linetype = 'none', alpha = 'none')")
    }
    eval(parse(text = p.str))
  }
  
  p.str <- str_c("p <- ggdraw() +",
                 "draw_plot(p.trail.", sp, ",     x = 0.050, y = 0.475, width = 0.95, height = 0.475) +",
                 "draw_plot(p.road.", sp, ",      x = 0.050, y = 0,     width = 0.475, height = 0.475) +",
                 "draw_plot(p.ohv.", sp, ",       x = 0.525, y = 0,     width = 0.475, height = 0.475) +",
                 "draw_plot_label('Birds per km sqr', x = 0, y = 0.5, size = 20, angle = 90, hjust = 0.5) +",
                 "draw_plot_label('", spp.names[sp], "', x = 0.5, y = 1, size = 20, hjust = 0.5)")
  eval(parse(text = p.str))
  
  save_plot(str_c("species_relations/", sp, "_relations_series.jpg"), p, ncol = 1.5, nrow = 2, dpi = 200)
}
