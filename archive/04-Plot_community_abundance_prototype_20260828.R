# 04-Plot_community_abundance.R
#
# Community-level abundance relationship plots paralleling the Hill-Shannon
# diversity relationship plots produced by 04-Plot_community_relations.R
# (manuscript Figs. 3-4), in response to the Editor/Reviewer 3 request
# (EAP25-0577, 2nd review) to report community-level abundance alongside
# diversity.
#
# Community (or species-group) abundance is derived by aggregating
# species-level predicted density (N.pred.calc output, birds/km^2, already
# incorporating each species' own cluster size and effective detection area)
# across the species in a group, at each posterior draw and covariate value.
# Two aggregation methods are compared here for the overall community:
#   ArithMean - simple across-species mean density. Dominated by whichever
#     species are locally abundant, since mean(exp(x)) >= exp(mean(x))
#     (Jensen's inequality).
#   GeomMean  - geometric mean across-species density, i.e. the mean of
#     species-specific log-densities, back-transformed. Species differ not
#     only in true density but in area.circle (species-specific effective
#     detection area set by how far each species is reliably detected), so
#     beta0/betaVec are not on a common scale across species until each
#     species' own cluster-size/area normalization has been applied (i.e.
#     until N.pred.calc has already produced a density). Averaging on the
#     log-density scale keeps everything in density units throughout and is
#     far less sensitive than ArithMean to a handful of very abundant
#     species, giving something closer to a "typical species" trend.
#
# This script currently covers the overall community (all species) only, to
# allow a visual comparison of the two methods before extending to the
# species groups used in Table 1 and Figs. 3-4.

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

series <- TRUE # generates N.trail.series, N.road.series, N.OHV.series (total & unexplained)
source(str_c(git.repo, "Path_analysis_source.R"))

###############################################################
# Traffic & speed series (Fig. 4 analogs; Path_analysis_source.R only
# builds the management-covariate series above, so these are built here the
# same way Tabulate_community_plotting_values.R builds them).
###############################################################
x.HumPres <- c(min(X.beta[,"HumanPresence"]), max(X.beta[,"HumanPresence"]) %>% rep(n.series - 1))
x.Traffic <- c(0, seq(quantile(X.beta[, "LogTrafficNoZeros"], probs = 0.01, type = 8),
                       quantile(X.beta[, "LogTrafficNoZeros"], probs = 0.99, type = 8),
                       length.out = n.series - 1))
X.traffic.series <- array(NA, c(nsims, ncol(X.beta), n.series),
                          dimnames = list(NULL, dimnames(X.beta)[[2]], NULL))
N.traffic.series <- array(NA, c(nsims, length(Spp), n.series),
                          dimnames = list(NULL, Spp, NULL))
X.traffic.series[,c(Mangmt.vars, Hab.vars, "Speed", "Speed2"),] <- 0
for(i in 1:n.series) {
  X.traffic.series[,"HumanPresence",i] <- x.HumPres[i]
  X.traffic.series[,"LogTrafficNoZeros",i] <- x.Traffic[i]
  N.traffic.series[,,i] <- N.pred.calc(X.traffic.series[,,i], Spp)
}

x.Speed <- seq(quantile(X.beta[, "Speed"], probs = 0.01, type = 8),
               quantile(X.beta[, "Speed"], probs = 0.99, type = 8),
               length.out = n.series)
X.speed.series <- array(NA, c(nsims, ncol(X.beta), n.series),
                        dimnames = list(NULL, dimnames(X.beta)[[2]], NULL))
N.speed.series <- array(NA, c(nsims, length(Spp), n.series),
                        dimnames = list(NULL, Spp, NULL))
X.speed.series[,c(Mangmt.vars, Hab.vars, "HumanPresence", "LogTrafficNoZeros"),] <- 0
for(i in 1:n.series) {
  X.speed.series[,"Speed",i] <- x.Speed[i]
  X.speed.series[,"Speed2",i] <- x.Speed[i] ^ 2
  N.speed.series[,,i] <- N.pred.calc(X.speed.series[,,i], Spp)
}

###############################################################
# Aggregation methods and helpers to compile plotting values
###############################################################
agg.funs <- list(ArithMean = function(N) mean(N),
                 GeomMean  = function(N) exp(mean(log(N))))

# N.arr: [sim, species, x-value, total/unexplained] -> long df w/ Method, Series
CompileSeries4D <- function(N.arr, x.vals, spp) {
  do.call(rbind, lapply(names(agg.funs), function(m) {
    D <- apply(N.arr[,spp,,], c(1, 3, 4), agg.funs[[m]])
    data.frame(
      x = rep(x.vals, 2),
      Series = factor(rep(c("total", "unexplained"), each = length(x.vals)),
                      levels = c("unexplained", "total")),
      Method = m,
      md = c(apply(D[,,"total"], 2, median), apply(D[,,"unexplained"], 2, median)),
      lo = c(apply(D[,,"total"], 2, quantile, probs = 0.1, type = 8),
             apply(D[,,"unexplained"], 2, quantile, probs = 0.1, type = 8)),
      hi = c(apply(D[,,"total"], 2, quantile, probs = 0.9, type = 8),
             apply(D[,,"unexplained"], 2, quantile, probs = 0.9, type = 8))
    )
  }))
}

# N.arr: [sim, species, x-value] -> long df w/ Method (no total/unexplained split)
CompileSeries3D <- function(N.arr, x.vals, spp) {
  do.call(rbind, lapply(names(agg.funs), function(m) {
    D <- apply(N.arr[,spp,], c(1, 3), agg.funs[[m]])
    data.frame(
      x = x.vals,
      Method = m,
      md = apply(D, 2, median),
      lo = apply(D, 2, quantile, probs = 0.1, type = 8),
      hi = apply(D, 2, quantile, probs = 0.9, type = 8)
    )
  }))
}

###############################################################
# Compile overall-community (all species) abundance predictions
###############################################################
dat.trail <- CompileSeries4D(N.trail.series, x.trail * cov.sd["TrailTotm"] + cov.mn["TrailTotm"], Spp)
dat.road  <- CompileSeries4D(N.road.series,  x.road  * cov.sd["RoadTotm"]  + cov.mn["RoadTotm"],  Spp)
dat.ohv   <- CompileSeries4D(N.OHV.series,   x.OHV   * cov.sd["Prp_MotRestricted"] + cov.mn["Prp_MotRestricted"], Spp)

LogTrafficVol <- x.Traffic * cov.sd["LogTrafficNoZeros"] + cov.mn["LogTrafficNoZeros"]
LogTraffic.step <- LogTrafficVol[3] - LogTrafficVol[2]
LogTrafficVol[1] <- LogTrafficVol[2] - LogTraffic.step
HumanPresence.raw <- x.HumPres * cov.sd["HumanPresence"] + cov.mn["HumanPresence"]
dat.traffic <- CompileSeries3D(N.traffic.series, LogTrafficVol, Spp) %>%
  mutate(HumanPresence = rep(HumanPresence.raw, length(agg.funs)))

dat.speed <- CompileSeries3D(N.speed.series, (x.Speed * cov.sd["Speed"] + cov.mn["Speed"]) * 1.609, Spp)
             # Multiplying by 1.609 converts mi/hr in data to km/hr for manuscript

###############################################################
# Panel plotting functions (mirror 04-Plot_community_relations.R styling)
###############################################################
PlotMgmt <- function(dat, xlabel) {
  ggplot(dat, aes(x = x, y = md)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, linetype = Series, alpha = Series), linewidth = 0.5) +
    geom_line(aes(linetype = Series), linewidth = 1) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_alpha_manual(values = c(0.1, 0.3)) +
    guides(linetype = "none", alpha = "none") +
    xlab(xlabel) + ylab("Birds / km2")
}

PlotTraffic <- function(dat) {
  hp0 <- dat %>% filter(HumanPresence == 0)
  hp1 <- dat %>% filter(HumanPresence == 1)
  x.breaks <- seq(min(dat$x), max(dat$x), length.out = 5)
  x.labs <- as.character(round(exp(x.breaks), digits = c(3, 2, 0, -2, -4)))
  x.labs[1] <- "0"
  ggplot(dat, aes(x = x, y = md)) +
    geom_ribbon(data = hp1, aes(ymin = lo, ymax = hi), linewidth = 0.5, alpha = 0.3) +
    geom_line(data = hp1, linewidth = 1) +
    geom_errorbar(data = hp0, aes(ymin = lo, ymax = hi), linewidth = 1) +
    geom_point(data = hp0, size = 3) +
    scale_x_continuous(breaks = x.breaks, labels = x.labs) +
    xlab("Traffic intensity") + ylab("Birds / km2")
}

PlotSpeed <- function(dat) {
  ggplot(dat, aes(x = x, y = md)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), linewidth = 0.5, alpha = 0.3) +
    geom_line(linewidth = 1) +
    xlab("Traffic speed") + ylab("Birds / km2")
}

panels <- list()
for(m in names(agg.funs)) {
  panels[[str_c("trail.", m)]]   <- PlotMgmt(dat.trail   %>% filter(Method == m), "Trail density")
  panels[[str_c("road.", m)]]    <- PlotMgmt(dat.road    %>% filter(Method == m), "Road density")
  panels[[str_c("ohv.", m)]]     <- PlotMgmt(dat.ohv     %>% filter(Method == m), "Prp. trails no OHV")
  panels[[str_c("traffic.", m)]] <- PlotTraffic(dat.traffic %>% filter(Method == m))
  panels[[str_c("speed.", m)]]   <- PlotSpeed(dat.speed  %>% filter(Method == m))
}

###############################################################
# Assemble comparison figure: rows = aggregation method, columns = covariate
###############################################################
p <- ggdraw() +
  draw_plot(panels$trail.ArithMean,   x = 0.05, y = 0.55, width = 0.19, height = 0.40) +
  draw_plot(panels$road.ArithMean,    x = 0.24, y = 0.55, width = 0.19, height = 0.40) +
  draw_plot(panels$ohv.ArithMean,     x = 0.43, y = 0.55, width = 0.19, height = 0.40) +
  draw_plot(panels$traffic.ArithMean, x = 0.62, y = 0.55, width = 0.19, height = 0.40) +
  draw_plot(panels$speed.ArithMean,   x = 0.81, y = 0.55, width = 0.19, height = 0.40) +
  draw_plot(panels$trail.GeomMean,    x = 0.05, y = 0.05, width = 0.19, height = 0.40) +
  draw_plot(panels$road.GeomMean,     x = 0.24, y = 0.05, width = 0.19, height = 0.40) +
  draw_plot(panels$ohv.GeomMean,      x = 0.43, y = 0.05, width = 0.19, height = 0.40) +
  draw_plot(panels$traffic.GeomMean,  x = 0.62, y = 0.05, width = 0.19, height = 0.40) +
  draw_plot(panels$speed.GeomMean,    x = 0.81, y = 0.05, width = 0.19, height = 0.40) +
  draw_plot_label("Arithmetic mean", x = 0.02, y = 0.78, size = 16, angle = 90, hjust = 0.5) +
  draw_plot_label("Geometric mean",  x = 0.02, y = 0.28, size = 16, angle = 90, hjust = 0.5)

save_plot("community_relations/Community_abundance_method_comparison.jpg", p,
         ncol = 5, nrow = 2, base_height = 3.2, dpi = 200)
