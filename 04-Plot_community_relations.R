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
tab.plotting.values <- FALSE # Set to true if updating plotted values. Otherwise, will read from cached files.
git.repo <- "COREC-analysis/"
mod <- R.utils::loadObject(str_c("mod_", mod.nam))
nsims <- dim(mod$mcmcOutput)[1]
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
source(str_c(git.repo, "Functions_source.R"))
source(str_c(git.repo, "Cluster_sizes.R"))
#______________________________________#

#########################
# Define species groups #
#########################

spp_assignments <- read.csv("data/Species_list_assigned.csv", header = TRUE, stringsAsFactors = FALSE)
groups <- list(
  community = Spp,
  HabitatSpecialist = spp_assignments %>% filter(Hab_specialist) %>% pull(BirdCode),
  migratory = spp_assignments %>% filter(Migratory) %>% pull(BirdCode),
  small = spp_assignments %>% filter(Mass < median(Mass)) %>% pull(BirdCode),
  HumComm = spp_assignments %>% filter(HumanCommensal) %>% pull(BirdCode),
  DietSpecialist = spp_assignments %>% filter(!Omnivore) %>% pull(BirdCode),
  insectivore = spp_assignments %>% filter(Insectivore) %>% pull(BirdCode),
  ground = spp_assignments %>% filter(Ground) %>% pull(BirdCode),
  SGCN = spp_assignments %>% filter(SGCN) %>% pull(BirdCode)
)
write.csv(data.frame(Groups = names(groups), nSpec = lengths(groups)), "nSpp_groups.csv", row.names = FALSE)
spp_assignments %>% # Write assignments to table for manuscript
  mutate(DietSpecialist = !Omnivore,
         Small = Mass < median(Mass)) %>%
  select(Species, ScientificName, BirdCode, Sum_counts, Hab_specialist, Migratory,
         Small, HumanCommensal, DietSpecialist, Insectivore, Ground, SGCN) %>%
  mutate(Hab_specialist = as.numeric(Hab_specialist),
         Migratory = as.integer(Migratory),
         Small = as.integer(Small),
         HumanCommensal = as.integer(HumanCommensal),
         DietSpecialist = as.integer(DietSpecialist),
         Insectivore = as.integer(Insectivore),
         Ground = as.integer(Ground),
         SGCN = as.integer(SGCN)) %>%
  write.csv("data/Species_list_assignments_final.csv", row.names = FALSE)

##########################
# Compile values to plot #
##########################
if(tab.plotting.values) source(str_c(git.repo, "Tabulate_community_plotting_values.R"))

dat.plt.trail <- read.csv("data/Dat_plot_community_trail_density.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(Series = Series %>% factor(levels = c("unexplained", "total")))
dat.plt.road <- read.csv("data/Dat_plot_community_road_density.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(Series = Series %>% factor(levels = c("unexplained", "total")))
dat.plt.ohv <- read.csv("data/Dat_plot_community_OHV.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(Series = Series %>% factor(levels = c("unexplained", "total")))

dat.pct.expl <- read.csv("Pct_man_effects_explained_community.csv", header = T, stringsAsFactors = FALSE) %>%
  rename(Group = X) %>%
  mutate(Group = ifelse(Group == "Community", "community", Group)) %>%
  mutate(Group = ifelse(Group == "Migratory", "migratory", Group)) %>%
  mutate(Group = ifelse(Group == "Small", "small", Group)) %>%
  mutate(Group = ifelse(Group == "Insectivore", "insectivore", Group)) %>%
  mutate(Group = ifelse(Group == "Ground", "ground", Group)) %>%
  filter(Group %in% names(groups)) %>%
  mutate(trail = ifelse(str_sub(Trail.total, -1, -1) == "*" & str_sub(Trail.pctExplained, -1, -1) == "*", "**",
                        ifelse(str_sub(Trail.total, -1, -1) == "*" & str_sub(Trail.pctExplained, -1, -1) != "*",
                               "*", ""))) %>%
  mutate(road = ifelse(str_sub(Road.total, -1, -1) == "*" & str_sub(Road.pctExplained, -1, -1) == "*", "**",
                       ifelse(str_sub(Road.total, -1, -1) == "*" & str_sub(Road.pctExplained, -1, -1) != "*",
                              "*", ""))) %>%
  mutate(ohv = ifelse(str_sub(OHV.total, -1, -1) == "*" & str_sub(OHV.pctExplained, -1, -1) == "*", "**",
                      ifelse(str_sub(OHV.total, -1, -1) == "*" & str_sub(OHV.pctExplained, -1, -1) != "*",
                             "*", ""))) %>%
  select(Group, trail:ohv)
dat.stat.supp <- dat.pct.expl %>% select(trail:ohv) %>% as.matrix
row.names(dat.stat.supp) <- dat.pct.expl$Group

dat.plt.traffic <- read.csv("data/Dat_plot_community_traffic.csv", header = TRUE, stringsAsFactors = FALSE)
dat.plt.speed <- read.csv("data/Dat_plot_community_speed.csv", header = TRUE, stringsAsFactors = FALSE)

y.labs <- c("All species", "Habitat specialists", "Migratory species", "Small species", "Human commensals",
            "Diet specialists", "Insectivores", "Ground species", "SGCN species")
names(y.labs) <- names(groups)

# Calculate y-axis limits and breaks (enforce y axis consistency across columns)
y.limits <- matrix(NA, nrow = length(groups), ncol = 2)
dimnames(y.limits) <- list(names(groups), c("ymin", "ymax"))
y.breaks <- matrix(NA, nrow = length(groups), ncol = 4)
dimnames(y.breaks) <- list(names(groups), NULL)
for(g in names(groups)) {
  ymax.str <- str_c("max(c(dat.plt.trail$", g, ".hi, dat.plt.road$", g, ".hi, dat.plt.ohv$", g,
                    ".hi, dat.plt.traffic$", g, ".hi, dat.plt.speed$", g, ".hi))")
  ymax <- eval(parse(text = ymax.str))
  ymin.str <- str_c("min(c(dat.plt.trail$", g, ".lo, dat.plt.road$", g, ".lo, dat.plt.ohv$", g,
                    ".lo, dat.plt.traffic$", g, ".lo, dat.plt.speed$", g, ".lo))")
  ymin <- eval(parse(text = ymin.str))
  y.marg <- (ymax - ymin) / 100
  y.limits[g,] <- c(ymin - y.marg, ymax + y.marg)
  y.breaks[g,] <- seq(ymin, ymax, length.out = 4) %>% round(digits = 1)
}

########################
# Generate panel plots #
########################

## Management panels ##

v.stub <- c("trail", "road", "ohv")
v.clabs <- c("TrailDensity", "RoadDensity", "OHVRestrict")
v.xlabs <- c("Trail density", "Road density", "Prp. trails no OHV")
names(v.clabs) <- names(v.xlabs) <- v.stub
for(g in names(groups)) {
  ymax <- y.limits[g, "ymax"]
  ymin <- y.limits[g, "ymin"]
  y.stat.supp.label <- ymax - (ymax - ymin) * 0.05
  for(v in v.stub) {
    xmax <- eval(parse(text = str_c("max(dat.plt.", v, "$", v.clabs[v], ")")))
    p.str <- str_c(str_c("p", v, g, sep = "."),
                   "<- ggplot(dat = dat.plt.", v,", aes(x = ", v.clabs[v],", y = ", g, ".md)) +",
                   "geom_ribbon(aes(ymin = ", g, ".lo, ymax = ", g, ".hi, linetype = Series, alpha = Series), linewidth = 0.5) +",
                   "geom_line(aes(linetype = Series), linewidth = 1) +",
                   "scale_linetype_manual(values = c('dashed', 'solid')) +",
                   "scale_alpha_manual(values = c(0.1, 0.3)) +",
                   "guides(linetype = 'none', alpha = 'none') +",
                   "scale_y_continuous(name = '", y.labs[g], "', limits = y.limits[g,], breaks = y.breaks[g,]) +",
                   "xlab('", v.xlabs[v], "') +",
                   "annotate('text', x = xmax, y = y.stat.supp.label, label = dat.stat.supp[g, v], size = 5)")
    eval(parse(text = p.str))
  }
}

## Traffic plots ##
n.breaks <- 5
x.breaks <- seq(min(dat.plt.traffic$LogTrafficVol), max(dat.plt.traffic$LogTrafficVol), length.out = n.breaks)
x.labs.vals <- exp(x.breaks)
ndig <- c(3, 2, 0, -2, -4)
x.labs.vals <- as.character(round(x.labs.vals, digits = ndig))
x.labs.vals[1] <- "0"
dat.plt.HP0 <- (dat.plt.traffic %>% filter(HumanPresence == 0))
dat.plt.HP1 <- (dat.plt.traffic %>% filter(HumanPresence == 1))
for(g in names(groups)) {
  p.str <- str_c(str_c("p.traffic", g, sep = "."),
                 "<- ggplot(dat = dat.plt.traffic, aes(x = LogTrafficVol, y = ", g, ".md)) +",
                 "geom_ribbon(data = dat.plt.HP1, aes(x = LogTrafficVol, ymin = ", g,
                 ".lo, ymax = ", g, ".hi), linewidth = 0.5, alpha = 0.3) +",
                 "geom_line(data = dat.plt.HP1, aes(x = LogTrafficVol, y = ", g,
                 ".md), linewidth = 1) +",
                 "geom_errorbar(data = dat.plt.HP0, aes(x = LogTrafficVol,",
                 "ymin = ", g, ".lo, ymax = ", g,".hi), linewidth = 1) +",
                 "geom_point(data = dat.plt.HP0, aes(x = LogTrafficVol, y = ", g,
                 ".md), size = 3) +",
                 "scale_x_continuous(breaks = x.breaks, labels = x.labs.vals)+",
                 "scale_y_continuous(name = '", y.labs[g], "', limits = y.limits[g,], breaks = y.breaks[g,]) +",
                 "xlab('Traffic intensity')")
  eval(parse(text = p.str))
}

## Speed plots ##
for(g in names(groups)) {
  p.str <- str_c(str_c("p.speed", g, sep = "."),
                 "<- ggplot(dat = dat.plt.speed, aes(x = Speed, y = ", g, ".md)) +",
                 "geom_ribbon(aes(ymin = ", g, ".lo, ymax = ", g, ".hi), linewidth = 0.5, alpha = 0.3) +",
                 "geom_line(linewidth = 1) +",
                 "xlab('Traffic speed') + ylab('", y.labs[g], "')+",
                 "scale_y_continuous(name = '", y.labs[g], "', limits = y.limits[g,], breaks = y.breaks[g,])")
  eval(parse(text = p.str))
}

# Combine the two above, i.e., management relations in columns 1-3 and human mobility relations in columns 4-5
p <- ggdraw() +
  draw_plot(p.trail.community,           x = 0.05, y = 0.84444,   width = 0.19, height = 0.1055556) +
  draw_plot(p.road.community,            x = 0.24, y = 0.84444,   width = 0.19, height = 0.1055556) +
  draw_plot(p.ohv.community,             x = 0.43, y = 0.84444,   width = 0.19, height = 0.1055556) +
  draw_plot(p.traffic.community,         x = 0.62, y = 0.84444,   width = 0.19, height = 0.1055556) +
  draw_plot(p.speed.community,           x = 0.81, y = 0.84444,   width = 0.19, height = 0.1055556) +
  draw_plot(p.trail.HabitatSpecialist,   x = 0.05, y = 0.73888,   width = 0.19, height = 0.1055556) +
  draw_plot(p.road.HabitatSpecialist,    x = 0.24, y = 0.73888,   width = 0.19, height = 0.1055556) +
  draw_plot(p.ohv.HabitatSpecialist,     x = 0.43, y = 0.73888,   width = 0.19, height = 0.1055556) +
  draw_plot(p.traffic.HabitatSpecialist, x = 0.62, y = 0.73888,   width = 0.19, height = 0.1055556) +
  draw_plot(p.speed.HabitatSpecialist,   x = 0.81, y = 0.73888,   width = 0.19, height = 0.1055556) +
  draw_plot(p.trail.migratory,           x = 0.05, y = 0.63333,   width = 0.19, height = 0.1055556) +
  draw_plot(p.road.migratory,            x = 0.24, y = 0.63333,   width = 0.19, height = 0.1055556) +
  draw_plot(p.ohv.migratory,             x = 0.43, y = 0.63333,   width = 0.19, height = 0.1055556) +
  draw_plot(p.traffic.migratory,         x = 0.62, y = 0.63333,   width = 0.19, height = 0.1055556) +
  draw_plot(p.speed.migratory,           x = 0.81, y = 0.63333,   width = 0.19, height = 0.1055556) +
  draw_plot(p.trail.small,               x = 0.05, y = 0.52777,   width = 0.19, height = 0.1055556) +
  draw_plot(p.road.small,                x = 0.24, y = 0.52777,   width = 0.19, height = 0.1055556) +
  draw_plot(p.ohv.small,                 x = 0.43, y = 0.52777,   width = 0.19, height = 0.1055556) +
  draw_plot(p.traffic.small,             x = 0.62, y = 0.52777,   width = 0.19, height = 0.1055556) +
  draw_plot(p.speed.small,               x = 0.81, y = 0.52777,   width = 0.19, height = 0.1055556) +
  draw_plot(p.trail.HumComm,             x = 0.05, y = 0.42222,   width = 0.19, height = 0.1055556) +
  draw_plot(p.road.HumComm,              x = 0.24, y = 0.42222,   width = 0.19, height = 0.1055556) +
  draw_plot(p.ohv.HumComm,               x = 0.43, y = 0.42222,   width = 0.19, height = 0.1055556) +
  draw_plot(p.traffic.HumComm,           x = 0.62, y = 0.42222,   width = 0.19, height = 0.1055556) +
  draw_plot(p.speed.HumComm,             x = 0.81, y = 0.42222,   width = 0.19, height = 0.1055556) +
  draw_plot(p.trail.DietSpecialist,      x = 0.05, y = 0.31666,   width = 0.19, height = 0.1055556) +
  draw_plot(p.road.DietSpecialist,       x = 0.24, y = 0.31666,   width = 0.19, height = 0.1055556) +
  draw_plot(p.ohv.DietSpecialist,        x = 0.43, y = 0.31666,   width = 0.19, height = 0.1055556) +
  draw_plot(p.traffic.DietSpecialist,    x = 0.62, y = 0.31666,   width = 0.19, height = 0.1055556) +
  draw_plot(p.speed.DietSpecialist,      x = 0.81, y = 0.31666,   width = 0.19, height = 0.1055556) +
  draw_plot(p.trail.insectivore,         x = 0.05, y = 0.21111,   width = 0.19, height = 0.1055556) +
  draw_plot(p.road.insectivore,          x = 0.24, y = 0.21111,   width = 0.19, height = 0.1055556) +
  draw_plot(p.ohv.insectivore,           x = 0.43, y = 0.21111,   width = 0.19, height = 0.1055556) +
  draw_plot(p.traffic.insectivore,       x = 0.62, y = 0.21111,   width = 0.19, height = 0.1055556) +
  draw_plot(p.speed.insectivore,         x = 0.81, y = 0.21111,   width = 0.19, height = 0.1055556) +
  draw_plot(p.trail.ground,              x = 0.05, y = 0.10556,   width = 0.19, height = 0.1055556) +
  draw_plot(p.road.ground,               x = 0.24, y = 0.10556,   width = 0.19, height = 0.1055556) +
  draw_plot(p.ohv.ground,                x = 0.43, y = 0.10556,   width = 0.19, height = 0.1055556) +
  draw_plot(p.traffic.ground,            x = 0.62, y = 0.10556,   width = 0.19, height = 0.1055556) +
  draw_plot(p.speed.ground,              x = 0.81, y = 0.10556,   width = 0.19, height = 0.1055556) +
  draw_plot(p.trail.SGCN,                x = 0.05, y = 0.00000,   width = 0.19, height = 0.1055556) +
  draw_plot(p.road.SGCN,                 x = 0.24, y = 0.00000,   width = 0.19, height = 0.1055556) +
  draw_plot(p.ohv.SGCN,                  x = 0.43, y = 0.00000,   width = 0.19, height = 0.1055556) +
  draw_plot(p.traffic.SGCN,              x = 0.62, y = 0.00000,   width = 0.19, height = 0.1055556) +
  draw_plot(p.speed.SGCN,                x = 0.81, y = 0.00000,   width = 0.19, height = 0.1055556) +
  draw_plot_label("Hill-Shannon diversity", x = 0, y = 0.5, size = 20, angle = 90, hjust = 0.5)

save_plot("community_relations/Management_plus_HumanCov_relations.jpg", p, ncol = 2, nrow = 4, dpi = 200)
