series <- TRUE # Turns on component of 'Path_analysis_source.R' that generates values to plot here.
source(str_c(git.repo, "Path_analysis_source.R"))

# Trail density #
dat.plt = data.frame(TrailDensity = (x.trail * cov.sd["TrailTotm"] + cov.mn["TrailTotm"]) %>%
                       rep(2),
                     Series = c(rep("total", length(x.trail)), rep("unexplained", length(x.trail))) %>%
                       factor(levels = c("unexplained", "total")))
for(g in names(groups)) {
  spp <- groups[[g]]
  D <- apply(N.trail.series[,spp,,], c(1, 3, 4), HillShannon)
  dat.plt$v.md <- c(apply(D[,,"total"], 2, median), apply(D[,,"unexplained"], 2, median))
  dat.plt$v.lo <- c(apply(D[,,"total"], 2, function(x) quantile(x, prob = 0.1, type = 8)),
                    apply(D[,,"unexplained"], 2, function(x) quantile(x, prob = 0.1, type = 8)))
  dat.plt$v.hi <- c(apply(D[,,"total"], 2, function(x) quantile(x, prob = 0.9, type = 8)),
                    apply(D[,,"unexplained"], 2, function(x) quantile(x, prob = 0.9, type = 8)))
  names(dat.plt)[which(names(dat.plt) %in% c("v.md", "v.lo", "v.hi"))] <-
    str_c(g, c(".md", ".lo", ".hi"))
}
write.csv(dat.plt, "data/Dat_plot_community_trail_density.csv", row.names = FALSE)

# Road density #
dat.plt = data.frame(RoadDensity = (x.road * cov.sd["RoadTotm"] + cov.mn["RoadTotm"]) %>%
                       rep(2),
                     Series = c(rep("total", length(x.road)), rep("unexplained", length(x.road))) %>%
                       factor(levels = c("unexplained", "total")))
for(g in names(groups)) {
  spp <- groups[[g]]
  D <- apply(N.road.series[,spp,,], c(1, 3, 4), HillShannon)
  dat.plt$v.md <- c(apply(D[,,"total"], 2, median), apply(D[,,"unexplained"], 2, median))
  dat.plt$v.lo <- c(apply(D[,,"total"], 2, function(x) quantile(x, prob = 0.1, type = 8)),
                    apply(D[,,"unexplained"], 2, function(x) quantile(x, prob = 0.1, type = 8)))
  dat.plt$v.hi <- c(apply(D[,,"total"], 2, function(x) quantile(x, prob = 0.9, type = 8)),
                    apply(D[,,"unexplained"], 2, function(x) quantile(x, prob = 0.9, type = 8)))
  names(dat.plt)[which(names(dat.plt) %in% c("v.md", "v.lo", "v.hi"))] <-
    str_c(g, c(".md", ".lo", ".hi"))
}
write.csv(dat.plt, "data/Dat_plot_community_road_density.csv", row.names = FALSE)

# OHV #
dat.plt = data.frame(OHVRestrict = (x.OHV * cov.sd["Prp_MotRestricted"] + cov.mn["Prp_MotRestricted"]) %>%
                       rep(2),
                     Series = c(rep("total", length(x.OHV)), rep("unexplained", length(x.OHV))) %>%
                       factor(levels = c("unexplained", "total")))
for(g in names(groups)) {
  spp <- groups[[g]]
  D <- apply(N.OHV.series[,spp,,], c(1, 3, 4), HillShannon)
  dat.plt$v.md <- c(apply(D[,,"total"], 2, median), apply(D[,,"unexplained"], 2, median))
  dat.plt$v.lo <- c(apply(D[,,"total"], 2, function(x) quantile(x, prob = 0.1, type = 8)),
                    apply(D[,,"unexplained"], 2, function(x) quantile(x, prob = 0.1, type = 8)))
  dat.plt$v.hi <- c(apply(D[,,"total"], 2, function(x) quantile(x, prob = 0.9, type = 8)),
                    apply(D[,,"unexplained"], 2, function(x) quantile(x, prob = 0.9, type = 8)))
  names(dat.plt)[which(names(dat.plt) %in% c("v.md", "v.lo", "v.hi"))] <-
    str_c(g, c(".md", ".lo", ".hi"))
}
write.csv(dat.plt, "data/Dat_plot_community_OHV.csv", row.names = FALSE)

# Traffic #
x.HumPres <- c(min(X.beta[,"HumanPresence"]), max(X.beta[,"HumanPresence"]) %>% rep(n.series - 1))
x.Traffic <- c(0, seq(quantile(X.beta[, "LogTrafficNoZeros"], probs = 0.01, type = 8),
                       quantile(X.beta[, "LogTrafficNoZeros"], probs = 0.99, type = 8),
                       length.out = n.series - 1))
X.traffic.series <- array(NA, c(nsims, ncol(X.beta), n.series))
dimnames(X.traffic.series)[[2]] <- dimnames(X.beta)[[2]]
N.traffic.series <- array(NA, c(nsims, length(Spp), n.series))
dimnames(N.traffic.series)[[2]] <- Spp
X.traffic.series[,c(Mangmt.vars, Hab.vars, "Speed", "Speed2"),] <- 0
for(i in 1:n.series) {
  X.traffic.series[,"HumanPresence",i] <- x.HumPres[i]
  X.traffic.series[,"LogTrafficNoZeros",i] <- x.Traffic[i]
  N.traffic.series[,,i] <- N.pred.calc(X.traffic.series[,,i], Spp)
}

dat.plt = data.frame(HumanPresence = (x.HumPres * cov.sd["HumanPresence"] + cov.mn["HumanPresence"]),
                     LogTrafficVol = (x.Traffic * cov.sd["LogTrafficNoZeros"] + cov.mn["LogTrafficNoZeros"]))
LogTraffic.step <- dat.plt$LogTrafficVol[3] - dat.plt$LogTrafficVol[2]
dat.plt$LogTrafficVol[1] <- dat.plt$LogTrafficVol[2] - LogTraffic.step
dat.plt <- dat.plt %>%
  mutate(TrafficVol = ifelse(HumanPresence == 1, exp(LogTrafficVol), NA))
for(g in names(groups)) {
  spp <- groups[[g]]
  D <- apply(N.traffic.series[,spp,], c(1, 3), HillShannon)
  dat.plt$v.md <- apply(D, 2, median)
  dat.plt$v.lo <- apply(D, 2, function(x) quantile(x, prob = 0.1, type = 8))
  dat.plt$v.hi <- apply(D, 2, function(x) quantile(x, prob = 0.9, type = 8))
  names(dat.plt)[which(names(dat.plt) %in% c("v.md", "v.lo", "v.hi"))] <-
    str_c(g, c(".md", ".lo", ".hi"))
}
write.csv(dat.plt, "data/Dat_plot_community_traffic.csv", row.names = FALSE)

# Speed #
x.Speed <- seq(quantile(X.beta[, "Speed"], probs = 0.01, type = 8),
               quantile(X.beta[, "LogTrafficNoZeros"], probs = 0.99, type = 8),
               length.out = n.series)
X.speed.series <- array(NA, c(nsims, ncol(X.beta), n.series))
dimnames(X.speed.series)[[2]] <- dimnames(X.beta)[[2]]
N.speed.series <- array(NA, c(nsims, length(Spp), n.series))
dimnames(N.speed.series)[[2]] <- Spp
X.speed.series[,c(Mangmt.vars, Hab.vars, "HumanPresence", "LogTrafficNoZeros"),] <- 0
for(i in 1:n.series) {
  X.speed.series[,"Speed",i] <- x.Speed[i]
  X.speed.series[,"Speed2",i] <- x.Speed[i] ^ 2
  N.speed.series[,,i] <- N.pred.calc(X.speed.series[,,i], Spp)
}

dat.plt = data.frame(Speed = (x.Speed * cov.sd["Speed"] + cov.mn["Speed"]))
for(g in names(groups)) {
  spp <- groups[[g]]
  D <- apply(N.speed.series[,spp,], c(1, 3), HillShannon)
  dat.plt$v.md <- apply(D, 2, median)
  dat.plt$v.lo <- apply(D, 2, function(x) quantile(x, prob = 0.1, type = 8))
  dat.plt$v.hi <- apply(D, 2, function(x) quantile(x, prob = 0.9, type = 8))
  names(dat.plt)[which(names(dat.plt) %in% c("v.md", "v.lo", "v.hi"))] <-
    str_c(g, c(".md", ".lo", ".hi"))
}
write.csv(dat.plt, "data/Dat_plot_community_speed.csv", row.names = FALSE)

rm(dat.plt)