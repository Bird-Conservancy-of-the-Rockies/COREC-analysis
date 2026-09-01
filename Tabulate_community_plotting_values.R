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

# Percent change in diversity across the full covariate range (community  #
# and HumComm only, total relationship) -- supports the percent-change    #
# values reported without uncertainty in the Results "Diversity           #
# relationships with management" paragraph (Reviewer 3, EAP25-0577 2nd    #
# review, Results Line 274-277 comment). Computed per posterior draw at   #
# the low/high ends of the series above, so draw-level correlation        #
# between the two endpoints is preserved (unlike summarizing D.lo and     #
# D.hi separately). Differs from 03-Tabulate_pct_explained.R, which       #
# reports a log-ratio effect size for a +/-1SD covariate perturbation     #
# (N.pred) rather than the full empirical range used here.                #
PctChange <- function(N.arr, spp, ind.lo = 1, ind.hi = n.series) {
  D.lo <- N.arr[, spp, ind.lo, "total"] %>% apply(1, HillShannon)
  D.hi <- N.arr[, spp, ind.hi, "total"] %>% apply(1, HillShannon)
  100 * (D.hi - D.lo) / D.lo
}
FormatPctChange <- function(pct.change, BCIpercent = 80, ndig = 0) {
  # Manuscript-ready "increased/declined by X% (80% CI: A-B%)" string.
  alpha <- (1 - BCIpercent / 100) / 2
  ci <- quantile(pct.change, probs = c(alpha, 1 - alpha), type = 8)
  md <- median(pct.change)
  direction <- ifelse(md >= 0, "increased", "declined")
  str_c(direction, " by ", round(abs(md), ndig), "% (80% CI: ",
        round(min(abs(ci)), ndig), "-", round(max(abs(ci)), ndig), "%)")
}
pct.trail.community <- PctChange(N.trail.series, groups$community)
pct.trail.HumComm <- PctChange(N.trail.series, groups$HumComm)
pct.OHV.community <- PctChange(N.OHV.series, groups$community)
dat.pct.change <- data.frame(
  Group = c("community", "HumComm", "community"),
  Covariate = c("TrailDensity", "TrailDensity", "OHVRestriction"),
  Sentence = c(FormatPctChange(pct.trail.community), FormatPctChange(pct.trail.HumComm),
               FormatPctChange(pct.OHV.community)),
  # Median/Lo80/Hi80 are the raw *signed* percent change (negative =        #
  # decline); Sentence reorders these into ascending magnitude for direct   #
  # quoting in text.
  Median = round(c(median(pct.trail.community), median(pct.trail.HumComm), median(pct.OHV.community)), 0),
  Lo80 = round(c(quantile(pct.trail.community, 0.1, type = 8), quantile(pct.trail.HumComm, 0.1, type = 8),
                 quantile(pct.OHV.community, 0.1, type = 8)), 0),
  Hi80 = round(c(quantile(pct.trail.community, 0.9, type = 8), quantile(pct.trail.HumComm, 0.9, type = 8),
                 quantile(pct.OHV.community, 0.9, type = 8)), 0)
)
write.csv(dat.pct.change, "data/Tab_diversity_pct_change_management.csv", row.names = FALSE)


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
               quantile(X.beta[, "Speed"], probs = 0.99, type = 8),
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

dat.plt = data.frame(Speed = (x.Speed * cov.sd["Speed"] + cov.mn["Speed"]) * 1.609)
              #Multiplying by 1.609 converts mi/hr in data to km/hr for manuscript
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