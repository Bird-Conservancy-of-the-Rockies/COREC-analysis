# Compile data objects #
n <- n.mat
ngrdyrs <- max(CovIndMat[, "GrdYrInd"])
nDet <- sum(n > 0)
yearInd <- CovIndMat[, "YearInd"]
nyear <- max(yearInd)
ngrid <- max(CovIndMat[, "GridInd"])

effort <- CovIndMat[, "Effort"]

nD <- nG
K <- max(detects.cutoff$TimePeriod)
dtobs.sum <- dtobs %>% data.frame() %>%
  select(SppInd, GridYrInd) %>%
  distinct()
for(d in 1:nD) {
  add <- dtobs %>% data.frame() %>%
    dplyr::group_by(SppInd, GridYrInd) %>%
    summarise(DC = sum(dclass == d))
  names(add)[ncol(add)] <- str_c("DC", d)
  dtobs.sum <- dtobs.sum %>%
    left_join(add, by = c("SppInd", "GridYrInd"))
}
for(k in 1:K) {
  add <- dtobs %>% data.frame() %>%
    dplyr::group_by(SppInd, GridYrInd) %>%
    summarise(TP = sum(TimePeriod == k))
  names(add)[ncol(add)] <- str_c("TP", k)
  dtobs.sum <- dtobs.sum %>%
    left_join(add, by = c("SppInd", "GridYrInd"))
}

dtobs.sum <- data.matrix(dtobs.sum)
spp.ind <- dtobs.sum[, "SppInd"]
det.ind <- dtobs.sum[, "GridYrInd"]
dclass <- dtobs.sum[, which(str_detect(dimnames(dtobs.sum)[[2]], "DC"))]
tint <- dtobs.sum[, which(str_detect(dimnames(dtobs.sum)[[2]], "TP"))]
rm(k, d, add)

# Initial values for latent states #
N.init <- n

# Covariate matrices #
X.pp <- as.matrix(CovIndMat[,pp.vars])
dimnames(X.pp)[[2]] <- pp.vars
X.pp <- apply(X.pp, 2, function(x) (x - mean(x, na.rm = T))/ sd(x, na.rm = T))
# X.pp[which(is.na(X.pp))] <- 0
n.Xpp <- ncol(X.pp)

X.pa <- as.matrix(CovIndMat[,pa.vars])
dimnames(X.pa)[[2]] <- pa.vars
X.pa <- apply(X.pa, 2, function(x) (x - mean(x, na.rm = T))/ sd(x, na.rm = T))
# X.pa[which(is.na(X.pa))] <- 0
if(any(pa.vars.quad)) {
  X.pa2 <- as.matrix(X.pa[,which(pa.vars.quad)] ^ 2)
  dimnames(X.pa2)[[2]] <- str_c(pa.vars[which(pa.vars.quad)], "2")
  X.pa <- cbind(X.pa, X.pa2)
  rm(X.pa2)
}
n.Xpa <- ncol(X.pa)

# Ecological covariates #
beta.vars <- c(Mangmt.vars, Human.vars, Hab.vars)
X.beta <- as.matrix(CovIndMat[, beta.vars])
dimnames(X.beta)[[2]] <- beta.vars
if(str_detect(mod.nam, "path")) X.beta.raw <- X.beta # Store raw values before scaling for path models.
X.beta <- apply(X.beta, 2, function(x) (x - mean(x, na.rm = T))/ sd(x, na.rm = T))
X.beta[which(is.na(X.beta))] <- 0
beta.vars.quad <- c(Mangmt.vars.quad, Human.vars.quad, Hab.vars.quad)
if(any(beta.vars.quad)) {
  X.beta2 <- as.matrix(X.beta[,which(beta.vars.quad)] ^ 2)
  dimnames(X.beta2)[[2]] <- str_c(beta.vars[which(beta.vars.quad)], "2")
  X.beta <- cbind(X.beta, X.beta2)
  rm(X.beta2)
}
n.Xbeta <- dim(X.beta)[2]

if(str_detect(mod.nam, "path")) {
  CovIndMat.grid <- CovIndMat %>% data.frame() %>%
    group_by(GridInd) %>%
    summarise(HumanPresence = mean(HumanPresence, na.rm = TRUE),
              LogTrafficNoZeros = mean(LogTrafficNoZeros, na.rm = TRUE),
              Speed = mean(Speed, na.rm = TRUE)) %>%
    data.matrix()
  
  ngrid.hpresent <- sum(CovIndMat.grid[, "HumanPresence"] == 1)
  ind.hpresent <- which(CovIndMat.grid[, "HumanPresence"] == 1)
  
  ngrid.Speed <- sum(!is.na(CovIndMat.grid[, "Speed"]))
  ind.SpeedPresent <- which(!is.na(CovIndMat.grid[, "Speed"]))

  X.beta.grid <- X.beta %>% data.frame() %>%
    bind_cols(GridInd = CovIndMat[, "GridInd"]) %>%
    group_by(GridInd) %>%
    summarise(TrailTotm = mean(TrailTotm),
              RoadTotm = mean(RoadTotm),
              Prp_MotRestricted = mean(Prp_MotRestricted)) %>%
    select(-GridInd) %>%
    data.matrix()
  
  for(i in 1:length(Human.vars)) assign(str_c("ind.", Human.vars[i]),
                                        which(dimnames(CovIndMat.grid)[[2]] == Human.vars[i]))
  for(i in 1:length(Mangmt.vars)) assign(str_c("ind.", Mangmt.vars[i]),
                                         which(dimnames(X.beta.grid)[[2]] == Mangmt.vars[i]))
  HumanPresence <- CovIndMat.grid[, ind.HumanPresence]
  Traffic <- exp(CovIndMat.grid[ind.hpresent, ind.LogTrafficNoZeros])
  Speed <- CovIndMat.grid[ind.SpeedPresent, ind.Speed]
}