# Compile data objects #
n <- n.mat
ngrdyrs <- max(CovIndMat[, "GrdYrInd"])
nDet <- sum(n > 0)
yearInd <- CovIndMat[, "YearInd"]
nyear <- max(yearInd)

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
  dimnames(X.pa2)[[2]] <- str_c(dimnames(X.pa2)[[2]], "2")
  X.pa <- cbind(X.pa, X.pa2)
  rm(X.pa2)
}
n.Xpa <- ncol(X.pa)

# Ecological covariates #
X.eco <- as.matrix(CovIndMat[,Eco.vars])
dimnames(X.eco)[[2]] <- Eco.vars
X.eco <- apply(X.eco, 2, function(x) (x - mean(x, na.rm = T))/ sd(x, na.rm = T))
X.eco[which(is.na(X.eco))] <- 0
if(any(Eco.vars.quad)) {
  X.ecp2 <- X.eco[,which(Eco.vars.quad)] ^ 2
  dimnames(X.ecp2)[[2]] <- str_c(dimnames(X.ecp2)[[2]], "2")
  X.eco <- cbind(X.eco, X.ecp2)
  rm(X.ecp2)
}
n.Xeco <- ncol(X.eco)

X.beta <- X.eco[, which(str_detect_any(dimnames(X.eco)[[2]], beta.vars))]
n.Xbeta <- dim(X.beta)[2]