library(stringr)
library(tidyr)
library(dplyr)
library(mcmcOutput)
library(ggplot2)
library(FunctionsBCR)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load(str_c("Data_compiled.RData"))

#_____ Script inputs _____#
git.repo <- "COREC-analysis/"
mod <- R.utils::loadObject("mod_community")
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
#________________________#

params <- c("N0", str_c("beta.", dimnames(X.beta)[[2]]),
            str_c("zeta.", dimnames(X.pp)[[2]]),
            str_c("theta.", dimnames(X.pa)[[2]]))
cols <- c(Spp)
out <- matrix("", nrow = length(params), ncol = length(cols),
              dimnames = list(params, cols))

logit.parms <- c("pa0")
logit.parms.coefs <- c("alpha0", "theta0")
names(logit.parms.coefs) <- logit.parms

log.parms <- c("N0", "a0")
log.parms.coefs <- c("beta0", "zeta0")
names(log.parms.coefs) <- log.parms

for(p in params) {
  if(p %in% logit.parms) {
    par.ind <- which(str_detect(names(mod$mcmcOutput),
                                str_c(logit.parms.coefs[p], "\\[")) &
                       !str_detect(names(mod$mcmcOutput), "dev"))
    prm <- FunctionsBCR::expit(mod$mcmcOutput[,par.ind])
    out[p, ] <- apply(prm, 2, FunctionsBCR::BCI, flag.sig = F)
  } else if(p %in% log.parms) {
    par.ind <- which(str_detect(names(mod$mcmcOutput),
                                str_c(log.parms.coefs[p], "\\[")) &
                       !str_detect(names(mod$mcmcOutput), "dev"))
    prm <- FunctionsBCR::expit(mod$mcmcOutput[,par.ind])
    out[p, ] <- apply(prm, 2, FunctionsBCR::BCI, flag.sig = F)
  } else {
    prm.nam <- str_c(str_split(p, "\\.", simplify = T)[1], "Vec")
    prm.mat <- suppressWarnings(eval(parse(text = str_c("mod$mcmcOutput$", prm.nam))))
    if(str_detect(p, "alpha")) X <- X.alpha
    if(str_detect(p, "beta")) X <- X.beta
    if(str_detect(p, "theta")) X <- X.pa
    if(str_detect(p, "zeta")) X <- X.pp
    prm.ind <- which(dimnames(X)[[2]] == str_split(p, "\\.", simplify = T)[2])
    prm <- prm.mat[, , prm.ind]
    out[p, ] <- apply(prm, 2, FunctionsBCR::BCI)
  }
}

write.csv(t(out), str_c("Parameter_est.csv"), row.names = T)
