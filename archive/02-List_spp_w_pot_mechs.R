#***Setting this aside for now. Might not be needed.

library(stringr)
library(tidyr)
library(dplyr)
library(mcmcOutput)
library(FunctionsBCR)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load(str_c("Data_compiled.RData"))

#_____ Script inputs _____#
git.repo <- "COREC-analysis/"
mod.nam <- "community"
mod <- R.utils::loadObject(str_c("mod_", mod.nam))
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
#________________________#

nsamp <- dim(mod$mcmcOutput)[1]

source(str_c(git.repo, "Functions_source.R"))

covs <- dimnames(X.beta)[[2]]

cov <- covs[1]
beta.ind <- which(dimnames(X.beta)[[2]] == cov)
beta <- mod$mcmcOutput$betaVec[,,beta.ind]
dat <- tabulate_community_effect(Spp, beta, BCIpercent = 80)
names(dat)[-c(1:2)] <- str_c("beta.", cov, c(".md", ".lo", ".hi", ".supp"))
for(cov in covs[-1]) {
  beta.ind <- which(dimnames(X.beta)[[2]] == cov)
  beta <- mod$mcmcOutput$betaVec[,,beta.ind]
  dat <- dat %>% left_join(tabulate_community_effect(Spp, beta, BCIpercent = 80), by = c("Spp", "index"))
  names(dat)[-c(1:(ncol(dat) - 4))] <- str_c("beta.", cov, c(".md", ".lo", ".hi", ".supp"))
}

# Conceivable mechanisms:
# - trail density + -> h