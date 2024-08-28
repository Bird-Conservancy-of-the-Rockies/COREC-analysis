library(nimble)
library(stringr)
library(tidyr)
library(dplyr)
library(FunctionsBCR)

setwd("~/COREC")
#setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")

#_____ Script inputs _____#
git.repo <- "COREC-analysis/"
#GOF <- FALSE # Set to true to include and monitor goodness of fit metrics (not sure if/how I'll do this.)
mod.nam <- "interm_paths" # Options: "community", "interm_paths", "path
model.file <- str_c("model_", mod.nam, ".nimble")
parallel.process <- T # Set to true if running nimble on analysis server (i.e., not Windows)
max.samples.saved <- 1000 # Maximum number of posterior samples to save.
par.ignore.Rht <- c() # Parameters to ignore for calculating Rhat and neff.
#source(str_c(scripts.loc, "RunNimbleParallel_", data.set, ".R"))
#_________________________#

load("data/Data_compiled.RData")
nspp <- length(Spp)

# MCMC values
nc <- 3 # number of chains
nb <- 10000 # Proportion of chain to discard as burn in
ni <- 20000 # number of iterations
nt <- 100 # thinning
#_________________________#

# Data objects to send to JAGS
data.nams.comm <- c("n", "dclass", "tint",
                    "X.beta", "X.pp", "X.pa")
data.nams.paths <- c("HumanPresence", "Traffic", "Speed", "X.beta")

constant.nams.comm <- c("nspp", "ngrdyrs",
                        "yearInd", "nyear",
                        "breaks", "area.prop", "nD",
                        "K", "ndet", "effort",
                        
                        "n.Xpa", "n.Xpp", "n.Xbeta",
                        "det.ind", "spp.ind", "nDet")
constant.nams.path <- c("ngrdyrs", "ngrdyrs.hpresent",
                        "ngrdyrs.Speed",
                        
                        "ind.hpresent", "ind.SpeedPresent",
                        
                        "ind.TrailTotm", "ind.RoadTotm",
                        "ind.Prp_MotRestricted", "ind.Prp_HorseRestricted")

if(mod.nam == "community") {
  data.nams <- data.nams.comm
  constant.nams <- constant.nams.comm
} else if(mod.nam == "interm_paths") {
  data.nams <- data.nams.paths
  constant.nams <- constant.nams.path
} else {
  data.nams <- c(data.nams.comm, data.nams.paths) %>% unique()
  constant.nams <- c(constant.nams.comm, constant.nams.path) %>% unique()
}

source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
source(str_c(git.repo, str_c("ModelInits.R")))

# Generate data and constant objects to send to NIMBLE #
#if(is.null(dim(X.pa))) data.nams <- data.nams[-which(data.nams == "X.pa")]
data <- list()
data.nams.keep <- logical(length = length(data.nams))
for(i in 1:length(data.nams)) {
  if(exists(data.nams[i])) data[[length(data) + 1]] <- eval(as.name(data.nams[i]))
  if(exists(data.nams[i])) data.nams.keep[i] <- T
}
names(data) <- data.nams[which(data.nams.keep)]

#if(is.null(dim(X.pa))) constant.nams <- constant.nams[-which(constant.nams == "n.Xpa")]
constants <- list()
constants.nams.keep <- logical(length = length(constant.nams))
for(i in 1:length(constant.nams)) {
  if(exists(constant.nams[i])) constants[[length(constants) + 1]] <- eval(as.name(constant.nams[i]))
  if(exists(constant.nams[i])) constants.nams.keep[i] <- T
}
names(constants) <- constant.nams[which(constants.nams.keep)]

source(str_c(git.repo, model.file))
rm(.Random.seed, envir=.GlobalEnv)

# All at once using built-in wrapper... #
if(!parallel.process) {
  out <- nimbleMCMC(code = model,
                    constants = constants,
                    data=data,
                    inits=inits,
                    nchains = nc,
                    nburnin = ifelse(nb < 1, ni * nb, nb),
                    niter = ni,
                    thin = nt,
                    samplesAsCodaMCMC = T,
                    summary = TRUE,
                    WAIC = FALSE,
                    monitors = parameters)
  
  mod <- coda::as.mcmc.list(lapply(out$samples, coda::as.mcmc))
  library(mcmcOutput)
  #if(nc > 1) mod.raw <- coda::as.mcmc.list(lapply(out$samples, coda::mcmc))
  mod <- mcmcOutput(mod)
  sumTab <- summary(mod, MCEpc = F, Rhat = T, n.eff = T, f = T, overlap0 = T, verbose = F)
  sumTab <- sumTab %>%
    as_tibble() %>%
    mutate(Parameter = row.names(sumTab)) %>%
    select(Parameter, mean:f)
  mod <- list(mcmcOutput = mod, summary = sumTab)
  R.utils::saveObject(mod, str_c("mod_", mod.nam)) # If running all in one.
}

if(parallel.process) {
  RunNimbleParallel(model = model, inits = inits, data = data, constants = constants,
                    parameters = parameters, par.ignore.Rht = par.ignore.Rht,
                    nc = nc, ni = ni, nb = nb, nt = nt,
                    mod.nam = str_c("mod_", mod.nam), max.samples.saved = max.samples.saved)
}
