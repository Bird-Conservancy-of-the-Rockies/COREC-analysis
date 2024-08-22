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
mod.nam <- "path"
mod <- R.utils::loadObject(str_c("mod_", mod.nam))
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
#________________________#

###################################
# Bird community model parameters #
###################################

params <- c("N0", str_c("beta.", dimnames(X.beta)[[2]]),
            "a0", str_c("zeta.", dimnames(X.pp)[[2]]),
            "pa0", str_c("theta.", dimnames(X.pa)[[2]]))
cols <- c(Spp)
out <- matrix("", nrow = length(params), ncol = length(cols),
              dimnames = list(params, cols))

logit.parms <- c("pa0")
logit.parms.coefs <- c("theta0")
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
    out[p, ] <- apply(prm, 2, (function(x) FunctionsBCR::BCI(x, BCIpercent = 80, flag.sig = F)))
  } else if(p %in% log.parms) {
    par.ind <- which(str_detect(names(mod$mcmcOutput),
                                str_c(log.parms.coefs[p], "\\[")) &
                       !str_detect(names(mod$mcmcOutput), "dev"))
    prm <- exp(mod$mcmcOutput[,par.ind])
    out[p, ] <- apply(prm, 2, (function(x) FunctionsBCR::BCI(x, BCIpercent = 80, flag.sig = F)))
  } else {
    prm.nam <- str_c(str_split(p, "\\.", simplify = T)[1], "Vec")
    prm.mat <- suppressWarnings(eval(parse(text = str_c("mod$mcmcOutput$", prm.nam))))
    if(str_detect(p, "alpha")) X <- X.alpha
    if(str_detect(p, "beta")) X <- X.beta
    if(str_detect(p, "theta")) X <- X.pa
    if(str_detect(p, "zeta")) X <- X.pp
    prm.ind <- which(dimnames(X)[[2]] == str_split(p, "\\.", simplify = T)[2])
    prm <- prm.mat[, , prm.ind]
    out[p, ] <- apply(prm, 2, (function(x) FunctionsBCR::BCI(x, BCIpercent = 80)))
  }
}

write.csv(t(out), str_c("Parameter_est.csv"), row.names = T)

#####################################
# Management human model parameters #
#####################################

path.parms <- mod$summary$Parameter[which(str_detect(mod$summary$Parameter, "BETA"))]
path.intercepts <- path.parms[which(str_detect(path.parms, "BETA0"))]
out.tab <- data.frame(Parameter = path.parms,
                      Estimate95 = "",
                      Estimate80 = "",
                      Estimate70 = "")
for(p in 1:length(path.parms)) {
  parm <- path.parms[p]
  if(parm %in% path.intercepts) {
    out.tab$Estimate95[p] <- FunctionsBCR::BCI(mod$mcmcOutput[, parm], flag.sig = FALSE)
    out.tab$Estimate80[p] <- FunctionsBCR::BCI(mod$mcmcOutput[, parm], BCIpercent = 80, flag.sig = FALSE)
    out.tab$Estimate70[p] <- FunctionsBCR::BCI(mod$mcmcOutput[, parm], BCIpercent = 70, flag.sig = FALSE)
  } else {
    out.tab$Estimate95[p] <- FunctionsBCR::BCI(mod$mcmcOutput[, parm], flag.sig = TRUE)
    out.tab$Estimate80[p] <- FunctionsBCR::BCI(mod$mcmcOutput[, parm], BCIpercent = 80, flag.sig = TRUE)
    out.tab$Estimate70[p] <- FunctionsBCR::BCI(mod$mcmcOutput[, parm], BCIpercent = 70, flag.sig = TRUE)
  }
}

write.csv(out.tab, "Path_estimates.csv", row.names = FALSE)

#################################################################
# Summarize species results for preliminary report (June, 2024) #
#################################################################

# SGCN_spp <- read.csv("C:/Users/quresh.latif/files/data/CPW SWAP Species List.csv", header = TRUE) %>%
#   filter(Group == "Birds")
# library(GetDataBCR)
# library(FunctionsBCR)
# database_spp <- BirdData(select.cols = c("ScientificName", "BirdCode"), State.filter = "CO", group_by = TRUE)
# SGCN_spp <- database_spp %>% filter(str_detect_any(ScientificName, SGCN_spp$Species)) %>% pull(BirdCode)
# SGCN_spp <- SGCN_spp[which(SGCN_spp %in% Spp)]
# 
# rows <- c(str_sub(dimnames(out)[[1]][2:10], 6, -1))
# cols <- c("positive", "negative", "positive_SGCN", "negative_SGCN")
# sum.out <- matrix(NA, nrow = length(rows), ncol = length(cols),
#                   dimnames = list(rows, cols))
# 
# sum.out[1:9, "positive"] <- apply(out[2:10,], 1,
#                                   function(x) sum(str_sub(x, 1, 1) != "-" &
#                                                     str_sub(x, -1, -1) == "*"))
# sum.out[1:9, "negative"] <- apply(out[2:10,], 1,
#                                   function(x) sum(str_sub(x, 1, 1) == "-" &
#                                                     str_sub(x, -1, -1) == "*"))
# sum.out[1:9, "positive_SGCN"] <- apply(out[2:10, SGCN_spp], 1,
#                                   function(x) sum(str_sub(x, 1, 1) != "-" &
#                                                     str_sub(x, -1, -1) == "*"))
# sum.out[1:9, "negative_SGCN"] <- apply(out[2:10, SGCN_spp], 1,
#                                   function(x) sum(str_sub(x, 1, 1) == "-" &
#                                                     str_sub(x, -1, -1) == "*"))
# write.csv(sum.out, "Summary_spp_relations.csv")
# 
# # Number of species with negative human presence but positive traffic relations:
# sum(str_sub(out["beta.HumanPresence",], 1, 1) == "-" & str_sub(out["beta.HumanPresence",], -1, -1) == "*" &
#       str_sub(out["beta.LogTrafficNoZeros",], 1, 1) != "-" & str_sub(out["beta.LogTrafficNoZeros",], -1, -1) == "*")
# dimnames(out)[[2]][which(str_sub(out["beta.HumanPresence",], 1, 1) == "-" & str_sub(out["beta.HumanPresence",], -1, -1) == "*" &
#       str_sub(out["beta.LogTrafficNoZeros",], 1, 1) != "-" & str_sub(out["beta.LogTrafficNoZeros",], -1, -1) == "*")]
# 
# # Number of species with quadratic relationships with date or time of traffic
# sum(str_sub(out["beta.Traffic_DOY_mn2",], -1, -1) == "*" | str_sub(out["beta.TOD_mean2",], -1, -1) == "*")
