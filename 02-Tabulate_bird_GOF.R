library(stringr)
library(tidyr)
library(dplyr)
library(mcmcOutput)
library(FunctionsBCR)

setwd("C:/Users/quresh.latif/files/projects/CPW/Rec_overlay")
load(str_c("data/Data_compiled.RData"))

#_____ Script inputs _____#
git.repo <- "COREC-analysis/"
mod.nam <- "path"
mod <- R.utils::loadObject(str_c("mod_", mod.nam, "_GOF"))
source(str_c(git.repo, "Param_list.R"))
source(str_c(git.repo, "Data_processing.R"))
#________________________#

# Function for calculating Bayesian R-squared
# Script from Gemini with some adjustments.

calculate_bayes_R2 <- function(E.n) {
  npsamp <- dim(E.n)[1]
  r2_posterior <- numeric(npsamp)
  
  for (i in 1:npsamp) {
    indices <- rep(list(substitute()), length(dim(E.n)))
    indices[[1]] <- i
    E.n.i <- do.call("[", c(list(E.n), indices))

    # 1. Extract the expected counts for this draw and move to link scale (log)
    # This represents (Fixed Effects + Species Random Effects)
    log_mu <- log(E.n.i)
    
    # 2. Calculate the variance of the linear predictor (Var_fit)
    # We treat all species/site combinations as the total variation pool
    var_fit <- var(as.vector(log_mu))
    
    # 3. Calculate Distributional Variance (Var_res)
    # For Poisson with a log link, the observation-level noise on the 
    # link scale is approx log(1 + 1/mean(E.n))
    # Reference: Nakagawa et al. (2017) / Gelman et al. (2019)
    avg_mu <- mean(E.n.i)
    var_res <- log(1 + (1 / avg_mu))
    
    # 4. Calculate R-squared for this draw
    r2_posterior[i] <- var_fit / (var_fit + var_res)
  }
  
  return(r2_posterior)
}

out_GOF <- data.frame(
  Spp = Spp,
  n = apply(n, 1, sum),
  # r.md = as.numeric(NA),
  # r.10 = as.numeric(NA),
  # r.90 = as.numeric(NA),
  # r.pct.na = as.numeric(NA),
  ChiSqr_p = as.numeric(NA),
  Bays_Rsqr.md = as.numeric(NA),
  Bays_Rsqr.10 = as.numeric(NA),
  Bays_Rsqr.90 = as.numeric(NA)
)

n.sim <- mod$mcmcOutput$n_sim
E.n <- mod$mcmcOutput$prob_n
npsamp <- dim(mod$mcmcOutput)[1]

for(sp in Spp) {
  sp.ind <- which(Spp == sp)
  n.obs.sp <- n[sp.ind,]
  n.sim.sp <- n.sim[,sp.ind,]
  E.n.sp <- E.n[,sp.ind,]
  
  # Correlation of n obs vs sim
  # r <- suppressWarnings(apply(n.sim.sp, 1, function(x) cor(x, n.obs.sp)))
  # if(!any(is.na(r))) {
  #   out_GOF$r.md[sp.ind] <- median(r)
  #   out_GOF$r.10[sp.ind] <- quantile(r, probs = 0.1, type = 8)
  #   out_GOF$r.90[sp.ind] <- quantile(r, probs = 0.9, type = 8)
  # } else {
  #   out_GOF$r.md[sp.ind] <- median(r[which(!is.na(r))])
  #   out_GOF$r.10[sp.ind] <- quantile(r[which(!is.na(r))], probs = 0.1, type = 8)
  #   out_GOF$r.90[sp.ind] <- quantile(r[which(!is.na(r))], probs = 0.9, type = 8)
  # }
  # out_GOF$r.pct.na[sp.ind] <- round(sum(is.na(r)) / npsamp * 100)
  
  # Chi-square GOF
  Diff_prod_obs <- t((n.obs.sp - mean(n.obs.sp)) *
    t(E.n.sp - apply(E.n.sp, 1, mean)))
  Diff_prod_sim <- (n.sim.sp - apply(n.sim.sp, 1, mean)) *
    (E.n.sp - apply(E.n.sp, 1, mean))
  Diff_sqr_obs <- ((n.obs.sp - mean(n.obs.sp)) ^ 2) %>%
    matrix(nrow = ngrdyrs, ncol = npsamp) %>% t
  Diff_sqr_sim <- (n.sim.sp - apply(n.sim.sp, 1, mean)) ^ 2
  Diff_sqr_En <- (E.n.sp - apply(E.n.sp, 1, mean)) ^ 2
  
  GOF_X2_obs <- apply(Diff_prod_obs, 1, sum) /
    sqrt(apply(Diff_sqr_obs, 1, sum) * apply(Diff_sqr_En, 1, sum))
  GOF_X2_sim <- apply(Diff_prod_sim, 1, sum) /
    sqrt(apply(Diff_sqr_sim, 1, sum) * apply(Diff_sqr_En, 1, sum))
  out_GOF$ChiSqr_p[sp.ind] <- sum(GOF_X2_sim - GOF_X2_obs > 0) / npsamp
  
  # Bayesian R-sqr
  R2_spp <- calculate_bayes_R2(E.n.sp)
  out_GOF$Bays_Rsqr.md[sp.ind] <- median(R2_spp)
  out_GOF$Bays_Rsqr.10[sp.ind] <- quantile(R2_spp, prob = 0.1, type = 8)
  out_GOF$Bays_Rsqr.90[sp.ind] <- quantile(R2_spp, prob = 0.9, type = 8)
}

# # Global correlation
# r.global <- apply(n.sim, 1, function(x) cor(as.numeric(x), as.numeric(n)))
# BCI(r.global, BCIpercent = 80, flag.sig = FALSE, ndig = 3)
# # Result: 80% BCI = "0.792 (0.788,0.796)"

# Global chi-square
Diff_prod_obs <- (array(n - mean(n), dim = c(dim(n), npsamp)) %>%
                    aperm(perm = c(3, 1, 2))) *
  (E.n - apply(E.n, 1, mean))
Diff_prod_sim <- (n.sim - apply(n.sim, 1, mean)) *
  (E.n - apply(E.n, 1, mean))
Diff_sqr_obs <- ((n - mean(n)) ^ 2) %>%
  array(dim = c(dim(n), npsamp)) %>%
  aperm(perm = c(3, 1, 2))
Diff_sqr_sim <- (n.sim - apply(n.sim, 1, mean)) ^ 2
Diff_sqr_En <- (E.n - apply(E.n, 1, mean)) ^ 2

GOF_X2_obs <- apply(Diff_prod_obs, 1, sum) /
  sqrt(apply(Diff_sqr_obs, 1, sum) * apply(Diff_sqr_En, 1, sum))
GOF_X2_sim <- apply(Diff_prod_sim, 1, sum) /
  sqrt(apply(Diff_sqr_sim, 1, sum) * apply(Diff_sqr_En, 1, sum))
sum(GOF_X2_sim - GOF_X2_obs > 0) / npsamp
# p = 1

# Bayesian R-squared
R2 <- calculate_bayes_R2(E.n)
BCI(R2, BCIpercent = 80, ndig = 3, flag.sig = FALSE)

write.csv(out_GOF, "Species_GOF.csv", row.names = FALSE)
