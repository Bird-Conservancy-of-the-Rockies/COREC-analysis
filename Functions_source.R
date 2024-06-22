logit <- function(x) log(x/(1-x))

expit <- function(x) exp(x)/(1+exp(x))

BCI <- function (x, ndig = 2, BCIpercent = 95, flag.sig = T) {
  plo <- 0 + (1 - BCIpercent/100)/2
  phi <- 1 - (1 - BCIpercent/100)/2
  md <- median(x)
  lo <- quantile(x, prob = plo, type = 8)
  hi <- quantile(x, prob = phi, type = 8)
  x.sum <- ifelse((lo > 0 | hi < 0) & flag.sig,
                  str_c(round(md, digits = ndig),
                        " (", round(lo, digits = ndig),
                        ",", round(hi, digits = ndig), ")*"),
                  str_c(round(md, digits = ndig),
                        " (", round(lo, digits = ndig),
                        ",", round(hi, digits = ndig), ")"))
  return(x.sum)
}

# dplt.view_Trt_trend.fn <- function(ind.EA, ind.GZ, nsamp,
#                                    alpha0, alphaVec = NULL, X.alpha,
#                                    DELTA0, DELTAVec = NULL, X.DELTA,
#                                    ETA0, ETAVec = NULL, X.ETA,
#                                    beta0, betaVec = NULL, X.beta,
#                                    delta0, deltaVec = NULL, X.delta,
#                                    CIpct = 80, community.model = TRUE) {
# 
#   qnt.lo <- 0 + (1 - (CIpct / 100)) / 2
#   qnt.hi <- 1 - (1 - (CIpct / 100)) / 2
#   
#   # Initial occupancy
#   alpha0 <- alpha0 %>%
#     array(dim = c(nsamp, 2))
#   
#   X.EA <- apply(X.alpha[ind.EA,], 2, mean)
#   X.EA.hi <- X.EA.lo <- X.EA
#   X.GZ <- apply(X.alpha[ind.GZ,], 2, mean)
#   X.GZ.hi <- X.GZ.lo <- X.GZ
#   
#   X.EA.lo["Trt_EA"] <- min(X.alpha[,"Trt_EA"])
#   X.EA.hi["Trt_EA"] <- max(X.alpha[,"Trt_EA"])
#   X.EA.lo["Trt_GZ"] <- min(X.alpha[,"Trt_GZ"])
#   X.EA.hi["Trt_GZ"] <- min(X.alpha[,"Trt_GZ"])
#   
#   X.GZ.lo["Trt_GZ"] <- min(X.alpha[,"Trt_GZ"])
#   X.GZ.hi["Trt_GZ"] <- max(X.alpha[,"Trt_GZ"])
#   X.GZ.lo["Trt_EA"] <- min(X.alpha[,"Trt_EA"])
#   X.GZ.hi["Trt_EA"] <- min(X.alpha[,"Trt_EA"])
#   
#   BX.EA.lo <- as.numeric(alphaVec %*% X.EA.lo)
#   BX.EA.hi <- as.numeric(alphaVec %*% X.EA.hi)
#   BX.EA <- cbind(BX.EA.lo, BX.EA.hi)
#   
#   BX.GZ.lo <- as.numeric(alphaVec %*% X.GZ.lo)
#   BX.GZ.hi <- as.numeric(alphaVec %*% X.GZ.hi)
#   BX.GZ <- cbind(BX.GZ.lo, BX.GZ.hi)
#   
#   psi0.EA <- expit(alpha0 + BX.EA)
#   psi0.GZ <- expit(alpha0 + BX.GZ)
#   
#   # Colonization
#   DELTA0 <- DELTA0 %>%
#     array(dim = c(nsamp, 2))
#   
#   X.EA <- apply(X.DELTA[ind.EA,], 2, mean)
#   X.EA.hi <- X.EA.lo <- X.EA
#   X.EA.lo["Trt_EA"] <- min(X.DELTA[,"Trt_EA"])
#   X.EA.lo["Trt_GZ"] <- min(X.DELTA[,"Trt_GZ"])
#   BX.EA.lo <- as.numeric(DELTAVec %*% X.EA.lo)
#   X.EA.hi["Trt_EA"] <- max(X.DELTA[,"Trt_EA"])
#   X.EA.hi["Trt_GZ"] <- min(X.DELTA[,"Trt_GZ"])
#   BX.EA.hi <- as.numeric(DELTAVec %*% X.EA.hi)
#   BX.EA <- cbind(BX.EA.lo, BX.EA.hi)
#   
#   X.GZ <- apply(X.DELTA[ind.GZ,], 2, mean)
#   X.GZ.hi <- X.GZ.lo <- X.GZ
#   X.GZ.lo["Trt_GZ"] <- min(X.DELTA[,"Trt_GZ"])
#   X.GZ.lo["Trt_EA"] <- min(X.DELTA[,"Trt_EA"])
#   BX.GZ.lo <- as.numeric(DELTAVec %*% X.GZ.lo)
#   X.GZ.hi["Trt_GZ"] <- max(X.DELTA[,"Trt_GZ"])
#   X.GZ.hi["Trt_EA"] <- min(X.DELTA[,"Trt_EA"])
#   BX.GZ.hi <- as.numeric(DELTAVec %*% X.GZ.hi)
#   BX.GZ <- cbind(BX.GZ.lo, BX.GZ.hi)
#   
#   GAMMA.EA <- expit(DELTA0 + BX.EA)
#   GAMMA.GZ <- expit(DELTA0 + BX.GZ)
#   
#   # Persistence
#   ETA0 <- ETA0 %>%
#     array(dim = c(nsamp, 2))
#   
#   X.EA <- apply(X.ETA[ind.EA,], 2, mean)
#   X.EA.hi <- X.EA.lo <- X.EA
#   X.EA.lo["Trt_EA"] <- min(X.ETA[,"Trt_EA"])
#   X.EA.lo["Trt_GZ"] <- min(X.ETA[,"Trt_GZ"])
#   BX.EA.lo <- as.numeric(ETAVec %*% X.EA.lo)
#   X.EA.hi["Trt_EA"] <- max(X.ETA[,"Trt_EA"])
#   X.EA.hi["Trt_GZ"] <- min(X.ETA[,"Trt_GZ"])
#   BX.EA.hi <- as.numeric(ETAVec %*% X.EA.hi)
#   BX.EA <- cbind(BX.EA.lo, BX.EA.hi)
#   
#   X.GZ <- apply(X.ETA[ind.GZ,], 2, mean)
#   X.GZ.hi <- X.GZ.lo <- X.GZ
#   X.GZ.lo["Trt_GZ"] <- min(X.ETA[,"Trt_GZ"])
#   X.GZ.lo["Trt_EA"] <- min(X.ETA[,"Trt_EA"])
#   BX.GZ.lo <- as.numeric(ETAVec %*% X.GZ.lo)
#   X.GZ.hi["Trt_GZ"] <- max(X.ETA[,"Trt_GZ"])
#   X.GZ.hi["Trt_EA"] <- min(X.ETA[,"Trt_EA"])
#   BX.GZ.hi <- as.numeric(ETAVec %*% X.GZ.hi)
#   BX.GZ <- cbind(BX.GZ.lo, BX.GZ.hi)
#   
#   OMEGA.EA <- expit(ETA0 + BX.EA)
#   OMEGA.GZ <- expit(ETA0 + BX.GZ)
#   
#   # Initial abundance at occupied grid cells
#   beta0 <- beta0 %>%
#     array(dim = c(nsamp, 2))
#   
#   X.EA <- apply(X.beta[ind.EA,], 2, mean)
#   X.EA.hi <- X.EA.lo <- X.EA
#   X.EA.lo["Trt_EA"] <- min(X.beta[,"Trt_EA"])
#   X.EA.lo["Trt_GZ"] <- min(X.beta[,"Trt_GZ"])
#   BX.EA.lo <- as.numeric(betaVec %*% X.EA.lo)
#   X.EA.hi["Trt_EA"] <- max(X.beta[,"Trt_EA"])
#   X.EA.hi["Trt_GZ"] <- min(X.beta[,"Trt_GZ"])
#   BX.EA.hi <- as.numeric(betaVec %*% X.EA.hi)
#   BX.EA <- cbind(BX.EA.lo, BX.EA.hi)
#   
#   X.GZ <- apply(X.beta[ind.GZ,], 2, mean)
#   X.GZ.hi <- X.GZ.lo <- X.GZ
#   X.GZ.lo["Trt_GZ"] <- min(X.beta[,"Trt_GZ"])
#   X.GZ.lo["Trt_EA"] <- min(X.beta[,"Trt_EA"])
#   BX.GZ.lo <- as.numeric(betaVec %*% X.GZ.lo)
#   X.GZ.hi["Trt_GZ"] <- max(X.beta[,"Trt_GZ"])
#   X.GZ.hi["Trt_EA"] <- min(X.beta[,"Trt_EA"])
#   BX.GZ.hi <- as.numeric(betaVec %*% X.GZ.hi)
#   BX.GZ <- cbind(BX.GZ.lo, BX.GZ.hi)
#   
#   N0.EA <- exp(beta0 + BX.EA)
#   N0.GZ <- exp(beta0 + BX.GZ)
#   
#   # Population growth at occupied grid cells
#   delta0 <- delta0 %>%
#     array(dim = c(nsamp, 2))
#   
#   X.EA <- apply(X.delta[ind.EA,], 2, mean)
#   X.EA.hi <- X.EA.lo <- X.EA
#   X.EA.lo["Trt_EA"] <- min(X.delta[,"Trt_EA"])
#   X.EA.lo["Trt_GZ"] <- min(X.delta[,"Trt_GZ"])
#   BX.EA.lo <- as.numeric(deltaVec %*% X.EA.lo)
#   X.EA.hi["Trt_EA"] <- max(X.delta[,"Trt_EA"])
#   X.EA.hi["Trt_GZ"] <- min(X.delta[,"Trt_GZ"])
#   BX.EA.hi <- as.numeric(deltaVec %*% X.EA.hi)
#   BX.EA <- cbind(BX.EA.lo, BX.EA.hi)
#   
#   X.GZ <- apply(X.delta[ind.GZ,], 2, mean)
#   X.GZ.hi <- X.GZ.lo <- X.GZ
#   X.GZ.lo["Trt_GZ"] <- min(X.delta[,"Trt_GZ"])
#   X.GZ.lo["Trt_EA"] <- min(X.delta[,"Trt_EA"])
#   BX.GZ.lo <- as.numeric(deltaVec %*% X.GZ.lo)
#   X.GZ.hi["Trt_GZ"] <- max(X.delta[,"Trt_GZ"])
#   X.GZ.hi["Trt_EA"] <- min(X.delta[,"Trt_EA"])
#   BX.GZ.hi <- as.numeric(deltaVec %*% X.GZ.hi)
#   BX.GZ <- cbind(BX.GZ.lo, BX.GZ.hi)
#   
#   r.EA <- exp(delta0 + BX.EA)
#   r.GZ <- exp(delta0 + BX.GZ)
#   
#   # Yearly predicted abundance
#   psi.EA <- N_cond.EA <- psi.GZ <- N_cond.GZ <-
#     array(NA, dim = c(nsamp, 2, length(year)))
#   psi.EA[,,1] <- psi0.EA
#   N_cond.EA[,,1] <- N0.EA
#   psi.GZ[,,1] <- psi0.GZ
#   N_cond.GZ[,,1] <- N0.GZ
#   
#   for(t in 2:length(year)) {
#     psi.EA[,,t] <- psi.EA[,,(t-1)] * OMEGA.EA + (1 - psi.EA[,,(t-1)]) * GAMMA.EA
#     N_cond.EA[,,t] <- N_cond.EA[,,(t-1)] * r.EA
#     psi.GZ[,,t] <- psi.GZ[,,(t-1)] * OMEGA.GZ + (1 - psi.GZ[,,(t-1)]) * GAMMA.GZ
#     N_cond.GZ[,,t] <- N_cond.GZ[,,(t-1)] * r.GZ
#   }
#   N.EA <- psi.EA * N_cond.EA
#   lo.trend <- N.EA[,1,length(year)] / N.EA[,1,1]
#   hi.trend <- N.EA[,2,length(year)] / N.EA[,2,1]
#   trend.EA <<- c(lo = BCI(lo.trend, BCIpercent = CIpct, flag.sig = F),
#                  hi = BCI(hi.trend, BCIpercent = CIpct, flag.sig = F),
#                  p = (sum((hi.trend - lo.trend) > 0) / nsamp) %>% round(digits = 2))
#   N.EA <- abind::abind(N.EA[,1,], N.EA[,2,], along = 2)
#   
#   N.GZ <- psi.GZ * N_cond.GZ
#   lo.trend <- N.GZ[,1,length(year)] / N.GZ[,1,1]
#   hi.trend <- N.GZ[,2,length(year)] / N.GZ[,2,1]
#   trend.GZ <<- c(lo = BCI(lo.trend, BCIpercent = CIpct, flag.sig = F),
#                  hi = BCI(hi.trend, BCIpercent = CIpct, flag.sig = F),
#                  p = (sum((hi.trend - lo.trend) > 0) / nsamp) %>% round(digits = 2))
#   N.GZ <- abind::abind(N.GZ[,1,], N.GZ[,2,], along = 2)
#   
#   dat.plt.EA <<- data.frame(Trt_EA = rep(c(min(Cov_grid[,"Trt_EA"]),
#                                            max(Cov_grid[,"Trt_EA"])),
#                                          each = length(year)),
#                             Year = rep(year, 2),
#                             N.md = apply(N.EA, 2, median),
#                             N.lo = apply(N.EA, 2, function(x) quantile(x, prob = qnt.lo, type = 8)),
#                             N.hi = apply(N.EA, 2, function(x) quantile(x, prob = qnt.hi, type = 8)))
#   
#   dat.plt.GZ <<- data.frame(Trt_GZ = rep(c(min(Cov_grid[,"Trt_GZ"]),
#                                            max(Cov_grid[,"Trt_GZ"])),
#                                          each = length(year)),
#                             Year = rep(year, 2),
#                             N.md = apply(N.GZ, 2, median),
#                             N.lo = apply(N.GZ, 2, function(x) quantile(x, prob = qnt.lo, type = 8)),
#                             N.hi = apply(N.GZ, 2, function(x) quantile(x, prob = qnt.hi, type = 8)))
# }

tabulate_community_effect <- function(Spp, beta, BCIpercent = 95) {
  qlo <- (1 - (BCIpercent / 100)) / 2
  qhi <- 1 - qlo
  dat <- data.frame(Spp = Spp,
                    index = rev(1:length(Spp)),
                    beta.md = apply(beta, 2, median),
                    beta.lo = apply(beta, 2,
                                    function(x)
                                      quantile(x, prob = qlo, type = 8)),
                    beta.hi = apply(beta, 2,
                                    function(x)
                                      quantile(x, prob = qhi, type = 8))) %>%
    mutate(beta.supp = ifelse(beta.hi < 0, "neg", ifelse(beta.lo > 0, "pos", "none"))) %>%
    mutate(beta.supp = factor(beta.supp, levels = c("neg", "none", "pos")))
  return(dat)
}

plot_community_effects <- function(dat, min.y, max.y, pnam, vnam) {
  p <- ggplot(dat = dat, aes(x = index, y = beta.md)) +
    geom_errorbar(aes(ymin = beta.lo, ymax = beta.hi, color = beta.supp),
                  linewidth = 1, width = 0) +
    geom_point(size = 2.5, aes(color = beta.supp)) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_x_continuous(breaks = dat$index, labels = dat$Spp, expand=c(0, 1)) +
    scale_y_continuous(lim = c(min.y, max.y))
  if(any(dat$beta.supp == "neg")) {
    p <- p + scale_color_manual(values = c("#0072B2", "#000000", "#D55E00"))
  } else {
    if(any(dat$beta.supp == "none")) {
      p <- p + scale_color_manual(values = c("#000000", "#D55E00"))
    } else {
      p <- p + scale_color_manual(values = c("#D55E00"))
    }
  }
  eval(parse(text = str_c("p <- p + ylab(expression(hat(", pnam, ")['", vnam, "']))")))
  p <- p + xlab(NULL) +
    theme(axis.title.y=element_text(size=30)) +
    theme(axis.title.x=element_text(size=30)) +
    theme(axis.text.x=element_text(size=15)) +
    theme(axis.text.y=element_text(size=15)) +
    guides(color = "none")
  return(p)
}
