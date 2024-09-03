## Cluster sizes (need to multiply estimated abundance of clusters by this to get abundance) ##
#*** Just using the mean cluster size for now. Need to figure out the correct way for including uncertainty in the mean.
cl.size.tab <- dtobs[, c("SppInd", "CL_Count")] %>% as.data.frame %>%
  group_by(SppInd) %>%
  summarise(n = n(),
            cl_mean = mean(CL_Count),
            cl_se = sd(CL_Count) / sqrt(n())) %>%
  mutate(BirdCode = Spp[SppInd],
         cl_se = ifelse(cl_se == 0, NA, cl_se))
cl_mean <- cl.size.tab$cl_mean
cl_se <- cl.size.tab$cl_se
names(cl_mean) <- names(cl_se) <- cl.size.tab$BirdCode

# cl.list <- list()
# for(i in 1:length(Spp)) cl.list[[i]] <- dtobs[which(dtobs[, "SppInd"] == i), "CL_Count"]
# names(cl.list) <- Spp
# 
# md <- glm((cl.list[[1]] - 0.9) ~ 1, family = Gamma(link = 'log'))
# md$coefficients
# 0.9 + exp(rnorm(10, md$coefficients, vcov(md)))
