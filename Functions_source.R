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
  quadratic <- str_sub(vnam, -2, -1) == "^2"
  if(quadratic) vnam <- str_sub(vnam, 1, -3)
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
  if(quadratic) {
    eval(parse(text = str_c("p <- p + ylab(expression(hat(", pnam, ")['", vnam, "'^2]))")))
  } else {
    eval(parse(text = str_c("p <- p + ylab(expression(hat(", pnam, ")['", vnam, "']))")))
  }
  p <- p + xlab(NULL) +
    theme(axis.title.y=element_text(size=30)) +
    theme(axis.title.x=element_text(size=30)) +
    theme(axis.text.x=element_text(size=15)) +
    theme(axis.text.y=element_text(size=15)) +
    guides(color = "none")
  return(p)
}

HillShannon <- function(N) {
  p <- N / sum(N)
  D <- exp(-1*sum(p * log(p)))
  return(D)
}
