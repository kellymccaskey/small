
# set working directory
setwd("~/Dropbox/projects/small-sample-logit")

# clear workspace
rm(list = ls())

# load packages
library(compactr)

# load simulation data
d <- read.csv("R/simulations/sims.csv")

# lpm bias
d$lpm.fd.bias <- d$e.lpm.fd - true.fd
d$lpm.fd.bias.percent <- (d$e.lpm.fd - d$true.fd)/d$true.fd
# mle bias
d$mle.coef.bias <- d$e.mle.coef - true.coef
d$mle.coef.bias.percent <- (d$e.mle.coef - d$true.coef)/d$true.coef
d$mle.fd.bias <- d$e.mle.fd - true.fd
d$mle.fd.bias.percent <- (d$e.mle.fd - d$true.fd)/d$true.fd
d$mle.rr.bias <- d$e.mle.rr - true.rr
d$mle.rr.bias.percent <- (d$e.mle.rr - d$true.rr)/d$true.rr
# pmle bias
d$pmle.coef.bias <- d$e.pmle.coef - d$true.coef
d$pmle.coef.bias.percent <- (d$e.pmle.coef - d$true.coef)/d$true.coef
d$pmle.fd.bias <- d$e.mle.fd - true.fd
d$pmle.fd.bias.percent <- (d$e.pmle.fd - d$true.fd)/d$true.fd
d$pmle.rr.bias <- d$e.mle.rr - true.rr
d$pmle.rr.bias.percent <- (d$e.pmle.rr - d$true.rr)/d$true.rr

# coefficient bias

plot.um <- function(names, main = NULL, ylab = NULL, labels = NULL, h.at = 0) {
  k <- sort(unique(d$k))
  n <- sort(unique(d$n))
  b0 <- sort(unique(d$b0))
  sims <- NULL
  all.data <- NULL
  for (i in 1:length(names)) {
    all.data <- as.vector(d[, names])
  }
  par(mfrow = c(length(b0), length(k)),
      mar = c(1, 1, 1, 1),
      oma = c(3, 3, 3, 1))
  for (m in 1:length(b0)) {
    for (j in 1:length(k)) {
      subset <- (d$b0 == b0[m]) & (d$k == k[j])
      d0 <- d[subset, ]
      eplot(xlim = mm(d0$n), ylim = mm(all.data),
            xlab = "Sample Size",
            ylab = ylab,
            ylabpos = 2.1,
            main = paste(round(100*mean(d0$prop.ones)), "% events, ", k[j], " predictors", sep = ""))
      abline(h = h.at)
      if (m == 1 & j == 1) {
        print(m)
        legend(par("usr")[2], par("usr")[4], xjust = 1, yjust = 1,
               lty = 1:length(names), col = 1:length(names), lwd = 2,
               legend = labels, bty = "n")
      }
      for (i in 1:length(names)) {
        lines(d0$n, d0[, names[i]], lty = i, col = i, lwd = 2)
      }
    }

  }
  mtext(outer = TRUE, side = 3, line = 1, main)
}

plot.um(names = c("pmle.coef.bias.percent", "mle.coef.bias.percent"), 
        main = "Coefficient Bias",
        ylab = "Bias (%)",
        labels = c("PMLE", "MLE"))
plot.um(names = c("pmle.fd.bias.percent", "mle.fd.bias.percent", "lpm.fd.bias.percent"),
        main = "First Difference Bias",
        ylab = "Bias (%)",
        labels = c("PMLE", "MLE", "OLS"))
plot.um(names = c("mle.rr.bias.percent", "pmle.rr.bias.percent"),
        main = "Risk Ratio Bias",
        ylab = "Bias (%)",
        labels = c("PMLE", "MLE"))
plot.um(names = c("coverage.pmle.bs.coef", "coverage.pmle.lp.coef", 
                  "coverage.pmle.aa.coef", "coverage.mle.coef"),
        main = "Coefficient Coverage",
        ylab = "90% CI Coverage",
        labels = c("PMLE w/ BS", "PMLE w/ LP",
                   "PMLE w/ AA", "MLE"),
        h.at = 0.9)
plot.um(names = c("coverage.pmle.bs.fd", "coverage.pmle.aa.fd", 
                  "coverage.mle.fd", "coverage.lpm.fd"),
        main = "First Difference Coverage",
        ylab = "90% CI Coverage",
        labels = c("PMLE w/ BS", "PMLE w/ AA", 
                   "MLE", "OLS"),
        h.at = 0.9)
plot.um(names = c("coverage.pmle.bs.rr", "coverage.pmle.aa.rr", 
                  "coverage.mle.rr"),
        main = "Risk Ratio Coverage",
        ylab = "90% CI Coverage",
        labels = c("PMLE w/ BS", "PMLE w/ AA", 
                   "MLE"),
        h.at = 0.9)
