
# clear working directory
rm(list = ls())

# set working directory
setwd("~/Dropbox/projects/small-sample-logit/weisiger-replication/")

# load packages
library(logistf)
library(texreg)
library(MASS)
library(compactr)
library(boot)
library(arm)
library(cvTools)
library(brglm)
library(texreg)
library(scoring)


# load data
vars <- c("resist", "polity_conq", "lndist",
          "terrain", "soldperterr", "gdppc2", "coord")
d <- na.omit(read.csv("data/conq_ins_data.tab", sep = "\t")[, vars])


# create variables
d$terrain_alt <- d$terrain/100
d$gdppc2_alt <- d$gdppc2/1000

# create function
f <- resist ~ polity_conq + lndist + terrain_alt + soldperterr + gdppc2_alt + coord

# estimate models
mle <- glm(f, d, family = "binomial")
pmle <- brglm(f, d, family = "binomial")

screenreg(list(mle, pmle), stars = 0.1)
coef(pmle)/coef(mle)
# in-sample fit
brier.mle <- brierscore(d$resist ~ predict(mle, type = "response")); mean(brier.mle)
brier.pmle <- brierscore(d$resist ~ predict(pmle, type = "response")); mean(brier.pmle)
log.mle <- logscore(d$resist ~ predict(mle, type = "response")); mean(log.mle)
log.pmle <- logscore(d$resist ~ predict(pmle, type = "response")); mean(log.pmle)


# calculate and compare first differences
# set values to calculate first differences
x.lndist <- median(d$lndist)
x.terrain_alt <- median(d$terrain_alt, na.rm = TRUE)
x.soldperterr <- median(d$soldperterr, na.rm = TRUE)
x.polity_conq.dem <- 10
x.polity_conq.aut <- -10
x.gdppc2_alt <- median(d$gdppc2_alt, na.rm = TRUE)
x.coord.hi <- quantile(d$coord, 1, na.rm = TRUE)
x.coord.lo <- quantile(d$coord, 0, na.rm = TRUE)

# calculating first differences
# creating beta.hats; X.lo, X.hi, and X.c matrices; Sigmas; and beta.tildes
beta.hat.mle <- coef(mle)
beta.hat.pmle <- coef(pmle)

X.hi <- c(1, x.polity_conq.dem, x.lndist, x.terrain_alt, x.soldperterr, x.gdppc2_alt, x.coord.hi)
X.lo <- c(1, x.polity_conq.dem, x.lndist, x.terrain_alt, x.soldperterr, x.gdppc2_alt, x.coord.lo)
X.c <- rbind(X.hi, X.lo)

Sigma.mle <- vcov(mle)
beta.tilde.mle <- mvrnorm(10000, beta.hat.mle, Sigma.mle)

Sigma.pmle <- vcov(pmle)
beta.tilde.pmle <- mvrnorm(10000, beta.hat.pmle, Sigma.pmle)

# MLE first differences
p.tilde.mle <- plogis(X.c%*%t(beta.tilde.mle))
fd.tilde.mle <- p.tilde.mle[1, ] - p.tilde.mle[2, ]
rr.tilde.mle <- p.tilde.mle[1, ]/p.tilde.mle[2, ]
p.hat.mle <- plogis(X.c%*%(beta.hat.mle))
fd.hat.mle <- p.hat.mle[1, ] - p.hat.mle[2, ]
rr.hat.mle <- p.hat.mle[1, ]/p.hat.mle[2, ]

# PMLE first differences
p.tilde.pmle <- plogis(X.c%*%t(beta.tilde.pmle))
fd.tilde.pmle <- p.tilde.pmle[1, ] - p.tilde.pmle[2, ]
rr.tilde.pmle <- p.tilde.pmle[1, ]/p.tilde.pmle[2, ]
p.hat.pmle <- plogis(X.c%*%(beta.hat.pmle))
fd.hat.pmle <- p.hat.pmle[1, ] - p.hat.pmle[2, ]
rr.hat.pmle <- p.hat.pmle[1, ]/p.hat.pmle[2, ]

# create first difference plot
# plot the 90% confidence intervals of the first differences
par(mfrow = c(1,1), mar = c(3,1,1,1), oma = c(0,0,0,0))
eplot(xlim = c(-0.01, 0.7), ylim = c(1, 3),
      xlab = "First Difference", 
      anny = FALSE,
      main = "Effect of Coordination")
abline(v = 0, col = "grey50")

# add PMLE
ht <- 1.5
est <- median(fd.hat.pmle)
lwr <- quantile(fd.tilde.pmle, 0.05)
upr <- quantile(fd.tilde.pmle, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht,ht))
text(est, ht, "PMLE", pos = 3, cex = .8)

# add MLE 
ht <- 2.5
est <- median(fd.hat.mle)
lwr <- quantile(fd.tilde.mle, 0.05)
upr <- quantile(fd.tilde.mle, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht, ht))
text(est, ht, "MLE", pos = 3, cex = .8)

# create risk ratio plot
# plot the 90% confidence intervals of the first differences
par(mfrow = c(1,1), mar = c(3,1,1,1), oma = c(0,0,0,0))
eplot(xlim = log(c(1, 30)), ylim = c(1, 3),
      xlab = "Risk Ratio", 
      anny = FALSE,
      main = "Effect of Coordination",
      xat = log(c(1, 2, 5, 10, 30)),
      xticklab = c(1, 2, 5, 10, 30))
abline(v = 0, col = "grey50")

# add PMLE
ht <- 1.5
est <- rr.hat.pmle
lwr <- quantile(rr.tilde.pmle, 0.05)
upr <- quantile(rr.tilde.pmle, 0.95)
points(log(est), ht, pch = 19)
lines(log(c(lwr, upr)), c(ht,ht))
text(log(est), ht, "PMLE", pos = 3, cex = .8)

# add MLE 
ht <- 2.5
est <- rr.hat.mle
lwr <- quantile(rr.tilde.mle, 0.05)
upr <- quantile(rr.tilde.mle, 0.95)
points(log(est), ht, pch = 19)
lines(log(c(lwr, upr)), c(ht,ht))
text(log(est), ht, "MLE", pos = 3, cex = .8)










# out-sample fit
p <- matrix(NA, ncol = 2, nrow = nrow(d))
colnames(p) <- c("mle", "pmle")
for (i in 1:nrow(d)) {
  # partition training and test sets
  d.train <- d[-i, ]
  d.test <- d[i, ]
  # fit models to training data
  mle <- glm(f, d.train, family = "binomial")
  pmle <- brglm(f, d.train, family = "binomial")
  # predict test set
  p[i, "mle"] <- predict(mle, newdata = d.test, type = "response")
  p[i, "pmle"] <- predict(pmle, newdata = d.test, type = "response")
}
brier.mle <- brierscore(d$resist ~ p[, "mle"]); mean(brier.mle)
brier.pmle <- brierscore(d$resist ~ p[, "pmle"]); mean(brier.pmle)

log.mle <- logscore(d$resist ~ p[, "mle"]); mean(log.mle)
log.pmle <- logscore(d$resist ~ p[, "pmle"]); mean(log.pmle)




