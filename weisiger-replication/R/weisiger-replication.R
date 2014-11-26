# set working directory
setwd("~/Dropbox/projects/small/weisiger-replication/")

# load data
d <- read.csv("data/conq_ins_data.tab", sep = "\t")

# load packages
library(logistf)
library(coda)
library(texreg)
library(MASS)
library(compactr)

# create variables
d$terrain_alt <- d$terrain/100
d$gdppc2_alt <- d$gdppc2/1000

# create function
f <- resist ~ polity_conq + lndist + terrain_alt + soldperterr + gdppc2_alt + coord

# estimate models
lpm <- lm(f, d)
logit <- glm(f, d, family = binomial)
logistf <- logistf(f, d)
# run int-lp-jeffreys and fn-jeffreys-logit first
jeffreys <- jeffreys.logit(f, d, n.chains = 4, n.sims = 50000, n.burnin = 5000)
# this will take a while to run. 
# if it returns an error, try running it again, it's temperamental. 

# calculate and compare first differences
# set values to calculate first differences
x.lndist <- median(d$lndist)
x.terrain_alt <- median(d$terrain_alt, na.rm = TRUE)
x.soldperterr <- median(d$soldperterr, na.rm = TRUE)
x.polity_conq.dem <- 10
x.polity_conq.aut <- -10
x.gdppc2_alt <- median(d$gdppc2_alt, na.rm = TRUE)
x.coord <- median(d$coord, na.rm = TRUE)

# calculating first differences
# creating beta.hats; X.lo, X.hi, and X.c matrices; Sigmas; and beta.tildes
beta.hat.lpm <- coef(lpm)
beta.hat.logit <- coef(logit)
beta.hat.logistf <- coef(logistf)

X.hi <- c(1, x.polity_conq.dem, x.lndist, x.terrain_alt, x.soldperterr, x.gdppc2_alt, quantile(d$coord, 1, na.rm= TRUE))
X.lo <- c(1, x.polity_conq.dem, x.lndist, x.terrain_alt, x.soldperterr, x.gdppc2_alt, quantile(d$coord, 0, na.rm= TRUE))
X.c <- rbind(X.hi, X.lo)

Sigma.lpm <- vcov(lpm)
beta.tilde.lpm <- mvrnorm(1000, beta.hat.lpm, Sigma.logit)

Sigma.logit <- vcov(logit)
beta.tilde.logit <- mvrnorm(1000, beta.hat.logit, Sigma.logit)

Sigma.logistf <- vcov(logistf)
beta.tilde.logistf <- mvrnorm(1000, beta.hat.logistf, Sigma.logistf)

# OLS first differences
p.tilde.lpm <- X.c%*%t(beta.tilde.lpm)
fd.tilde.lpm <- p.tilde.lpm[1, ] - p.tilde.lpm[2, ]
hist(fd.tilde.lpm)
p.hat.lpm <- X.c%*%beta.hat.lpm
fd.hat.lpm <- p.hat.lpm[1, ] - p.hat.lpm[2, ]

# MLE first differences
p.tilde.logit <- plogis(X.c%*%t(beta.tilde.logit))
fd.tilde.logit <- p.tilde.logit[1, ] - p.tilde.logit[2, ]
hist(fd.tilde.logit)
p.hat.logit <- plogis(X.c%*%(beta.hat.logit))
fd.hat.logit <- p.hat.logit[1, ] - p.hat.logit[2, ]

# PMLE first differences
p.tilde.logistf <- plogis(X.c%*%t(beta.tilde.logistf))
fd.tilde.logistf <- p.tilde.logistf[1, ] - p.tilde.logistf[2, ]
hist(fd.tilde.logistf)
p.hat.logistf <- plogis(X.c%*%(beta.hat.logistf))
fd.hat.logistf <- p.hat.logistf[1, ] - p.hat.logistf[2, ]

# MCMC first differences
p.tilde.j <- plogis(X.c%*%t(jeffreys$mcmc))
fd.tilde.j <- p.tilde.j[1, ] - p.tilde.j[2, ]
hist(fd.tilde.j)
p.hat.j <- plogis(X.c%*%t(jeffreys$mcmc))
fd.hat.j <- p.hat.j[1, ] - p.hat.j[2, ]

# creating Figure 2
# plot the 90% confidence intervals of the first differences
par(mfrow = c(1,1), mar = c(3,1,1,1), oma = c(0,0,0,0))
eplot(xlim = c(-0.5, 1.1), ylim = c(0, 7.5),
      xlab = "90% Confidence Intervals of Coordination First Difference", anny = FALSE)
abline(v = 0, col = "grey50")

# add OLS
ht <- 0.5
est <- median(fd.hat.lpm)
lwr <- quantile(fd.tilde.lpm, 0.05)
upr <- quantile(fd.tilde.lpm, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht, ht))
text(est, ht, "OLS", pos = 3, cex = .8)

# add MLE 
ht <- 2.5
est <- median(fd.hat.logit)
lwr <- quantile(fd.tilde.logit, 0.05)
upr <- quantile(fd.tilde.logit, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht, ht))
text(est, ht, "MLE", pos = 3, cex = .8)

# add PMLE
ht <- 4.5
est <- median(fd.hat.logistf)
lwr <- quantile(fd.tilde.logistf, 0.05)
upr <- quantile(fd.tilde.logistf, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht,ht))
text(est, ht, "PMLE w/ AA", pos = 3, cex = .8)

# add MCMC
ht <- 6.5
est <- median(fd.hat.j)
lwr <- quantile(fd.tilde.j, 0.05)
upr <- quantile(fd.tilde.j, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht,ht))
text(est, ht, "PMLE w/ MCMC", pos = 3, cex = .8)


# these are extra checks, you don't need to run these

# some checks on the Jeffrey's prior simulations
coda::effectiveSize(jeffreys$mcmc.chains)
coda::autocorr.plot(jeffreys$mcmc.chains)
coda::gelman.diag(jeffreys$mcmc.chains)
coda::gelman.plot(jeffreys$mcmc.chains)

# compare jeffreys' logit highest posterior density with logistf profile likelihood
HPDinterval(as.mcmc(jeffreys$mcmc))
logistf

# distribution plot of first differences
plot(density(fd.tilde.logit), main = "Coordination First Differences", 
     lwd = 2, xlab = "", ylim = c(0, 2.5))
lines(quantile(fd.tilde.logit, 0.05), lty = 3)
lines(quantile(fd.tilde.logit, 0.95), lty = 3)
lines(density(fd.tilde.logistf), col = "grey", lwd = 3)
lines(density(fd.tilde.j), col = "red", lwd = 2)

# some confidence intervals
quantile(fd.tilde.lpm, c(0.05, 0.50, 0.95))
quantile(fd.tilde.logit, c(0.05, 0.50, 0.95))
quantile(fd.tilde.logistf, c(0.05, 0.50, 0.95))
quantile(fd.tilde.j, c(0.05, 0.50, 0.95))

# some p-values
mean(fd.tilde.lpm < 0)
mean(fd.tilde.logistf < 0)
mean(fd.tilde.logit < 0)
mean(fd.tilde.j < 0)

