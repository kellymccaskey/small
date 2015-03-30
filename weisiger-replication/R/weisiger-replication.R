# set working directory
setwd("~/Dropbox/projects/small-sample-logit/weisiger-replication/")

# load data
d <- read.csv("data/conq_ins_data.tab", sep = "\t")

# load packages
library(logistf)
library(coda)
library(texreg)
library(MASS)
library(compactr)
library(boot)

# create variables
d$terrain_alt <- d$terrain/100
d$gdppc2_alt <- d$gdppc2/1000

# create function
f <- resist ~ polity_conq + lndist + terrain_alt + soldperterr + gdppc2_alt + coord

# estimate models
lpm <- lm(f, d)
logit <- glm(f, d, family = binomial)
logistf <- logistf(f, d)

# create function for bootstrap
firth <- function(f, data, indices) {
  data <- d[indices,] # allows boot to select sample 
  m <- logistf(f, data)
  return(m$coef) 
} 

# bootstrap
bootstrap <- boot(d, statistic = firth, R = 1000, f = f)

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
beta.hat.lpm <- coef(lpm)
beta.hat.logit <- coef(logit)
beta.hat.logistf <- coef(logistf)

X.hi <- c(1, x.polity_conq.dem, x.lndist, x.terrain_alt, x.soldperterr, x.gdppc2_alt, x.coord.hi)
X.lo <- c(1, x.polity_conq.dem, x.lndist, x.terrain_alt, x.soldperterr, x.gdppc2_alt, x.coord.lo)
X.c <- rbind(X.hi, X.lo)

Sigma.lpm <- vcov(lpm)
beta.tilde.lpm <- mvrnorm(1000, beta.hat.lpm, Sigma.lpm)

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

# bootstrap first differences
p.tilde.b <- plogis(X.c%*%t(bootstrap$t))
fd.tilde.b <- p.tilde.b[1, ] - p.tilde.b[2, ]
hist(fd.tilde.b)
p.hat.b <- plogis(X.c%*%(beta.hat.logistf))
fd.hat.b <- p.hat.b[1, ] - p.hat.b[2, ]

# create first difference plot
# plot the 90% confidence intervals of the first differences
par(mfrow = c(1,1), mar = c(3,1,1,1), oma = c(0,0,0,0))
eplot(xlim = c(-0.01, 1), ylim = c(0, 4.5),
      xlab = "First Difference", 
      anny = FALSE,
      main = "Effect of Coordination")
abline(v = 0, col = "grey50")

# add bootstrap
ht <- 0.5
est <- median(fd.hat.b)
lwr <- quantile(fd.tilde.b, 0.05)
upr <- quantile(fd.tilde.b, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht,ht))
text(est, ht, "PMLE w/ bootstrap", pos = 3, cex = .8)

# add PMLE
ht <- 1.5
est <- median(fd.hat.logistf)
lwr <- quantile(fd.tilde.logistf, 0.05)
upr <- quantile(fd.tilde.logistf, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht,ht))
text(est, ht, "PMLE w/ asymp. approx.", pos = 3, cex = .8)

# add MLE 
ht <- 2.5
est <- median(fd.hat.logit)
lwr <- quantile(fd.tilde.logit, 0.05)
upr <- quantile(fd.tilde.logit, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht, ht))
text(est, ht, "MLE", pos = 3, cex = .8)

# add OLS
ht <- 3.5
est <- median(fd.hat.lpm)
lwr <- quantile(fd.tilde.lpm, 0.05)
upr <- quantile(fd.tilde.lpm, 0.95)
points(est, ht, pch = 19)
lines(c(lwr, upr), c(ht, ht))
text(est, ht, "OLS", pos = 3, cex = .8)

# some p-values
mean(fd.tilde.lpm < 0)
mean(fd.tilde.logistf < 0)
mean(fd.tilde.logit < 0)
mean(fd.tilde.b < 0)

# these are extra checks, you don't need to run these

# some checks on the Jeffrey's prior simulations
# coda::effectiveSize(jeffreys$mcmc.chains)
# coda::autocorr.plot(jeffreys$mcmc.chains)
# coda::gelman.diag(jeffreys$mcmc.chains)
# coda::gelman.plot(jeffreys$mcmc.chains)

# compare jeffreys' logit highest posterior density with logistf profile likelihood
# HPDinterval(as.mcmc(jeffreys$mcmc))
# logistf

# distribution plot of first differences
plot(density(fd.tilde.logit), main = "Coordination First Differences", 
     lwd = 2, xlab = "", ylim = c(0, 2.5))
lines(quantile(fd.tilde.logit, 0.05), lty = 3)
lines(quantile(fd.tilde.logit, 0.95), lty = 3)
lines(density(fd.tilde.logistf), col = "grey", lwd = 3)
lines(density(fd.tilde.b), col = "red", lwd = 2)

# some confidence intervals
quantile(fd.tilde.lpm, c(0.05, 0.50, 0.95))
quantile(fd.tilde.logit, c(0.05, 0.50, 0.95))
quantile(fd.tilde.logistf, c(0.05, 0.50, 0.95))
quantile(fd.tilde.b, c(0.05, 0.50, 0.95))



