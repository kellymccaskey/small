
# set working directory
setwd("~/Dropbox/projects/small-sample-logit")

# clear workspace
rm(list = ls())

# load packages
library(compactr)
library(logistf)
library(MASS)
#source("R/fn-logistf-fit.R")

# create function for bootstrap
firth <- function(f, data, indices) {
  data <- d[indices,] # allows boot to select sample 
  m <- logistf(f, data)
  return(m$coef) 
} 

simulate <- function(n, k, b0, b1, n.sims, n.bs = 1000) {
  # set progress bar
  pb <- txtProgressBar(min = 0, max = n.sims, style = 3)
  # set covariates and probabilty
  X <- cbind(1, matrix(rnorm(n*k), nrow = n))
  b <- c(b0, b1, rep(.15, k - 1))
  p <- plogis(X%*%b)
  # set x.hi and x.lo
  x.hi <- qnorm(0.9)
  x.lo <- qnorm(0.1)
  # true values
  true.coef <- b1
  true.fd <- plogis(b0 + b1*x.hi) - plogis(b0 + b1*x.lo)
  true.rr<- plogis(b0 + b1*x.hi)/plogis(b0 + b1*x.lo)
  # create holders
  lpm.fd <- 
    mle.coef <- mle.fd <- mle.rr <- 
    pmle.coef <- pmle.fd <- pmle.rr <- 
    covers.lpm.fd <- 
    covers.mle.coef <- covers.mle.fd <- covers.mle.rr <- 
    covers.pmle.aa.coef <- covers.pmle.aa.fd <- covers.pmle.aa.rr <- 
    covers.pmle.lp.coef <- 
    covers.pmle.bs.coef <- covers.pmle.bs.fd <- covers.pmle.bs.rr <- 
    prop.ones <- numeric(n.sims)
  for (i in 1:n.sims) {
    # simulate outcome variable
    y <- rbinom(n, 1, p)
    mle.fit <- glm(y ~ X - 1, family = "binomial")
    # just start over if data set has separation
    if (sum(abs(coef(mle.fit)) > 7) == 0) {
      # lpm
      lpm.fit <- lm(y ~ X - 1)
      lpm.coef <- coef(lpm.fit)
      lpm.fd[i] <- lpm.coef[2]*x.hi - lpm.coef[2]*x.lo
      lpm.sim <- mvrnorm(10000, coef(lpm.fit), vcov(lpm.fit))
      lpm.fd.ci <- quantile(lpm.sim[, 2]*x.hi - lpm.sim[, 2]*x.lo, c(0.05, 0.95))
      covers.lpm.fd[i] <- (true.fd > lpm.fd.ci[1]) & (true.fd < lpm.fd.ci[2])
      # mle
      # mle.fit <- glm(y ~ X - 1, family = "binomial") already done above!
      mle.coef[i] <- coef(mle.fit)[2]
      mle.fd[i] <- plogis(coef(mle.fit)[1] + coef(mle.fit)[2]*x.hi) - 
        plogis(coef(mle.fit)[1] + coef(mle.fit)[2]*x.lo)
      mle.rr[i] <- plogis(coef(mle.fit)[1] + coef(mle.fit)[2]*x.hi)/
        plogis(coef(mle.fit)[1] + coef(mle.fit)[2]*x.lo)
      mle.sim <- mvrnorm(10000, coef(mle.fit), vcov(mle.fit))
      p.hi.sim <- plogis(mle.sim[, 1] + mle.sim[, 2]*x.hi)
      p.lo.sim <- plogis(mle.sim[, 1] + mle.sim[, 2]*x.lo)
      est <- coef(mle.fit)[2]
      se <- sqrt(diag(vcov(mle.fit)))[2]
      mle.coef.ci <- c(est - 1.64*se, est + 1.64*se)
      mle.fd.ci <- quantile(p.hi.sim - p.lo.sim, c(0.05, 0.95))
      mle.rr.ci <- quantile(p.hi.sim/p.lo.sim, c(0.05, 0.95))
      covers.mle.coef[i] <- (true.coef > mle.coef.ci[1]) & (true.coef < mle.coef.ci[2])
      covers.mle.fd[i] <- (true.fd > mle.fd.ci[1]) & (true.fd < mle.fd.ci[2])
      covers.mle.rr[i] <- (true.rr > mle.rr.ci[1]) & (true.rr < mle.rr.ci[2])
      # pmle
      pmle.fit <- logistf(y ~ X - 1, alpha = 0.1)
      pmle.coef[i] <- coef(pmle.fit)[2]
      pmle.fd[i] <- plogis(coef(pmle.fit)[1] + coef(pmle.fit)[2]*x.hi) - 
        plogis(coef(pmle.fit)[1] + coef(pmle.fit)[2]*x.lo)
      pmle.rr[i] <- plogis(coef(pmle.fit)[1] + coef(pmle.fit)[2]*x.hi)/
        plogis(coef(pmle.fit)[1] + coef(pmle.fit)[2]*x.lo)
      ## asymp. approx
      pmle.sim <- mvrnorm(10000, coef(pmle.fit), vcov(pmle.fit))
      p.hi.sim <- plogis(pmle.sim[, 1] + pmle.sim[, 2]*x.hi)
      p.lo.sim <- plogis(pmle.sim[, 1] + pmle.sim[, 2]*x.lo)
      est <- coef(pmle.fit)[2]
      se <- sqrt(diag(vcov(pmle.fit)))[2]
      pmle.coef.ci <- c(est - 1.64*se, est + 1.64*se)
      pmle.fd.ci <- quantile(p.hi.sim - p.lo.sim, c(0.05, 0.95))
      pmle.rr.ci <- quantile(p.hi.sim/p.lo.sim, c(0.05, 0.95))
      covers.pmle.aa.coef[i] <- (true.coef > pmle.coef.ci[1]) & (true.coef < pmle.coef.ci[2])
      covers.pmle.aa.fd[i] <- (true.fd > pmle.fd.ci[1]) & (true.fd < pmle.fd.ci[2])
      covers.pmle.aa.rr[i] <- (true.rr > pmle.rr.ci[1]) & (true.rr < pmle.rr.ci[2])
      ## lik.prof
      covers.pmle.lp.coef[i] <- (true.coef > pmle.fit$ci.lower[2]) & (true.coef < pmle.fit$ci.upper[2])
      # bootstrap
      ## method 1
      bs.pmle <- matrix(NA, nrow = n.bs, ncol = k + 1)
      for (bs.iter in 1:n.bs) {
        bs.sample <- sample(1:n, n, replace = TRUE)
        bs.fit <- logistf(y[bs.sample] ~ X[bs.sample, ] - 1)
        bs.pmle[bs.iter, ] <- coef(bs.fit)
      }
      #       ## method 2
      #       bs.pmle <- matrix(NA, nrow = n.bs, ncol = k + 1)
      #       control <- logistf.control()
      #       for (bs.iter in 1:n.bs) {
      #         bs.sample <- sample(1:n, n, replace = TRUE)
      #         bs.pmle[bs.iter, ] <- logistf.fit(X[bs.sample, ], y[bs.sample], 
      #                               weight = 1, control = control,
      #                               init = coef(pmle.fit))$beta
      #         print(bs.pmle[bs.iter, ])
    }
    
    bs.p.hi <- plogis(bs.pmle[, 1] + bs.pmle[, 2]*x.hi)
    bs.p.lo <- plogis(bs.pmle[, 1] + bs.pmle[, 2]*x.lo)
    pmle.coef.bs.ci <- quantile(bs.pmle[, 2], c(0.05, 0.95))   
    pmle.fd.bs.ci <- quantile(bs.p.hi - bs.p.lo, c(0.05, 0.95))
    pmle.rr.bs.ci <- quantile(bs.p.hi/bs.p.lo, c(0.05, 0.95))
    covers.pmle.bs.coef[i] <- (true.coef > pmle.coef.bs.ci[1]) & (true.coef < pmle.coef.bs.ci[2])
    covers.pmle.bs.fd[i] <- (true.fd > pmle.fd.bs.ci[1]) & (true.fd < pmle.fd.bs.ci[2])
    covers.pmle.bs.rr[i] <- (true.rr > pmle.rr.bs.ci[1]) & (true.rr < pmle.rr.bs.ci[2])   
    setTxtProgressBar(pb, i)
  }
  
res <- c(n = n,
         k = k,
         b0 = b0, 
         b1 = b1, 
         n.sims = n.sims,
         n.bs = n.bs,
         true.coef = true.coef,
         e.mle.coef = mean(mle.coef),
         e.pmle.coef = mean(pmle.coef),
         true.fd = true.fd,
         e.lpm.fd = mean(lpm.fd),
         e.mle.fd = mean(mle.fd),
         e.pmle.fd = mean(pmle.fd),
         true.rr = true.rr,
         e.mle.rr = mean(mle.rr),
         e.pmle.rr = mean(pmle.rr), 
         coverage.mle.coef = mean(covers.mle.coef),
         coverage.pmle.aa.coef = mean(covers.pmle.aa.coef),
         coverage.pmle.lp.coef = mean(covers.pmle.lp.coef),
         coverage.pmle.bs.coef = mean(covers.pmle.bs.coef),
         coverage.lpm.fd = mean(covers.lpm.fd),
         coverage.mle.fd = mean(covers.mle.fd),
         coverage.pmle.aa.fd = mean(covers.pmle.aa.fd),
         coverage.pmle.bs.fd = mean(covers.pmle.bs.fd),
         coverage.mle.rr = mean(covers.mle.rr),
         coverage.pmle.aa.rr = mean(covers.pmle.aa.rr),
         coverage.pmle.bs.rr = mean(covers.pmle.bs.rr),
         prop.ones = mean(p))
return(res)
}

# # test
sims <- simulate(n = 50, k = 4, b0 = 0, b1 = 0.25, n.sims = 1000, n.bs = 1000)
m <- matrix(sims); rownames(m) <- names(sims); round(m, 3)

k <- c(4, 8)
n <- c(50, 100, 150, 200)[c(1, 2)]
b0 <- c(-1, 0)
b1 <- 0.25
n.sims <- 10
sims <- NULL
for (m in 1:length(b0)) {
  cat(paste("-- b0 = ", b0[m], "\n"))
  for (j in 1:length(k)) {
    cat(paste("---- k = ", k[j], "\n"))
    for (i in 1:length(n)) {
      cat(paste("------ n = ", n[i], "\n"))
      sims0 <- simulate(n[i], k[j], b0[m], b1, n.sims, n.bs = 100)
      sims <- rbind(sims, sims0)
      cat("\n")
    }
  }
}

write.csv(sims, "R/simulations/sims.csv")

