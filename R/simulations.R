
# clear workspace
rm(list = ls())

# set seed
## > runif(1)
## [1] 0.7358836
set.seed(7358836)

# load packages
library(logistf)
library(MASS)
library(readr)
library(dplyr)
library(ggplot2)

# function to perform simulations
simulate <- function(n, k, b0, b1, n.sims = 1000) {
  # set progress bar
  pb <- txtProgressBar(min = 0, max = n.sims, style = 3)
  # set covariates and probabilty
  rho <- 0.0
  sigma <- matrix(rho, k, k); diag(sigma) <- 1
  X <- cbind(1, mvrnorm(n, rep(0, k), Sigma = sigma))
  b <- c(b0, b1, rep(.2, k - 1))
  p <- plogis(X%*%b)
  # true values
  true.int <- b0
  true.coef <- b1
  # create holders
  mle.coef <- pmle.coef <- mle.int <- pmle.int <-
    prop.ones <- numeric(n.sims)
  for (i in 1:n.sims) {
    # simulate outcome variable
    y <- rbinom(n, 1, p)
    mle.fit <- glm(y ~ X - 1, family = "binomial")
    # just start over if data set has separation
    if (sum(abs(coef(mle.fit)) > 7) == 0) {
      # mle
      # mle.fit <- glm(y ~ X - 1, family = "binomial") already done above!
      mle.int[i] <- coef(mle.fit)[1]
      mle.coef[i] <- coef(mle.fit)[2]
      # pmle
      pmle.fit <- logistf(y ~ X - 1, alpha = 0.1)
      pmle.int[i] <- coef(pmle.fit)[1]
      pmle.coef[i] <- coef(pmle.fit)[2]
    }
    setTxtProgressBar(pb, i)
  }
  # mle vars
  res1 <- data.frame(n = n,
           k = k,
           b0 = b0, 
           b1 = b1, 
           n_sims = n.sims,
           true_int = true.int,
           true_coef = true.coef,
           method = "MLE",
           e_int = mean(mle.int),
           sd_int = sd(mle.int),
           mse_int = mean((mle.int - true.int)^2),
           e_coef = mean(mle.coef),
           sd_coef = sd(mle.coef),
           mse_coef = mean((mle.coef - true.coef)^2),
           prop_ones = mean(p))
  # pmle vars
  res2 <- data.frame(n = n,
                     k = k,
                     b0 = b0, 
                     b1 = b1, 
                     n_sims = n.sims,
                     true_int = true.int,
                     true_coef = true.coef,
                     method = "PMLE",
                     e_int = mean(pmle.int),
                     sd_int = sd(pmle.int),
                     mse_int = mean((pmle.int - true.int)^2),
                     e_coef = mean(pmle.coef),
                     sd_coef = sd(pmle.coef),
                     mse_coef = mean((pmle.coef - true.coef)^2),
                     prop_ones = mean(p))
  # combine mle and pmle rows into single data frame
  res <- rbind(res1, res2) 
  return(res)
}

## ----------------------------------
## perform simulations
## ----------------------------------

# set number of variables
k <- c(2, 4, 6)

# set sample sizes
n <- ceiling(seq(40, 150, length.out = 10))

# set values of intercept
b0 <- c(-1, -0.5, 0)

# set values of coefficient of interest
b1 <- 0.5

# number of mc simulations
n.sims <- 10000

# create holder for simulations
sims <- NULL

# do simulations
for (m in 1:length(b0)) {  # loop over intercepts
  cat(paste("-- b0 = ", b0[m], "\n"))  # report intercept
  for (j in 1:length(k)) {  # loop over number of variables 
    cat(paste("---- k = ", k[j], "\n"))  # report number of variables
    for (i in 1:length(n)) {  # loop over sample sizes
      cat(paste("------ n = ", n[i], "\n"))  # report sample sizes
      sims0 <- simulate(n[i], k[j], b0[m], b1, n.sims = n.sims)  # simulate for given parameters
      sims <- rbind(sims, sims0)  # combine with previous data
      cat("\n")  # new line
    }
  }
}

# fix rownames
rownames(sims) <- NULL

# write data
write_csv(sims, "R/simulations/sims.csv")

