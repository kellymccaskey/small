# load packages
library(tidyverse)
library(brglm2)
library(logistf)
library(foreach)
library(doParallel)
library(doRNG)
library(MASS)

# set seed
# > runif(1)
# [1] 0.760381
set.seed(760381)


# function to perform simulations
simulate <- function(n, k, b0, b1) {
  # set progress bar
  # pb <- txtProgressBar(min = 0, max = n.sims, style = 3)
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
  	# mle
  	mle.fit <- glm(y ~ X - 1, family = "binomial")
  	mle.int[i] <- coef(mle.fit)[1]
  	mle.coef[i] <- coef(mle.fit)[2]
  	# pmle
  	pmle.fit <- logistf(y ~ X - 1, alpha = 0.1)
  	pmle.int[i] <- coef(pmle.fit)[1]
  	pmle.coef[i] <- coef(pmle.fit)[2]
  	#setTxtProgressBar(pb, i)
  }
  mle.coef <- mle.coef[abs(mle.coef) < 10]
  # mle vars
  res1 <- data.frame(n = n,
                     k = k,
                     b0 = b0, 
                     b1 = b1, 
                     n_sims = n.sims,
                     true_coef = true.coef,
                     method = "ML",
                     ev = mean(mle.coef),
                     mc_error_ev = sd(mle.coef)/sqrt(n.sims),
                     bias = mean(mle.coef) - true.coef,
                     percent_bias = 100*(mean(mle.coef)/true.coef - 1),
                     var = var(mle.coef),
                     mse = mean((mle.coef - true.coef)^2),
                     prop_ones = mean(p))
  # pmle vars
  res2 <- data.frame(n = n,
                     k = k,
                     b0 = b0, 
                     b1 = b1, 
                     n_sims = n.sims,
                     true_coef = true.coef,
                     method = "PML",
                     ev = mean(pmle.coef),
                     mc_error_ev = sd(pmle.coef)/sqrt(n.sims),
                     bias = mean(pmle.coef) - true.coef,
                     percent_bias = 100*(mean(pmle.coef)/true.coef - 1),
                     var = var(pmle.coef),
                     mse = mean((pmle.coef - true.coef)^2),
                     prop_ones = mean(p))
  # combine mle and pmle rows into single data frame
  res <- rbind(res1, res2) 
  return(res)
}

## ----------------------------------
## perform simulations
## ----------------------------------
cat("\n\ndoing simulations...\n\n")


# set number of variables
k <- c(3, 6, 9) #seq(1, 10, by = 1)

# set sample sizes
n <- seq(30, 210, by = 10)

# set values of intercept
b0 <- c(-1, -0.5, 0) #seq(-1, 0, by = 0.1)

# set values of coefficient of interest
b1 <- 0.5

par_df <- crossing(k, n, b0, b1) %>%
  mutate(scenario_id = 1:n()) %>%
  glimpse()

# number of mc simulations
n.sims <- 100000

# create holder for simulations
sims <- NULL

# do simulations
if (file.exists("simulations-progress.log")) file.remove("simulations-progress.log")
cl <- makeCluster(detectCores(), outfile = "simulations-progress.log")
registerDoParallel(cl)
start_time <- Sys.time()
sims <- foreach (i = par_df$scenario_id, .combine = rbind, 
                 .packages = c("tidyverse", "brglm", "logistf", "MASS")) %dorng% {  # loop over sample sizes
  par_df_i <- par_df %>%
    filter(scenario_id == i)
  time_working <- difftime(Sys.time(), start_time, units = "auto")
  cat(paste0("\nStarting Scenario ",  i, " of ", 
             max(par_df$scenario_id), " after ", 
             round(time_working[[1]]),  " ", units(time_working), " of working...."))
  simulate(par_df_i$n, par_df_i$k, par_df_i$b0, par_df_i$b1)  # simulate for given parameters
}
stopCluster(cl)

# fix rownames
rownames(sims) <- 1:nrow(sims)

# write data
cat("\n\nsaving simulations to simulations/simulations.rds...\n\n")
write_rds(sims, "simulations/simulations.rds")
