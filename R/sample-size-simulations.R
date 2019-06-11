
# load packages
library(tidyverse)
library(brglm2)
library(foreach)
library(doParallel)
library(doRNG)

# set seed
# > runif(1)
# [1] 0.760381
set.seed(760381)


simulate_pars <- function() {
  k <- floor(runif(1, 3, 13))  # number of covariates
  n <- floor(runif(1, 200, 3001))  # number of observations
  b_cons <- runif(1, -4, 4)  # value of the interecept
  b <- rnorm(k, sd = 0.5)  # value of the other coefficients
  beta <- c(b_cons, b)  # combine intercept with other coefficients
  Sigma <- genPositiveDefMat("unifcorrmat", dim = k, rangeVar = c(0.25, 2))$Sigma  # covariance matrix for covariates
  X <- cbind(1, mvrnorm(n, rep(0, k), Sigma))  # design matrix
  Xbeta <- X%*%beta  # predicted values on latent scale
  res <- list(n = n,
              k = k,
              beta = beta,
              Sigma = Sigma,
              X = X, 
              Xbeta = Xbeta)
  return(res)
  }

generate_data_and_est <- function(pars) {
  p <- plogis(pars$Xbeta)
  y <- rbinom(pars$n, 1, p)
  mle <- glm.fit(pars$X, y, family = binomial())
  pmle_est <- brglm.fit(pars$X, y)$coefficient
  mle_est <- mle$coefficient
  res <- list(pmle_est = pmle_est,
              mle_est = mle_est,
              sep = sum(abs(mle_est) > 10) > 0)
  return(res)
}

calc_mse <- function(ests, true) {
  apply(t(t(ests) - true)^2, 2, mean)
}


simulate_ests <- function(pars, n_sims) {
  pmle_res <- mle_res <- matrix(NA, nrow = n_sims, ncol = pars$k + 1)
  sep <- 0
  for (i in 1:n_sims) {
    sde <- generate_data_and_est(pars)
    pmle_res[i, ] <- sde$pmle_est
    mle_res[i, ] <- sde$mle_est
    sep <- sep + sde$sep
  }
  pmle_mse <- calc_mse(pmle_res, pars$beta)
  mle_mse <- calc_mse(mle_res, pars$beta)
  res <- list(pmle_ests = pmle_res,
              mle_ests = mle_res,
              sep = sep,
              pmle_mse = pmle_mse,
              mle_mse = mle_mse,
              mse_infl = 100*(mle_mse/pmle_mse - 1),
              true = pars$beta,
              info = min(sum(plogis(pars$Xbeta)), sum(1 - plogis(pars$Xbeta)))/pars$k)
  return(res)
}

simulate <- function(n_sims) {
	pars <- simulate_pars()
	ests <- simulate_ests(pars, n_sims = n_sims)
	beta_k <- 0:pars$k
	par_type <- rep("Intercept", length(beta_k))
	par_type[beta_k > 0] <- "Slope Coefficient"
	df <- data.frame(
		k = pars$k,
		beta_k = beta_k, 
		par_type = par_type,
		true_value = pars$beta,
		mse_infl = ests$mse_infl, 
		info = ests$info,
		n = pars$n, 
		sep = ests$sep)
	return(df)
} 

if (file.exists("sample-size-simulations-progress.log")) file.remove("sample-size-simulations-progress.log")
cl <- makeCluster(detectCores(), outfile = "sample-size-simulations-progress.log")
registerDoParallel(cl)
start_time <- Sys.time()
n_dgps <- 1000
df <- foreach (i = 1:n_dgps, 
               .combine = rbind, 
               .packages = c("clusterGeneration", "brglm")) %dorng% {
                 start_time <- Sys.time()
                 time_working <- difftime(Sys.time(), start_time, units = "auto")
                 
                 cat(paste("Working on DGP ", i, " of ", n_dgps, " after ", 
                           round(time_working[[1]]),  " ", units(time_working), " of working. Random draw is ", runif(1), ".\n\n", sep = ""))
                 simulate(n_sims = 2000)
                 
               }
stopCluster(cl)

write_rds(df, path = "simulations/sample-size-simulations.rds")

