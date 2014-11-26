#' @title Obtain Posterior Simulations Using Jeffreys Prior
#' 
#' @description
#' \code{logit.jeffreys()} obtains the posterior simulations using Jeffrey's prior.
#' 
#' 
#' @param f A logistic regression model.
#' @param d A data frame.
#' @param n.sims The amount of simulations.
#' @param n.burnin The number of burn-in iterations for the sample.
#' @param n.thin The thinning interval used in the simulation. The number of MCMC 
#'                iterations must be divisible by this value.
#' @param n.chains The number of MCMC chains being run.
#' @param n.cores The number of MCMC cores.
#'                  



jeffreys.logit <- function(f, data, n.sims = 1000, n.burnin = 100, n.thin = 1,
                  tune = 1, n.chains = 3, n.cores = n.chains) {
  # see MCMCmetrop1R in 
  require(MCMCpack)
  require(Matrix)
  require(logistf)
  require(parallel)
  mf <- model.frame(f, data) 
  y <- model.response(mf)
  X <- model.matrix(f, data)
  X <- Matrix(X)
  cat("\nComputing proposal distribution...\n")
  pmle <- logistf(f, d)
  V <- vcov(pmle)
  #print(summary(mle))
  mcmc.chains <- mcmc.list()
  mcmc <- NULL
  l.seed <- runif(6, 100000, 999999)
  init.seed <- runif(n.chains, 100000, 999999)
  run.mcmc <- function(x) {
    set.seed(init.seed[x])
    init <- rnorm(ncol(X), coef(pmle), 1)
    mcmc <- MCMCmetrop1R(fun = lp.jeffreys, 
                         theta.init = init,  V = V,
                         X = X, y = y, 
                         thin = n.thin, burnin = n.burnin, mcmc = n.sims,
                         tune = tune, verbose = 0,
                         seed = list(l.seed, x))
    return(mcmc)
  }
  cat(paste("\nRunning ", n.chains, " chains in parallel of ", n.sims + n.burnin, " iterations each--this may take a while...", sep = ""))
  mcmc.chains <- mclapply(1:n.chains, run.mcmc, mc.cores = n.cores)
  cat(paste("\nFinished running chains!\n", sep = ""))
  mcmc.chains <- as.mcmc.list(mcmc.chains)
  for (i in 1:n.chains) {  
    mcmc <- rbind(mcmc, mcmc.chains[[i]])
  }
  colnames(mcmc) <- colnames(X)
  R.hat <- gelman.diag(mcmc.chains)
  cat(paste("\nChecking convergence...\n", sep = ""))
  if (R.hat[[2]] <= 1.02) {
    cat(paste("\nThe multivariate R-hat statistic of ", round(R.hat[[2]], 2), 
              " suggests that the chains have converged.\n\n", sep = ""))
  }
  if (R.hat[[2]] > 1.02) {
    cat(paste("\n######## WARNING: #########\n\nThe multivariate R-hat statistic of ", round(R.hat[[2]], 2), 
              " suggests that the chains have NOT converged.\n\n", sep = ""))
  }
  data <- list(X = matrix(X),
               y = y)
  fn.args <- list(f = f, 
               data = data,
               n.sims = n.sims,
               n.burnin = n.burnin, 
               n.thin = n.thin,
               tune = tune, 
               n.chains = n.chains, 
               n.cores = n.cores)
  res <- list(mcmc.chains = mcmc.chains,
              mcmc = mcmc,
              R.hat = R.hat,
              pmle = pmle,
              data = data,
              fn.args = fn.args)
  return(res)
}

# # test
# set.seed(1234)
# x <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
# y <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
# d <- data.frame(x, y)
# m <- jeffreys.logit(y ~ x, d, n.sims = 100, n.burnin = 10, n.chains = 3)
# plot(m$mcmc.chains)

