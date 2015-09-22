
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

# number of mc simulations
n.sims <- 10000

# create holder for simulations
sims <- NULL

# do simulations
total_iter <- length(b0)*length(k)*length(n)
iter <- 0
pb <- txtProgressBar(min = 0, max = total_iter, style = 3)
for (m in 1:length(b0)) {  # loop over intercepts
  #cat(paste("-- b0 = ", b0[m], "\n"))  # report intercept
  for (j in 1:length(k)) {  # loop over number of variables 
    #cat(paste("---- k = ", k[j], "\n"))  # report number of variables
    for (i in 1:length(n)) {  # loop over sample sizes
      #cat(paste("------ n = ", n[i], "\n"))  # report sample sizes
      sims0 <- simulate(n[i], k[j], b0[m], b1)  # simulate for given parameters
      sims <- rbind(sims, sims0)  # combine with previous data
      #cat("\n")  # new line
      iter <- iter + 1
      setTxtProgressBar(pb, iter)
    }
  }
}

# fix rownames
rownames(sims) <- 1:nrow(sims)

# write data
cat("\n\nsaving simulations to R/simulations/sims.csv...\n\n")
write_csv(sims, "R/simulations/sims.csv")
