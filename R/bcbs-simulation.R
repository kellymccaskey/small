
# load packages
library(boot)

# simulation parameters
n <- 50
n_sims <- 10000
b0 <- 0
b1 <- .5

# generate data
x <- rnorm(n)
p <- plogis(x)

# create functions
mse <- function(est, true) {
  sum((est - t)^2)
}
bs_fn <- function(formula, data, indices) {
  bs_data <- data[indices,] # allows boot to select sample 
  bs_fit <- glm(formula, data = bs_data, family = "binomial")
  return(coef(bs_fit)) 
} 


ml <- pml <- bs <- matrix(NA, nrow = n_sims, ncol = 2)
pb <- txtProgressBar(min = 0, max = n_sims, style = 3)
for (i in 1:n_sims) {
  y <- rbinom(n, 1, p)
  d <- data.frame(x, y)
  ml0 <- glm(y ~ x, family = "binomial", data = d)
  ml[i, ] <- coef(ml0)
  pml0 <- logistf::logistf(y ~ x, data = d)
  pml[i, ] <- coef(pml0)
  bs0 <- boot(data = d, statistic = bs_fn, 
              R = 10000, formula = y ~ x)
  bs[i, ] <- bs0$t0 - (apply(bs0$t, 2, mean)  - bs0$t0)
  setTxtProgressBar(pb, i)
}

apply(ml, 2, mean)
apply(pml, 2, mean)
apply(bs, 2, mean)
apply(ml, 2, var)
apply(pml, 2, var)
apply(bs, 2, var)
