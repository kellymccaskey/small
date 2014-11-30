rm(list = ls())
library(compactr)
library(logistf)

simulate <- function(n, k, b0, b1, n.sims) {
  X <- cbind(1, matrix(rnorm(n*k), nrow = n))
  b <- c(b0, b1, rep(.15, k - 1))
  p <- plogis(X%*%b)
  mle <- pmle <- prop.ones <- numeric(n.sims)
  for (i in 1:n.sims) {
    y <- rbinom(n, 1, p)
    m.mle <- glm(y ~ X - 1, family = "binomial")
    m.pmle <- logistf(y ~ X - 1)
    if (sum(abs(coef(m.mle)) > 10) == 0) {
      mle[i] <- coef(m.mle)[2]
      pmle[i] <- coef(m.pmle)[2]
      prop.ones[i] <- mean(p)
    }
  }
  prop.ones <- mean(prop.ones)
  e.mle <- mean(mle)
  e.pmle <- mean(pmle)
  true.coef <- b1
  bias.mle <- 100*e.mle/true.coef - 100
  bias.pmle <- 100*e.pmle/true.coef - 100
  res <- list(bias.mle = bias.mle,
              bias.pmle = bias.pmle,
              prop.ones = prop.ones)
  return(res)
}

par(mfrow = c(3, 3), 
    mar = c(.7, .7, .7, .7),
    oma = c(4, 4, 2, 2))
k <- c(3, 6, 9)
n <- c(50, 100, 150, 200)
b1 <- .25
b0 <- c(-1.5, -.75, 0)
n.sims <- 10000
bias.mle <- bias.pmle <- prop.ones <- numeric(length(n))
iter <- 0
for (m in 1:length(b0)) {
  for (j in 1:length(k)) {
    iter <- iter + 1
    for (i in 1:length(n)) {
      sims <- simulate(n[i], k[j], b0[m], b1, n.sims)
      bias.mle[i] <- sims$bias.mle
      bias.pmle[i] <- sims$bias.pmle
      prop.ones[i] <- sims$prop.ones
    }
    prop.ones <- mean(prop.ones)
    eplot(xlim = mm(n), ylim = c(0, 60),
          xlab = "Sample Size", ylab = "Bias (%)",
          ylabpos = 2.3,
          main = paste(round(100*prop.ones), "% events, ", k[j], " predictors", sep = ""))
    if (iter == 1) {
      text(55, bias.mle[1], "MLE", pos = 3, cex = .7)
      text(55, bias.pmle[1], "PMLE", pos = 3, col = "red", cex = .7)
    }
    abline(h = 0, lty = 3, col = 1)
    lines(n, bias.mle, col = 1, lty = 1, lwd = 3)
    lines(n, bias.pmle, col = 2, lty = 3, lwd = 3)
  }
}





