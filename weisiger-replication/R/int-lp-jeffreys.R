lp.jeffreys <- function(beta, X, y) {
  # args have the obv. meanings
  p <- plogis(matrix(X%*%beta))
  W <- Matrix(0, nrow = length(p), ncol = length(p))
  diag(W) <- p*(1 - p)
  I <- crossprod(X, W)%*%X
  lp <- sum(y*log(p)) + sum((1-y)*log(1 - p)) + 0.5*log(det(I))
  return(lp)
}

# # example
# set.seed(1234)
# n <- 100
# beta <- c(1, 1)
# X <- cbind(1, rnorm(n))
# y <- rbinom(n, 1, plogis(X%*%beta))
# for (b1 in -5:5) {
#   lp <- lp.jeffreys(c(1, b1), X, y)
#   cat("b1 = "); cat(b1); cat("\t  -->\t lp = "); cat(lp); cat("\n")
# }
