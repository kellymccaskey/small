
# clear working directory
rm(list = ls())

# load packages
library(readr)
library(texreg)
library(MASS)
library(brglm)
library(scoring)
library(ggplot2)

# load data
vars <- c("resist", "polity_conq", "lndist",
          "terrain", "soldperterr", "gdppc2", "coord", "default")
d <- na.omit(read_tsv("weisiger-replication/data/conq_ins_data.tab")[, vars])
d <- d[d$default == 1, ]

# write data file for example code
write_csv(d, "weisiger-replication/data/weisiger.csv")


sort(names(d2))

# create variables
d$terrain_alt <- d$terrain/100
d$gdppc2_alt <- d$gdppc2/1000

# create function
f <- resist ~ polity_conq + lndist + terrain_alt + soldperterr + gdppc2_alt + coord

# estimate models
mle <- glm(f, d, family = "binomial")
pmle <- brglm(f, d, family = "binomial")

screenreg(list(mle, pmle), stars = 0.1)
coef(pmle)/coef(mle)

# plot the coefficients
# function to tidy the models
tidy_models <- function(model_list, model_names) {
  df <- NULL
  n_models <- length(model_list)
  for (i in 1:n_models) {
    model <- model_list[[i]]
    est <- coef(model)
    se <- sqrt(diag(vcov(model)))
    df0 <- data.frame(var_name = names(coef(model)),
                     est = est,
                     se = se,
                     lwr_se = est - se,
                     upr_se = est + se,
                     lwr_90 = est - 1.64*se,
                     upr_90 = est + 1.64*se) 
    df0$model_name <- model_names[i]
    df <- rbind(df, df0)
  }
  return(df)
}
models_df <- tidy_models(list(mle, pmle), c("mle", "pmle"))

gg <- ggplot(models_df, aes(var_name, est, 
                      ymin = lwr_90,
                      ymax = upr_90,
                      color = model_name)) + 
  geom_pointrange(width = 0, position = position_dodge(width = 0.2)) +
  coord_flip()
ggsave("manuscript/figs/weisiger-coefs.pdf", gg)

# plot change in coefficients
percent_change <- 100*(coef(pmle)/coef(mle) - 1)
change_df <- data.frame(var_names = names(coef(mle)),
                        percent_change = percent_change)
change_df$var_names <- reorder(change_df$var_names, change_df$percent_change)
gg <- ggplot(change_df, aes(x = var_names, y = percent_change)) + 
  geom_bar(stat = "identity") + coord_flip()
ggsave("manuscript/figs/weisiger-perc-change.pdf", gg)

# in-sample fit
brier.mle <- brierscore(d$resist ~ predict(mle, type = "response")); mean(brier.mle)
brier.pmle <- brierscore(d$resist ~ predict(pmle, type = "response")); mean(brier.pmle)
log.mle <- logscore(d$resist ~ predict(mle, type = "response")); mean(log.mle)
log.pmle <- logscore(d$resist ~ predict(pmle, type = "response")); mean(log.pmle)

# out-sample fit
p <- matrix(NA, ncol = 2, nrow = nrow(d))
colnames(p) <- c("mle", "pmle")
for (i in 1:nrow(d)) {
  # partition training and test sets
  d.train <- d[-i, ]
  d.test <- d[i, ]
  # fit models to training data
  mle <- glm(f, d.train, family = "binomial")
  pmle <- brglm(f, d.train, family = "binomial")
  # predict test set
  p[i, "mle"] <- predict(mle, newdata = d.test, type = "response")
  p[i, "pmle"] <- predict(pmle, newdata = d.test, type = "response")
}
brier.mle <- brierscore(d$resist ~ p[, "mle"]); mean(brier.mle)
brier.pmle <- brierscore(d$resist ~ p[, "pmle"]); mean(brier.pmle)
log.mle <- logscore(d$resist ~ p[, "mle"]); mean(log.mle)
log.pmle <- logscore(d$resist ~ p[, "pmle"]); mean(log.pmle)

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

# calculating qis
# creating beta.hats; X.lo, X.hi, and X.c matrices; Sigmas; and beta.tildes
beta.hat.mle <- coef(mle)
beta.hat.pmle <- coef(pmle)
X.hi <- c(1, x.polity_conq.dem, x.lndist, x.terrain_alt, x.soldperterr, x.gdppc2_alt, x.coord.hi)
X.lo <- c(1, x.polity_conq.dem, x.lndist, x.terrain_alt, x.soldperterr, x.gdppc2_alt, x.coord.lo)
X.c <- rbind(X.hi, X.lo)
Sigma.mle <- vcov(mle)
beta.tilde.mle <- mvrnorm(10000, beta.hat.mle, Sigma.mle)
Sigma.pmle <- vcov(pmle)
beta.tilde.pmle <- mvrnorm(10000, beta.hat.pmle, Sigma.pmle)

# MLE first differences
p.tilde.mle <- plogis(X.c%*%t(beta.tilde.mle))
fd.tilde.mle <- p.tilde.mle[1, ] - p.tilde.mle[2, ]
rr.tilde.mle <- p.tilde.mle[1, ]/p.tilde.mle[2, ]
p.hat.mle <- plogis(X.c%*%(beta.hat.mle))
fd.hat.mle <- p.hat.mle[1, ] - p.hat.mle[2, ]
rr.hat.mle <- p.hat.mle[1, ]/p.hat.mle[2, ]

# PMLE first differences
p.tilde.pmle <- plogis(X.c%*%t(beta.tilde.pmle))
fd.tilde.pmle <- p.tilde.pmle[1, ] - p.tilde.pmle[2, ]
rr.tilde.pmle <- p.tilde.pmle[1, ]/p.tilde.pmle[2, ]
p.hat.pmle <- plogis(X.c%*%(beta.hat.pmle))
fd.hat.pmle <- p.hat.pmle[1, ] - p.hat.pmle[2, ]
rr.hat.pmle <- p.hat.pmle[1, ]/p.hat.pmle[2, ]