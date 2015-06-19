
# clear working directory
rm(list = ls())

# set working directory
setwd("~/Dropbox/projects/small-sample-logit/weisiger-replication/")

# load packages
library(texreg)
library(brglm)
library(scoring)
library(arm)


# load data
vars <- c("resist", "polity_conq", "lndist",
          "terrain", "soldperterr", "gdppc2", "coord")
d <- na.omit(read.csv("data/conq_ins_data.tab", sep = "\t")[, vars])

# standardize vars
for (i in 2:length(vars)) {
  var <- d[, vars[i]]
  new_var <- rescale(var)
  d[, vars[i]] <- new_var
}

# create variables
#d$terrain_alt <- d$terrain/100
#d$gdppc2_alt <- d$gdppc2/1000

# create function
f <- resist ~ polity_conq + lndist + terrain + soldperterr + gdppc2 + coord

# estimate models
mle <- glm(f, d, family = "binomial")
pmle <- brglm(f, d, family = "binomial")

screenreg(list(mle, pmle), stars = 0.1)
coef(pmle)/coef(mle)

# tidy models
# function to do the work
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

ggplot(models_df, aes(var_name, est, 
                      ymin = lwr_90,
                      ymax = upr_90,
                      color = model_name)) + 
  geom_pointrange(width = 0, position = position_dodge(width = 0.2)) +
  coord_flip()