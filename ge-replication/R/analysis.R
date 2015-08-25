
# plot parameters
ann_size <- 3
ann_color <- "grey50"

# load data
ge <- read_csv("ge-replication/data/ge.csv")

# drop missing
keep <- c("court", "dq", "cr", "pc", "ag", "sp", "pe", "cc", "ap", "dc", "st", "sg")
ge <- na.omit(ge[, keep])

# formula
f <- court ~ dq + cr + pc + ag + sp + pe + cc + ap + dc + st + sg

# estimate models
ls <- lm(f, data = ge)  # linear probability model
mle <- glm(f, data = ge, family = "binomial")
pmle <- brglm(f, data = ge, family = "binomial")
cat(screenreg(list(mle, pmle), stars = 0.1, 
              custom.model.names = c("MLE", "PMLE")))


# count ls predictions outside [0, 1]

pred <- predict(ls)
sort(pred)
sum(pred > 1)
sum(pred < 0)
sum(pred < 0 | pred > 1)/length(pred)

# percent change
cat("\ncalculating percent change...\n\n")
perc_change <- 100*(coef(pmle)/coef(mle) - 1)
print(round(perc_change, 0))

# ------------------------------- #
# plots of coefficients           #
# 1 - coefficients themselves     #
# 2 - changes in the coefficients #
# ------------------------------- #
cat("\nplotting the coefficients...\n\n")

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
  rownames(df) <- NULL
  return(df)
}

# tidy model output
models_df <- tidy_models(list(mle, pmle), c("ML Estimate", "PML Estimate"))

# add variable names for printing
vnp <- c("Intercept",
         "Death-Qualified",
         "Crime",
         "Particularizing Circumstances",
         "Aggravating Factors",
         "State Psychiatric Examination",
         "Political Environment",
         "Court Change",
         "Appellant",
         "Defendant Counsel",
         "State",
         "Solicitor General")
models_df <- mutate(models_df, var_name_print = rep(vnp, 2))
models_df <- mutate(models_df, var_name_print = factor(var_name_print, 
                                                       levels = vnp))

# plot coefficients
gg <- ggplot(subset(models_df, var_name_print != "Intercept"), 
                    aes(var_name_print, est, 
                            ymin = lwr_90,
                            ymax = upr_90,
                            color = model_name,
                            linetype = model_name)) + 
  geom_pointrange(width = 0, position = position_dodge(width = 0.6), size = 0.7) +
  coord_flip() + 
  labs(title = "Logistic Regression Model Explaining\nConservative Court Decisions") + 
  labs(y = "Logistic Regression Coefficients and 90% Confidence Intervals\n(Intercept Not Shown)") + 
  labs(x = NULL) +
  labs(color = "Method", linetype = "Method") + 
  scale_color_manual(values = c("#998ec3", "#f1a340")) +
  annotate("text", .7, 8, label = "N = 64 (29 events)", 
           color = ann_color, size = ann_size) + 
  theme
ggsave("manuscript/figs/ge-coefs.pdf", gg,
       height = 5, width = 9)

# plot change in coefficients
percent_change <- 100*(coef(pmle)/coef(mle) - 1)
change_df <- data.frame(var_name = names(coef(mle)),
                        var_name_print = vnp,
                        percent_change = percent_change)
change_df <- mutate(change_df, var_name_print = reorder(var_name_print, percent_change))
gg <- ggplot(change_df, aes(x = var_name_print, y = percent_change)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Percent Change in Logistic Regression Coefficients") + 
  labs(y = "Percent Change from ML Estimates to PML Estimates") + 
  labs(x = NULL) + 
  theme
ggsave("manuscript/figs/ge-perc-change.pdf", gg,
       height = 3, width = 6)

# ------------------------------------- #
# model fit                             #
# 1 - in-sample predictionand plot      #
# 2 - out-of-sample prediction and plot #
# ------------------------------------- #
cat("\ncalculating and plotting model fit...\n\n")

# in-sample fit
y <- with(ge, court)
p_mle <- predict(mle, type = "response")
p_pmle <- predict(pmle, type = "response")
brier_mle <- brierscore(y ~ p_mle); mean(brier_mle)
brier_pmle <- brierscore(y ~ p_pmle); mean(brier_pmle)
log_mle <- logscore(y ~ p_mle); mean(log_mle)
log_pmle <- logscore(y ~ p_pmle); mean(log_pmle)
in_sample_fit <- matrix(NA, nrow = 2, ncol = 2)
rownames(in_sample_fit) <- c("ML", "PML")
colnames(in_sample_fit) <- c("Brier Score", "Log Score")
in_sample_fit[1, 1] <- mean(brier_mle)
in_sample_fit[2, 1] <- mean(brier_pmle)
in_sample_fit[1, 2] <- mean(log_mle)
in_sample_fit[2, 2] <- mean(log_pmle)
cat("\nin-sample fit\n")
print(in_sample_fit)

# plot in-sample fit
method <- c("ML", "ML", "PML", "PML")
score_type <- c("Brier Score", "Log Score", "Brier Score", "Log Score")
score <- c(mean(brier_mle), mean(log_mle), mean(brier_pmle), mean(log_pmle))
isf_df <- data_frame(method, score_type, score)
gg <- ggplot(isf_df, aes(x = score_type, y = score, fill = method)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "In-Sample Prediction Scores") + 
  labs(y = "Score") + 
  labs(x = "Score Type") +
  labs(fill = "Method") + 
  theme
ggsave("manuscript/figs/ge-in-sample-fit.pdf", gg,
       height = 3, width = 5)

# out-sample fit
n_predict <- nrow(ge)
p_mle <- p_pmle <- numeric(n_predict)
for (i in 1:n_predict) {
  # partition training and test sets
  d_train <- ge[-i, ]
  d_test <- ge[i, ]
  # fit models to training data
  mle0 <- glm(f, d_train, family = "binomial")
  pmle0 <- brglm(f, d_train, family = "binomial")
  # predict test set
  p_mle[i] <- predict(mle0, newdata = d_test, type = "response")
  p_pmle[i] <- predict(pmle0, newdata = d_test, type = "response")
}

y <- with(ge, court)
brier_mle <- brierscore(y ~ p_mle); mean(brier_mle)
brier_pmle <- brierscore(y ~ p_pmle); mean(brier_pmle)
log_mle <- logscore(y ~ p_mle); mean(log_mle)
log_pmle <- logscore(y ~ p_pmle); mean(log_pmle)
out_sample_fit <- matrix(NA, nrow = 2, ncol = 2)
rownames(out_sample_fit) <- c("ML", "PML")
colnames(out_sample_fit) <- c("Brier Score", "Log Score")
out_sample_fit[1, 1] <- mean(brier_mle)
out_sample_fit[2, 1] <- mean(brier_pmle)
out_sample_fit[1, 2] <- mean(log_mle)
out_sample_fit[2, 2] <- mean(log_pmle)
cat("\nin-sample fit\n")
print(out_sample_fit)

# plot out-sample fit
method <- c("ML", "ML", "PML", "PML")
score_type <- c("Brier Score", "Log Score", "Brier Score", "Log Score")
score <- c(mean(brier_mle), mean(log_mle), mean(brier_pmle), mean(log_pmle))
osf_df <- data_frame(method, score_type, score)
gg <- ggplot(osf_df, aes(x = score_type, y = score, fill = method)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Out-of-Sample Prediction Scores") + 
  labs(y = "Score") + 
  labs(x = "Score Type") +
  labs(fill = "Method") + 
  theme
ggsave("manuscript/figs/ge-out-sample-fit.pdf", gg,
       height = 3, width = 5)

# --------------------------------- #
# quanitities of interest for coord #
# 1 - predicted probabilities       #
# 2 - first differences             #
# 3 - risk ratios                   #
# 4 - combine all into one plot     #
# --------------------------------- #
cat("\ncalculating and plotting quantities of interest...\n\n")

# a function to do most of the work
qi <- function(model, sims, var_name, var_quantiles = c(0, 1)) {
  var_values <- quantile(data.frame(ge)[, var_name], var_quantiles)
  X <- set_rest_at_median(f, ge, var_name = var_name, 
                          var_values = var_values)
  X <- list_to_matrix(X, f)
  # estimates
  p <- plogis(X%*%coef(model))
  p_lo_est <- p[1, ]
  p_hi_est <- p[2, ]
  fd_est <- p_hi_est - p_lo_est
  rr_est <- p_hi_est/p_lo_est
  est_df <- data_frame(p_lo_est, p_hi_est, fd_est, rr_est)
  # simulations
  p <- plogis(X%*%t(sims))
  p_lo_sims <- p[1, ]
  p_hi_sims <- p[2, ]
  fd_sims <- p_hi_sims - p_lo_sims
  rr_sims <- p_hi_sims/p_lo_sims
  sims_df <- data_frame(p_lo_sims, p_hi_sims, fd_sims, rr_sims)
  return(list(est_df = est_df, sims_df = sims_df, X = X))
}

# simulate coefficients
sims_mle <- mvrnorm(10000, coef(mle), vcov(mle))
sims_pmle <- mvrnorm(10000, coef(pmle), vcov(pmle))

# calculate qis
qi_mle <- qi(mle, sims_mle, "sg")
qi_mle_est <- with(qi_mle, est_df)
qi_mle_sims <- with(qi_mle, sims_df)
qi_pmle <- qi(pmle, sims_pmle, "sg")
qi_pmle_est <- with(qi_pmle, est_df)
qi_pmle_sims <- with(qi_pmle, sims_df)

method <- c("ML", "ML", "PML", "PML")
sg <- c("Solicitor General Files Amicus Brief", 
           "Solicitor General Does Not File Amicus Brief",
           "Solicitor General Files Amicus Brief", 
           "Solicitor General Does Not File Amicus Brief")
prob <- c(with(qi_mle_est, p_hi_est),
          with(qi_mle_est, p_lo_est),
          with(qi_pmle_est, p_hi_est),
          with(qi_pmle_est, p_lo_est))
prob_df <- data_frame(method, sg, prob)
cat("\npredicted probabilities\n")
print(as.data.frame(prob_df))

# plot
prob_df <- mutate(prob_df, sg = reorder(factor(sg), prob))
prob_gg <- ggplot(prob_df, aes(x = sg, y = prob, color = method)) +
  geom_point(size = 2.2) + 
  geom_line(aes(x = as.numeric(sg)), size = 0.7) + 
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Probability of a Conservative Decision") +
  labs(x = "") + 
  labs(y = "Probability") +
  labs(color = "Method") +
  annotate("text", x = c(2, 1, 2, 1), y = prob, label = round(prob, 2), 
           hjust = c(-0.3, 1.3, -0.3, 1.3), 
           vjust = c(0.5, 1.0, 0.5, 0.0), 
           size = ann_size,
           color = ann_color) + 
  scale_color_manual(values = c("#998ec3", "#f1a340")) +
  theme

# first difference
method = c("ML", "PML")
est <- c(with(qi_mle_est, fd_est),
         with(qi_pmle_est, fd_est))
percent_change <- round(100*(est[2]/est[1] - 1), 0)
caption <- paste("PML estimate is ", abs(percent_change), 
                 "% lower\nthan ML estimate.", sep = "")
lwr_90 <- c(with(qi_mle_sims, quantile(fd_sims, .05)),
            with(qi_pmle_sims, quantile(fd_sims, .05)))
upr_90 <- c(with(qi_mle_sims, quantile(fd_sims, .95)),
            with(qi_pmle_sims, quantile(fd_sims, .95)))
fd_df <- data_frame(method, est, lwr_90, upr_90)
cat("\nfirst differences\n")
print(as.data.frame(fd_df))

# plot
fd_gg <- ggplot(fd_df, aes(method, est, color = method,
                           ymin = lwr_90, ymax = upr_90)) +
  geom_pointrange(size = 0.7) +
  geom_text(aes(label = round(est, 2)), 
            vjust = -0.7, size = ann_size, color = ann_color) +
  coord_flip() + 
  labs(title = "First Difference") +
  labs(x = "Method") + 
  labs(y = "First Difference") +
  annotate("text", y = 0.54, x = 0.7, label = caption, 
           size = ann_size,
           color = ann_color) + 
  scale_color_manual(values = c("#998ec3", "#f1a340")) +
  theme + theme(legend.position = "none") 

# risk ratio
method = c("ML", "PML")
est <- c(with(qi_mle_est, rr_est),
         with(qi_pmle_est, rr_est))
percent_change <- round(100*(est[2]/est[1] - 1), 0)
caption <- paste("PML estimate is ", abs(percent_change), 
                 "% lower\nthan ML estimate.", sep = "")
lwr_90 <- c(with(qi_mle_sims, quantile(rr_sims, .05)),
            with(qi_pmle_sims, quantile(rr_sims, .05)))
upr_90 <- c(with(qi_mle_sims, quantile(rr_sims, .95)),
            with(qi_pmle_sims, quantile(rr_sims, .95)))
rr_df <- data_frame(method, est, lwr_90, upr_90)
cat("\nrisk ratios\n")
print(as.data.frame(rr_df))

# plot
rr_gg <- ggplot(rr_df, aes(method, est, color = method,
                           ymin = lwr_90, ymax = upr_90)) +
  geom_pointrange(size = 0.7) +
  geom_text(aes(label = round(est, 1)), 
            vjust = -0.7, size = ann_size, color = ann_color) +
  coord_flip() + 
  scale_y_log10() +
  labs(title = "Risk Ratio") +
  labs(x = "Method") + 
  labs(y = "Risk Ratio (Log Scale)") +
  annotate("text", y = 10.7, x = 0.7, label = caption,
           size = ann_size,
           color = ann_color) + 
  scale_color_manual(values = c("#998ec3", "#f1a340")) +
  theme + theme(legend.position = "none")

# combine plots
pdf("manuscript/figs/ge-qis.pdf", width = 8, height = 6)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(prob_gg, vp = vplayout(1, 1:2))
print(fd_gg, vp = vplayout(2, 1))
print(rr_gg, vp = vplayout(2, 2))
dev.off()



