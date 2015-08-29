
# plot parameters
ann_size <- 3
ann_color <- "grey50"

# read data
sims <- read_csv("R/simulations/sims.csv")

# create table of simulation estimates of bias and variance
sims_table <- dplyr::select(sims, n, k, b0, method, bias, var)
filter(sims_table, n == 60 & k == 6 & b0 == 0.0)

s1 <- spread(dplyr::select(sims_table, n, k, b0, method, bias), method, bias)
colnames(s1)[4:5] <- c("ml_bias", "pml_bias")
s2 <- spread(dplyr::select(sims_table, n, k, b0, method, var), method, var)
colnames(s2)[4:5] <- c("ml_var", "pml_var")

# scatterplot of mean and var contributions
sims_wide <- left_join(s1, s2)
sims_wide <- mutate(sims_wide, var_contrib = 100*ml_var/(pml_var + pml_bias^2))
sims_wide <- mutate(sims_wide, bias_contrib = 100*(ml_bias^2)/(pml_var + pml_bias^2))
sims_wide <- mutate(sims_wide, rel_contrib = var_contrib/bias_contrib)
gg1 <- ggplot(sims_wide, aes(x = var_contrib, 
                      y = bias_contrib,
                      color = factor(b0),
                      shape = factor(k),
                      size = n)) + 
  geom_point(alpha = 0.8) +
  scale_color_brewer(type = "qual", palette = 2) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks() +
  labs(title = "The Contributions of the Bias and Variance\nto the Mean Squared Error",
       x = "Contribution of Variance to MSE Inflation",
       y = "Contribution of Bias to Variance Inflation",
       color = "Intercept",
       size = "Sample Size",
       shape = "Number of Variables") +
  theme
ggsave("manuscript/figs/contrib-bias-var-scatter.pdf", gg1,  width = 8, height = 5)

# scatter plot of relative contributions and sample size
gg2 <- ggplot(sims_wide, aes(x = n, 
                      y = rel_contrib,
                      color = factor(b0),
                      shape = factor(k))) + 
  geom_point(alpha = 0.8, size = 3.3) +
  scale_color_brewer(type = "qual", palette = 2) + 
  #scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks() +
  labs(title = "The Relative Contribution of the Variance Compared\nto the Bias as the Sample Size Varies",
       x = "Sample Size",
       y = "Relative Contribution of Variance Compared to Bias",
       color = "Intercept",
       shape = "Number of Variables") +
  theme
ggsave("manuscript/figs/relcontrib-n-scatter.pdf", gg2, width = 8, height = 5)

# combine the two previous plots
pdf("manuscript/figs/relcomb-combined.pdf", width = 15, height = 6)
grid.arrange(gg1, gg2, ncol=2)
dev.off()

# create factors
sims <- mutate(sims, k_factor = factor(paste("k == ", k, sep = "")))
sims <- mutate(sims, b0_factor = factor(paste("beta[cons] == ", b0, sep = "")))
sims <- mutate(sims, b0_factor = reorder(b0_factor, prop_ones))

# basic plot
cat("\n\nmaking and saving plots...\n\n")
gg <- ggplot(sims, aes(x = n, color = method, linetype = method)) + 
  facet_grid(k_factor ~ b0_factor, labeller = "label_parsed") +
  theme + 
	scale_color_manual(values = c("#998ec3", "#f1a340")) +
	labs(x = "Sample Size (N)", 
  		 color = "Method", linetype = "Method")

# ev
gg + geom_line(aes(y = ev), size = 1.1) +
  labs(y = "Expected Value") +
  labs(title = "Expected Value of ML and PML Estimators")
ggsave("manuscript/figs/sims-ev.pdf", width = 8, height = 5)

# bias
gg + geom_line(aes(y = bias), size = 1.1) +
  labs(y = "Bias") +
  labs(title = "Bias of ML and PML Estimators")
ggsave("manuscript/figs/sims-bias.pdf", width = 8, height = 5)

# percent bias
gg + geom_line(aes(y = percent_bias), size = 1.1) +
  labs(y = "Percent Bias") +
  labs(title = "Percent Bias of ML and PML Estimators")
ggsave("manuscript/figs/sims-percent-bias.pdf", width = 8, height = 5)

# var
gg + geom_line(aes(y = var), size = 1.1) +
  labs(y = "Variance") +
  labs(title = "Variance of ML and PML Estimators")
ggsave("manuscript/figs/sims-var.pdf", width = 8, height = 5)

# mse
gg + geom_line(aes(y = mse), size = 1.1) +
  labs(y = "Mean-Squared Error") +
  labs(title = "Mean-Squared Error of ML and PML Estimators")
ggsave("manuscript/figs/sims-mse.pdf", width = 8, height = 5)

# variance inflation
var_df <- dplyr::select(sims, n, k_factor, b0_factor, var, method)
var_infl_df <- spread(var_df, method, var)
var_infl_df <- mutate(var_infl_df, var_infl = 100*(ML/PML - 1))
ggplot(var_infl_df, aes(x = n, y = var_infl), size = 1.1) + 
  geom_line() +
  facet_grid(k_factor ~ b0_factor) +
  theme +
	scale_color_manual(values = c("#998ec3", "#f1a340")) +
	labs(x = "Sample Size") +
  labs(y = "Variance Inflation") +
  labs(title = "Variance Inflation (%) of ML Relative to PML")
ggsave("manuscript/figs/sims-var-infl.pdf", width = 8, height = 5)

# mse inflation
mse_df <- dplyr::select(sims, n, k_factor, b0_factor, mse, method)
mse_infl_df <- spread(mse_df, method, mse)
mse_infl_df <- mutate(mse_infl_df, mse_infl = 100*(ML/PML - 1))
ggplot(mse_infl_df, aes(x = n, y = mse_infl), size = 1.1) + 
  geom_line() +
  facet_grid(k_factor ~ b0_factor) +
  theme +
  scale_y_log10(limits = c(1, 1000), 
                breaks = c(1, 10, 100, 1000),
                minor_breaks = NULL) +
  annotation_logticks(sides = "lr") + 
	labs(x = "Sample Size") +
  labs(y = "Mean-Squared Error Inflation") +
  labs(title = "Mean-Squared Error Inflation (%) of ML Relative to PML")
ggsave("manuscript/figs/sims-mse-infl.pdf", width = 8, height = 5)
