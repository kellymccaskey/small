
# plot parameters
ann_size <- 3
ann_color <- "grey50"

# read data
sims <- read_csv("R/simulations/sims.csv")

# create factors
sims <- mutate(sims, k_factor = factor(paste("N. Variables = ", k, sep = "")))
sims <- mutate(sims, b0_factor = factor(paste("Intercept = ", b0, sep = "")))
sims <- mutate(sims, b0_factor = reorder(b0_factor, prop_ones))

# basic plot
cat("\n\nmaking and saving plots...\n\n")
gg <- ggplot(sims, aes(x = n, color = method)) + 
  facet_grid(k_factor ~ b0_factor) +
  theme +
  labs(x = "Sample Size")

# ev
gg + geom_line(aes(y = ev)) +
  labs(y = "Expected Value") +
  labs(title = "Expected Value of ML and PML Estimators")
ggsave("manuscript/figs/sims-ev.pdf", width = 8, height = 5)

# bias
gg + geom_line(aes(y = bias)) +
  labs(y = "Bias") +
  labs(title = "Bias of ML and PML Estimators")
ggsave("manuscript/figs/sims-bias.pdf", width = 8, height = 5)

# percent bias
gg + geom_line(aes(y = ev)) +
  labs(y = "Percent Bias") +
  labs(title = "Percent Bias of ML and PML Estimators")
ggsave("manuscript/figs/sims-percent-bias.pdf", width = 8, height = 5)

# var
gg + geom_line(aes(y = var)) +
  labs(y = "Variance") +
  labs(title = "Variance of ML and PML Estimators")
ggsave("manuscript/figs/sims-var.pdf", width = 8, height = 5)

# mse
gg + geom_line(aes(y = mse)) +
  labs(y = "Mean-Squared Error") +
  labs(title = "Mean-Squared Error of ML and PML Estimators")
ggsave("manuscript/figs/sims-mse.pdf", width = 8, height = 5)

# variance inflation
var_df <- select(sims, n, k_factor, b0_factor, var, method)
var_infl_df <- spread(var_df, method, var)
var_infl_df <- mutate(var_infl_df, var_infl = 100*(ML/PML - 1))
ggplot(var_infl_df, aes(x = n, y = var_infl)) + 
  geom_line() +
  facet_grid(k_factor ~ b0_factor) +
  theme +
  labs(x = "Sample Size") +
  labs(y = "Variance Inflation") +
  labs(title = "Variance Inflation (%) of ML Relative to PML")
ggsave("manuscript/figs/sims-var-infl.pdf", width = 8, height = 5)

# mse inflation
mse_df <- select(sims, n, k_factor, b0_factor, mse, method)
mse_infl_df <- spread(mse_df, method, mse)
mse_infl_df <- mutate(mse_infl_df, mse_infl = 100*(ML/PML - 1))
ggplot(mse_infl_df, aes(x = n, y = mse_infl)) + 
  geom_line() +
  facet_grid(k_factor ~ b0_factor) +
  theme +
  labs(x = "Sample Size") +
  labs(y = "Mean-Squared Error Inflation") +
  labs(title = "Mean-Squared Error Inflation (%) of ML Relative to PML")
ggsave("manuscript/figs/sims-mse-infl.pdf", width = 8, height = 5)
