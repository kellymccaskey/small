
# clear workspace
rm(list = ls())

# read data
sims <- read_csv("R/simulations/sims.csv")

# create factors
sims <- mutate(sims, k_factor = factor(paste(k, " Variables", sep = "")))
sims <- mutate(sims, b0_factor = factor(paste("Intercept = ", b0, sep = "")))
sims <- mutate(sims, b0_factor = reorder(b0_factor, prop_ones))

# percent bias coef
sims <- mutate(sims, perc_bias_coef = 100*(e_coef/true_coef - 1))

# mse reduction
sims <- mutate(sims, perc_bias_coef = 100*(e_coef/true_coef - 1))

# mse reduction
sims <- mutate(sims, se_e_coef = sd_coef/sqrt(n_sims))

# plot for coef ev
gg <- ggplot(sims, aes(x = n, y = e_coef, color = method)) + 
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymax = e_coef + se_e_coef, 
                    ymin = e_coef - se_e_coef),
                width = 3) +
  #geom_smooth(se = FALSE, adjust = 2) +
  facet_grid(k_factor ~ b0_factor)
ggsave("manuscript/figs/sims-coef-ev.pdf", gg, width = 8, height = 6)

# plot for coef percent bias
gg <- ggplot(sims, aes(x = n, y = perc_bias_coef, color = method)) + 
  geom_line() + 
  geom_point() +
  #geom_smooth(se = FALSE, adjust = 2) +
  facet_grid(k_factor ~ b0_factor) +
  labs(title = "Percent Bias in MLE and PMLE Estimators",
       x = "Sample Size",
       y = "Percent Bias",
       color = "Method"); gg
ggsave("manuscript/figs/sims-coef-perc-bias.pdf", gg, width = 8, height = 6)

# plot for coef variance
gg <- ggplot(sims, aes(x = n, y = sd_coef^2, color = method)) + 
  geom_line() + 
  geom_point() +
  #geom_smooth(se = FALSE, adjust = 2) +
  facet_grid(k_factor ~ b0_factor) +
  labs(title = "Variance of the MLE and PMLE Estimators",
       x = "Sample Size",
       y = "Variance",
       color = "Method"); gg
ggsave("manuscript/figs/sims-coef-var.pdf", gg, width = 8, height = 6)

# plot for coef mse
gg <- ggplot(sims, aes(x = n, y = mse_coef, color = method)) + 
  geom_line() + 
  geom_point() +
  #geom_smooth(se = FALSE, adjust = 2) +
  facet_grid(k_factor ~ b0_factor) +
  labs(title = "Mean Squared Error of the MLE and PMLE Estimators",
       x = "Sample Size",
       y = "Mean Squared Error",
       color = "Method"); gg
ggsave("manuscript/figs/sims-coef-mse.pdf", gg, width = 8, height = 6)


