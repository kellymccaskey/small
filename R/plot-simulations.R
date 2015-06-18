
# clear workspace
rm(list = ls())

# read data
sims <- read_csv("R/simulations/sims.csv")

# create factors
sims <- mutate(sims, k_factor = factor(paste("variables = ", k, sep = "")))
sims <- mutate(sims, b0_factor = factor(paste("intercept = ", b0, sep = "")))
sims <- mutate(sims, b0_factor = reorder(b0_factor, prop_ones))

# percent bias coef
sims <- mutate(sims, perc_bias_coef = 100*(e_coef/true_coef - 1))

# plot for coef
gg <- ggplot(sims, aes(x = n, y = perc_bias_coef, color = method)) + 
  geom_line() + 
  facet_wrap(k_factor ~ b0_factor)

# save plot
ggsave("manuscript/figs/sims-coef.pdf", gg, width = 8, height = 6)
