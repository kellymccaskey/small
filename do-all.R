# global parameters

run_sims <- TRUE  # it takes about 5 hours to run these

# ----------------------- #
# monte carlo simulations #
# ----------------------- #

# do monte carlo simulations
if (run_sims == TRUE) { source("R/simulations.R") }

# plot simulation results
source("R/plot-simulations.R")

# -------------------- #
# weisiger replciation #
# -------------------- #

# clean raw data
source("weisiger-replication/R/clean-data.R")

# do analysis and make plots
source("weisiger-replication/R/analysis.R")


