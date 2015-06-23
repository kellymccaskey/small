
# note: make sure that the working directory is set to the project folder.

# clear working directory
rm(list = ls())

# set seed
# > runif(1)
# [1] 0.541404
set.seed(541404)

# global parameters
run_sims <- TRUE  # it takes about 5 hours to run these

# load packages
library(readr)  # for read_csv() and write_csv()
library(dplyr)  # for data managements
library(ggplot2)  # for graphics
library(arm)  # for rescale()
library(texreg)  # for printing tables of coefs
library(MASS)  # for mvrnorm()
library(brglm)  # for pmle
library(scoring)  # for Brier- and log-scores

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


