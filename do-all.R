
# note: make sure that the working directory is set to the project folder.

# clear working directory
rm(list = ls())

# some options
install_packages <- FALSE
run_sims <- FALSE  # it takes about 5 hours to run these

# install packages
if (install_packages == TRUE) {
  # install packages from CRAN
  install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "arm", "texreg",
                     "brglm", "scoring"))
  # install packages from github
  install.packages("devtools")
  devtools::install_github("carlislerainey/separation")
}

# load packages
library(readr)  # for read_csv() and write_csv()
library(dplyr)  # for data management
library(tidyr)  # for spreading data
library(ggplot2)  # for graphics
library(grid)  # for drawing graphics in grids
library(arm)  # for rescale()
library(texreg)  # for printing tables of coefs
library(MASS)  # for mvrnorm()
library(brglm)  # for pmle
library(logistf)  # for pmle
library(scoring)  # for Brier- and log-scores
library(separation)  # for calculating qis (from GitHub)

# set seed
# > runif(1)
# [1] 0.541404
set.seed(541404)

# ggplot theme to use throughout
theme <- theme_bw()

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

# ------------------------------ #
# george and epstein replication #
# ------------------------------ #

# do analysis and make plots
source("weisiger-replication/R/analysis.R")


