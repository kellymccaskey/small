Reproduction Instructions
================

Note: In order to reproduce this analysis, you need the R package
separation from GitHub, which you can install using
`devtools::install_github("carlislerainey/separation")`. See below for
the details about other R pacakges.

## `Makefile`

To reproduce the analysis, first clear all created files. Run:

    make cleanALL

Then reproduce the analysis. Run:

    make

### `make`ing

The `Makfile` describes the structure of the code and allows the user to
reproduce the entire analysis or portions of it.

  - `make all` or just `make` reproduces the entire analysis, including
    the simulations, which take about five hours.
  - `make dag` reproduces a DAG that shows the structure of the
    dependencies in the `Makefile`.
  - `make sims` reproduces the `.rds` files of the simulations (created
    by both `simulations.R` and `sample-size-simulations.R`) and saves
    them in the `simulations` directory. You can monitor the progress of
    `simulations.R` and `sample-size-simulations.R` with
    `simulations.log` and `sample-size-simulations.log`, respectively.
    This takes about five hours.
  - `make simplots` reproduces the figures summarizing the simulations
    (Figures 2-5 and 8 from the main paper as well as figures for the
    appendix) and saves them in the `manuscript/figs` directory.
  - `make ge` reproduces our re-analysis of George and Epstein (1992)
    and saves the figures to the `manuscript/figs` directory.
  - `make weisiger` reproduces our re-analysis of Weisiger (2014) and
    saves the figures to the `manuscript/figs` directory.
  - `make manuscript` recompiles the LaTeX manuscript `small.tex` into
    `small.pdf` and `small-appendix.tex` into `small-appendix.pdf`. It
    automatically handles the bibliography.
  - `make readme` knits this document from the `.Rmd` file.
  - `make computedvalues` knits `computed-values.pdf`, which creates
    tables of the numeric quantities reported in the text.

### Cleaning

You can clear any produced files from by the code by running `make
clean*` and using any of the phonies above. For example,`make cleandag`
removes the figure `makefile-dag.png`. To clean the entire directory,
run `make cleanALL` (I put `ALL` in caps to remind myself of the
consequences).

### `illustrate-bias-annotated.pdf`

We used a combination of ggplot and Apple Keynote to create Figure 1
`manuscript/figs/illustrate-bias-annotated.pdf`, which illustrates the
source of the small sample bias. The R script `R/illustrate-bias.R`
create the underlying plot `manuscript/figs/illustrate-bias.pdf`, but we
added the annotations manually in Keynote.

## Additional Notes

  - The simulations take about five hours. We’ve set them up to run in
    parallel on four clusters. You might speed this up with a change
    here.
  - The code automatically stores the packages used in the last run in
    the file `session-info.txt`.
  - There is no log file, but all the figures are created and saved in
    the `manuscript/figs` directory. All quantities reported in the
    manuscript are computed and/or reported in the file
    `computed-values.pdf`.

## R

We ran the analysis using the system below.

``` r
R.version
```

    ##                _                           
    ## platform       x86_64-apple-darwin15.6.0   
    ## arch           x86_64                      
    ## os             darwin15.6.0                
    ## system         x86_64, darwin15.6.0        
    ## status                                     
    ## major          3                           
    ## minor          6.1                         
    ## year           2019                        
    ## month          07                          
    ## day            05                          
    ## svn rev        76782                       
    ## language       R                           
    ## version.string R version 3.6.1 (2019-07-05)
    ## nickname       Action of the Toes

## R Packages

In order to reproduce the analysis, several R packages, which you can
install with the following
code:

``` r
# list of packages on CRAN used in this project (exclusing base packages)
pkg <- c("brglm", 
         "brglm2", 
         "clusterGeneration",
         "devtools",
         "doParallel", 
         "doRNG",
         "foreach", 
         "ggraph",
         "gridExtra", 
         "gridExtra", 
         "igraph",
         "kableExtra",
         "logistf", 
         "quantreg", 
         "scoring", 
         "texreg", 
         "tidyverse", 
         "xtable")
```

To install these packages, you can run the code above along with the
command below.

    install.packages(pkg, repos = "http://cran.rstudio.com")

You also need the package separation from GitHub, which you can install
with the command below.

    devtools::install_github("carlislerainey/separation")

We recommend using the latest version of each package, but the versions
we used are saved to the file `package-versions.csv`.

``` r
library(tidyverse)
library(kableExtra)

devtools::package_info(pkgs = c(pkg, "separation"), dependencies = TRUE) %>%
  select(package, version = ondiskversion, date, source) %>%
  write_csv("package-versions.csv")
```

## Reproducing without `Make`

  - To create `makefile-dag.png`, run the R script `makefile-dag.R`.
  - To do the simulations for figures 2-5 and store them as
    `simulations/simulations.rds`, run the R script `R/simulations.R`.
  - To create the figures based on the simulations above, run the R
    script `R/plot-simulations.R`. These figures are stored as `.pdf`s
    in `manuscript/figs`.
  - To perform the sample size simulations for figure 8 and store them
    as `simulations/sample-size-simulations.rds`, run the R script
    `R/sample-size-simulations.R`.
  - To create the figures based on the sample size simulations above,
    run `R/plot-sample-size-simulations.R`. These figures are stored as
    `.pdf`s in `manuscript/figs`.
  - To reproduce the George and Epstein re-analysis, run the R script
    `ge-replication/R/analysis.R`. Figures are stored as `.pdf`s in
    `manuscript/figs`.
  - To reproduce the Weisiger re-analysis, run the R script
    `weisiger-replication/R/analysis.R`. Figures are stored as `.pdf`s
    in `manuscript/figs`.
  - To compile the manuscript and appendix, compile
    `manuscript/small.tex` and `manuscript/small-appendix.tex`,
    respectively, with pdftex and bibtex.
  - To render the `README.md`, compile `README.Rmd` with knitr.
  - To render the `computed-values.pdf`, compile `computed-values.pdf`
    with knitr.
