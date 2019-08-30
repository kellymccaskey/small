# phony
all: dag sims simplots ge weisiger manuscript readme computedvalues rep
	rm -f Rplots.pdf

dag: makefile-dag.png
sims: simulations/simulations.rds simulations/sample-size-simulations.rds 
simplots: manuscript/figs/contrib-bias-var-scatter.pdf manuscript/figs/relcontrib-n-scatter.pdf manuscript/figs/relcomb-combined.pdf manuscript/figs/sims-ev.pdf manuscript/figs/sims-bias.pdf manuscript/figs/sims-percent-bias.pdf manuscript/figs/sims-var.pdf manuscript/figs/sims-mse.pdf manuscript/figs/sims-var-infl.pdf manuscript/figs/sims-mse-infl.pdf manuscript/figs/mse-infl-larger-samples-4cat-slope.pdf manuscript/figs/mse-infl-larger-samples-4cat-intercept.pdf manuscript/figs/mse-infl-larger-samples.pdf
ge: manuscript/figs/ge-coefs.pdf manuscript/figs/ge-in-sample-fit.pdf manuscript/figs/ge-out-sample-fit.pdf manuscript/figs/ge-perc-change.pdf manuscript/figs/ge-qis.pdf
weisiger: weisiger-replication/data/weisiger.csv manuscript/figs/weisiger-coefs.pdf manuscript/figs/weisiger-in-sample-fit.pdf manuscript/figs/weisiger-out-sample-fit.pdf manuscript/figs/weisiger-perc-change.pdf manuscript/figs/weisiger-qis.pdf
manuscript: manuscript/small.pdf
readme: README.md
computedvalues: computed-values.pdf

# draw makefile dag
makefile-dag.png: makefile-dag.R Makefile
	Rscript $<

# do simulations (figures 2-5)
simulations/simulations.rds: R/simulations.R
	Rscript $<
	
# create figures 2-5
manuscript/figs/contrib-bias-var-scatter.pdf manuscript/figs/relcontrib-n-scatter.pdf manuscript/figs/relcomb-combined.pdf manuscript/figs/sims-ev.pdf manuscript/figs/sims-bias.pdf manuscript/figs/sims-percent-bias.pdf manuscript/figs/sims-var.pdf manuscript/figs/sims-mse.pdf manuscript/figs/sims-var-infl.pdf manuscript/figs/sims-mse-infl.pdf: R/plot-simulations.R simulations/simulations.rds
	Rscript $<
	
# do sample size simulations (figure 8)	
simulations/sample-size-simulations.rds : R/sample-size-simulations.R
	Rscript $<
	
# create figure 8 
manuscript/figs/mse-infl-larger-samples-4cat-slope.pdf manuscript/figs/mse-infl-larger-samples-4cat-intercept.pdf manuscript/figs/mse-infl-larger-samples.pdf: R/plot-sample-size-simulations.R simulations/sample-size-simulations.rds
	Rscript $<
	
# george and epstein re-analysis
manuscript/figs/ge-coefs.pdf manuscript/figs/ge-in-sample-fit.pdf manuscript/figs/ge-out-sample-fit.pdf manuscript/figs/ge-perc-change.pdf manuscript/figs/ge-qis.pdf: ge-replication/R/analysis.R ge-replication/data/ge.csv
	Rscript $<
	
# weisiger re-analysis
manuscript/figs/weisiger-coefs.pdf manuscript/figs/weisiger-in-sample-fit.pdf manuscript/figs/weisiger-out-sample-fit.pdf manuscript/figs/weisiger-perc-change.pdf manuscript/figs/weisiger-qis.pdf: weisiger-replication/R/analysis.R weisiger-replication/data/weisiger.csv
	Rscript $<
	
weisiger-replication/data/weisiger.csv: weisiger-replication/R/clean-data.R  weisiger-replication/data/weisiger-raw.tab
	Rscript $<
	
# manuscript and appendix
manuscript/small.pdf: manuscript/small.tex manuscript/bibliography.bib manuscript/figs/illustrate-bias-annotated.pdf manuscript/figs/contrib-bias-var-scatter.pdf manuscript/figs/relcontrib-n-scatter.pdf manuscript/figs/relcomb-combined.pdf manuscript/figs/sims-ev.pdf manuscript/figs/sims-bias.pdf manuscript/figs/sims-percent-bias.pdf manuscript/figs/sims-var.pdf manuscript/figs/sims-mse.pdf manuscript/figs/sims-var-infl.pdf manuscript/figs/sims-mse-infl.pdf manuscript/figs/mse-infl-larger-samples-4cat-slope.pdf manuscript/figs/mse-infl-larger-samples-4cat-intercept.pdf manuscript/figs/mse-infl-larger-samples.pdf manuscript/figs/ge-coefs.pdf manuscript/figs/ge-in-sample-fit.pdf manuscript/figs/ge-out-sample-fit.pdf manuscript/figs/ge-perc-change.pdf manuscript/figs/ge-qis.pdf weisiger-replication/data/weisiger.csv manuscript/figs/weisiger-coefs.pdf manuscript/figs/weisiger-in-sample-fit.pdf manuscript/figs/weisiger-out-sample-fit.pdf manuscript/figs/weisiger-perc-change.pdf manuscript/figs/weisiger-qis.pdf
  # cd into manuscript so that pdflatex runs in the doc directory
	cd manuscript; pdflatex small
	cd manuscript; bibtex small
	cd manuscript; pdflatex small 
	cd manuscript; pdflatex small 
	cd manuscript; rm -f *.bbl *.log *.synctex.gz *.aux *.out *blg
  # cd into manuscript so that pdflatex runs in the doc directory
	cd manuscript; pdflatex small-appendix
	cd manuscript; bibtex small-appendix
	cd manuscript; pdflatex small-appendix 
	cd manuscript; pdflatex small-appendix 
	cd manuscript; rm -f *.bbl *.log *.synctex.gz *.aux *.out *blg	

# render README
README.md: README.Rmd
	Rscript -e 'rmarkdown::render("$<")'
	
# render computed values
computed-values.pdf: computed-values.Rmd simulations/simulations.rds simulations/sample-size-simulations.rds ge-replication/R/analysis.R ge-replication/data/ge.csv
	Rscript -e 'rmarkdown::render("$<")'	
	rm -f computed-values.log
	
# make replication .zip file
rep: computed-values.pdf README.md manuscript/small.pdf
	rm -f dataverse.zip
	mkdir project-directory
	cp -R manuscript project-directory 
	cp -R ge-replication project-directory 
	cp -R simulations project-directory 
	cp -R weisiger-replication project-directory
	cp computed-values.pdf project-directory
	cp computed-values.Rmd project-directory
	cp README.md project-directory
	cp README.Rmd project-directory
	zip -r project-directory.zip project-directory
	mkdir dataverse
	cp project-directory.zip dataverse
	cp README.md dataverse
	zip -r dataverse.zip dataverse
	rm -r dataverse
	rm -r project-directory


# cleaning phonys

cleandag: 	
	rm -f makefile-dag.png

cleansims: 	
	rm -f simulations/simulations.rds 
	rm -f simulations/sample-size-simulations.rds
	rm -f simulations-progress.log 
	rm -f sample-size-simulations-progress.log
	
cleansimplots:
	rm -f manuscript/figs/contrib-bias-var-scatter.pdf manuscript/figs/relcontrib-n-scatter.pdf manuscript/figs/relcomb-combined.pdf manuscript/figs/sims-ev.pdf manuscript/figs/sims-bias.pdf manuscript/figs/sims-percent-bias.pdf manuscript/figs/sims-var.pdf manuscript/figs/sims-mse.pdf manuscript/figs/sims-var-infl.pdf manuscript/figs/sims-mse-infl.pdf manuscript/figs/mse-infl-larger-samples-4cat-slope.pdf manuscript/figs/mse-infl-larger-samples-4cat-intercept.pdf manuscript/figs/mse-infl-larger-samples.pdf
	
cleange:
	rm -f manuscript/figs/ge-coefs.pdf manuscript/figs/ge-in-sample-fit.pdf manuscript/figs/ge-out-sample-fit.pdf manuscript/figs/ge-perc-change.pdf manuscript/figs/ge-qis.pdf
	rm -f ge-replication/out-of-sample-fit.rds ge-replication/qi-fd.rds ge-replication/qi-probs.rds ge-replication/qi-rr.rds

cleanweisiger: 
	rm -f weisiger-replication/data/weisiger.csv
	rm -f manuscript/figs/weisiger-coefs.pdf manuscript/figs/weisiger-in-sample-fit.pdf manuscript/figs/weisiger-out-sample-fit.pdf manuscript/figs/weisiger-perc-change.pdf manuscript/figs/weisiger-qis.pdf

cleanmanuscript: 
	rm -f manuscript/small.pdf manuscript/small-appendix.pdf
	cd manuscript; rm -f *.bbl *.log *.synctex.gz *.aux *.out *blg	
	
cleanreadme:
	rm -f README.md
	rm -f package-versions.csv
	
cleancomputedvalues:
	rm -f computed-values.pdf

cleanALL: cleandag  cleansims cleansimplots cleange cleanweisiger cleanmanuscript cleanreadme cleancomputedvalues
	rm -f Rplots.pdf
