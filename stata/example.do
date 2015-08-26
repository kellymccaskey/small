* set working directory and load data
cd "~/dropbox/projects/small"
insheet using "stata/GE.csv", clear

* install firthlogit
findit firthlogit

/* estimate logistic regression with pmle 
and see coefficient values, standard errors, p-values, etc.*/ 
firthlogit court dq cr pc ag sp pe cc ap dc st sg 

* see help file for more 
help firthlogit