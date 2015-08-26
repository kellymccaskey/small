* set working directory and load data
cd "your working directory"
use GE.dta, clear

* install firthlogit
findit firthlogit

/* estimate logistic regression with pmle 
and see coefficient values, standard errors, p-values, etc.*/ 
firthlogit court dq cr pc ag sp pe cc ap dc st sg 

* see help file for more 
help firthlogit
