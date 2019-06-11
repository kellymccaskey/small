

* set working directory and load data
* data can be found at https://github.com/kellymccaskey/small/blob/master/stata/ge.csv

cd "your working directory"
insheet using "ge.csv", clear

* install firthlogit
ssc install firthlogit

* estimate logistic regression with pmle
* see coefficient values, standard errors, p-values, etc.
firthlogit court dq cr pc ag sp pe cc ap dc st sg

* see help file for more
help firthlogit
