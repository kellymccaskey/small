
# load packages
library(readr)  # for read_csv()
library(arm)  # for rescale()

# load data
weisiger_raw <- read_tsv("weisiger-replication/data/weisiger-raw.tab")

# keep cases where default == 1 (not sure why)
weisiger <- weisiger_raw[weisiger_raw$default == 1, ]

# subset vars used
vars <- c("resist", "polity_conq", "lndist",
          "terrain", "soldperterr", "gdppc2", "coord")
weisiger <- weisiger[, vars]

# listwise delete
weisiger <- na.omit(weisiger)

# rescale vars
#   mean = 0, sd = 1/2 for continuous vars
#   mean = 0 for binary vars
vars <- names(weisiger)
for (i in 2:length(vars)) {  # start at 2 -> don't rescale binary outcome
  temp_var <- weisiger[[i]]
  temp_var <- temp_var
  new_temp_var <- rescale(temp_var)
  weisiger[[i]] <- new_temp_var
}
#rm(temp_var, new_temp_var)

# quick look at cleaned data
glimpse(weisiger)

# write new data set
write_csv(weisiger, "weisiger-replication/data/weisiger.csv")
