
# load data
cat("\nload orginial data...\n\n")
weisiger_raw <- read_tsv("weisiger-replication/data/weisiger-raw.tab")

# quick look 
glimpse(weisiger_raw)

# keep cases where default == 1 (not sure why)
cat("\nkeep cases where default == 1...\n")
weisiger <- weisiger_raw[weisiger_raw$default == 1, ]

# subset vars used
cat("\ndrop variables not used...\n")
vars <- c("resist", "polity_conq", "lndist",
          "terrain", "soldperterr", "gdppc2", "coord")
weisiger <- weisiger[, vars]

# listwise delete
cat("\nlistwise delete...\n")
weisiger <- na.omit(weisiger)

# rescale vars
cat("\nrescale explanatory variables...\n\n")
#   mean = 0, sd = 1/2 for continuous vars
#   mean = 0 for binary vars
vars <- names(weisiger)
for (i in 2:length(vars)) {  # start at 2 -> don't rescale binary outcome
  temp_var <- weisiger[[i]]
  temp_var <- temp_var
  new_temp_var <- rescale(temp_var)
  weisiger[[i]] <- new_temp_var
}
rm(temp_var, new_temp_var)

# quick look at cleaned data
glimpse(weisiger)

# write new data set
cat("\nwrite new data set to file...\n")
write_csv(weisiger, "weisiger-replication/data/weisiger.csv")
