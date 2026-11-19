# Run once from this directory before sourcing asset-pricing.R.
install.packages(c(
  "tidyverse", "tidyfinance", "scales", "RSQLite", "dbplyr",
  "lmtest", "sandwich", "rlang", "RPostgres", "parallelly",
  "slider", "frenchdata", "broom", "furrr", "future"
))
